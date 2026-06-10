module Main where

import           Inventario
import           System.IO
  ( hSetBuffering, hFlush, stdout, BufferMode(..) )
import           System.Directory (doesFileExist, createDirectoryIfMissing)
import           Data.Time        (getCurrentTime)
import           Data.Maybe       (mapMaybe)
import qualified Data.Map.Strict  as Map

-- ---------------------------------------------------------------------------
-- Caminhos dos arquivos de persistência
-- ---------------------------------------------------------------------------

diretorioDados :: FilePath
diretorioDados = "dados"

arquivoInventario :: FilePath
arquivoInventario = diretorioDados ++ "/Inventario.dat"

arquivoLog :: FilePath
arquivoLog = diretorioDados ++ "/Auditoria.log"

-- ---------------------------------------------------------------------------
-- I/O: persistência do inventário
-- ---------------------------------------------------------------------------

-- | Sobrescreve Inventario.dat com o estado atual (via show).
salvarInventario :: Inventario -> IO ()
salvarInventario inv = do
  createDirectoryIfMissing True diretorioDados
  writeFile arquivoInventario (serializarInventario inv)

-- | Carrega o inventário do disco (via reads).
-- Retorna inventário vazio se o arquivo não existir ou estiver corrompido.
carregarInventario :: IO Inventario
carregarInventario = do
  existe <- doesFileExist arquivoInventario
  if not existe
    then do
      putStrLn "  [INFO] Inventario.dat não encontrado. Iniciando com inventário vazio."
      return inventarioVazio
    else do
      conteudo <- readFile arquivoInventario
      case desserializarInventario conteudo of
        Right inv -> return inv
        Left  err -> do
          putStrLn $ "  [AVISO] " ++ err ++ " Iniciando com inventário vazio."
          return inventarioVazio

-- ---------------------------------------------------------------------------
-- I/O: log de auditoria (append-only)
-- ---------------------------------------------------------------------------

-- | Persiste uma LogEntry no arquivo de auditoria (append-only).
-- Usa show para serializar; reads em mostrarLog para desserializar.
salvarEntradaLog :: LogEntry -> IO ()
salvarEntradaLog entry = do
  createDirectoryIfMissing True diretorioDados
  appendFile arquivoLog (show entry ++ "\n")

-- | Constrói uma LogEntry de FALHA e a persiste (usado quando
-- a função pura retorna Left, ou em caso de QueryFail).
registrarFalha :: AcaoLog -> String -> String -> IO ()
registrarFalha acaoTipo det errMsg = do
  agora <- getCurrentTime
  salvarEntradaLog LogEntry
    { timestamp = agora
    , acao      = acaoTipo
    , detalhes  = det
    , status    = Falha errMsg
    }

-- | Lê e exibe o log de auditoria de forma legível.
-- Cada linha é desserializada de volta para LogEntry via reads.
mostrarLog :: IO ()
mostrarLog = do
  existe <- doesFileExist arquivoLog
  if not existe
    then putStrLn "  Nenhum registro de auditoria encontrado."
    else do
      conteudo <- readFile arquivoLog
      let entradas = mapMaybe parsarLinha (lines conteudo)
      if null entradas
        then putStrLn "  O log de auditoria está vazio."
        else do
          putStrLn "\n=== LOG DE AUDITORIA ==="
          mapM_ exibirEntrada entradas
          putStrLn $ "======================== (" ++ show (length entradas) ++ " registros)"
  where
    parsarLinha l = case reads l :: [(LogEntry, String)] of
      [(e, "")] -> Just e
      [(e, _)]  -> Just e
      _         -> Nothing

    exibirEntrada e =
      let statStr = case status e of
            Sucesso   -> "[SUCESSO]"
            Falha msg -> "[FALHA: " ++ msg ++ "]"
      in putStrLn $ "  " ++ show (timestamp e)
                 ++ " | " ++ show (acao e)
                 ++ " | " ++ detalhes e
                 ++ " | " ++ statStr

-- ---------------------------------------------------------------------------
-- Funções auxiliares de leitura do terminal
-- ---------------------------------------------------------------------------

lerLinha :: String -> IO String
lerLinha prompt = do
  putStr prompt
  hFlush stdout
  getLine

lerInt :: String -> IO (Maybe Int)
lerInt prompt = do
  s <- lerLinha prompt
  case reads s :: [(Int, String)] of
    [(n, "")] -> return (Just n)
    _         -> return Nothing

-- ---------------------------------------------------------------------------
-- Exibição de itens
-- ---------------------------------------------------------------------------

exibirItem :: Item -> IO ()
exibirItem item = do
  putStrLn "  ┌────────────────────────────────────"
  putStrLn $ "  │ ID         : " ++ itemID item
  putStrLn $ "  │ Nome       : " ++ nome item
  putStrLn $ "  │ Quantidade : " ++ show (quantidade item)
  putStrLn $ "  │ Categoria  : " ++ categoria item
  putStrLn "  └────────────────────────────────────"

-- ---------------------------------------------------------------------------
-- Menu
-- ---------------------------------------------------------------------------

mostrarMenu :: IO ()
mostrarMenu = do
  putStrLn ""
  putStrLn "  ╔═══════════════════════════════════════╗"
  putStrLn "  ║      SISTEMA DE INVENTÁRIO  v3.0      ║"
  putStrLn "  ╠═══════════════════════════════════════╣"
  putStrLn "  ║  1. Listar todos os itens             ║"
  putStrLn "  ║  2. Buscar item por ID                ║"
  putStrLn "  ║  3. Adicionar novo item               ║"
  putStrLn "  ║  4. Remover item                      ║"
  putStrLn "  ║  5. Atualizar item                    ║"
  putStrLn "  ║  6. Visualizar log de auditoria       ║"
  putStrLn "  ║  0. Sair                              ║"
  putStrLn "  ╚═══════════════════════════════════════╝"

-- ---------------------------------------------------------------------------
-- Handlers — cada handler chama getCurrentTime e delega à função pura,
-- depois persiste o inventário e a LogEntry retornados.
-- ---------------------------------------------------------------------------

handleListar :: Inventario -> IO ()
handleListar inv =
  let itens = listarItens inv
  in if null itens
       then putStrLn "\n  O inventário está vazio."
       else do
         putStrLn $ "\n=== INVENTÁRIO (" ++ show (length itens) ++ " item(s)) ==="
         mapM_ exibirItem itens

handleBuscar :: Inventario -> IO ()
handleBuscar inv = do
  idStr <- lerLinha "\n  ID do item: "
  case buscarItem idStr inv of
    Nothing ->
      registrarFalha QueryFail
        ("buscar ID=\"" ++ idStr ++ "\"")
        "item não encontrado"
      >> putStrLn ("  Item \"" ++ idStr ++ "\" não encontrado.")
    Just item -> do
      putStrLn "\n=== ITEM ENCONTRADO ==="
      exibirItem item

handleAdicionar :: Inventario -> IO Inventario
handleAdicionar inv = do
  putStrLn "\n=== ADICIONAR ITEM ==="
  idStr <- lerLinha "  ID: "
  n     <- lerLinha "  Nome: "
  mQtd  <- lerInt   "  Quantidade: "
  cat   <- lerLinha "  Categoria: "
  case mQtd of
    Nothing -> do
      putStrLn "  Quantidade inválida. Operação cancelada."
      registrarFalha Add
        ("adicionar ID=\"" ++ idStr ++ "\"")
        "quantidade inválida"
      return inv
    Just q -> do
      let novoItem = Item { itemID = idStr, nome = n, quantidade = q, categoria = cat }
      agora <- getCurrentTime
      -- Função pura: recebe UTCTime, retorna Either String ResultadoOperacao
      case addItem agora novoItem inv of
        Left err -> do
          putStrLn $ "  Erro: " ++ err
          salvarEntradaLog LogEntry
            { timestamp = agora
            , acao      = Add
            , detalhes  = "adicionar ID=\"" ++ idStr ++ "\""
            , status    = Falha err
            }
          return inv
        Right (novoInv, entry) -> do
          salvarInventario novoInv
          salvarEntradaLog entry
          putStrLn "  Item adicionado com sucesso!"
          return novoInv

handleRemover :: Inventario -> IO Inventario
handleRemover inv = do
  putStrLn "\n=== REMOVER ITEM ==="
  idStr <- lerLinha "  ID do item a remover: "
  agora <- getCurrentTime
  -- Função pura: recebe UTCTime, retorna Either String ResultadoOperacao
  case removeItem agora idStr inv of
    Left err -> do
      putStrLn $ "  Erro: " ++ err
      salvarEntradaLog LogEntry
        { timestamp = agora
        , acao      = Remove
        , detalhes  = "remover ID=\"" ++ idStr ++ "\""
        , status    = Falha err
        }
      return inv
    Right (novoInv, entry) -> do
      salvarInventario novoInv
      salvarEntradaLog entry
      putStrLn "  Item removido com sucesso!"
      return novoInv

handleAtualizar :: Inventario -> IO Inventario
handleAtualizar inv = do
  putStrLn "\n=== ATUALIZAR ITEM ==="
  idStr <- lerLinha "  ID do item a atualizar: "
  putStrLn "  (Pressione Enter para manter o valor atual)"
  nomeStr <- lerLinha "  Novo nome: "
  qtdStr  <- lerLinha "  Nova quantidade: "
  catStr  <- lerLinha "  Nova categoria: "
  let mNome = if null nomeStr then Nothing else Just nomeStr
      mQtd  = case reads qtdStr :: [(Int, String)] of
                [(n, "")] -> Just n
                _         -> Nothing
      mCat  = if null catStr then Nothing else Just catStr
  agora <- getCurrentTime
  -- Função pura: recebe UTCTime, retorna Either String ResultadoOperacao
  case updateItem agora idStr mNome mQtd mCat inv of
    Left err -> do
      putStrLn $ "  Erro: " ++ err
      salvarEntradaLog LogEntry
        { timestamp = agora
        , acao      = Update
        , detalhes  = "atualizar ID=\"" ++ idStr ++ "\""
        , status    = Falha err
        }
      return inv
    Right (novoInv, entry) -> do
      salvarInventario novoInv
      salvarEntradaLog entry
      putStrLn "  Item atualizado com sucesso!"
      return novoInv

-- ---------------------------------------------------------------------------
-- Loop principal (estado imutável passado via >>=)
-- ---------------------------------------------------------------------------

loop :: Inventario -> IO ()
loop inv = do
  mostrarMenu
  opcao <- lerLinha "\n  Escolha uma opção: "
  case opcao of
    "0" -> putStrLn "\n  Encerrando o sistema. Até logo!\n"
    "1" -> handleListar  inv    >> loop inv
    "2" -> handleBuscar  inv    >> loop inv
    "3" -> handleAdicionar inv  >>= loop
    "4" -> handleRemover  inv   >>= loop
    "5" -> handleAtualizar inv  >>= loop
    "6" -> mostrarLog           >> loop inv
    _   -> putStrLn "  Opção inválida. Tente novamente." >> loop inv

-- ---------------------------------------------------------------------------
-- Ponto de entrada
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn ""
  putStrLn "  ╔═══════════════════════════════════════╗"
  putStrLn "  ║   Iniciando Sistema de Inventário...  ║"
  putStrLn "  ╚═══════════════════════════════════════╝"
  inv <- carregarInventario
  putStrLn $ "  Inventário carregado: " ++ show (Map.size inv) ++ " item(s)."
  loop inv
