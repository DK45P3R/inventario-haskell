module Main where

import           Inventario
import           System.IO
  ( hSetBuffering, hFlush, stdout, BufferMode(..) )
import           System.Directory (doesFileExist, createDirectoryIfMissing)
import           Data.Time        (getCurrentTime)
import           Data.Maybe       (mapMaybe)
import qualified Data.Map.Strict  as Map
import           Control.Exception (try, IOException)

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
-- Usa 'try' para capturar exceções de I/O sem encerrar o programa.
salvarInventario :: Inventario -> IO ()
salvarInventario inv = do
  createDirectoryIfMissing True diretorioDados
  resultado <- try (writeFile arquivoInventario (serializarInventario inv))
                 :: IO (Either IOException ())
  case resultado of
    Left err -> putStrLn $ "  [ERRO de I/O] Não foi possível salvar inventário: " ++ show err
    Right _  -> return ()

-- | Carrega o inventário do disco.
-- Trata três situações:
--   1. Arquivo inexistente → inventário vazio (primeira execução).
--   2. Erro de I/O (permissão, disco cheio…) → inventário vazio + aviso.
--   3. Formato inválido (corrompido) → inventário vazio + aviso.
carregarInventario :: IO Inventario
carregarInventario = do
  existe <- doesFileExist arquivoInventario
  if not existe
    then do
      putStrLn "  [INFO] Inventario.dat não encontrado. Iniciando com inventário vazio."
      return inventarioVazio
    else do
      resultado <- try (readFile arquivoInventario)
                     :: IO (Either IOException String)
      case resultado of
        Left err -> do
          putStrLn $ "  [ERRO de I/O] " ++ show err
                  ++ "\n  Iniciando com inventário vazio."
          return inventarioVazio
        Right conteudo ->
          case desserializarInventario conteudo of
            Right inv -> return inv
            Left  msg -> do
              putStrLn $ "  [AVISO] " ++ msg ++ " Iniciando com inventário vazio."
              return inventarioVazio

-- ---------------------------------------------------------------------------
-- I/O: log de auditoria (append-only)
-- ---------------------------------------------------------------------------

-- | Persiste uma LogEntry no arquivo de auditoria (append-only).
-- Serializa via show; a leitura posterior usa reads (desserialização).
-- Usa 'try' para tratar falhas de I/O sem encerrar o programa.
salvarEntradaLog :: LogEntry -> IO ()
salvarEntradaLog entry = do
  createDirectoryIfMissing True diretorioDados
  resultado <- try (appendFile arquivoLog (show entry ++ "\n"))
                 :: IO (Either IOException ())
  case resultado of
    Left err -> putStrLn $ "  [ERRO de I/O] Não foi possível salvar log: " ++ show err
    Right _  -> return ()

-- | Constrói uma LogEntry de FALHA e a persiste no log de auditoria.
-- Chamada quando a função pura retorna Left ou em caso de QueryFail.
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
-- Usa 'try' para tratar falhas de I/O.
mostrarLog :: IO ()
mostrarLog = do
  existe <- doesFileExist arquivoLog
  if not existe
    then putStrLn "  Nenhum registro de auditoria encontrado."
    else do
      resultado <- try (readFile arquivoLog)
                     :: IO (Either IOException String)
      case resultado of
        Left err ->
          putStrLn $ "  [ERRO de I/O] Não foi possível ler o log: " ++ show err
        Right conteudo -> do
          let entradas = mapMaybe parsarLinha (lines conteudo)
          if null entradas
            then putStrLn "  O log de auditoria está vazio."
            else do
              putStrLn "\n=== LOG DE AUDITORIA ==="
              mapM_ exibirEntrada entradas
              putStrLn $ "======================== (" ++ show (length entradas) ++ " registro(s))"
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

-- | Exibe um prompt e lê uma linha do terminal.
lerLinha :: String -> IO String
lerLinha prompt = do
  putStr prompt
  hFlush stdout
  getLine

-- | Exibe um prompt e tenta ler um inteiro da entrada.
-- Retorna Nothing se a entrada não for um inteiro válido.
lerInt :: String -> IO (Maybe Int)
lerInt prompt = do
  s <- lerLinha prompt
  case reads s :: [(Int, String)] of
    [(n, "")] -> return (Just n)
    _         -> return Nothing

-- ---------------------------------------------------------------------------
-- Exibição de itens
-- ---------------------------------------------------------------------------

-- | Exibe os campos de um Item de forma formatada no terminal.
exibirItem :: Item -> IO ()
exibirItem item = do
  putStrLn "  ┌────────────────────────────────────"
  putStrLn $ "  │ ID         : " ++ itemID item
  putStrLn $ "  │ Nome       : " ++ nome item
  putStrLn $ "  │ Quantidade : " ++ show (quantidade item)
  putStrLn $ "  │ Categoria  : " ++ categoria item
  putStrLn "  └────────────────────────────────────"

-- ---------------------------------------------------------------------------
-- Menu principal
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
  putStrLn "  ║  7. Relatório por categoria           ║"
  putStrLn "  ║  0. Sair                              ║"
  putStrLn "  ╚═══════════════════════════════════════╝"

-- ---------------------------------------------------------------------------
-- Handlers — cada handler chama getCurrentTime e delega à função pura,
-- depois persiste o inventário e a LogEntry retornados.
-- Funções puras ficam em Inventario.hs; todo IO fica aqui.
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
    Nothing -> do
      putStrLn $ "  Item \"" ++ idStr ++ "\" não encontrado."
      registrarFalha QueryFail
        ("buscar ID=\"" ++ idStr ++ "\"")
        "item não encontrado"
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
        "quantidade inválida (não é um inteiro)"
      return inv
    Just q -> do
      let novoItem = Item
            { itemID     = idStr
            , nome       = n
            , quantidade = q
            , categoria  = cat
            }
      agora <- getCurrentTime
      -- Delega a validação e a construção do estado à função pura addItem
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
  -- Delega a validação e a construção do estado à função pura removeItem
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
  idStr   <- lerLinha "  ID do item a atualizar: "
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
  -- Delega a validação e a construção do estado à função pura updateItem
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

-- | Exibe um relatório agrupado por categoria.
-- A função pura gerarRelatorio (em Inventario.hs) produz o Map;
-- este handler apenas formata e imprime o resultado.
handleRelatorio :: Inventario -> IO ()
handleRelatorio inv =
  let rel = gerarRelatorio inv
  in if Map.null rel
       then putStrLn "\n  O inventário está vazio — nenhum relatório disponível."
       else do
         putStrLn "\n=== RELATÓRIO POR CATEGORIA ==="
         mapM_ exibirLinha (Map.toAscList rel)
         putStrLn "================================"
  where
    exibirLinha (cat, (tipos, total)) =
      putStrLn $ "  " ++ cat
              ++ " | " ++ show tipos  ++ " tipo(s)"
              ++ " | " ++ show total  ++ " unidade(s) total"

-- ---------------------------------------------------------------------------
-- Loop principal
--
-- O estado (Inventario) é passado explicitamente entre chamadas recursivas —
-- sem variáveis mutáveis. Operações que modificam o inventário retornam o
-- novo estado via IO Inventario e usam >>= para encadeá-lo ao próximo loop.
-- ---------------------------------------------------------------------------

loop :: Inventario -> IO ()
loop inv = do
  mostrarMenu
  opcao <- lerLinha "\n  Escolha uma opção: "
  case opcao of
    "0" -> putStrLn "\n  Encerrando o sistema. Até logo!\n"
    "1" -> handleListar    inv >> loop inv
    "2" -> handleBuscar    inv >> loop inv
    "3" -> handleAdicionar inv >>= loop
    "4" -> handleRemover   inv >>= loop
    "5" -> handleAtualizar inv >>= loop
    "6" -> mostrarLog          >> loop inv
    "7" -> handleRelatorio inv >> loop inv
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
