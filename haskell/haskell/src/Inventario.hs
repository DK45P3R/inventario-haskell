module Inventario
  ( -- * Tipos de dados
    Item(..)
  , Inventario
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  , ResultadoOperacao
    -- * Estado inicial
  , inventarioVazio
    -- * Funções puras de negócio
  , addItem
  , removeItem
  , updateItem
  , buscarItem
  , buscarPorCategoria
  , listarItens
    -- * Relatórios puros
  , gerarRelatorio
  , RelatorioCategoria
    -- * Serialização
  , serializarInventario
  , desserializarInventario
  ) where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Time       (UTCTime)

-- ---------------------------------------------------------------------------
-- Tipos de dados do domínio
--
-- Todos derivam Show e Read para serialização/desserialização em disco.
-- O campo 'itemID' é a chave do mapa Inventario.
-- ---------------------------------------------------------------------------

-- | Representa um item armazenado no inventário.
data Item = Item
  { itemID     :: String  -- ^ Identificador único do item
  , nome       :: String  -- ^ Nome descritivo
  , quantidade :: Int     -- ^ Quantidade em estoque (>= 0)
  , categoria  :: String  -- ^ Categoria do item
  } deriving (Show, Read, Eq, Ord)

-- | O inventário é um mapa de itemID → Item (Data.Map.Strict).
-- Uso de Map garante acesso O(log n) e ordenação por chave.
type Inventario = Map String Item

-- | ADT para a ação registrada no log de auditoria.
data AcaoLog
  = Add        -- ^ Adição de um novo item
  | Remove     -- ^ Remoção de um item existente
  | Update     -- ^ Atualização de campos de um item
  | QueryFail  -- ^ Consulta que não encontrou resultado
  deriving (Show, Read, Eq, Ord)

-- | ADT para o resultado de uma operação.
data StatusLog
  = Sucesso        -- ^ Operação concluída com êxito
  | Falha String   -- ^ Operação falhou; inclui mensagem de erro
  deriving (Show, Read, Eq)

-- | Registro de auditoria de uma operação.
-- Serializado via Show/Read (append-only em Auditoria.log).
data LogEntry = LogEntry
  { timestamp :: UTCTime    -- ^ Momento da operação (UTC)
  , acao      :: AcaoLog    -- ^ Tipo de ação executada
  , detalhes  :: String     -- ^ Descrição textual da operação
  , status    :: StatusLog  -- ^ Resultado da operação
  } deriving (Show, Read)

-- | Par (novo estado do inventário, entrada de log) retornado pelas
-- funções puras. Nenhuma função pura realiza IO.
type ResultadoOperacao = (Inventario, LogEntry)

-- | Tupla de relatório por categoria: (nº de tipos distintos, total de unidades)
type RelatorioCategoria = Map String (Int, Int)

-- ---------------------------------------------------------------------------
-- Estado inicial
-- ---------------------------------------------------------------------------

-- | Inventário vazio — ponto de partida quando não há arquivo salvo.
inventarioVazio :: Inventario
inventarioVazio = Map.empty

-- ---------------------------------------------------------------------------
-- Funções puras de negócio
--
-- Contrato:
--   Left  String              → erro de validação (mensagem legível)
--   Right ResultadoOperacao   → (inventário atualizado, LogEntry montado)
--
-- As funções recebem UTCTime para construir a LogEntry sem precisar de IO.
-- ---------------------------------------------------------------------------

-- | Valida e adiciona um novo item ao inventário.
-- Falha se: ID vazio, ID duplicado, nome vazio, quantidade negativa ou
-- categoria vazia.
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem t item inv
  | null (itemID item) =
      Left "O ID do item não pode ser vazio."
  | Map.member (itemID item) inv =
      Left $ "Item com ID \"" ++ itemID item ++ "\" já existe no inventário."
  | null (nome item) =
      Left "O nome do item não pode ser vazio."
  | quantidade item < 0 =
      Left "A quantidade não pode ser negativa."
  | null (categoria item) =
      Left "A categoria não pode ser vazia."
  | otherwise =
      let novoInv = Map.insert (itemID item) item inv
          entry   = LogEntry
            { timestamp = t
            , acao      = Add
            , detalhes  = "ID=\""        ++ itemID item      ++
                          "\" nome=\""   ++ nome item        ++
                          "\" qtd="      ++ show (quantidade item) ++
                          " cat=\""      ++ categoria item   ++ "\""
            , status    = Sucesso
            }
      in Right (novoInv, entry)

-- | Remove um item do inventário pelo seu ID.
-- Falha se o ID não existir no inventário.
removeItem :: UTCTime -> String -> Inventario -> Either String ResultadoOperacao
removeItem t idBusca inv
  | Map.notMember idBusca inv =
      Left $ "Item com ID \"" ++ idBusca ++ "\" não encontrado no inventário."
  | otherwise =
      let novoInv = Map.delete idBusca inv
          entry   = LogEntry
            { timestamp = t
            , acao      = Remove
            , detalhes  = "ID=\"" ++ idBusca ++ "\""
            , status    = Sucesso
            }
      in Right (novoInv, entry)

-- | Atualiza os campos de um item existente.
-- Campos passados como Nothing mantêm o valor original (atualização parcial).
-- Falha se: ID não encontrado, nome vazio, quantidade negativa (estoque
-- insuficiente), ou categoria vazia.
updateItem
  :: UTCTime
  -> String         -- ^ ID do item a atualizar
  -> Maybe String   -- ^ Novo nome      (Nothing = manter atual)
  -> Maybe Int      -- ^ Nova quantidade (Nothing = manter atual)
  -> Maybe String   -- ^ Nova categoria  (Nothing = manter atual)
  -> Inventario
  -> Either String ResultadoOperacao
updateItem t idBusca mNome mQtd mCat inv =
  case Map.lookup idBusca inv of
    Nothing ->
      Left $ "Item com ID \"" ++ idBusca ++ "\" não encontrado no inventário."
    Just itemAtual ->
      let novoNome = maybe (nome       itemAtual) id mNome
          novaQtd  = maybe (quantidade itemAtual) id mQtd
          novaCat  = maybe (categoria  itemAtual) id mCat
      in validarEAtualizar novoNome novaQtd novaCat itemAtual
  where
    validarEAtualizar novoNome novaQtd novaCat itemAtual
      | null novoNome =
          Left "O nome do item não pode ser vazio."
      | novaQtd < 0 =
          Left $ "Estoque insuficiente: quantidade não pode ser negativa "
              ++ "(tentativa: " ++ show novaQtd ++ ")."
      | null novaCat =
          Left "A categoria não pode ser vazia."
      | otherwise =
          let itemNovo = itemAtual
                { nome       = novoNome
                , quantidade = novaQtd
                , categoria  = novaCat
                }
              novoInv = Map.insert idBusca itemNovo inv
              entry   = LogEntry
                { timestamp = t
                , acao      = Update
                , detalhes  = "ID=\""      ++ idBusca  ++
                              "\" nome=\"" ++ novoNome ++
                              "\" qtd="   ++ show novaQtd ++
                              " cat=\""   ++ novaCat  ++ "\""
                , status    = Sucesso
                }
          in Right (novoInv, entry)

-- | Busca um item pelo ID. Retorna Nothing se não encontrado.
-- (Consultas puras não geram log — o log de QueryFail é
-- construído em Main.hs após chamar getCurrentTime.)
buscarItem :: String -> Inventario -> Maybe Item
buscarItem = Map.lookup

-- | Retorna todos os itens de uma categoria específica.
buscarPorCategoria :: String -> Inventario -> [Item]
buscarPorCategoria cat inv =
  [ item | item <- Map.elems inv, categoria item == cat ]

-- | Retorna todos os itens ordenados por ID (chave do mapa).
listarItens :: Inventario -> [Item]
listarItens = Map.elems

-- ---------------------------------------------------------------------------
-- Relatório puro
-- ---------------------------------------------------------------------------

-- | Gera um relatório agrupado por categoria.
-- Retorna: categoria → (nº de tipos distintos, total de unidades em estoque)
-- Função 100% pura — não realiza IO.
gerarRelatorio :: Inventario -> RelatorioCategoria
gerarRelatorio inv =
  Map.foldr acumular Map.empty inv
  where
    acumular item rel =
      let cat           = categoria item
          qtd           = quantidade item
          (tipos, total) = Map.findWithDefault (0, 0) cat rel
      in Map.insert cat (tipos + 1, total + qtd) rel

-- ---------------------------------------------------------------------------
-- Serialização via Show / Read
-- ---------------------------------------------------------------------------

-- | Serializa o inventário para String usando show.
-- O formato é o Show padrão de Data.Map, legível pelo Read correspondente.
serializarInventario :: Inventario -> String
serializarInventario = show

-- | Desserializa o inventário a partir de uma String usando reads.
-- Retorna Left com mensagem de erro se o formato for inválido.
desserializarInventario :: String -> Either String Inventario
desserializarInventario s =
  case reads s :: [(Inventario, String)] of
    [(inv, "")] -> Right inv
    [(inv, _)]  -> Right inv   -- aceita espaços/quebras de linha no final
    _           -> Left "Formato de Inventario.dat inválido ou corrompido."
