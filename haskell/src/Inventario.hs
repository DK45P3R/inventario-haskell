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
  , listarItens
    -- * Serialização
  , serializarInventario
  , desserializarInventario
  ) where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Time       (UTCTime)

-- ---------------------------------------------------------------------------
-- Tipos de dados do domínio
-- Todos derivam Show e Read para serialização/desserialização em disco.
-- ---------------------------------------------------------------------------

-- | Representa um item no inventário.
data Item = Item
  { itemID     :: String  -- ^ Identificador único
  , nome       :: String  -- ^ Nome descritivo
  , quantidade :: Int     -- ^ Quantidade em estoque
  , categoria  :: String  -- ^ Categoria do item
  } deriving (Show, Read, Eq, Ord)

-- | O inventário é um mapa de itemID → Item (Data.Map).
type Inventario = Map String Item

-- | Tipo algébrico (ADT) para a ação registrada no log.
data AcaoLog
  = Add        -- ^ Adição de item
  | Remove     -- ^ Remoção de item
  | Update     -- ^ Atualização de item
  | QueryFail  -- ^ Consulta sem resultado
  deriving (Show, Read, Eq, Ord)

-- | Tipo algébrico (ADT) para o resultado de uma operação.
data StatusLog
  = Sucesso        -- ^ Operação concluída com êxito
  | Falha String   -- ^ Operação falhou (mensagem de erro inclusa)
  deriving (Show, Read, Eq)

-- | Registro de auditoria de uma operação.
data LogEntry = LogEntry
  { timestamp :: UTCTime    -- ^ Momento da operação (UTC)
  , acao      :: AcaoLog    -- ^ Tipo de ação executada
  , detalhes  :: String     -- ^ Descrição da operação
  , status    :: StatusLog  -- ^ Resultado
  } deriving (Show, Read)

-- | Par (novo estado do inventário, entrada de log) retornado pelas funções puras.
type ResultadoOperacao = (Inventario, LogEntry)

-- ---------------------------------------------------------------------------
-- Estado inicial
-- ---------------------------------------------------------------------------

-- | Inventário vazio (mapa sem entradas).
inventarioVazio :: Inventario
inventarioVazio = Map.empty

-- ---------------------------------------------------------------------------
-- Funções puras de negócio
--
-- Cada função recebe o timestamp (UTCTime) e o estado atual do inventário,
-- retorna Either:
--   Left  String              → falha de validação (mensagem de erro)
--   Right ResultadoOperacao   → (novo inventário, LogEntry já montado)
--
-- Nenhuma dessas funções realiza IO.
-- ---------------------------------------------------------------------------

-- | Valida e adiciona um item ao inventário.
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
            , detalhes  = "ID=\""   ++ itemID item  ++
                          "\" nome=\"" ++ nome item ++
                          "\" qtd="  ++ show (quantidade item) ++
                          " cat=\"" ++ categoria item ++ "\""
            , status    = Sucesso
            }
      in Right (novoInv, entry)

-- | Remove um item do inventário pelo ID.
removeItem :: UTCTime -> String -> Inventario -> Either String ResultadoOperacao
removeItem t idBusca inv
  | Map.notMember idBusca inv =
      Left $ "Item com ID \"" ++ idBusca ++ "\" não encontrado."
  | otherwise =
      let novoInv = Map.delete idBusca inv
          entry   = LogEntry
            { timestamp = t
            , acao      = Remove
            , detalhes  = "ID=\"" ++ idBusca ++ "\""
            , status    = Sucesso
            }
      in Right (novoInv, entry)

-- | Atualiza campos de um item existente.
-- Campos passados como Nothing mantêm o valor original.
updateItem
  :: UTCTime
  -> String         -- ^ ID do item
  -> Maybe String   -- ^ Novo nome      (Nothing = sem alteração)
  -> Maybe Int      -- ^ Nova quantidade (Nothing = sem alteração)
  -> Maybe String   -- ^ Nova categoria  (Nothing = sem alteração)
  -> Inventario
  -> Either String ResultadoOperacao
updateItem t idBusca mNome mQtd mCat inv =
  case Map.lookup idBusca inv of
    Nothing ->
      Left $ "Item com ID \"" ++ idBusca ++ "\" não encontrado."
    Just itemAtual ->
      let novoNome = maybe (nome       itemAtual) id mNome
          novaQtd  = maybe (quantidade itemAtual) id mQtd
          novaCat  = maybe (categoria  itemAtual) id mCat
      in if null novoNome
           then Left "O nome do item não pode ser vazio."
         else if novaQtd < 0
           then Left "A quantidade não pode ser negativa."
         else if null novaCat
           then Left "A categoria não pode ser vazia."
         else
           let itemNovo = itemAtual
                 { nome       = novoNome
                 , quantidade = novaQtd
                 , categoria  = novaCat
                 }
               novoInv = Map.insert idBusca itemNovo inv
               entry   = LogEntry
                 { timestamp = t
                 , acao      = Update
                 , detalhes  = "ID=\"" ++ idBusca ++
                               "\" nome=\"" ++ novoNome ++
                               "\" qtd=" ++ show novaQtd ++
                               " cat=\"" ++ novaCat ++ "\""
                 , status    = Sucesso
                 }
           in Right (novoInv, entry)

-- | Busca um item pelo ID. Retorna Nothing se não encontrado.
-- (Consultas puras não precisam de timestamp — o log de QueryFail é
-- construído em Main.hs após chamar getCurrentTime.)
buscarItem :: String -> Inventario -> Maybe Item
buscarItem = Map.lookup

-- | Retorna todos os itens ordenados por ID.
listarItens :: Inventario -> [Item]
listarItens = Map.elems

-- ---------------------------------------------------------------------------
-- Serialização via Show / Read
-- ---------------------------------------------------------------------------

-- | Serializa o inventário para String (usando show).
serializarInventario :: Inventario -> String
serializarInventario = show

-- | Desserializa o inventário a partir de uma String (usando reads).
desserializarInventario :: String -> Either String Inventario
desserializarInventario s =
  case reads s :: [(Inventario, String)] of
    [(inv, "")] -> Right inv
    [(inv, _)]  -> Right inv
    _           -> Left "Formato de Inventario.dat inválido."
