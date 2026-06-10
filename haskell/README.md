# Sistema de Gerenciamento de Inventário em Haskell

Sistema interativo de inventário via terminal, desenvolvido em Haskell, demonstrando **programação funcional pura separada de I/O**, tipos algébricos (ADTs), `Data.Map` e persistência em disco com log de auditoria append-only.

## Funcionalidades

| Operação | Descrição |
|---|---|
| Listar itens | Exibe todos os itens ordenados pelo ID |
| Buscar por ID | Localiza um item específico (registra `QueryFail` se não encontrado) |
| Adicionar item | Insere novo item com validações |
| Remover item | Remove item pelo ID |
| Atualizar item | Altera campos de um item existente |
| Log de auditoria | Exibe o histórico de operações com timestamp e status |

## Tipos de Dados

Definidos em `src/Inventario.hs` (módulo puro, sem IO):

```haskell
-- Registro de item no inventário
data Item = Item
  { itemID    :: String   -- identificador único
  , nome      :: String
  , quantidade :: Int
  , categoria :: String
  } deriving (Show, Read, Eq, Ord)

-- O inventário é um mapa itemID → Item
type Inventario = Map String Item

-- ADT para o tipo de ação registrada no log
data AcaoLog = Add | Remove | Update | QueryFail
  deriving (Show, Read, Eq, Ord)

-- ADT para o resultado da operação
data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

-- Registro de auditoria
data LogEntry = LogEntry
  { timestamp :: UTCTime    -- momento da operação (UTC)
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read)
```

## Arquitetura

```
src/
├── Inventario.hs   ← lógica de negócio PURA (sem IO)
│                      • Tipos: Item, Inventario, AcaoLog, StatusLog, LogEntry
│                      • adicionarItem  :: Item   → Inventario → Either String Inventario
│                      • removerItem    :: String → Inventario → Either String Inventario
│                      • atualizarItem  :: String → Maybe String → Maybe Int
│                      •                           → Maybe String → Inventario
│                      •                           → Either String Inventario
│                      • buscarItem     :: String → Inventario → Maybe Item
│                      • listarItens    :: Inventario → [Item]
│                      • serializarInventario / desserializarInventario
│
└── Main.hs         ← operações de I/O (terminal + arquivos)
                       • carregarInventario  (lê Inventario.dat via reads)
                       • salvarInventario    (sobrescreve Inventario.dat via show)
                       • registrarLog        (append-only em Auditoria.log via show LogEntry)
                       • mostrarLog          (lê Auditoria.log e parseia LogEntry via reads)
                       • loop interativo (estado imutável passado via >>=)

dados/
├── Inventario.dat  ← Map String Item serializado via show/read (sobrescrito)
└── Auditoria.log   ← LogEntry serializado via show/read, uma por linha (append-only)
```

### Separação pura / IO

| `Inventario.hs` — funções puras | `Main.hs` — funções IO |
|---|---|
| `adicionarItem`, `removerItem`, `atualizarItem` | `carregarInventario`, `salvarInventario` |
| `buscarItem`, `listarItens` | `registrarLog`, `mostrarLog` |
| `serializarInventario`, `desserializarInventario` | `loop`, `main`, handlers do menu |

Funções puras retornam `Either String Inventario` para erros — sem exceções, sem IO.

## Serialização (Show / Read)

**Inventario.dat** — `show` de `Map String Item`:
```
fromList [("P001",Item {itemID = "P001", nome = "Caneta Azul", quantidade = 50, categoria = "Papelaria"}), ...]
```

**Auditoria.log** — `show` de `LogEntry`, uma entrada por linha:
```
LogEntry {timestamp = 2026-06-09 20:35:21 UTC, acao = Add, detalhes = "adicionar ID=\"P001\"", status = Sucesso}
LogEntry {timestamp = 2026-06-09 20:35:34 UTC, acao = QueryFail, detalhes = "buscar ID=\"XXX\"", status = Falha "não encontrado"}
```

## Pré-requisitos

- GHC >= 9.0
- cabal-install >= 3.0

## Como executar

```bash
cd haskell

# Compila e executa (primeira vez faz download das dependências)
cabal run inventario

# Ou use o workflow "Inventário Haskell" no Replit
```

O programa carrega automaticamente `dados/Inventario.dat` e reconstrói o histórico de `dados/Auditoria.log` ao iniciar.
