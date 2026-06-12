[README.md](https://github.com/user-attachments/files/28896317/README.md)
#  Sistema de Inventário em Haskell

Esse é um sistema interativo de inventário via terminal feito em Haskell. O grande foco aqui é mostrar como a **programação funcional pura se separa de verdade do I/O**, usando tipos algébricos (ADTs), `Data.Map` e salvando os dados em disco com um log de auditoria que só cresce (append-only).

>  **versão completa?**
> Acesse a versão completa aqui: [Inventory Manager no Replit](https://inventory-manager-haskell--DKasper.replit.app)

---

## O que ele faz?

* **Listar itens:** Mostra tudo ordenado por ID.
* **Buscar por ID:** Acha um item específico (e avisa no log se não encontrar).
* **Adicionar / Remover / Atualizar:** Operações básicas de estoque com validações direto na lógica pura.
* **Log de auditoria:** Um histórico de tudo o que aconteceu, com direito a timestamp e status.

---

##  Como o código tá organizado?

O projeto é dividido estritamente entre o que é "puro" (lógica) e o que tem "efeito colateral" (I/O):

* `src/Inventario.hs` **(Lógica Pura - Sem IO):** Onde ficam os tipos de dados (`Item`, `LogEntry`, etc.) e as funções que manipulam o inventário. Se der erro, ela retorna `Either String Inventario` em vez de estourar uma exceção.
* `src/Main.hs` **(O "Trabalho Sujo" - Com IO):** Cuida do menu no terminal, lê/escreve nos arquivos (`Inventario.dat` e `Auditoria.log`) e gerencia o loop do programa.

---

##  Rodar no PowerShell!

Se você quer testar a parte core direto no terminal usando o PowerShell, o jeito mais rápido e sem precisar configurar o Cabal é usar o `runhaskell`.

Abra o seu PowerShell e mande bala:

```powershell
# 1. Entre na pasta do código-fonte
cd haskell/src

# 2. Rode o arquivo principal direto
runhaskell Main.hs
