namespace Data

module Core =
  open Chiron

  type TX = { sender: string; recipient: string; amount: int }
            static member ToJson(x: TX) = json {
              do! Json.write "sender" x.sender
              do! Json.write "recipient" x.recipient
              do! Json.write "amount" x.amount
            }

            static member FromJson(_: TX) = json {
              let! s = Json.read "sender"
              let! r = Json.read "recipient"
              let! a = Json.read "amount"
              return { sender = s; recipient = r; amount = a }
            }

  type Block = { index: int; ts: System.DateTime; transactions: TX list; proof: int; previousHash: string }
                static member ToJson(x: Block) = json {
                  do! Json.write "index" x.index
                  do! Json.write "ts" x.ts
                  do! Json.write "transactions" x.transactions
                  do! Json.write "proof" x.proof
                  do! Json.write "previousHash" x.previousHash
                }

                static member FromJson(_: Block) = json {
                  let! i = Json.read "index"
                  let! ts' = Json.read "ts"
                  let! tx = Json.read "transactions"
                  let! p = Json.read "proof"
                  let! pH = Json.read "previousHash"
                  return { index = i; ts = ts'; transactions = tx; proof = p; previousHash = pH }
                }

  type Blockchain = { chain: Block list; currentTransactions: TX list }
                    static member ToJson(x: Blockchain) = json {
                      do! Json.write "chain" x.chain
                      do! Json.write "currentTransactions" x.currentTransactions
                    }

                    static member FromJson(_: Blockchain) = json {
                      let! c = Json.read "chain"
                      let! tx = Json.read "currentTransactions"
                      return { chain = c; currentTransactions = tx }
                    }