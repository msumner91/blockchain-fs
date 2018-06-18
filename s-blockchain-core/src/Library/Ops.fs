namespace Ops

module Core =
  open Chiron
  open DataTypes.Core
  open FSharp.Data
  open System.Security.Cryptography

  let hash x =
    System.Text.Encoding.UTF8.GetBytes(x.ToString())
    |>  HashAlgorithm.Create(HashAlgorithmName.SHA256.Name).ComputeHash
    |>  System.Convert.ToBase64String

  let lastBlock bc = List.tryHead bc.chain

  let addBlock bc h prf =
    let block = { index = bc.chain.Length
                  ts = System.DateTime.Now
                  transactions = bc.currentTransactions
                  proof = prf
                  previousHash = h }

    { bc with chain = block::bc.chain; currentTransactions = [] }    

  let addTransaction bc tx = 
    let newTx = { sender = tx.sender; recipient = tx.recipient; amount = tx.amount }
    
    { bc with currentTransactions = newTx::bc.currentTransactions }

  let addNode bc n =
    { bc with nodes = bc.nodes + n}

  let isValidProof lastProof proof =
    let guessHash = (hash (lastProof + proof))
    printfn "Guessed: %s" guessHash
    guessHash.[..3] = "0000"

  let rec proofOfWork lastProof proof =
    if (isValidProof lastProof proof) then 
      proof
    else 
      proofOfWork lastProof (proof + 1)

  let rec isValidChain chain =
    let validateHash (lb: Block)(bl: Block) = 
      let lastBlockHash = hash lb
      printfn "last block: %s" (lb.ToString())
      printfn "last block hash: %s" lastBlockHash
      printfn "previous hash: %s" bl.previousHash
      bl.previousHash = lastBlockHash
    let validateProof lb bl = isValidProof lb.proof bl.proof

    match chain with 
        | _::[_] -> true
        | [_] -> true 
        | [] -> true
        | b2::b1::t -> (validateHash b1 b2) && (validateProof b1 b2) && (isValidChain (b1::t))

  let replaceChain bc nc = { bc with chain = nc }

  let mine s bl =
    let lastBlock = Option.defaultValue bl (lastBlock s)
    let lastProof = lastBlock.proof
    let proof = proofOfWork lastProof 0
    let hash = hash lastBlock
    (hash, proof)

  let resolveConflicts s =
    let rec resolve s' neighbours =
      match neighbours with 
        | h::t ->
            let reqStr = sprintf "http://%s/chain" h.id
            let response = Http.RequestString(reqStr)
            let chain' = response |> Json.parse |> Json.deserialize |> (fun x -> x.chain)
            let length' = chain'.Length
            printfn "chain' length: %i" length'
            printfn "chain length: %i" s'.chain.Length
            if(length' > s'.chain.Length && isValidChain chain') then
              printfn "Replacing chain with chain'..."
              resolve (replaceChain s' chain') t
            else
              resolve s' t 
        | [] -> s'

    resolve s (Set.toList s.nodes)

  let updateState s msg nodeUuid = 
    match msg with 
      | AddTx(tx) -> addTransaction s tx
      | AddNode(n) -> addNode s n
      | Mine ->
          let (hash, proof) = (mine s genesisBlock)
          let tx = addTransaction s { sender = "0"; recipient = nodeUuid; amount = 1}
          addBlock tx hash proof 
      | Resolve -> resolveConflicts s
      | GetState(reply) -> reply.Reply(s); s