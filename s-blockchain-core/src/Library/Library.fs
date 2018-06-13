namespace Library

open System.Security.Cryptography

module Blockchain = 
    type TX = { sender  : string; recipient : string; amount : int }

    type Block =
      { index         : int
        ts            : System.DateTime
        transactions  : TX seq 
        proof         : int 
        previousHash  : string }

    type Blockchain = { chain : Block seq; currentTransactions : TX seq }

    let hash x = 
      System.Text.Encoding.ASCII.GetBytes(x.ToString())
      |>  HashAlgorithm.Create(HashAlgorithmName.SHA256.Name).ComputeHash
      |>  System.Text.Encoding.ASCII.GetString

    let lastBlock bc = Seq.tryHead bc.chain

    let addBlock bc h prf =
      let block = 
        Seq.singleton
          { index         =  0
            ts            =  System.DateTime.Now
            transactions  =  bc.currentTransactions
            proof         =  prf
            previousHash  =  Option.defaultValue (hash (lastBlock bc)) h }
            
      { bc with chain = (Seq.append block bc.chain); currentTransactions = [] }    

    let addTransaction bc tx = 
      let newTx = Seq.singleton { sender = tx.sender; recipient = tx.recipient; amount = tx.amount }
      let lastBlockIdx = Option.map (fun x -> x.index) (lastBlock bc) |> Option.defaultValue 0

      (lastBlockIdx + 1, { bc with currentTransactions = Seq.append newTx bc.currentTransactions })

    let isValidProof lastProof proof =
      let guessHash = (hash (lastProof + proof))
      guessHash.[..4] = "0000"

    let rec proofOfWork lastProof proof = 
      if (isValidProof lastProof proof) then proof
      else proofOfWork lastProof (proof + 1)
    
    