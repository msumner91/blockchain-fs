namespace Ops

module Core =
  open Data.Core
  open System.Security.Cryptography
 
  let hash x =
    System.Text.Encoding.UTF8.GetBytes(x.ToString())
    |>  HashAlgorithm.Create(HashAlgorithmName.SHA256.Name).ComputeHash
    |>  System.Convert.ToBase64String

  let lastBlock bc = Seq.tryHead bc.chain

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

  let isValidProof lastProof proof =
    let guessHash = (hash (lastProof + proof))
    printfn "Guessed: %s" guessHash
    guessHash.[..3] = "0000"

  let rec proofOfWork lastProof proof =
    if (isValidProof lastProof proof) then proof
    else proofOfWork lastProof (proof + 1)
