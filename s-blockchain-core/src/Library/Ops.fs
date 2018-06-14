namespace Ops

module Core =
  open Data.Core
  open System.Security.Cryptography

  let genesisBlock = { index = 0; ts = System.DateTime.Now; transactions = []; proof = 100; previousHash = "" }

  let state = ref { chain = [genesisBlock]; currentTransactions = [] } // allocate on heap
 
  let hash x =
    System.Text.Encoding.ASCII.GetBytes(x.ToString())
    |>  HashAlgorithm.Create(HashAlgorithmName.SHA256.Name).ComputeHash
    |>  System.Text.Encoding.ASCII.GetString

  let lastBlock bc = Seq.tryHead bc.chain

  let addBlock bc h prf =
    let block = { index = 0
                  ts = System.DateTime.Now
                  transactions = bc.currentTransactions
                  proof = prf
                  previousHash = Option.defaultValue (hash (lastBlock bc)) h }

    { bc with chain = block::bc.chain; currentTransactions = [] }    

  let addTransaction bc tx = 
    let newTx = { sender = tx.sender; recipient = tx.recipient; amount = tx.amount }
    
    { bc with currentTransactions = newTx::bc.currentTransactions }

  let isValidProof lastProof proof =
    let guessHash = (hash (lastProof + proof))
    guessHash.[..4] = "0000"

  let rec proofOfWork lastProof proof = 
    if (isValidProof lastProof proof) then proof
    else proofOfWork lastProof (proof + 1)
