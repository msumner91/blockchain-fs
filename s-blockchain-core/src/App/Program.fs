open Suave

module Node =
  open Data.Core
  open Ops.Core
  open System

  let nodeUuid = Guid.NewGuid().ToString()

  let genesisBlock = { index = 0; ts = System.DateTime.Now; transactions = []; proof = 100; previousHash = "" }
  
  let state = ref { chain = [genesisBlock]; currentTransactions = [] }

  let addTransactionImpl tx =
    state := addTransaction !state tx
    !state
  
  let addBlockImpl bl h prf =
    state := addBlock bl h prf
    !state

  let mine () =
    let lastBlock = Option.defaultValue genesisBlock (lastBlock !state)
    let lastProof = lastBlock.proof
    let proof = proofOfWork lastProof 0
    let hash = hash lastBlock

    addTransactionImpl { sender = "0"; recipient = nodeUuid; amount = 1 } |> ignore
    addBlockImpl !state hash proof |> ignore
    
module Resource = 
  open Chiron
  open Suave.Filters
  open Suave.Operators
  open Suave.Successful

  let parseJson req =
    req.rawForm
    |> System.Text.Encoding.UTF8.GetString
    |> Json.parse

  let prettyPrint = Json.formatWith JsonFormattingOptions.Pretty
  
  let app =
    choose
      [ GET >=> choose
          [ path "/chain" >=> warbler (fun _ -> OK (!Node.state |> Json.serialize |> prettyPrint))
            path "/mine" >=> warbler (fun _ -> OK ((Node.mine ()); "Mined")) ] 
        POST >=> choose
          [ path "/transactions/new" >=> request (parseJson >> Json.deserialize >> Node.addTransactionImpl >> Json.serialize >> prettyPrint >> CREATED) ]
      ]

[<EntryPoint>]
let main argv =
  if(argv.Length <> 2) then failwith "Invalid arguments"
  else
    let host = argv.[0]
    let port = argv.[1] |> int
    startWebServer {defaultConfig with bindings = [HttpBinding.createSimple HTTP host port]} Resource.app
    0