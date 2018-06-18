open Suave
open DataTypes.Core

module Node =
  open Ops.Core
  let nodeUuid = System.Guid.NewGuid().ToString()

  let initialState = { chain = [genesisBlock]; currentTransactions = []; nodes = Set.empty }

  let agent = 
    MailboxProcessor.Start(fun inbox -> 
      let rec messageLoop oldState = async {
        let! msg = inbox.Receive()
        let newState = updateState oldState msg nodeUuid
        return! messageLoop newState
      } 
    
      messageLoop initialState
    )

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
          [ path "/chain" >=> warbler (fun _ -> OK (Node.agent.PostAndReply(fun reply -> GetState(reply)) |> Json.serialize |> prettyPrint))
            path "/mine" >=> warbler (fun _ -> OK (Node.agent.Post Mine |> (fun _ -> "Mining")))
            path "/nodes/resolve" >=> warbler (fun _ -> OK(Node.agent.Post Resolve |> (fun _ -> "Conflicts resolved")))] 
        POST >=> choose
          [ path "/transactions/new" >=> request (parseJson >> Json.deserialize >> AddTx >> Node.agent.Post >> (fun _ -> "Added transaction") >> CREATED) 
            path "/nodes/register" >=> request (parseJson >> Json.deserialize >> AddNode >> Node.agent.Post >> (fun _ -> "Registered node") >> CREATED)]
      ]

[<EntryPoint>]
let main argv =
  if(argv.Length <> 2) then 
    failwith "Invalid arguments"
  else
    let host = argv.[0]
    let port = argv.[1] |> int
    startWebServer {defaultConfig with bindings = [HttpBinding.createSimple HTTP host port]} Resource.app
    0