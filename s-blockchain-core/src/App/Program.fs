open Suave

module Resource = 
  open Suave.Filters
  open Suave.Operators
  open Suave.Successful
  open Ops.Core
  open Data.Core
  open Chiron

  let app =
    choose
      [ GET >=> choose
          [ path "/chain" >=> OK (!Ops.Core.state |> Json.serialize |> Json.formatWith JsonFormattingOptions.Pretty) ]
        POST >=> choose
          [  path "/hello" >=> OK "Hello POST" ] 
      ]

[<EntryPoint>]
let main _ =
  startWebServer defaultConfig Resource.app
  0
