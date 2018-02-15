open Suave
open Suave.Filters
open Suave.Operators
open Suave.Writers
open Newtonsoft.Json
open Interface

let jsonConverter = Fable.JsonConverter() :> JsonConverter

let serialize value = JsonConvert.SerializeObject(value, [| jsonConverter |])
let deserialize json = JsonConvert.DeserializeObject(json, [| jsonConverter |])

let setCORSHeaders =
    setHeader  "Access-Control-Allow-Origin" "*"
    >=> setHeader "Access-Control-Allow-Headers" "content-type"

let jsonOut responseFunction data =
    data
    |> serialize
    |> responseFunction

let jsonIn (request : HttpRequest) =
    request.rawForm
    |> System.Text.Encoding.UTF8.GetString
    |> deserialize

let (|CorrectLogin|IncorrectLogin|) { name = name; password = password } =
    if name = "bob" && password = "myPassword" then
        CorrectLogin (LoginSuccess { name = name })
    else
        IncorrectLogin LoginFailure

let login request =
    match jsonIn request with
    | CorrectLogin successful ->
        jsonOut Successful.OK successful
    | IncorrectLogin failure ->
        jsonOut RequestErrors.UNAUTHORIZED failure

let app =
    choose [
        path "/login" >=>
            choose [
                POST >=> request login >=> setCORSHeaders
            ]
    ]

[<EntryPoint>]
let main argv =
    let binding =
        HttpBinding.createSimple HTTP "0.0.0.0" 8081

    let suaveConfig =
        { defaultConfig with bindings = [ binding ] }

    startWebServer suaveConfig app
    0
