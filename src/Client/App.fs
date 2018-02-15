module FabledPokemon

open Elmish
open Elmish.Debug
open Elmish.React
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fulma.Layouts
open Fable.PowerPack
open Fable.PowerPack.Fetch

module R = Fable.Helpers.React

[<PassGenerics>]
let fetchUrl<'a> url =
    promise {
        let props =
            [ RequestProperties.Method HttpMethod.GET
              requestHeaders [
                  HttpRequestHeaders.Accept "application/json"
              ] ]
        return! fetchAs<'a> url props
    }

type Model =
    { loaders : PokemonLoader.Model []
      logIn : LoginForm.Model }

type Msg =
    | LoaderMsg of int * PokemonLoader.Msg
    | LogIn of LoginForm.Msg

let getPokemon =
    PokemonLoader.getPokemon
        fetchUrl<PokemonLoader.Pokemon>
        "https://pokeapi.co/api/v2/pokemon/"

let update getPokemon msg model =
    match msg with
    | LoaderMsg (index, loaderMsg) ->
        let res, cmd =
            PokemonLoader.update getPokemon loaderMsg model.loaders.[index]
        model.loaders.[index] <- res
        model, Cmd.map (fun msg -> LoaderMsg(index, msg)) cmd
    | LogIn m ->
        let res, cmd =
            LoginForm.update m model.logIn
        { model with logIn = res }, Cmd.map LogIn cmd

let init () =
    let initialization =
        [| for _ in 1..10 -> PokemonLoader.init() |] 
        |> Array.map fst
    let login, loginCmd = LoginForm.init()
    { loaders = initialization
      logIn = login },
    Cmd.batch [ loginCmd ]
let view model dispatch =
    Container.container [ Container.IsFluid ]
        [
            match model.logIn.loggedIn with
            | Some name ->
                yield R.h1 [] [R.str <| sprintf "Hello %s" name]
                yield!
                    model.loaders
                    |> Array.mapi (fun i loader ->
                                       PokemonLoader.view loader
                                           (fun msg -> dispatch <| LoaderMsg(i, msg)))
            | None ->
                yield LoginForm.view model.logIn (LogIn >> dispatch)
        ]

Program.mkProgram
    init
    (update getPokemon)
    view
|> Program.withReact "elmish-app"
|> Program.withDebugger
|> Program.runWith ()
