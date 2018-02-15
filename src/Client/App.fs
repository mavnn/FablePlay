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
    { top : PokemonLoader.Model
      bottom : PokemonLoader.Model
      logIn : LoginForm.Model }

type Msg =
    | Top of PokemonLoader.Msg
    | Bottom of PokemonLoader.Msg
    | LogIn of LoginForm.Msg

let getPokemon =
    PokemonLoader.getPokemon
        fetchUrl<PokemonLoader.Pokemon>
        "https://pokeapi.co/api/v2/pokemon/"

let update getPokemon msg model =
    match msg with
    | Top m ->
        let res, cmd =
            PokemonLoader.update getPokemon m model.top
        { model with top = res }, Cmd.map Top cmd
    | Bottom m ->
        let res, cmd =
            PokemonLoader.update getPokemon m model.bottom
        { model with bottom = res }, Cmd.map Bottom cmd
    | LogIn m ->
        let res, cmd =
            LoginForm.update m model.logIn
        { model with logIn = res }, Cmd.map LogIn cmd

let init () =
    let top, topCmd = PokemonLoader.init()
    let bottom, bottomCmd = PokemonLoader.init()
    let login, loginCmd = LoginForm.init()
    { top = top
      bottom = bottom
      logIn = login },
    Cmd.batch [ topCmd; bottomCmd; loginCmd ]

let view model dispatch =
    Container.container [ Container.IsFluid ]
        [
            match model.logIn.loggedIn with
            | Some name ->
                yield PokemonLoader.view model.top (Top >> dispatch)
                yield PokemonLoader.view model.bottom (Bottom >> dispatch)
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
