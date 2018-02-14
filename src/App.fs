module FabledPokemon

open Elmish
open Elmish.Debug
open Elmish.React
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React.Props
open Fulma
open Fulma.Elements
open Fulma.Layouts
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.PowerPack.Fetch.Fetch_types

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
      bottom : PokemonLoader.Model }

type Msg =
    | Top of PokemonLoader.Msg
    | Bottom of PokemonLoader.Msg

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

let init () =
    let top, topCmd = PokemonLoader.init()
    let bottom, bottomCmd = PokemonLoader.init()
    { top = top
      bottom = bottom },
    Cmd.batch [ topCmd; bottomCmd ]

let view model dispatch =
    Container.container [ Container.IsFluid ]
        [
            PokemonLoader.view model.top (Top >> dispatch)
            PokemonLoader.view model.bottom (Bottom >> dispatch)
        ]

Program.mkProgram
    init
    (update getPokemon)
    view
|> Program.withReact "elmish-app"
|> Program.withDebugger
|> Program.runWith ()
