module FabledPokemon

open Elmish
open Elmish.Debug
open Elmish.React
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fulma.Elements
open Fulma.Layouts
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.Helpers.React.Props
open System
open Aether

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
    { loaders : Map<Guid, PokemonLoader.Model>
      loading : bool
      logIn : LoginForm.Model }

let loaderModel_ guid =
    let get guid (model : Model) : PokemonLoader.Model option =
        let baseModel =
            Map.tryFind guid model.loaders
        match baseModel with
        | Some pokemonModel ->
            Some { pokemonModel with loading = model.loading }
        | None ->
            None
    let set guid pokemonModel model =
        match Map.tryFind guid model.loaders with
        | Some _ ->
            let newLoaders = Optic.set (Map.key_ guid) pokemonModel model.loaders 
            { model with 
                 loading = pokemonModel.loading
                 loaders = newLoaders }
        | None -> model
    get guid, set guid

type Msg =
    | LoaderMsg of Guid * PokemonLoader.Msg
    | AddLoader
    | LogIn of LoginForm.Msg

let getPokemon =
    PokemonLoader.getPokemon
        fetchUrl<PokemonLoader.Pokemon>
        "https://pokeapi.co/api/v2/pokemon/"

let update getPokemon msg model =
    match msg with
    | LoaderMsg (gid, loaderMsg) ->
        let loader = Optic.get (loaderModel_ gid) model
        match loader with
        | Some loader ->
            let res, cmd =
                PokemonLoader.update getPokemon loaderMsg loader
            Optic.set (loaderModel_ gid) res model, Cmd.map (fun msg -> LoaderMsg(gid, msg)) cmd
        | None ->
            model, Cmd.none
    | AddLoader ->
        let newGuid = Guid.NewGuid()
        let newLoader, initCmds =
            PokemonLoader.init()
        let updatedLoaders =
            Map.add newGuid newLoader model.loaders
        { model with loaders = updatedLoaders }, initCmds
    | LogIn m ->
        let res, cmd =
            LoginForm.update m model.logIn
        { model with logIn = res }, Cmd.map LogIn cmd

let init () =
    let login, loginCmd = LoginForm.init()
    { loaders = Map.empty
      loading = false
      logIn = login },
    Cmd.batch [ loginCmd ]

let getLoaderViewModel model gid =
    Optic.get (loaderModel_ gid) model

let view model dispatch =
    Container.container [ Container.IsFluid ]
        [
            match model.logIn.loggedIn with
            | Some name ->
                yield R.h1 [] [R.str <| sprintf "Hello %s" name]
                yield Button.button
                          [ Button.Props [ OnClick (fun _ -> dispatch AddLoader) ] ]
                          [ R.str "add loader" ]
                yield!
                    model.loaders
                    |> Seq.map (fun kv -> 
                        let guid = kv.Key
                        let loaderModel = getLoaderViewModel model guid
                        match loaderModel with
                        | Some lm ->
                            PokemonLoader.view lm (fun msg -> dispatch <| LoaderMsg(guid, msg))
                            |> Some
                        | None -> None)
                    |> Seq.choose id
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
