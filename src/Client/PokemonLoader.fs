module PokemonLoader

open Elmish
open Fable.Helpers.React.Props
open Fulma
open Fulma.Elements
open Fulma.Layouts
module R = Fable.Helpers.React

type Pokemon =
    { name : string
      weight : int }

type Model =
    { x : int
      pageContent : Pokemon option
      loading : bool }

type Msg =
    | Inc
    | Dec
    | PageLoaded of Pokemon
    | LoadPage
    | Error of string

let getPokemon fetch url pokeId =
    Cmd.ofPromise
        fetch (sprintf "%s%d/" url pokeId)
        PageLoaded
        (fun e -> Error <| e.Message)

let init () =
    { x = 0
      pageContent = None
      loading = false },
    Cmd.none

let update getPokemon msg model =
    match msg with
    | Inc ->
        { model with x = model.x + 1 },
        Cmd.none
    | Dec ->
        { model with x = model.x - 1 },
        Cmd.none
    | PageLoaded content ->
        { model with
            pageContent = Some content
            loading = false },
        Cmd.none
    | Error str ->
        printf "Error: %s" str
        model, Cmd.none
    | LoadPage ->
        { model with loading = true },
        getPokemon model.x

let button dispatch color msg text =
    Button.button
        [ Button.Color color
          Button.Props
              [ OnClick (fun _ -> dispatch msg) ]
        ]
        [ R.str text ]

let triggerLoad dispatch model =
    Button.button
        [ Button.Color IsPrimary
          Button.Disabled model.loading
          Button.OnClick (fun _ -> dispatch LoadPage) ]
        [ R.str "Load Pokémon" ]

let counter model dispatch =
    Level.level []
        [
            Level.item [ Level.Item.HasTextCentered ]
                [ button dispatch IsDanger Dec "-" ]
            Level.item [ Level.Item.HasTextCentered ]
                [ R.str (sprintf "%d" model.x) ]
            Level.item [ Level.Item.HasTextCentered ]
                [ button dispatch IsSuccess Inc "+" ]
            Level.item
                [ Level.Item.HasTextCentered ]
                [ triggerLoad dispatch model ]
        ]

let loadedContent model =
    match model.pageContent with
    | Some poke ->
        Level.level []
            [
                Level.item [ Level.Item.HasTextCentered ]
                    [ R.str ("Name: " + poke.name) ]
                Level.item [ Level.Item.HasTextCentered ]
                    [ R.str (sprintf "Weight: %d" poke.weight) ]
            ]
    | None ->
        R.p [] [ R.str "Nothing yet" ]

let view model dispatch =
    Content.content []
        [
            R.h1 [] [ R.str "My counter!" ]
            counter model dispatch
            loadedContent model
        ]
