module Tests

open Elmish
open FSharp.Core
open PokemonLoader

QUnit.registerModule "My tests"

QUnit.test "Tests test" <| fun test ->
    let before =
        { x = 10
          pageContent = None
          loading = false }
    let (after, _) = update (fun _ -> Cmd.none) Inc before
    test.equal 11 after.x
