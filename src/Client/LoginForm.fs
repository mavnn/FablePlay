module LoginForm

open Elmish
open Fable.Core.JsInterop
open Fable.Helpers.React.Props
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.PowerPack.Fetch.Fetch_types
open Fulma
open Fulma.Common
open Fulma.Elements
open Fulma.Elements.Form
open Fulma.Layouts
open Interface
module R = Fable.Helpers.React

type Model =
    { loggedIn : string option
      request : LoginRequest
      waiting : bool }

type Msg =
    | TryLogIn
    | LogIn of LoginSuccessful
    | LogOut
    | UpdateName of string
    | UpdatePassword of string

let init () =
    { loggedIn = None
      request =
          { name = ""
            password = "" }
      waiting = false }, Cmd.none

let postLogin (request : LoginRequest) =
    promise {
        let props =
            [ RequestProperties.Method HttpMethod.POST
              RequestProperties.Body (BodyInit.Case3 <| toJson request)
              requestHeaders [
                  HttpRequestHeaders.Accept "application/json"
              ] ]
        return! fetchAs<LoginResponse> "http://localhost:8081/login" props
    }

let update msg model =
    match msg with
    | LogIn loginResponse ->
        { model with loggedIn = Some loginResponse.name
                     waiting = false },
        Cmd.none
    | LogOut ->
        { model with loggedIn = None
                     waiting = false },
        Cmd.none
    | UpdateName str ->
        { model with
            request = { model.request with name = str } },
        Cmd.none
    | UpdatePassword str ->
        { model with
            request = { model.request with password = str } },
        Cmd.none
    | TryLogIn ->
        { model with waiting = true },
        Cmd.ofPromise
            postLogin
            model.request
            (fun response -> match response with
                             | LoginSuccess ls -> LogIn ls
                             | LoginFailure -> LogOut)
            (fun _ -> LogOut)

let dispatchOnChange dispatch map =
    R.Props.OnChange
        (fun formEvent ->
             formEvent.target?value
             |> unbox<string>
             |> map
             |> dispatch)

let view model dispatch =
    R.form
        []
        [ Field.div []
              [ Label.label [] [ R.str "Username" ]
                Control.div []
                    [ Input.text [ Input.Placeholder "Ex: Username"
                                   Input.Props [
                                       dispatchOnChange dispatch UpdateName
                                   ] ] ] ]
          Field.div []
              [ Label.label [] [ R.str "Password" ]
                Control.div []
                    [ Input.password [ Input.Placeholder "Ex: Password"
                                       Input.Props [
                                           dispatchOnChange dispatch UpdatePassword
                                       ] ] ] ]
          Field.div [ Field.IsGrouped ]
              [ Control.div []
                    [ Button.button
                        [ Button.Color IsPrimary
                          Button.Disabled model.waiting
                          Button.OnClick
                              (fun e ->
                                   e.preventDefault()
                                   dispatch TryLogIn) ]
                        [ R.str "Login" ] ] ]
        ]
