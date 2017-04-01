module Main exposing (..)

import Html exposing (text, div, h1, h2, input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)


type Model
    = Anonymous String
    | LoggedIn String Int


type AnonymousMsg
    = Login String
    | NameChanged String


type LoggedInMsg
    = Increment
    | Reset
    | SetCount Int
    | Logout


type Msg
    = AMessage AnonymousMsg
    | LMessage LoggedInMsg


init =
    ( Anonymous "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AMessage (Login "") ->
            Anonymous "" ! []

        AMessage (Login name) ->
            LoggedIn name 0 ! []

        AMessage (NameChanged name) ->
            Anonymous name ! []

        LMessage msg ->
            case model of
                LoggedIn name count ->
                    case msg of
                        Increment ->
                            LoggedIn name (count + 1) ! []

                        Reset ->
                            LoggedIn name 0 ! []

                        SetCount i ->
                            LoggedIn name i ! []

                        Logout ->
                            Anonymous "" ! []

                Anonymous _ ->
                    Debug.crash "impossible"


view : Model -> Html.Html Msg
view model =
    case model of
        Anonymous name ->
            div []
                [ h1 []
                    [ text "Log In" ]
                , div []
                    [ input [ type_ "text", value name, onInput (NameChanged >> AMessage) ] []
                    , input [ type_ "button", value "Login", onClick (AMessage (Login name)) ] []
                    ]
                ]

        LoggedIn name count ->
            div []
                [ h1 [] [ text "Counter" ]
                , div []
                    [ h2 [] [ text ("Hello " ++ name) ]
                    , input [ type_ "button", value "Logout", onClick (LMessage Logout) ] []
                    ]
                , h2 [] [ text ("Current count: " ++ (toString count)) ]
                , div []
                    [ h2 [] [ text "Actions" ]
                    , input [ type_ "button", value "Increment", onClick (LMessage Increment) ] []
                    , input [ type_ "button", value "Reset", onClick (LMessage Reset) ] []
                    , input [ type_ "button", value "== 42", onClick (LMessage (SetCount 42)) ] []
                    ]
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
