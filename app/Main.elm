module Main exposing (..)

import Html exposing (text, div, h1, h2, input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick)


type alias Model =
    Int


type Msg
    = Increment
    | Reset
    | SetCount Int


model =
    0


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html.Html Msg
view model =
    div []
        [ h1 [] [ text "Counter" ]
        , text ("Current count: " ++ (toString model))
        , div []
            [ h2 [] [ text "Actions" ]
            , input [ type_ "button", value "Increment", onClick Increment ] []
            , input [ type_ "button", value "Reset", onClick Reset ] []
            , input [ type_ "button", value "== 42", onClick (SetCount 42) ] []
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
