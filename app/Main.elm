module Main exposing (..)

import Html exposing (text, div, h1, h2, input)
import Html.Attributes exposing (type_, value)


type alias Model =
    Int


type Msg
    = NoOp


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
            , input [ type_ "button", value "Increment" ] []
            , input [ type_ "button", value "Reset" ] []
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
