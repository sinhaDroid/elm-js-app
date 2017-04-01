module Main exposing (..)

import Html


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
    Html.text "hello world"


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
