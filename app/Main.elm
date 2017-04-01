port module Main exposing (..)

import Html exposing (text, div, h1, h2, input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time)
import Task
import Json.Decode as JD
import Json.Encode as JE


port toJS : String -> Cmd a


port fromJS : (String -> msg) -> Sub msg


type Model
    = Anonymous String (Maybe Time)
    | LoggedIn String Int


decoder : JD.Decoder Model
decoder =
    JD.map2 LoggedIn (JD.field "name" JD.string) (JD.field "count" JD.int)


type AnonymousMsg
    = Login String
    | SetTime Time
    | NameChanged String
    | FromJS String


type LoggedInMsg
    = Increment
    | Reset
    | SetCount Int
    | Logout


type Msg
    = AMessage AnonymousMsg
    | LMessage LoggedInMsg


init =
    ( Anonymous "" Nothing, Task.perform (SetTime >> AMessage) Time.now )


setNameCount : String -> Int -> ( Model, Cmd Msg )
setNameCount name count =
    LoggedIn name count
        ! [ toJS
                (JE.encode 4
                    (JE.object
                        [ ( "name", JE.string name )
                        , ( "count", JE.int count )
                        ]
                    )
                )
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AMessage msg ->
            case model of
                Anonymous name time ->
                    case msg of
                        Login name ->
                            setNameCount name 0

                        SetTime time ->
                            Anonymous name (Just time) ! []

                        NameChanged name ->
                            Anonymous name time ! []

                        FromJS data ->
                            case JD.decodeString decoder data of
                                Ok m ->
                                    m ! []

                                Err msg ->
                                    Anonymous msg time ! []

                LoggedIn _ _ ->
                    Debug.crash "impossible"

        LMessage msg ->
            case model of
                LoggedIn name count ->
                    case msg of
                        Increment ->
                            setNameCount name (count + 1)

                        Reset ->
                            setNameCount name 0

                        SetCount i ->
                            setNameCount name i

                        Logout ->
                            Anonymous "" Nothing ! [ Task.perform (SetTime >> AMessage) Time.now ]

                Anonymous _ _ ->
                    Debug.crash "impossible"


view : Model -> Html.Html Msg
view model =
    case model of
        Anonymous name time ->
            div []
                [ h1 []
                    [ text "Log In" ]
                , div []
                    [ (case time of
                        Just s ->
                            div [] [ text ("Current time: " ++ (toString time)) ]

                        Nothing ->
                            div [] [ text "Current time: Loading" ]
                      )
                    , input [ type_ "text", value name, onInput (NameChanged >> AMessage) ] []
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
    case model of
        Anonymous _ _ ->
            Sub.batch
                [ Time.every Time.second (SetTime >> AMessage)
                , fromJS (FromJS >> AMessage)
                ]

        _ ->
            Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
