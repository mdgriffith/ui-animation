module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (second)
import Animation exposing (px)
import Color exposing (green, complement)
import String
import Result exposing (Result)


type alias Model =
    { style : Animation.State
    , spring : ( Float, Float )
    }


type Msg
    = Test
    | SetDamping String
    | SetStiffness String
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Test ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.toWith (Animation.spring { stiffness = fst model.spring, damping = snd model.spring })
                            [ Animation.left (Animation.percent 100.0) ]
                        , Animation.toWith (Animation.spring { stiffness = fst model.spring, damping = snd model.spring })
                            [ Animation.left (Animation.percent 0.0) ]
                        ]
                        model.style
              }
            , Cmd.none
            )

        SetStiffness stiffStr ->
            let
                stiff =
                    Result.withDefault 0.0 <| String.toFloat stiffStr
            in
                ( { model
                    | spring = ( stiff, snd model.spring )
                  }
                , Cmd.none
                )

        SetDamping dampStr ->
            let
                damp =
                    Result.withDefault 0.0 <| String.toFloat dampStr
            in
                ( { model
                    | spring = ( fst model.spring, damp )
                  }
                , Cmd.none
                )

        Animate animMsg ->
            ( { model
                | style = Animation.update animMsg model.style
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        []
        [ div []
            [ button [ onClick Test ] [ text "Test!" ]
            , label [] [ text "stiffness" ]
            , input [ onInput SetStiffness, value <| toString <| fst model.spring ] []
            , label [] [ text "damping" ]
            , input [ onInput SetDamping, value <| toString <| snd model.spring ] []
            ]
        , div
            [ style
                [ ( "position", "relative" )
                , ( "margin", "50px auto" )
                , ( "width", "600px" )
                , ( "height", "1px" )
                , ( "background-color", "#ccc" )
                , ( "color", "white" )
                ]
            ]
            [ div
                (Animation.render model.style
                    ++ [ style
                            [ ( "position", "absolute" )
                            , ( "top", "-10px" )
                            , ( "width", "20px" )
                            , ( "height", "20px" )
                            , ( "background-color", "rgb(58,40,69)" )
                            , ( "border-radius", "10px" )
                            ]
                       ]
                )
                []
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription [ model.style ] Animate


init : ( Model, Cmd Msg )
init =
    ( { style = Animation.style [ Animation.left (Animation.percent 0.0) ]
      , spring = ( 170, 26 )
      }
    , Cmd.none
    )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
