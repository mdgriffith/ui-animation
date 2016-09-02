module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (second)
import Animation exposing (px)
import Color exposing (green, complement)
import Task


type alias Model =
    { style : Animation.State Msg
    }


type Msg
    = QueueAnimations
    | Animate Animation.Msg


styles =
    { open =
        [ Animation.left (px 0.0)
        , Animation.opacity 1.0
        ]
    , closed =
        [ Animation.left (px -350.0)
        , Animation.opacity 0.0
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        QueueAnimations ->
            ( { model
                | style =
                    Animation.queue
                        [ Animation.to styles.open
                        ]
                        model.style
              }
            , Cmd.none
            )

        Animate time ->
            let
                ( anim, cmd ) =
                    Animation.update time model.style
            in
                ( { model
                    | style = anim
                  }
                , cmd
                )


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", "-175px" )
            , ( "top", "100px" )
            , ( "margin-left", "50%" )
            , ( "width", "350px" )
            , ( "height", "350px" )
            , ( "border", "2px dashed #AAA" )
            ]
        ]
        [ h1 [ style [ ( "padding", "25px" ) ] ]
            [ text "Hover here to see menu!" ]
        , div
            (Animation.render model.style
                ++ [ style
                        [ ( "position", "absolute" )
                        , ( "top", "-2px" )
                        , ( "margin-left", "-2px" )
                        , ( "width", "350px" )
                        , ( "height", "350px" )
                        , ( "background-color", "rgb(58,40,69)" )
                        , ( "color", "white" )
                        , ( "border", "2px solid rgb(58,40,69)" )
                        ]
                   ]
            )
            []
        , viewSliderBar model
        ]


viewSliderBar : Model -> Html Msg
viewSliderBar model =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", "-75px" )
            , ( "bottom", "-70px" )
            , ( "width", "500px" )
            , ( "height", "1px" )
            , ( "background-color", "#CCC" )
            ]
        ]
        [ div
            [ class "dot"
            , style
                [ ( "position", "absolute" )
                , ( "left", "0px" )
                , ( "top", "-10px" )
                , ( "width", "20px" )
                , ( "height", "20px" )
                , ( "border-radius", "10px" )
                , ( "background-color", "#CCC" )
                ]
            ]
            []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription model.style Animate


init : ( Model, Cmd Msg )
init =
    ( { style = Animation.style styles.closed }
    , Task.perform identity identity (Task.succeed QueueAnimations)
    )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
