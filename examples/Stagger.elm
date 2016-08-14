module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Animation
import Animation.List


type alias Model =
    { widgets : List (Animation.State Msg)
    , open : Bool
    }


type Msg
    = Show
    | Hide
    | Toggle
    | Animate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Toggle ->
            if model.open then
                update Hide model
            else
                update Show model

        Show ->
            ( { model
                | open = True
                , widgets =
                    List.indexedMap
                        (\i widget ->
                            Animation.interrupt
                                [ Animation.wait (toFloat i * 0.05 * second)
                                , Animation.to [ Animation.left (Animation.px 300) ]
                                ]
                                widget
                        )
                        model.widgets
              }
            , Cmd.none
            )

        Hide ->
            ( { model
                | open = False
                , widgets =
                    List.indexedMap
                        (\i widget ->
                            Animation.interrupt
                                [ Animation.wait (toFloat i * 0.05 * second)
                                , Animation.to [ Animation.left (Animation.px 150) ]
                                ]
                                widget
                        )
                        model.widgets
              }
            , Cmd.none
            )

        Animate time ->
            let
                ( newWidgets, msgs ) =
                    Animation.List.tick time model.widgets
            in
                ( { model
                    | widgets = newWidgets
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    div
        [ onClick Toggle
        , style
            [ ( "position", "relative" )
            , ( "left", "0px" )
            , ( "top", "0px" )
            , ( "width", "300px" )
            , ( "margin-top", "250px" )
            , ( "margin-left", "auto" )
            , ( "margin-right", "auto" )
            , ( "padding", "25px" )
            , ( "text-align", "center" )
            , ( "border-radius", "5px" )
            , ( "background-color", "#AAA" )
            , ( "cursor", "pointer" )
            ]
        ]
        [ h1 [ style [ ( "padding", "25px" ) ] ]
            [ text "Click me!" ]
        , p [] [ text "This example shows staggered animations" ]
        , div [] (List.map viewWidget model.widgets)
        ]


viewWidget : Animation.State msg -> Html Msg
viewWidget anim =
    div
        [ style <|
            [ ( "border-radius", "20px" )
            , ( "width", "40px" )
            , ( "height", "40px" )
            , ( "position", "fixed" )
            , ( "background-color", "#4e9a06" )
            , ( "z-index", "0" )
            , ( "display", "inline-block" )
            , ( "margin", "10px" )
            , ( "text-align", "center" )
            , ( "line-height", "40px" )
            ]
                ++ Animation.render anim
        ]
        []


init : ( Model, Cmd Msg )
init =
    ( { widgets = List.map initWidget [0..10]
      , open = False
      }
    , Cmd.none
    )


initWidget i =
    Animation.style
        [ Animation.left (Animation.px 150.0)
        , Animation.top (Animation.px (i * 45.0))
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.List.subscription model.widgets Animate


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
