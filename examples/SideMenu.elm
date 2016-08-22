module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (second)
import Animation exposing (px)
import Color exposing (green, complement)


type alias Model =
    { style : Animation.State Msg
    }


type Msg
    = Show
    | Hide
    | Animate Float


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
        Show ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.to styles.open
                        ]
                        model.style
              }
            , Cmd.none
            )

        Hide ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.to styles.closed
                        ]
                        model.style
              }
            , Cmd.none
            )

        Animate time ->
            let
                ( anim, cmds ) =
                    Animation.tick time model.style
            in
                updates msgs
                    ( { model
                        | style = anim
                      }
                    , Cmd.batch cmds
                    )


view : Model -> Html Msg
view model =
    div
        [ onMouseEnter Show
        , onMouseLeave Hide
        , style
            [ ( "position", "absolute" )
            , ( "left", "0px" )
            , ( "top", "0px" )
            , ( "width", "350px" )
            , ( "height", "100%" )
            , ( "border", "2px dashed #AAA" )
            ]
        ]
        [ h1 [ style [ ( "padding", "25px" ) ] ]
            [ text "Hover here to see menu!" ]
        , div
            [ style
                ([ ( "position", "absolute" )
                 , ( "top", "-2px" )
                 , ( "margin-left", "-2px" )
                 , ( "padding", "25px" )
                 , ( "width", "300px" )
                 , ( "height", "100%" )
                 , ( "background-color", "rgb(58,40,69)" )
                 , ( "color", "white" )
                 , ( "border", "2px solid rgb(58,40,69)" )
                 ]
                    ++ (Animation.render model.style)
                )
            ]
            [ h1 [] [ text "Hidden Menu" ]
            , ul []
                [ li [] [ text "Some things" ]
                , li [] [ text "in a list" ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription model.style Animate


init : ( Model, Cmd Msg )
init =
    ( { style = Animation.style styles.closed }
    , Cmd.none
    )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
