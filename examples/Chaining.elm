module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation
import Time exposing (Time, second)
import Color exposing (rgba)


type alias Model =
    { style : Animation.State Msg
    }


type Msg
    = ChangeColor
    | Animate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        ChangeColor ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.backgroundColor (rgba 100 100 100 1.0)
                            ]
                        , Animation.to
                            [ Animation.backgroundColor (rgba 178 201 14 1.0)
                            ]
                        , Animation.to
                            [ Animation.backgroundColor (rgba 58 40 69 1.0)
                            ]
                        ]
                        model.style
              }
            , Cmd.none
            )

        Animate time ->
            let
                ( newStyle, cmd ) =
                    Animation.tick time model.style
            in
                ( { model
                    | style = newStyle
                  }
                , cmd
                )


view : Model -> Html Msg
view model =
    div
        [ onClick ChangeColor
        , style
            ([ ( "position", "relative" )
             , ( "margin", "200px auto" )
             , ( "width", "250px" )
             , ( "height", "250px" )
             , ( "text-align", "center" )
             , ( "line-height", "250px" )
             , ( "color", "white" )
             , ( "cursor", "pointer" )
             ]
                ++ Animation.renderStyle model.style
            )
        ]
        [ text "Click to Change Color" ]


init : ( Model, Cmd Msg )
init =
    ( { style =
            Animation.style [ Animation.backgroundColor (rgba 58 40 69 1.0) ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription model.style Animate


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
