module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation exposing (deg, px)
import Time exposing (Time, second)
import String exposing (concat)


type alias Model =
    { style : Animation.State Msg
    }


type Msg
    = Transform
    | Animate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Transform ->
            let
                style =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.rotate (deg 20)
                            ]
                        , Animation.to
                            [ Animation.translateY (px -200)
                            ]
                          -- , Animation.update
                          --     [ Animation.rotate identity Deg
                          --     , Animation.rotate (\_ -> 360) Deg
                          --     ]
                        , Animation.to
                            [ Animation.rotate (deg 380)
                            ]
                        , Animation.wait (1 * second)
                        , Animation.to
                            [ Animation.rotate (deg 0.0)
                            , Animation.translateY (px 0.0)
                            , Animation.rotate (deg 0.0)
                            ]
                        ]
                        model.style
            in
                ( { model | style = style }
                , Cmd.none
                )

        Animate time ->
            let
                ( style, msgs ) =
                    Animation.tick time model.style
            in
                ( { model
                    | style = style
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    let
        boxStyle =
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

        renderToString style =
            String.concat <|
                List.map (\( name, value ) -> name ++ ": " ++ value)
                    style
    in
        div [ onClick Transform ]
            [ div [ style <| boxStyle ++ (Animation.render model.style) ]
                [ h1 [ style [ ( "padding", "25px" ) ] ]
                    [ text "Click to see a Stacked Transform" ]
                ]
            , small
                [ style
                    [ ( "position", "fixed" )
                    , ( "left", "50px" )
                    , ( "top", "50px" )
                    ]
                ]
                [ text <| renderToString <| (Animation.render model.style) ]
            ]


init : ( Model, Cmd Msg )
init =
    ( { style =
            Animation.style
                [ Animation.rotate (deg 0.0)
                , Animation.translateY (px 0.0)
                , Animation.translateX (px 0.0)
                , Animation.rotate (deg 0.0)
                ]
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
