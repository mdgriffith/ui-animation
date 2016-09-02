module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy
import Animation exposing (px, turn)
import Color exposing (rgb, rgba)
import Time exposing (Time, second)
import Ease


type alias Model =
    { widgets : List Widget }


type alias Widget =
    { label : String
    , action : Msg
    , style : Animation.State Msg
    }


type Msg
    = RotateWidget
    | RotateAllAxis
    | ChangeColors
    | ChangeMultipleColors
    | FadeOutFadeIn
    | FadeOut
    | Loopty
    | Spring
    | Animate Animation.Msg


onStyle : (Animation.State Msg -> Animation.State Msg) -> Widget -> Widget
onStyle styleFn widget =
    { widget | style = styleFn widget.style }


onIndex : Int -> List a -> (a -> a) -> List a
onIndex i list fn =
    List.indexedMap
        (\j val ->
            if i == j then
                fn val
            else
                val
        )
        list


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        RotateWidget ->
            ( model, Cmd.none )

        --( { model
        --    | rotateWidget =
        --        Dict.update
        --            (toString RotateWidget)
        --            identity
        --            model.styles
        --  }
        --, Cmd.none
        --)
        RotateAllAxis ->
            ( { model
                | widgets =
                    onIndex 1 model.widgets <|
                        onStyle
                            (Animation.interrupt
                                [ Animation.loop
                                    [ Animation.to [ Animation.rotate (turn 1) ]
                                    ]
                                ]
                            )
              }
            , Cmd.none
            )

        Loopty ->
            ( { model
                | widgets =
                    onIndex 2 model.widgets <|
                        onStyle
                            (Animation.interrupt
                                [ Animation.to
                                    [ Animation.rotate (turn -0.5)
                                    , Animation.rotate (turn 0.5)
                                    , Animation.translateY (px 50)
                                    ]
                                , Animation.to
                                    [ Animation.rotate (turn -1.0)
                                    , Animation.rotate (turn 1)
                                    , Animation.translateY (px 0)
                                    ]
                                ]
                            )
              }
            , Cmd.none
            )

        Spring ->
            ( { model
                | widgets =
                    onIndex 3 model.widgets <|
                        onStyle
                            (Animation.interrupt
                                [ Animation.to
                                    [ Animation.scale 1.5
                                    ]
                                , Animation.to
                                    [ Animation.scale 1.0
                                    ]
                                ]
                            )
              }
            , Cmd.none
            )

        ChangeColors ->
            ( { model
                | widgets =
                    onIndex 4 model.widgets <|
                        onStyle
                            (Animation.interrupt
                                [ Animation.to
                                    [ Animation.backgroundColor (rgba 100 100 100 1.0)
                                    , Animation.borderColor (rgba 100 100 100 1.0)
                                    ]
                                ]
                            )
              }
            , Cmd.none
            )

        ChangeMultipleColors ->
            ( { model
                | widgets =
                    onIndex 5 model.widgets <|
                        onStyle
                            (Animation.interrupt
                                [ Animation.to
                                    [ Animation.backgroundColor (rgba 100 100 100 1.0)
                                    , Animation.borderColor (rgba 100 100 100 1.0)
                                    ]
                                , Animation.to
                                    [ Animation.backgroundColor (rgba 178 201 14 1.0)
                                    , Animation.borderColor (rgba 178 201 14 1.0)
                                    ]
                                ]
                            )
              }
            , Cmd.none
            )

        FadeOutFadeIn ->
            ( { model
                | widgets =
                    onIndex 6 model.widgets <|
                        onStyle
                            (Animation.interrupt
                                [ Animation.to
                                    [ Animation.opacity 0
                                    ]
                                , Animation.to
                                    [ Animation.opacity 1
                                    ]
                                ]
                            )
              }
            , Cmd.none
            )

        FadeOut ->
            ( { model
                | widgets =
                    onIndex 7 model.widgets <|
                        onStyle
                            (Animation.interrupt
                                [ Animation.to
                                    [ Animation.opacity 0
                                    ]
                                , Animation.set
                                    [ Animation.display Animation.none
                                    ]
                                ]
                            )
              }
            , Cmd.none
            )

        Animate time ->
            ( { model
                | widgets =
                    List.map
                        (onStyle (Animation.update time))
                        model.widgets
              }
            , Animation.getCmds
                (List.map .style model.widgets)
            )


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "relative" )
            , ( "left", "0px" )
            , ( "top", "0px" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]
        (List.map viewWidget model.widgets)


viewWidget : Widget -> Html Msg
viewWidget widget =
    div
        (Animation.render widget.style
            ++ [ style
                    [ ( "position", "relative" )
                    , ( "display", "inline-block" )
                    , ( "margin", "50px 50px" )
                    , ( "padding", "25px" )
                    , ( "text-align", "center" )
                    , ( "width", "100px" )
                    , ( "height", "100px" )
                    , ( "color", "white" )
                    , ( "cursor", "pointer" )
                    , ( "border-style", "solid" )
                    , ( "vertical-align", "middle" )
                    ]
               , onClick (widget.action)
               ]
        )
        [ text widget.label ]


init : ( Model, Cmd Msg )
init =
    let
        initialWidgetStyle =
            Animation.style
                [ Animation.display Animation.inlineBlock
                , Animation.rotate (turn 0.0)
                , Animation.rotateX (turn 0.0)
                , Animation.rotateY (turn 0.0)
                , Animation.translateY (px 0)
                , Animation.translateX (px 0)
                , Animation.rotate (turn 0)
                , Animation.opacity 1
                , Animation.backgroundColor (rgba 58 40 69 1.0)
                , Animation.color (rgba 255 255 255 1.0)
                , Animation.scale 1.0
                , Animation.borderColor (rgb 136 96 161)
                , Animation.borderWidth (px 4)
                , Animation.borderRadius (px 8)
                ]
    in
        ( { widgets =
                [ { label = "Rotate"
                  , action = RotateWidget
                  , style = initialWidgetStyle
                  }
                , { label = "Rotate in All Kinds of Ways"
                  , action = RotateAllAxis
                  , style = initialWidgetStyle
                  }
                , { label = "Change Colors"
                  , action = ChangeColors
                  , style = initialWidgetStyle
                  }
                , { label = "Change Through Multiple Colors"
                  , action = ChangeMultipleColors
                  , style = initialWidgetStyle
                  }
                , { label = "Fade Out Fade In"
                  , action = FadeOutFadeIn
                  , style = initialWidgetStyle
                  }
                , { label = "Fade Out and display:none"
                  , action = FadeOut
                  , style = initialWidgetStyle
                  }
                , { label = "Loop About"
                  , action = Loopty
                  , style = initialWidgetStyle
                  }
                , { label = "Use a Spring"
                  , action = Spring
                  , style = initialWidgetStyle
                  }
                ]
          }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription
        (List.map .style model.widgets)
        Animate


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
