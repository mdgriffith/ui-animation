module Main exposing (..)

--where

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy
import Animation exposing (px, turn)
import Animation.Dict
import Dict exposing (Dict)
import Color exposing (rgb, rgba)
import Time exposing (Time, second)
import Ease
import Task


type alias Model =
    { widgets : List Widget
    , styles : Dict String (Animation.State Msg)
    }


type alias Widget =
    { label : String
    , action : Msg
    }


type Msg
    = RotateWidget
    | RotateAllAxis
    | RotateCustomEasingDuration
    | ChangeColors
    | ChangeMultipleColors
    | FadeOutFadeIn
    | FadeOut
    | Loopty
    | Spring
    | Animate Time


(=>) =
    (,)


mapToIndex : Int -> (a -> a) -> List a -> List a
mapToIndex j fn list =
    List.indexedMap
        (\i x ->
            if i == j then
                fn x
            else
                x
        )
        list


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        RotateWidget ->
            ( { model
                | styles =
                    Dict.update
                        (toString RotateWidget)
                        identity
                        model.styles
              }
            , Cmd.none
            )

        RotateAllAxis ->
            ( { model
                | styles =
                    Dict.update (toString RotateAllAxis)
                        (Maybe.map <|
                            Animation.interrupt
                                [ Animation.to
                                    [ Animation.rotateX (turn 1)
                                    , Animation.rotateY (turn 1)
                                    , Animation.rotate (turn 1)
                                    ]
                                ]
                        )
                        model.styles
              }
            , Cmd.none
            )

        RotateCustomEasingDuration ->
            ( model, Cmd.none )

        --let
        --    widgets =
        --        Animation.queue
        --            []
        --            |> Animation.duration (2 * second)
        --            |> Animation.easing Ease.inBounce
        --            |> Animation.update
        --                [ Rotate ((+) 1) Turn
        --                ]
        --            |> (\act ->
        --                    mapToIndex i
        --                        (\widget ->
        --                            { widget
        --                                | style = Animation.on widget.style act
        --                            }
        --                        )
        --                        model.widgets
        --               )
        --in
        --    ( { model | widgets = widgets }
        --    , Cmd.none
        --    )
        Loopty ->
            ( { model
                | styles =
                    Dict.update (toString Loopty)
                        (Maybe.map <|
                            Animation.interrupt
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
                        model.styles
              }
            , Cmd.none
            )

        Spring ->
            ( { model
                | styles =
                    Dict.update (toString Spring)
                        (Maybe.map <|
                            Animation.interrupt
                                [ Animation.to
                                    [ Animation.scale 1.5
                                    ]
                                , Animation.to
                                    [ Animation.scale 1.0
                                    ]
                                ]
                        )
                        model.styles
              }
            , Cmd.none
            )

        ChangeColors ->
            ( { model
                | styles =
                    Dict.update (toString ChangeColors)
                        (Maybe.map
                            (Animation.interrupt
                                [ Animation.to
                                    [ Animation.backgroundColor (rgba 100 100 100 1.0)
                                    , Animation.borderColor (rgba 100 100 100 1.0)
                                    ]
                                ]
                            )
                        )
                        model.styles
              }
            , Cmd.none
            )

        ChangeMultipleColors ->
            ( { model
                | styles =
                    Dict.update (toString ChangeMultipleColors)
                        (Maybe.map
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
                        )
                        model.styles
              }
            , Cmd.none
            )

        FadeOutFadeIn ->
            ( { model
                | styles =
                    Dict.update (toString FadeOutFadeIn)
                        (Maybe.map
                            (Animation.interrupt
                                [ Animation.to
                                    [ Animation.opacity 0
                                    ]
                                , Animation.to
                                    [ Animation.opacity 1
                                    ]
                                ]
                            )
                        )
                        model.styles
              }
            , Cmd.none
            )

        FadeOut ->
            ( { model
                | styles =
                    Dict.update (toString FadeOut)
                        (Maybe.map
                            (Animation.interrupt
                                [ Animation.to
                                    [ Animation.opacity 0
                                    ]
                                , Animation.set
                                    [ Animation.display Animation.none
                                    ]
                                ]
                            )
                        )
                        model.styles
              }
            , Cmd.none
            )

        Animate time ->
            let
                ( styles, cmds ) =
                    Animation.Dict.tick time model.styles
            in
                ( { model
                    | styles = styles
                  }
                , Cmd.batch cmds
                )



--updates : List Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
--updates msgs modelCmd =
--    List.foldl
--        (\msg ( model, cmd ) ->
--            let
--                ( newModel, newCmd ) =
--                    update msg model
--            in
--                ( newModel, Cmd.batch [ cmd, newCmd ] )
--        )
--        modelCmd
--        msgs
-- VIEW


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
    <|
        List.map
            (box model.styles)
            model.widgets


box : Dict String (Animation.State Msg) -> Widget -> Html Msg
box styles widget =
    let
        boxStyle =
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
    in
        div
            (Animation.Dict.render (toString widget.action) styles
                ++ [ onClick (widget.action) ]
            )
            [ text widget.label ]


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


init : ( Model, Cmd Msg )
init =
    ( { widgets =
            [ { label = "Rotate"
              , action = RotateWidget
              }
            , { label = "Rotate in All Kinds of Ways"
              , action = RotateAllAxis
              }
            , { label = "Rotate with custom easing and duration"
              , action = RotateCustomEasingDuration
              }
            , { label = "Change Colors"
              , action = ChangeColors
              }
            , { label = "Change Through Multiple Colors"
              , action = ChangeMultipleColors
              }
            , { label = "Fade Out Fade In"
              , action = FadeOutFadeIn
              }
            , { label = "Fade Out and display:none"
              , action = FadeOut
              }
            , { label = "Loop About"
              , action = Loopty
              }
            , { label = "Use a Spring"
              , action = Spring
              }
            ]
      , styles =
            Dict.fromList <|
                List.map
                    (\x -> ( toString x, initialWidgetStyle ))
                    [ RotateWidget
                    , RotateAllAxis
                    , RotateCustomEasingDuration
                    , ChangeColors
                    , ChangeMultipleColors
                    , FadeOutFadeIn
                    , FadeOut
                    , Loopty
                    , Spring
                    ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.Dict.subscription model.styles Animate


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
