module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import Animation


type alias Model =
    { menuItems : List MenuItem
    , open : Bool
    }


type alias MenuItem =
    { style : Animation.State
    , title : String
    }


type Msg
    = Show
    | Hide
    | Toggle
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Toggle ->
            if model.open then
                update Hide model
            else
                update Show model

        Show ->
            let
                newMenuItems =
                    List.indexedMap
                        (\i item ->
                            { item
                                | style =
                                    Animation.interrupt
                                        [ Animation.wait (toFloat i * 0.05 * second)
                                        , Animation.to [ Animation.left (Animation.px 200) ]
                                        ]
                                        item.style
                            }
                        )
                        model.menuItems
            in
                ( { model
                    | open = True
                    , menuItems = newMenuItems
                  }
                , Cmd.none
                )

        Hide ->
            let
                newMenuItems =
                    List.indexedMap
                        (\i item ->
                            { item
                                | style =
                                    Animation.interrupt
                                        [ Animation.wait (toFloat i * 0.05 * second)
                                        , Animation.to [ Animation.left (Animation.px 0) ]
                                        ]
                                        item.style
                            }
                        )
                        model.menuItems
            in
                ( { model
                    | open = False
                    , menuItems = newMenuItems
                  }
                , Cmd.none
                )

        Animate time ->
            let
                updatedMenuItems =
                    List.map
                        (\item ->
                            { item
                                | style = Animation.update time item.style
                            }
                        )
                        model.menuItems
            in
                ( { model
                    | menuItems = updatedMenuItems
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ style
                [ ( "width", "200px" )
                , ( "height", "100%" )
                , ( "position", "fixed" )
                , ( "top", "0px" )
                , ( "left", "-200px" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                ]
            ]
          <|
            (div
                [ style
                    [ ( "position", "relative" )
                    , ( "left", "200px" )
                    , ( "margin", "30px" )
                    , ( "cursor", "pointer" )
                    ]
                , onClick Toggle
                ]
                [ text <|
                    if model.open then
                        "hide menu"
                    else
                        "show menu"
                ]
            )
                :: (List.map viewItem model.menuItems)
        ]


viewItem : MenuItem -> Html Msg
viewItem anim =
    div
        (Animation.render anim.style
            ++ [ style
                    [ ( "position", "relative" )
                    , ( "padding", "10px 30px" )
                    , ( "width", "140px" )
                    ]
               ]
        )
        [ text anim.title ]


init : ( Model, Cmd Msg )
init =
    ( { menuItems = List.map initMenuItem [0..10]
      , open = False
      }
    , Cmd.none
    )


initMenuItem : Float -> MenuItem
initMenuItem i =
    { style =
        Animation.style
            [ Animation.left (Animation.px 0.0)
            ]
    , title = "menu item " ++ toString i
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate <|
        List.map .style model.menuItems


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
