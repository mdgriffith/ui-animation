module Main exposing (..)

import Time exposing (second)
import Html.App
import Html exposing (h1, div, Html)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Animation
import Color exposing (black, rgb)


type alias Model =
    { style : Animation.State Action
    , index : Int
    }


type Action
    = Morph
    | Animate Float


startLogo =
    [ Animation.moveTo 212 220
    , Animation.curveTo
        [ ( 197, 171 )
        , ( 156, 153 )
        , ( 123, 221 )
        , ( 109, 157 )
        , ( 120, 109 )
        , ( 159, 63.6 )
        , ( 190, 114 )
        , ( 234, 115 )
        , ( 254, 89.8 )
        , ( 260, 82.3 )
        , ( 268, 69.6 )
        , ( 270, 60.3 )
        , ( 273, 66.5 )
        , ( 275, 71.6 )
        , ( 280, 75.6 )
        , ( 286, 79.5 )
        , ( 294, 79.8 )
        , ( 300, 79.8 )
        , ( 306, 79.8 )
        , ( 314, 79.5 )
        , ( 320, 75.6 )
        , ( 325, 71.6 )
        , ( 327, 66.5 )
        , ( 330, 60.3 )
        , ( 332, 69.6 )
        , ( 340, 82.3 )
        , ( 346, 89.8 )
        , ( 366, 115 )
        , ( 410, 114 )
        , ( 441, 63.6 )
        , ( 480, 109 )
        , ( 491, 157 )
        , ( 477, 221 )
        , ( 444, 153 )
        , ( 403, 171 )
        , ( 388, 220 )
        , ( 366, 188 )
        , ( 316, 200 )
        , ( 300, 248 )
        , ( 284, 200 )
        , ( 234, 188 )
        , ( 212, 220 )
        ]
    , Animation.close
    ]


batmanLogos =
    [ [ Animation.moveTo 256 213
      , Animation.curveTo
            [ ( 245, 181 )
            , ( 206, 187 )
            , ( 234, 262 )
            , ( 147, 181 )
            , ( 169, 71.2 )
            , ( 233, 18 )
            , ( 220, 56 )
            , ( 235, 81 )
            , ( 283, 88 )
            , ( 285, 78.7 )
            , ( 286, 69.3 )
            , ( 288, 60 )
            , ( 289, 61.3 )
            , ( 290, 62.7 )
            , ( 291, 64 )
            , ( 291, 64 )
            , ( 297, 63 )
            , ( 300, 63 )
            , ( 303, 63 )
            , ( 309, 64 )
            , ( 309, 64 )
            , ( 310, 62.7 )
            , ( 311, 61.3 )
            , ( 312, 60 )
            , ( 314, 69.3 )
            , ( 315, 78.7 )
            , ( 317, 88 )
            , ( 365, 82 )
            , ( 380, 56 )
            , ( 367, 18 )
            , ( 431, 71 )
            , ( 453, 181 )
            , ( 366, 262 )
            , ( 394, 187 )
            , ( 356, 181 )
            , ( 344, 213 )
            , ( 328, 185 )
            , ( 309, 184 )
            , ( 300, 284 )
            , ( 291, 184 )
            , ( 272, 185 )
            , ( 256, 213 )
            ]
      , Animation.close
      ]
    , [ Animation.moveTo 212 220
      , Animation.curveTo
            [ ( 197, 171 )
            , ( 156, 153 )
            , ( 123, 221 )
            , ( 109, 157 )
            , ( 120, 109 )
            , ( 159, 63.6 )
            , ( 190, 114 )
            , ( 234, 115 )
            , ( 254, 89.8 )
            , ( 260, 82.3 )
            , ( 268, 69.6 )
            , ( 270, 60.3 )
            , ( 273, 66.5 )
            , ( 275, 71.6 )
            , ( 280, 75.6 )
            , ( 286, 79.5 )
            , ( 294, 79.8 )
            , ( 300, 79.8 )
            , ( 306, 79.8 )
            , ( 314, 79.5 )
            , ( 320, 75.6 )
            , ( 325, 71.6 )
            , ( 327, 66.5 )
            , ( 330, 60.3 )
            , ( 332, 69.6 )
            , ( 340, 82.3 )
            , ( 346, 89.8 )
            , ( 366, 115 )
            , ( 410, 114 )
            , ( 441, 63.6 )
            , ( 480, 109 )
            , ( 491, 157 )
            , ( 477, 221 )
            , ( 444, 153 )
            , ( 403, 171 )
            , ( 388, 220 )
            , ( 366, 188 )
            , ( 316, 200 )
            , ( 300, 248 )
            , ( 284, 200 )
            , ( 234, 188 )
            , ( 212, 220 )
            ]
      , Animation.close
      ]
    , [ Animation.moveTo 213 222
      , Animation.curveTo
            [ ( 219, 150 )
            , ( 165, 139 )
            , ( 130, 183 )
            , ( 125, 123 )
            , ( 171, 73.8 )
            , ( 247, 51.6 )
            , ( 205, 78 )
            , ( 236, 108 )
            , ( 280, 102 )
            , ( 281, 90.3 )
            , ( 282, 79 )
            , ( 286, 68.2 )
            , ( 287, 72 )
            , ( 288, 75.8 )
            , ( 289, 79.7 )
            , ( 293, 79.7 )
            , ( 296, 79.7 )
            , ( 300, 79.7 )
            , ( 304, 79.7 )
            , ( 307, 79.7 )
            , ( 311, 79.7 )
            , ( 312, 75.8 )
            , ( 313, 72 )
            , ( 314, 68.2 )
            , ( 318, 79 )
            , ( 319, 90.3 )
            , ( 320, 102 )
            , ( 364, 108 )
            , ( 395, 78 )
            , ( 353, 51.6 )
            , ( 429, 73.8 )
            , ( 475, 123 )
            , ( 470, 183 )
            , ( 435, 139 )
            , ( 381, 150 )
            , ( 387, 222 )
            , ( 364, 176 )
            , ( 315, 172 )
            , ( 300, 248 )
            , ( 285, 172 )
            , ( 236, 176 )
            , ( 213, 222 )
            ]
      , Animation.close
      ]
    , [ Animation.moveTo 218 231
      , Animation.curveTo
            [ ( 191, 238 )
            , ( 165, 252 )
            , ( 140, 266 )
            , ( 144, 209 )
            , ( 156, 153 )
            , ( 193, 93.7 )
            , ( 218, 106 )
            , ( 249, 105 )
            , ( 280, 102 )
            , ( 282, 90.3 )
            , ( 284, 78.6 )
            , ( 289, 67.8 )
            , ( 290, 71.6 )
            , ( 291, 75.8 )
            , ( 292, 79.7 )
            , ( 292, 79.7 )
            , ( 297, 79.7 )
            , ( 300, 79.7 )
            , ( 303, 79.7 )
            , ( 308, 79.7 )
            , ( 308, 79.7 )
            , ( 309, 75.8 )
            , ( 310, 71.6 )
            , ( 311, 67.8 )
            , ( 316, 78.6 )
            , ( 318, 90.3 )
            , ( 320, 102 )
            , ( 351, 105 )
            , ( 382, 106 )
            , ( 407, 93.7 )
            , ( 444, 153 )
            , ( 456, 209 )
            , ( 460, 266 )
            , ( 435, 252 )
            , ( 409, 238 )
            , ( 382, 231 )
            , ( 355, 224 )
            , ( 328, 223 )
            , ( 300, 223 )
            , ( 272, 223 )
            , ( 245, 224 )
            , ( 218, 231 )
            ]
      , Animation.close
      ]
    ]


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Morph ->
            let
                wrappedIndex =
                    if List.length batmanLogos < model.index then
                        model.index - List.length batmanLogos
                    else
                        model.index

                newPath =
                    Maybe.withDefault startLogo <|
                        List.head <|
                            (List.drop wrappedIndex batmanLogos)
                                ++ (List.take wrappedIndex batmanLogos)
            in
                ( { model
                    | index = wrappedIndex + 1
                    , style =
                        Animation.interrupt
                            [ Animation.to
                                [ Animation.path newPath ]
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


view : Model -> Html Action
view model =
    div
        [ onClick Morph
        , Attr.style [ ( "margin", "200px auto" ), ( "width", "1000px" ), ( "height", "1000px" ), ( "cursor", "pointer" ) ]
        ]
        [ h1 [] [ text "Click to morph!" ]
        , svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 1000 1000"
            ]
            [ Svg.path (Animation.render model.style) []
            ]
        ]


init : ( Model, Cmd Action )
init =
    ( { style =
            Animation.style
                [ Animation.fill black
                , Animation.path startLogo
                ]
      , index = 2
      }
    , Cmd.none
    )


main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\model -> Animation.subscription model.style Animate)
        }
