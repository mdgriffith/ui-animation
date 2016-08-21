module Animation.Dict exposing (subscription, tick, render)

import Animation
import AnimationFrame
import Time exposing (Time)
import Dict exposing (Dict)
import Html


type alias StyleDict comparable msg =
    Dict comparable (Animation.State msg)


subscription : StyleDict comparable msg -> (Time -> msg) -> Sub msg
subscription dict msg =
    if List.any Animation.isRunning (Dict.values dict) then
        AnimationFrame.times msg
    else
        Sub.none


{-|

-}
tick : Time -> StyleDict comparable msg -> ( StyleDict comparable msg, List msg )
tick time dict =
    Dict.foldl
        (\id style ( dict2, msgs ) ->
            let
                ( ticked, newMsgs ) =
                    Animation.tick time style
            in
                ( Dict.insert id ticked dict2, msgs ++ newMsgs )
        )
        ( Dict.empty, [] )
        dict


render : comparable -> StyleDict comparable msg -> List (Html.Attribute msg)
render id dict =
    case Dict.get id dict of
        Just style ->
            Animation.render style

        Nothing ->
            []
