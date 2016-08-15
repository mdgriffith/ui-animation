module Animation.Dict exposing (subscription, tick, render, style, styleWith, styleWithEach)

import Animation
import AnimationFrame
import Time exposing (Time)
import Dict exposing (Dict)
import Html


type alias StyleDict id msg =
    Dict id (Animation.State msg)


subscription : StyleDict id msg -> (Time -> msg) -> Sub msg
subscription dict msg =
    if List.any Animation.isRunning Dict.values then
        AnimationFrame.times msg
    else
        Sub.none


{-|

-}
tick : Time -> StyleDict id msg -> ( StyleDict id msg, List msg )
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


render : StyleDict id msg -> id -> List Html.Attribute
render dict id =
    case Dict.get dict id of
        Just style ->
            Animation.render style

        Nothing ->
            []
