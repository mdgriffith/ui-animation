module Animation.Dict exposing (subscription, tick, render)

import Animation
import AnimationFrame
import Time exposing (Time)
import Dict exposing (Dict)
import Html


subscription : Dict comparable (Animation.State msg) -> (Time -> msg) -> Sub msg
subscription dict msg =
    if List.any Animation.isRunning (Dict.values dict) then
        AnimationFrame.times msg
    else
        Sub.none


{-|

-}
tick : Time -> Dict comparable (Animation.State msg) -> ( Dict comparable (Animation.State msg), Cmd msg )
tick time dict =
    Dict.foldl
        (\id style ( dict2, cmd ) ->
            let
                ( ticked, newCmd ) =
                    Animation.tick time style
            in
                ( Dict.insert id ticked dict2, Cmd.batch [ cmd, newCmd ] )
        )
        ( Dict.empty, Cmd.none )
        dict


render : comparable -> Dict comparable (Animation.State msg) -> List (Html.Attribute msg)
render id dict =
    case Dict.get id dict of
        Just style ->
            Animation.render style

        Nothing ->
            []
