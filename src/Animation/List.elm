module Animation.List exposing (subscription, tick)

import Animation
import AnimationFrame
import List
import Time exposing (Time)


subscription : List (Animation.State msg) -> (Time -> msg) -> Sub msg
subscription list msg =
    if List.any Animation.isRunning list then
        AnimationFrame.times msg
    else
        Sub.none


{-|

-}
tick : Time -> List (Animation.State msg) -> ( List (Animation.State msg), List msg )
tick time list =
    List.foldl
        (\style ( styleStack, msgs ) ->
            let
                ( ticked, newMsgs ) =
                    Animation.tick time style
            in
                ( styleStack ++ [ ticked ], msgs ++ newMsgs )
        )
        ( [], [] )
        list
