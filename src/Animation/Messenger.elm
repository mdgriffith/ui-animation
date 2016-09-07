module Animation.Messenger exposing (State, update, send)

{-| Send a Msg at any point in an animation.

@docs State, update, send

-}

import Animation.Model exposing (..)


{-| An Animation State that also tracks your `Msg` type.
-}
type State msg
    = Animation msg


{-| An update that returns the updated animation as well as any messages sent, in `Cmd` form.
-}
update : Tick -> Animation msg -> ( Animation msg, Cmd msg )
update tick animation =
    updateAnimation tick animation


{-| An animation `Step` which will send a message.  For example

    Animation.interrupt
        [ Animation.to [Animation.opacity 0]
        , Animation.send OpacityIsNotZero
        ]


-}
send : msg -> Step msg
send msg =
    Send msg
