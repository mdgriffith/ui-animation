module Animation.Messenger exposing (State, update)

import Animation.Model exposing (..)


type State msg
    = Animation msg


update : Msg -> Animation msg -> Animation msg
update tick animation =
    updateAnimation tick animation
