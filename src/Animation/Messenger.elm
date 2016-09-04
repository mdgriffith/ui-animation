module Animation.Messenger exposing (State, update)

import Animation.Model exposing (..)


type State msg
    = Animation msg


update : Tick -> Animation msg -> ( Animation msg, Cmd msg )
update tick animation =
    updateAnimation tick animation
