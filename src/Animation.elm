module Animation
    exposing
        ( render
        , interrupt
        , queue
        , wait
        , subscription
        , State
        , Msg
        , to
        , set
        , repeat
        , loop
        , update
        , style
        , styleWith
        , styleWithEach
        , opacity
        , display
        , inline
        , inlineBlock
        , flex
        , inlineFlex
        , block
        , none
        , top
        , left
        , right
        , bottom
        , width
        , height
        , padding
        , paddingLeft
        , paddingRight
        , paddingTop
        , paddingBottom
        , margin
        , marginLeft
        , marginRight
        , marginTop
        , marginBottom
        , color
        , backgroundColor
        , borderColor
        , borderWidth
        , borderLeftWidth
        , borderRightWidth
        , borderTopWidth
        , borderBottomWidth
        , borderRadius
        , borderTopLeftRadius
        , borderTopRightRadius
        , borderBottomLeftRadius
        , borderBottomRightRadius
        , shadow
        , insetShadow
        , fill
        , stroke
        , scale
        , scaleX
        , scaleY
        , scaleZ
        , rotate
        , rotateX
        , rotateY
        , rotateZ
        , translate
        , translateX
        , translateY
        , translateZ
        , points
        , path
        , move
        , moveTo
        , close
        , curve
        , curveTo
        , quadratic
        , quadraticTo
        , smooth
        , smoothTo
        , smoothQuadratic
        , smoothQuadraticTo
        , px
        , percent
        , em
        , rem
        , turn
        , deg
        , grad
        , rad
        , custom
        , exactly
        , isRunning
        )

import Color exposing (Color)
import Time exposing (Time, second)
import AnimationFrame
import String
import Html
import Html.Attributes
import Svg.Attributes
import Task


type State msg
    = State
        { steps : Animation msg
        , style : List Property
        , timing : Timing
        , running : Bool
        , interruption : List ( Time, Animation msg )
        }


type alias Timing =
    { current : Time
    , dt : Time
    }


type alias Animation msg =
    List (Step msg)


{-|

-}
type Step msg
    = To (List Property)
    | Set (List Property)
    | Step
    | Wait Time
    | Send msg
    | Repeat Int (List (Step msg))
    | Loop (List (Step msg))


type Msg
    = Tick Time


type Interpolation
    = Spring
        { stiffness : Float
        , damping : Float
        }
    | Easing
        { progress : Float
        , duration : Time
        , start : Time
        , ease : Float -> Float
        }


type alias Style =
    List Property


{-| For each 'value' of a property, we track position, velocity, interpolation, and target.
-}
type Property
    = ExactProperty String String
    | ColorProperty String Motion Motion Motion Motion
    | ShadowProperty String Bool ShadowMotion
    | Property String Motion
    | Property2 String Motion Motion
    | Property3 String Motion Motion Motion
    | AngleProperty String Motion
    | Points (List ( Motion, Motion ))
    | Path (List PathCommand)


type alias Motion =
    { position : Float
    , velocity : Float
    , target : Float
    , interpolation : Interpolation
    , unit : String
    }


{-| A Display value used for the display property.
A display mode is not animated but can be set using Html.Animation.set
-}
type DisplayMode
    = None
    | Inline
    | InlineBlock
    | Block
    | Flex
    | InlineFlex
    | ListItem



---------------------------
-- Setting Defaults
--------------------------


{-| Given a property, return the same property with the value set to a default.

TODO: Path property could have a more intelligent default
-}
default : Property -> Property
default property =
    case property of
        ExactProperty name value ->
            ExactProperty name value

        ColorProperty name _ _ _ _ ->
            Debug.log (name ++ " has no initial value.  Defaulting to transparent white.") <|
                colorProp name (Color.rgba 255 255 255 0)

        ShadowProperty name inset shade ->
            ShadowProperty
                name
                inset
                { offsetX = initMotion 0 (shade.offsetX.unit)
                , offsetY = initMotion 0 (shade.offsetY.unit)
                , size = initMotion 0 (shade.size.unit)
                , blur = initMotion 0 (shade.blur.unit)
                , red = initMotion 0 (shade.red.unit)
                , green = initMotion 0 (shade.green.unit)
                , blue = initMotion 0 (shade.blue.unit)
                , alpha = initMotion 0 (shade.alpha.unit)
                }

        Property name x ->
            length name 0 x.unit

        Property2 name x y ->
            length2 name ( 0, x.unit ) ( 0, y.unit )

        Property3 name x y z ->
            length3 name ( 0, x.unit ) ( 0, y.unit ) ( 0, z.unit )

        AngleProperty name x ->
            AngleProperty name (initMotion 0 "deg")

        Points pnts ->
            Points <| List.map (\_ -> ( initMotion 0 "", initMotion 0 "" )) pnts

        Path cmds ->
            Path []


setDefaultInterpolation : Property -> Property
setDefaultInterpolation prop =
    let
        interp =
            defaultInterpolationByProperty prop
    in
        setInterpolation interp prop


{-|

-}
defaultInterpolationByProperty : Property -> Interpolation
defaultInterpolationByProperty prop =
    let
        spring =
            Spring
                { stiffness = 170
                , damping = 26
                }

        -- progress is set to 1 because it is changed to 0 when the animation actually starts
        -- This is analagous to the spring starting at rest.
        linear duration =
            Easing
                { progress = 1
                , start = 0
                , duration = duration
                , ease = identity
                }
    in
        case prop of
            ExactProperty _ _ ->
                spring

            ColorProperty _ _ _ _ _ ->
                linear (0.4 * second)

            ShadowProperty _ _ _ ->
                spring

            Property _ _ ->
                spring

            Property2 _ _ _ ->
                spring

            Property3 _ _ _ _ ->
                spring

            AngleProperty _ _ ->
                linear (2 * second)

            Points _ ->
                spring

            Path _ ->
                spring


setInterpolation : Interpolation -> Property -> Property
setInterpolation interp prop =
    case prop of
        ExactProperty name value ->
            ExactProperty name value

        ColorProperty name m1 m2 m3 m4 ->
            ColorProperty name
                { m1 | interpolation = interp }
                { m2 | interpolation = interp }
                { m3 | interpolation = interp }
                { m4 | interpolation = interp }

        ShadowProperty name inset shadow ->
            let
                offsetX =
                    shadow.offsetX

                offsetY =
                    shadow.offsetY

                size =
                    shadow.size

                blur =
                    shadow.blur

                red =
                    shadow.red

                green =
                    shadow.green

                blue =
                    shadow.blue

                alpha =
                    shadow.alpha
            in
                ShadowProperty
                    name
                    inset
                    { offsetX = { offsetX | interpolation = interp }
                    , offsetY = { offsetY | interpolation = interp }
                    , size = { size | interpolation = interp }
                    , blur = { blur | interpolation = interp }
                    , red = { red | interpolation = interp }
                    , green = { green | interpolation = interp }
                    , blue = { blue | interpolation = interp }
                    , alpha = { alpha | interpolation = interp }
                    }

        Property name m1 ->
            Property name
                { m1 | interpolation = interp }

        Property2 name m1 m2 ->
            Property2 name
                { m1 | interpolation = interp }
                { m2 | interpolation = interp }

        Property3 name m1 m2 m3 ->
            Property3 name
                { m1 | interpolation = interp }
                { m2 | interpolation = interp }
                { m3 | interpolation = interp }

        AngleProperty name m1 ->
            AngleProperty name
                { m1 | interpolation = interp }

        Points ms ->
            Points <|
                List.map
                    (\( x, y ) ->
                        ( { x | interpolation = interp }
                        , { y | interpolation = interp }
                        )
                    )
                    ms

        Path cmds ->
            Path <|
                List.map
                    (setPathInterpolation interp)
                    cmds


setPathInterpolation : Interpolation -> PathCommand -> PathCommand
setPathInterpolation interp cmd =
    let
        setCoordInterp coords =
            List.map
                (\( x, y ) ->
                    ( { x | interpolation = interp }
                    , { y | interpolation = interp }
                    )
                )
                coords
    in
        case cmd of
            Move m1 m2 ->
                Move
                    { m1 | interpolation = interp }
                    { m2 | interpolation = interp }

            MoveTo m1 m2 ->
                MoveTo
                    { m1 | interpolation = interp }
                    { m2 | interpolation = interp }

            Line m1 m2 ->
                Line
                    { m1 | interpolation = interp }
                    { m2 | interpolation = interp }

            LineTo m1 m2 ->
                LineTo
                    { m1 | interpolation = interp }
                    { m2 | interpolation = interp }

            Horizontal motion ->
                Horizontal
                    { motion | interpolation = interp }

            HorizontalTo motion ->
                HorizontalTo
                    { motion | interpolation = interp }

            Vertical motion ->
                Vertical
                    { motion | interpolation = interp }

            VerticalTo motion ->
                VerticalTo
                    { motion | interpolation = interp }

            Curve coords ->
                Curve <| setCoordInterp coords

            CurveTo coords ->
                CurveTo <| setCoordInterp coords

            Quadratic coords ->
                Quadratic <| setCoordInterp coords

            QuadraticTo coords ->
                QuadraticTo <| setCoordInterp coords

            SmoothQuadratic coords ->
                SmoothQuadratic <| setCoordInterp coords

            SmoothQuadraticTo coords ->
                SmoothQuadraticTo <| setCoordInterp coords

            Smooth coords ->
                Smooth <| setCoordInterp coords

            SmoothTo coords ->
                SmoothTo <| setCoordInterp coords

            ArcCmd arc ->
                ArcCmd <|
                    let
                        x =
                            arc.x

                        y =
                            arc.y

                        radiusX =
                            arc.radiusX

                        radiusY =
                            arc.radiusY

                        xAxis =
                            arc.xAxisRotation
                    in
                        { arc
                            | x = { x | interpolation = interp }
                            , y = { y | interpolation = interp }
                            , radiusX = { radiusX | interpolation = interp }
                            , radiusY = { radiusY | interpolation = interp }
                            , xAxisRotation = { xAxis | interpolation = interp }
                        }

            ArcTo arc ->
                ArcTo <|
                    let
                        x =
                            arc.x

                        y =
                            arc.y

                        radiusX =
                            arc.radiusX

                        radiusY =
                            arc.radiusY

                        xAxis =
                            arc.xAxisRotation
                    in
                        { arc
                            | x = { x | interpolation = interp }
                            , y = { y | interpolation = interp }
                            , radiusX = { radiusX | interpolation = interp }
                            , radiusY = { radiusY | interpolation = interp }
                            , xAxisRotation = { xAxis | interpolation = interp }
                        }

            Close ->
                Close



--------------------
-- Animation Steps
-------------------


wait : Time -> Step msg
wait till =
    Wait till


to : List Property -> Step msg
to props =
    To props



--{-| Animate two properties along a relative curve
---}
--along : List (Float, Float) -> (Property, Property) -> Step msg


set : List Property -> Step msg
set props =
    Set props


send : msg -> Step msg
send msg =
    Send msg


repeat : Int -> List (Step msg) -> Step msg
repeat n steps =
    Repeat n steps


loop : List (Step msg) -> Step msg
loop steps =
    Loop steps


initialState : Style -> State msg
initialState current =
    State
        { steps = []
        , style = current
        , timing =
            { current = 0
            , dt = 0
            }
        , running = False
        , interruption = []
        }


{-| Set an initial style for an animation.

Uses standard defaults for interpolation

-}
style : List Property -> State msg
style props =
    initialState <| List.map setDefaultInterpolation props


{-| Set an initial style for an animation and override the standard default for interpolation.

-}
styleWith : Interpolation -> List Property -> State msg
styleWith interp props =
    initialState <| List.map (setInterpolation interp) props


{-| Set an initial style for an animation and specify the interpolation to be used for each property.

Any property not listed will receive interpolation based on the standard defaults.
-}
styleWithEach : List ( Interpolation, Property ) -> State msg
styleWithEach props =
    initialState <| List.map (\( interp, prop ) -> setInterpolation interp prop) props


{-| Add an animation to the queue, execiting once the current animation finishes

-}
queue : Animation msg -> State msg -> State msg
queue steps (State model) =
    State
        { model
            | steps = model.steps ++ steps
            , running = True
        }


{-| Interrupt any running animations with the following animation.

-}
interrupt : Animation msg -> State msg -> State msg
interrupt steps (State model) =
    State
        { model
            | interruption = extractInitialWait steps :: model.interruption
            , running = True
        }


{-| Sums all leading `Wait` steps and removes them from the animation.

This is used because the wait at the start of an interruption works differently than a normal wait.


-}
extractInitialWait : Animation msg -> ( Time, Animation msg )
extractInitialWait steps =
    case List.head steps of
        Nothing ->
            ( 0, [] )

        Just step ->
            case step of
                Wait till ->
                    let
                        ( additionalTime, remainingSteps ) =
                            extractInitialWait (List.drop 1 steps)
                    in
                        ( till + additionalTime, remainingSteps )

                _ ->
                    ( 0, steps )


{-| Create a subscription to AnimationFrame.times.

It is throttled based on whether the current animation is running or not.

-}
subscription : State msg -> (Msg -> msg) -> Sub msg
subscription (State model) msg =
    if model.running then
        Sub.map msg (AnimationFrame.times Tick)
    else
        Sub.none


{-| Used by Animation.Dict

-}
isRunning : State msg -> Bool
isRunning (State model) =
    model.running


refreshTiming : Time -> Timing -> Timing
refreshTiming now timing =
    let
        dt =
            now - timing.current

        -- dt is set to 0 if it is a large dt,
        -- because that usually means that the user
        -- left the browser window and came back.
        -- Perhaps a better way of handling it would be to modify the spring equations
        -- so that they can handle large dts without overshooting their target.
        -- The initial frame is where current == 0, in which case dt should be 0 as well.
    in
        { current = now
        , dt =
            if (dt > 300) || (timing.current == 0) then
                0.0
            else
                dt
        }


debug : State msg -> List ( String, Motion, Time )
debug (State model) =
    let
        time =
            model.timing.current

        getValueTuple prop =
            case prop of
                ExactProperty _ _ ->
                    []

                ColorProperty name r g b a ->
                    [ ( name ++ "-red", r, time )
                    , ( name ++ "-green", g, time )
                    , ( name ++ "-blue", b, time )
                    , ( name ++ "-alpha", a, time )
                    ]

                ShadowProperty propName inset shadow ->
                    let
                        name =
                            if inset then
                                propName ++ "-inset"
                            else
                                propName
                    in
                        [ ( name ++ "-offsetX", shadow.offsetX, time )
                        , ( name ++ "-offsetY", shadow.offsetY, time )
                        , ( name ++ "-size", shadow.size, time )
                        , ( name ++ "-blur", shadow.blur, time )
                        , ( name ++ "-red", shadow.red, time )
                        , ( name ++ "-green", shadow.green, time )
                        , ( name ++ "-blue", shadow.blue, time )
                        , ( name ++ "-alpha", shadow.alpha, time )
                        ]

                Property name m1 ->
                    [ ( name, m1, time ) ]

                Property2 name m1 m2 ->
                    [ ( name ++ "-x", m1, time )
                    , ( name ++ "-y", m2, time )
                    ]

                Property3 name m1 m2 m3 ->
                    [ ( name ++ "-x", m1, time )
                    , ( name ++ "-y", m2, time )
                    , ( name ++ "-z", m2, time )
                    ]

                AngleProperty name m1 ->
                    [ ( name, m1, time ) ]

                Points ms ->
                    let
                        name =
                            "points"
                    in
                        List.concat <|
                            List.indexedMap
                                (\i ( x, y ) ->
                                    [ ( toString i ++ name ++ "-x", x, time )
                                    , ( toString i ++ name ++ "-y", y, time )
                                    ]
                                )
                                ms

                Path cmds ->
                    []
    in
        List.concatMap getValueTuple model.style


update : Msg -> State msg -> ( State msg, Cmd msg )
update (Tick now) (State model) =
    let
        -- set current and dt time
        timing =
            refreshTiming now model.timing

        -- Resolve potential interrutions
        ( readyInterruption, queuedInterruptions ) =
            List.map
                (\( wait, steps ) ->
                    ( wait - timing.dt, steps )
                )
                model.interruption
                |> List.partition
                    (\( wait, steps ) -> wait <= 0)

        -- if there is more than one matching interruptions,
        -- we only take the first, which is the one that was most recently assigned.
        queue =
            case List.head readyInterruption of
                Just ( wait, interrupt ) ->
                    interrupt

                Nothing ->
                    model.steps

        ( revisedStyle, sentMessages, revisedQueue ) =
            resolveQueue model.style queue timing.dt
    in
        ( State
            { model
                | timing = timing
                , interruption = queuedInterruptions
                , running = List.length revisedQueue /= 0
                , steps = revisedQueue
                , style = revisedStyle
            }
        , Cmd.batch <| List.map (\m -> Task.perform identity identity (Task.succeed m)) sentMessages
        )


resolveQueue : List Property -> Animation msg -> Time -> ( List Property, List msg, Animation msg )
resolveQueue currentStyle steps dt =
    case List.head steps of
        Nothing ->
            ( currentStyle, [], [] )

        Just currentStep ->
            case currentStep of
                Wait n ->
                    if n <= 0 then
                        resolveQueue currentStyle (List.drop 1 steps) dt
                    else
                        -- What about a slight overage of time?
                        ( currentStyle, [], (Wait <| n - dt) :: List.drop 1 steps )

                Send msg ->
                    let
                        ( newStyle, msgs, remainingSteps ) =
                            resolveQueue currentStyle (List.drop 1 steps) dt
                    in
                        ( newStyle, msg :: msgs, remainingSteps )

                To target ->
                    -- Add starting time to any properties with duration/easing
                    resolveQueue
                        (startTowards currentStyle target)
                        (Step :: List.drop 1 steps)
                        dt

                Set props ->
                    let
                        replaced =
                            replaceProps currentStyle props
                    in
                        resolveQueue replaced (List.drop 1 steps) dt

                Step ->
                    let
                        stepped =
                            step dt currentStyle
                    in
                        ( stepped
                        , []
                        , if List.all isDone stepped then
                            List.drop 1 steps
                          else
                            steps
                        )

                Loop steps ->
                    resolveQueue
                        currentStyle
                        (steps ++ [ Loop steps ])
                        dt

                Repeat n steps ->
                    if n == 0 then
                        ( currentStyle, [], List.drop 1 steps )
                    else
                        resolveQueue
                            currentStyle
                            (steps ++ [ Repeat (n - 1) steps ])
                            dt


{-|
-}
replaceProps : List Property -> List Property -> List Property
replaceProps props replacements =
    let
        replacementNames =
            List.map propertyName replacements

        removed =
            List.filter (\prop -> not <| List.member (propertyName prop) replacementNames) props
    in
        removed ++ replacements


{-| Property is done?

TODO: What about interlaced property animations?

-}
isDone : Property -> Bool
isDone property =
    let
        motionDone motion =
            case motion.interpolation of
                Spring _ ->
                    motion.velocity == 0 && motion.position == motion.target

                Easing eased ->
                    eased.progress == 1
    in
        case property of
            ExactProperty _ _ ->
                True

            ColorProperty _ m1 m2 m3 m4 ->
                List.all motionDone [ m1, m2, m3, m4 ]

            ShadowProperty _ _ shadow ->
                List.all
                    motionDone
                    [ shadow.offsetX
                    , shadow.offsetY
                    , shadow.size
                    , shadow.blur
                    , shadow.red
                    , shadow.green
                    , shadow.blue
                    , shadow.alpha
                    ]

            Property _ m1 ->
                motionDone m1

            Property2 _ m1 m2 ->
                motionDone m1 && motionDone m2

            Property3 _ m1 m2 m3 ->
                List.all motionDone [ m1, m2, m3 ]

            AngleProperty _ m1 ->
                motionDone m1

            Points ms ->
                List.all (\( x, y ) -> motionDone x && motionDone y) ms

            Path cmds ->
                List.all isCmdDone cmds


isCmdDone : PathCommand -> Bool
isCmdDone cmd =
    let
        motionDone motion =
            motion.velocity == 0 && motion.position == motion.target
    in
        case cmd of
            Move m1 m2 ->
                motionDone m1 && motionDone m2

            MoveTo m1 m2 ->
                motionDone m1 && motionDone m2

            Line m1 m2 ->
                motionDone m1 && motionDone m2

            LineTo m1 m2 ->
                motionDone m1 && motionDone m2

            Horizontal motion ->
                motionDone motion

            HorizontalTo motion ->
                motionDone motion

            Vertical motion ->
                motionDone motion

            VerticalTo motion ->
                motionDone motion

            Curve coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            CurveTo coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            Quadratic coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            QuadraticTo coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            SmoothQuadratic coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            SmoothQuadraticTo coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            Smooth coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            SmoothTo coords ->
                List.all (\( x, y ) -> motionDone x && motionDone y) coords

            ArcCmd arc ->
                motionDone arc.x
                    && motionDone arc.y
                    && motionDone arc.radiusX
                    && motionDone arc.radiusY
                    && motionDone arc.xAxisRotation

            ArcTo arc ->
                motionDone arc.x
                    && motionDone arc.y
                    && motionDone arc.radiusX
                    && motionDone arc.radiusY
                    && motionDone arc.xAxisRotation

            Close ->
                True


{-| Set a new target for a style.
If a property doesn't exist in the current style(listA), use a default instead.

If a property doesn't exist as a target, then leave it as is.

Order matters (mostly for transformation stacking)

-}
startTowards : List Property -> List Property -> List Property
startTowards current target =
    List.filterMap
        (\propPair ->
            case propPair of
                ( Nothing, Nothing ) ->
                    Nothing

                ( Just cur, Just to ) ->
                    Just <| setTarget cur to

                ( Just prop, Nothing ) ->
                    Just prop

                ( Nothing, Just target ) ->
                    Just <| setTarget (setDefaultInterpolation <| default target) target
        )
        (zipPropertiesGreedy current target)


setTarget : Property -> Property -> Property
setTarget current newTarget =
    let
        setMotionTarget motion targetMotion =
            case motion.interpolation of
                Spring _ ->
                    { motion | target = targetMotion.position }

                Easing ease ->
                    { motion
                        | target = targetMotion.position
                        , interpolation =
                            Easing
                                { ease
                                    | start = motion.position
                                    , progress = 0
                                }
                    }
    in
        case current of
            ExactProperty name value ->
                ExactProperty name value

            ColorProperty name m1 m2 m3 m4 ->
                case newTarget of
                    ColorProperty _ t1 t2 t3 t4 ->
                        ColorProperty name
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)
                            (setMotionTarget m3 t3)
                            (setMotionTarget m4 t4)

                    _ ->
                        current

            ShadowProperty name inset shadow ->
                case newTarget of
                    ShadowProperty _ _ targetShadow ->
                        ShadowProperty
                            name
                            inset
                            { offsetX = setMotionTarget shadow.offsetX targetShadow.offsetX
                            , offsetY = setMotionTarget shadow.offsetY targetShadow.offsetY
                            , size = setMotionTarget shadow.size targetShadow.size
                            , blur = setMotionTarget shadow.blur targetShadow.blur
                            , red = setMotionTarget shadow.red targetShadow.red
                            , green = setMotionTarget shadow.green targetShadow.green
                            , blue = setMotionTarget shadow.blue targetShadow.blue
                            , alpha = setMotionTarget shadow.alpha targetShadow.alpha
                            }

                    _ ->
                        current

            Property name m1 ->
                case newTarget of
                    Property _ t1 ->
                        Property name
                            (setMotionTarget m1 t1)

                    _ ->
                        current

            Property2 name m1 m2 ->
                case newTarget of
                    Property2 _ t1 t2 ->
                        Property2 name
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)

                    _ ->
                        current

            Property3 name m1 m2 m3 ->
                case newTarget of
                    Property3 _ t1 t2 t3 ->
                        Property3 name
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)
                            (setMotionTarget m3 t3)

                    _ ->
                        current

            AngleProperty name m1 ->
                case newTarget of
                    AngleProperty _ t1 ->
                        AngleProperty name
                            (setMotionTarget m1 t1)

                    _ ->
                        current

            Points currentPts ->
                case newTarget of
                    Points targetPts ->
                        let
                            ( m1s, m2s ) =
                                matchPoints currentPts targetPts
                        in
                            Points <|
                                List.map2
                                    (\( x1, y1 ) ( x2, y2 ) ->
                                        ( (setMotionTarget x1 x2)
                                        , (setMotionTarget y1 y2)
                                        )
                                    )
                                    m1s
                                    m2s

                    _ ->
                        current

            --
            Path cmds ->
                case newTarget of
                    Path targets ->
                        Path <|
                            List.map2
                                setPathTarget
                                cmds
                                targets

                    _ ->
                        current


setPathTarget : PathCommand -> PathCommand -> PathCommand
setPathTarget cmd targetCmd =
    let
        setMotionTarget motion targetMotion =
            case motion.interpolation of
                Spring _ ->
                    { motion | target = targetMotion.position }

                Easing ease ->
                    { motion
                        | target = targetMotion.position
                        , interpolation =
                            Easing
                                { ease | start = motion.position }
                    }
    in
        case cmd of
            Move m1 m2 ->
                case targetCmd of
                    Move t1 t2 ->
                        Move
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)

                    _ ->
                        cmd

            MoveTo m1 m2 ->
                case targetCmd of
                    MoveTo t1 t2 ->
                        MoveTo
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)

                    _ ->
                        cmd

            Line m1 m2 ->
                case targetCmd of
                    Line t1 t2 ->
                        Line
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)

                    _ ->
                        cmd

            LineTo m1 m2 ->
                case targetCmd of
                    LineTo t1 t2 ->
                        LineTo
                            (setMotionTarget m1 t1)
                            (setMotionTarget m2 t2)

                    _ ->
                        cmd

            Horizontal m1 ->
                case targetCmd of
                    Horizontal t1 ->
                        Horizontal
                            (setMotionTarget m1 t1)

                    _ ->
                        cmd

            HorizontalTo m1 ->
                case targetCmd of
                    HorizontalTo t1 ->
                        HorizontalTo
                            (setMotionTarget m1 t1)

                    _ ->
                        cmd

            Vertical m1 ->
                case targetCmd of
                    Vertical t1 ->
                        Vertical
                            (setMotionTarget m1 t1)

                    _ ->
                        cmd

            VerticalTo m1 ->
                case targetCmd of
                    VerticalTo t1 ->
                        VerticalTo
                            (setMotionTarget m1 t1)

                    _ ->
                        cmd

            Curve coords ->
                case targetCmd of
                    Curve targetCoords ->
                        Curve <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            CurveTo coords ->
                case targetCmd of
                    CurveTo targetCoords ->
                        CurveTo <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            Quadratic coords ->
                case targetCmd of
                    Quadratic targetCoords ->
                        Quadratic <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            QuadraticTo coords ->
                case targetCmd of
                    QuadraticTo targetCoords ->
                        QuadraticTo <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            SmoothQuadratic coords ->
                case targetCmd of
                    SmoothQuadratic targetCoords ->
                        SmoothQuadratic <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            SmoothQuadraticTo coords ->
                case targetCmd of
                    SmoothQuadraticTo targetCoords ->
                        SmoothQuadraticTo <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            Smooth coords ->
                case targetCmd of
                    Smooth targetCoords ->
                        Smooth <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            SmoothTo coords ->
                case targetCmd of
                    SmoothTo targetCoords ->
                        SmoothTo <|
                            List.map2
                                (\( x1, y1 ) ( x2, y2 ) ->
                                    ( (setMotionTarget x1 x2)
                                    , (setMotionTarget y1 y2)
                                    )
                                )
                                coords
                                targetCoords

                    _ ->
                        cmd

            ArcCmd arc ->
                case targetCmd of
                    ArcCmd target ->
                        ArcCmd <|
                            let
                                x =
                                    arc.x

                                y =
                                    arc.y

                                radiusX =
                                    arc.radiusX

                                radiusY =
                                    arc.radiusY

                                xAxis =
                                    arc.xAxisRotation
                            in
                                { arc
                                    | x = (setMotionTarget x target.x)
                                    , y = (setMotionTarget y target.y)
                                    , radiusX = (setMotionTarget radiusX target.radiusX)
                                    , radiusY = (setMotionTarget radiusY target.radiusY)
                                    , xAxisRotation = (setMotionTarget xAxis target.xAxisRotation)
                                }

                    _ ->
                        cmd

            ArcTo arc ->
                case targetCmd of
                    ArcTo target ->
                        ArcTo <|
                            let
                                x =
                                    arc.x

                                y =
                                    arc.y

                                radiusX =
                                    arc.radiusX

                                radiusY =
                                    arc.radiusY

                                xAxis =
                                    arc.xAxisRotation
                            in
                                { arc
                                    | x = (setMotionTarget x target.x)
                                    , y = (setMotionTarget y target.y)
                                    , radiusX = (setMotionTarget radiusX target.radiusX)
                                    , radiusY = (setMotionTarget radiusY target.radiusY)
                                    , xAxisRotation = (setMotionTarget xAxis target.xAxisRotation)
                                }

                    _ ->
                        cmd

            Close ->
                Close


{-| We match two sets of properties that have any degree of overlap.

We do a fold over the maximum number of combinations it could be
(the lengths of boths lists together).

Order matters.


-}
zipPropertiesGreedy : List Property -> List Property -> List ( Maybe Property, Maybe Property )
zipPropertiesGreedy listA listB =
    let
        propertyMatch prop1 prop2 =
            propertyName prop1 == propertyName prop2

        ( _, _, zipped ) =
            List.foldl
                (\_ ( stackA, stackB, result ) ->
                    case ( List.head stackA, List.head stackB ) of
                        ( Nothing, Nothing ) ->
                            ( [], [], result )

                        ( Just a, Just b ) ->
                            if propertyMatch a b then
                                ( List.drop 1 stackA
                                , List.drop 1 stackB
                                , result ++ [ ( Just a, Just b ) ]
                                )
                            else
                                ( List.drop 1 stackA
                                , stackB
                                , result ++ [ ( Just a, Nothing ) ]
                                )

                        ( Just _, Nothing ) ->
                            ( []
                            , []
                            , result ++ List.map (\a -> ( Just a, Nothing )) stackA
                            )

                        ( Nothing, Just _ ) ->
                            ( []
                            , []
                            , result ++ List.map (\b -> ( Nothing, Just b )) stackB
                            )
                )
                ( listA, listB, [] )
                (List.repeat (List.length listA + List.length listB) 0)
    in
        zipped


{-| Move one step in our interpolation strategy.

For angle properties, wrap at 360 and -360 degrees.
-}
step : Time -> List Property -> List Property
step dt props =
    let
        stepProp property =
            case property of
                ExactProperty name value ->
                    ExactProperty name value

                Property name motion ->
                    Property name (stepInterpolation dt motion)

                Property2 name motion1 motion2 ->
                    Property2 name
                        (stepInterpolation dt motion1)
                        (stepInterpolation dt motion2)

                Property3 name motion1 motion2 motion3 ->
                    Property3 name
                        (stepInterpolation dt motion1)
                        (stepInterpolation dt motion2)
                        (stepInterpolation dt motion3)

                AngleProperty name motion ->
                    let
                        stepped =
                            stepInterpolation dt motion

                        wrapped =
                            if stepped.position >= 360 && stepped.target >= 360 then
                                { stepped
                                    | position = stepped.position - 360
                                    , target = stepped.target - 360
                                }
                            else if stepped.position <= -360 && stepped.target <= -360 then
                                { stepped
                                    | position = stepped.position + 360
                                    , target = stepped.target + 360
                                }
                            else if stepped.position >= 360 then
                                { stepped
                                    | position = stepped.position - 360
                                }
                            else if stepped.position <= -360 then
                                { stepped
                                    | position = stepped.position + 360
                                }
                            else
                                stepped
                    in
                        AngleProperty name wrapped

                ColorProperty name red green blue alpha ->
                    ColorProperty name
                        (stepInterpolation dt red)
                        (stepInterpolation dt green)
                        (stepInterpolation dt blue)
                        (stepInterpolation dt alpha)

                ShadowProperty name inset shadow ->
                    ShadowProperty
                        name
                        inset
                        { offsetX = stepInterpolation dt shadow.offsetX
                        , offsetY = stepInterpolation dt shadow.offsetY
                        , size = stepInterpolation dt shadow.size
                        , blur = stepInterpolation dt shadow.blur
                        , red = stepInterpolation dt shadow.red
                        , green = stepInterpolation dt shadow.green
                        , blue = stepInterpolation dt shadow.blue
                        , alpha = stepInterpolation dt shadow.alpha
                        }

                Points points ->
                    Points <|
                        List.map
                            (\( x, y ) ->
                                ( stepInterpolation dt x
                                , stepInterpolation dt y
                                )
                            )
                            points

                Path cmds ->
                    Path <|
                        List.map (stepPath dt) cmds
    in
        List.map stepProp props


stepPath : Time -> PathCommand -> PathCommand
stepPath dt cmd =
    let
        stepCoords coords =
            List.map
                (\( x, y ) ->
                    ( stepInterpolation dt x
                    , stepInterpolation dt y
                    )
                )
                coords
    in
        case cmd of
            Move m1 m2 ->
                Move
                    (stepInterpolation dt m1)
                    (stepInterpolation dt m2)

            MoveTo m1 m2 ->
                MoveTo
                    (stepInterpolation dt m1)
                    (stepInterpolation dt m2)

            Line m1 m2 ->
                Line
                    (stepInterpolation dt m1)
                    (stepInterpolation dt m2)

            LineTo m1 m2 ->
                LineTo
                    (stepInterpolation dt m1)
                    (stepInterpolation dt m2)

            Horizontal motion ->
                Horizontal
                    (stepInterpolation dt motion)

            HorizontalTo motion ->
                HorizontalTo
                    (stepInterpolation dt motion)

            Vertical motion ->
                Vertical
                    (stepInterpolation dt motion)

            VerticalTo motion ->
                VerticalTo
                    (stepInterpolation dt motion)

            Curve coords ->
                Curve <| stepCoords coords

            CurveTo coords ->
                CurveTo <| stepCoords coords

            Quadratic coords ->
                Quadratic <| stepCoords coords

            QuadraticTo coords ->
                QuadraticTo <| stepCoords coords

            SmoothQuadratic coords ->
                SmoothQuadratic <| stepCoords coords

            SmoothQuadraticTo coords ->
                SmoothQuadraticTo <| stepCoords coords

            Smooth coords ->
                Smooth <| stepCoords coords

            SmoothTo coords ->
                SmoothTo <| stepCoords coords

            ArcCmd arc ->
                ArcCmd <|
                    { arc
                        | x = stepInterpolation dt arc.x
                        , y = stepInterpolation dt arc.y
                        , radiusX = stepInterpolation dt arc.radiusX
                        , radiusY = stepInterpolation dt arc.radiusY
                        , xAxisRotation = stepInterpolation dt arc.xAxisRotation
                    }

            ArcTo arc ->
                ArcTo <|
                    { arc
                        | x = stepInterpolation dt arc.x
                        , y = stepInterpolation dt arc.y
                        , radiusX = stepInterpolation dt arc.radiusX
                        , radiusY = stepInterpolation dt arc.radiusY
                        , xAxisRotation = stepInterpolation dt arc.xAxisRotation
                    }

            Close ->
                Close


tolerance =
    0.01


vTolerance =
    0.1


{-| We define duration/easing in terms of a super powerful spring
that is attached to where the easing function says the value should be.

-}
stepInterpolation : Time -> Motion -> Motion
stepInterpolation dtms motion =
    case motion.interpolation of
        Spring { stiffness, damping } ->
            let
                dt =
                    dtms / 1000

                fspring =
                    -stiffness * (motion.position - motion.target)

                fdamper =
                    -damping * motion.velocity

                a =
                    fspring + fdamper

                newVelocity =
                    motion.velocity + a * dt

                newPos =
                    motion.position + newVelocity * dt

                dx =
                    abs (motion.target - newPos)
            in
                if dx < tolerance && abs newVelocity < vTolerance then
                    { motion
                        | position = motion.target
                        , velocity = 0.0
                    }
                else
                    { motion
                        | position = newPos
                        , velocity = newVelocity
                    }

        Easing { progress, duration, ease, start } ->
            let
                newProgress =
                    if (dtms / duration) + progress < 1 then
                        (dtms / duration) + progress
                    else
                        1

                eased =
                    ease newProgress

                distance =
                    motion.target
                        - start

                newPos =
                    (eased * distance) + start

                newVelocity =
                    if newProgress == 1 then
                        0
                    else
                        (newPos - motion.position) / dtms
            in
                { motion
                    | position = newPos
                    , velocity = newVelocity
                    , interpolation =
                        Easing
                            { progress = newProgress
                            , duration = duration
                            , ease = ease
                            , start = start
                            }
                }



------------------------
-- Properties and Units
------------------------


type LengthUnit
    = NoUnit
    | Px
    | Percent
    | Rem
    | Em
    | Ex
    | Ch
    | Vh
    | Vw
    | Vmin
    | Vmax
    | Mm
    | Cm
    | In
    | Pt
    | Pc


lengthUnitName : LengthUnit -> String
lengthUnitName unit =
    case unit of
        NoUnit ->
            ""

        Px ->
            "px"

        Percent ->
            "%"

        Rem ->
            "rem"

        Em ->
            "em"

        Ex ->
            "ex"

        Ch ->
            "ch"

        Vh ->
            "vh"

        Vw ->
            "vw"

        Vmin ->
            "vmin"

        Vmax ->
            "vmax"

        Mm ->
            "mm"

        Cm ->
            "cm"

        In ->
            "in"

        Pt ->
            "pt"

        Pc ->
            "pc"


{-| I know this is very bizarre, why don't we just specify a Float in each type constructor of LengthUnit?

Length is only used in the assigning function before LengthUnit is converted into a string and added to a `Motion` type.

I can get the value and the length unit via unpacking the tuple instead of having a separate function to get the value.
-}
type alias Length =
    ( Float, LengthUnit )


type AngleUnit
    = Deg


angleUnitName : AngleUnit -> String
angleUnitName unit =
    case unit of
        Deg ->
            "deg"


{-| Similar weirdness, see `Length`
-}
type alias Angle =
    ( Float, AngleUnit )


initMotion : Float -> String -> Motion
initMotion position unit =
    { position = position
    , velocity = 0
    , target = position
    , unit = unit
    , interpolation =
        Spring
            { stiffness = 170
            , damping = 26
            }
    }


deg : Float -> Angle
deg a =
    ( a, Deg )


grad : Float -> Angle
grad a =
    ( (a / 400) * 360, Deg )


rad : Float -> Angle
rad a =
    ( (a / (2 * pi)) * 360, Deg )


turn : Float -> Angle
turn a =
    ( a * 360, Deg )


px : Float -> Length
px x =
    ( x, Px )


percent : Float -> Length
percent x =
    ( x, Percent )


rem : Float -> Length
rem x =
    ( x, Rem )


em : Float -> Length
em x =
    ( x, Em )


ex : Float -> Length
ex x =
    ( x, Ex )


ch : Float -> Length
ch x =
    ( x, Ch )


vh : Float -> Length
vh x =
    ( x, Vh )


vw : Float -> Length
vw x =
    ( x, Vw )


vmin : Float -> Length
vmin x =
    ( x, Vmin )


vmax : Float -> Length
vmax x =
    ( x, Vmax )


mm : Float -> Length
mm x =
    ( x, Mm )


cm : Float -> Length
cm x =
    ( x, Cm )


inches : Float -> Length
inches x =
    ( x, In )


pt : Float -> Length
pt x =
    ( x, Pt )


pc : Float -> Length
pc x =
    ( x, Pc )


length : String -> Float -> String -> Property
length name x unit =
    Property name (initMotion x unit)


length2 : String -> ( Float, String ) -> ( Float, String ) -> Property
length2 name ( x, len ) ( x2, len2 ) =
    Property2 name
        (initMotion x len)
        (initMotion x2 len2)


length3 : String -> ( Float, String ) -> ( Float, String ) -> ( Float, String ) -> Property
length3 name ( x, len ) ( x2, len2 ) ( x3, len3 ) =
    Property3 name
        (initMotion x len)
        (initMotion x2 len2)
        (initMotion x3 len3)


{-| We convert the rgb channels to a float because that allows us to use the motion type without parametricity.
When rendering we convert them back to ints because CSS does not recognize rgb as floats.

-}
colorProp : String -> Color -> Property
colorProp name color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        ColorProperty name
            (initMotion (toFloat red) "")
            (initMotion (toFloat green) "")
            (initMotion (toFloat blue) "")
            (initMotion alpha "")


custom : String -> Float -> String -> Property
custom name value unit =
    Property name (initMotion value unit)


exactly : String -> String -> Property
exactly name value =
    ExactProperty name value


opacity : Float -> Property
opacity x =
    custom "opacity" x ""


display : DisplayMode -> Property
display mode =
    ExactProperty "display" (displayModeName mode)


none : DisplayMode
none =
    None


inline : DisplayMode
inline =
    Inline


inlineBlock : DisplayMode
inlineBlock =
    InlineBlock


block : DisplayMode
block =
    Block


flex : DisplayMode
flex =
    Flex


inlineFlex : DisplayMode
inlineFlex =
    InlineFlex


listItem : DisplayMode
listItem =
    ListItem


height : Length -> Property
height ( x, len ) =
    length "height" x (lengthUnitName len)


width : Length -> Property
width ( x, len ) =
    length "width" x (lengthUnitName len)


left : Length -> Property
left ( x, len ) =
    length "left" x (lengthUnitName len)


top : Length -> Property
top ( x, len ) =
    length "top" x (lengthUnitName len)


right : Length -> Property
right ( x, len ) =
    length "right" x (lengthUnitName len)


bottom : Length -> Property
bottom ( x, len ) =
    length "bottom" x (lengthUnitName len)


maxHeight : Length -> Property
maxHeight ( x, len ) =
    length "max-height" x (lengthUnitName len)


maxWidth : Length -> Property
maxWidth ( x, len ) =
    length "max-width" x (lengthUnitName len)


minHeight : Length -> Property
minHeight ( x, len ) =
    length "min-height" x (lengthUnitName len)


minWidth : Length -> Property
minWidth ( x, len ) =
    length "min-width" x (lengthUnitName len)


padding : Length -> Property
padding ( x, len ) =
    length "padding" x (lengthUnitName len)


paddingLeft : Length -> Property
paddingLeft ( x, len ) =
    length "padding-left" x (lengthUnitName len)


paddingRight : Length -> Property
paddingRight ( x, len ) =
    length "padding-right" x (lengthUnitName len)


paddingTop : Length -> Property
paddingTop ( x, len ) =
    length "padding-top" x (lengthUnitName len)


paddingBottom : Length -> Property
paddingBottom ( x, len ) =
    length "padding-bottom" x (lengthUnitName len)


margin : Length -> Property
margin ( x, len ) =
    length "margin" x (lengthUnitName len)


marginLeft : Length -> Property
marginLeft ( x, len ) =
    length "margin-left" x (lengthUnitName len)


marginRight : Length -> Property
marginRight ( x, len ) =
    length "margin-right" x (lengthUnitName len)


marginTop : Length -> Property
marginTop ( x, len ) =
    length "margin-top" x (lengthUnitName len)


marginBottom : Length -> Property
marginBottom ( x, len ) =
    length "margin-bottom" x (lengthUnitName len)


borderWidth : Length -> Property
borderWidth ( x, len ) =
    length "border-width" x (lengthUnitName len)


borderLeftWidth : Length -> Property
borderLeftWidth ( x, len ) =
    length "border-left-width" x (lengthUnitName len)


borderRightWidth : Length -> Property
borderRightWidth ( x, len ) =
    length "border-right-width" x (lengthUnitName len)


borderTopWidth : Length -> Property
borderTopWidth ( x, len ) =
    length "border-top-width" x (lengthUnitName len)


borderBottomWidth : Length -> Property
borderBottomWidth ( x, len ) =
    length "border-bottom-width" x (lengthUnitName len)


borderRadius : Length -> Property
borderRadius ( x, len ) =
    length "border-radius" x (lengthUnitName len)


borderTopLeftRadius : Length -> Property
borderTopLeftRadius ( x, len ) =
    length "border-top-left-radius" x (lengthUnitName len)


borderTopRightRadius : Length -> Property
borderTopRightRadius ( x, len ) =
    length "border-top-right-radius" x (lengthUnitName len)


borderBottomLeftRadius : Length -> Property
borderBottomLeftRadius ( x, len ) =
    length "border-bottom-left-radius" x (lengthUnitName len)


borderBottomRightRadius : Length -> Property
borderBottomRightRadius ( x, len ) =
    length "border-bottom-right-radius" x (lengthUnitName len)


letterSpacing : Length -> Property
letterSpacing ( x, len ) =
    length "letter-spacing" x (lengthUnitName len)


lineHeight : Length -> Property
lineHeight ( x, len ) =
    length "line-height" x (lengthUnitName len)


backgroundPosition : Length -> Length -> Property
backgroundPosition ( x, len1 ) ( y, len2 ) =
    length2 "background-position" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 )


color : Color -> Property
color c =
    colorProp "color" c


backgroundColor : Color -> Property
backgroundColor c =
    colorProp "background-color" c


borderColor : Color -> Property
borderColor c =
    colorProp "border-color" c


transformOrigin : Length -> Length -> Length -> Property
transformOrigin ( x, len1 ) ( y, len2 ) ( z, len3 ) =
    length3 "transform-origin" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 ) ( z, lengthUnitName len3 )


translate : Length -> Length -> Property
translate ( x, len1 ) ( y, len2 ) =
    length2 "translate" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 )


translate3d : Length -> Length -> Length -> Property
translate3d ( x, len1 ) ( y, len2 ) ( z, len3 ) =
    length3 "translate3d" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 ) ( z, lengthUnitName len3 )


translateX : Length -> Property
translateX ( x, len ) =
    length "translateX" x (lengthUnitName len)


translateY : Length -> Property
translateY ( x, len ) =
    length "translateY" x (lengthUnitName len)


translateZ : Length -> Property
translateZ ( x, len ) =
    length "translateZ" x (lengthUnitName len)


scale : Float -> Property
scale x =
    custom "scale" x ""



-- scale3d : Float -> Float -> Float -> Property
-- scale3d x y z =
--     length3 "scale3d" x y z


scaleX : Float -> Property
scaleX x =
    custom "scaleX" x ""


scaleY : Float -> Property
scaleY x =
    custom "scaleY" x ""


scaleZ : Float -> Property
scaleZ x =
    custom "scaleZ" x ""



-- Internally, angles are always in degrees which is why we throw away the angleUnit here.  It was already checked.


rotate : Angle -> Property
rotate ( x, angle ) =
    AngleProperty "rotate" (initMotion x "deg")



-- rotate3d : Float -> Float -> Float -> Angle -> Property


rotateX : Angle -> Property
rotateX ( x, angle ) =
    AngleProperty "rotateX" (initMotion x "deg")


rotateY : Angle -> Property
rotateY ( x, angle ) =
    AngleProperty "rotateY" (initMotion x "deg")


rotateZ : Angle -> Property
rotateZ ( x, angle ) =
    AngleProperty "rotateZ" (initMotion x "deg")



-- skew : Angle -> Angle -> Property


skewX : Angle -> Property
skewX ( x, angle ) =
    AngleProperty "skewX" (initMotion x "deg")


skewY : Angle -> Property
skewY ( x, angle ) =
    AngleProperty "skewY" (initMotion x "deg")


perspective : Float -> Property
perspective x =
    custom "perspective" x ""


type alias Shadow =
    { offsetX : Float
    , offsetY : Float
    , size : Float
    , blur : Float
    , color : Color
    }


type alias ShadowMotion =
    { offsetX : Motion
    , offsetY : Motion
    , size : Motion
    , blur : Motion
    , red : Motion
    , green : Motion
    , blue : Motion
    , alpha : Motion
    }


shadow : Shadow -> Property
shadow shade =
    let
        { red, green, blue, alpha } =
            Color.toRgb shade.color
    in
        ShadowProperty
            "box-shadow"
            False
            { offsetX = initMotion shade.offsetX "px"
            , offsetY = initMotion shade.offsetY "px"
            , size = initMotion shade.size "px"
            , blur = initMotion shade.blur "px"
            , red = initMotion (toFloat red) "px"
            , green = initMotion (toFloat green) "px"
            , blue = initMotion (toFloat blue) "px"
            , alpha = initMotion alpha "px"
            }


insetShadow : Shadow -> Property
insetShadow shade =
    let
        { red, green, blue, alpha } =
            Color.toRgb shade.color
    in
        ShadowProperty
            "box-shadow"
            True
            { offsetX = initMotion shade.offsetX "px"
            , offsetY = initMotion shade.offsetY "px"
            , size = initMotion shade.size "px"
            , blur = initMotion shade.blur "px"
            , red = initMotion (toFloat red) "px"
            , green = initMotion (toFloat green) "px"
            , blue = initMotion (toFloat blue) "px"
            , alpha = initMotion alpha "px"
            }



-- SVG properties


x : Float -> Property
x x =
    custom "x" x ""


y : Float -> Property
y y =
    custom "y" y ""


cx : Float -> Property
cx x =
    custom "cx" x ""


cy : Float -> Property
cy y =
    custom "cy" y ""


radius : Float -> Property
radius r =
    custom "r" r ""


radiusX : Float -> Property
radiusX rx =
    custom "rx" rx ""


radiusY : Float -> Property
radiusY ry =
    custom "ry" ry ""


path : List (PathCommand) -> Property
path commands =
    Path commands


move : Float -> Float -> PathCommand
move x y =
    Move (initMotion x "") (initMotion y "")


moveTo : Float -> Float -> PathCommand
moveTo x y =
    MoveTo (initMotion x "") (initMotion y "")


line : Float -> Float -> PathCommand
line x y =
    Line (initMotion x "") (initMotion y "")


lineTo : Float -> Float -> PathCommand
lineTo x y =
    LineTo (initMotion x "") (initMotion y "")


horizontal : Float -> PathCommand
horizontal x =
    Horizontal (initMotion x "")


horizontalTo : Float -> PathCommand
horizontalTo x =
    HorizontalTo (initMotion x "")


vertical : Float -> PathCommand
vertical x =
    Vertical (initMotion x "")


verticalTo : Float -> PathCommand
verticalTo x =
    VerticalTo (initMotion x "")


curve : List ( Float, Float ) -> PathCommand
curve points =
    Curve <| pointsProp points


curveTo : List ( Float, Float ) -> PathCommand
curveTo points =
    CurveTo <| pointsProp points


quadratic : List ( Float, Float ) -> PathCommand
quadratic points =
    Quadratic <| pointsProp points


quadraticTo : List ( Float, Float ) -> PathCommand
quadraticTo points =
    QuadraticTo <| pointsProp points


smooth : List ( Float, Float ) -> PathCommand
smooth points =
    Smooth <| pointsProp points


smoothTo : List ( Float, Float ) -> PathCommand
smoothTo points =
    SmoothTo <| pointsProp points


smoothQuadratic : List ( Float, Float ) -> PathCommand
smoothQuadratic points =
    SmoothQuadratic <| pointsProp points


smoothQuadraticTo : List ( Float, Float ) -> PathCommand
smoothQuadraticTo points =
    SmoothQuadraticTo <| pointsProp points


arc : Arc -> PathCommand
arc arc =
    ArcCmd <| initArcMotion arc False False


arcTo : Arc -> PathCommand
arcTo arc =
    ArcTo <| initArcMotion arc False False


{-| The same as `arc` except it goes the long way around the ellipse created.
-}
largeArc : Arc -> PathCommand
largeArc arc =
    ArcCmd <| initArcMotion arc True True


{-| The same as `arcTo` except it goes the long way around the ellipse created.
-}
largeArcTo : Arc -> PathCommand
largeArcTo arc =
    ArcTo <| initArcMotion arc True True


{-| The same as `arc` except it goes the long way around the ellipse created.
-}
sweptArc : Arc -> PathCommand
sweptArc arc =
    ArcCmd <| initArcMotion arc False True


{-| The same as `arcTo` except it goes the long way around the ellipse created.
-}
sweptArcTo : Arc -> PathCommand
sweptArcTo arc =
    ArcTo <| initArcMotion arc False True


{-| Expands an arc.

Equivalent to a large, unswept arc.
https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
-}
expandedArc : Arc -> PathCommand
expandedArc arc =
    ArcCmd <| initArcMotion arc True False


{-| Expands an arcTo.

Equivalent to a large, unswept arc.
https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
-}
expandedArcTo : Arc -> PathCommand
expandedArcTo arc =
    ArcTo <| initArcMotion arc True False


close : PathCommand
close =
    Close


{-| Describe a path.  To be used in conjunction with the 'd' property for styling svg.

`To` versions of the commands are absolute, while others are relative.

-}
type PathCommand
    = Move Motion Motion
    | MoveTo Motion Motion
    | Line Motion Motion
    | LineTo Motion Motion
    | Horizontal Motion
    | HorizontalTo Motion
    | Vertical Motion
    | VerticalTo Motion
    | Curve (List ( Motion, Motion ))
    | CurveTo (List ( Motion, Motion ))
    | Quadratic (List ( Motion, Motion ))
    | QuadraticTo (List ( Motion, Motion ))
    | SmoothQuadratic (List ( Motion, Motion ))
    | SmoothQuadraticTo (List ( Motion, Motion ))
    | Smooth (List ( Motion, Motion ))
    | SmoothTo (List ( Motion, Motion ))
    | ArcCmd ArcMotion
    | ArcTo ArcMotion
    | Close


type alias Arc =
    { x : Float
    , y : Float
    , radiusX : Float
    , radiusY : Float
    , xAxisRotation : Float
    }


type alias ArcMotion =
    { x : Motion
    , y : Motion
    , radiusX : Motion
    , radiusY : Motion
    , xAxisRotation : Motion
    , sweep : Bool
    , large : Bool
    }


initArcMotion : Arc -> Bool -> Bool -> ArcMotion
initArcMotion arc large sweep =
    { x = initMotion arc.x "px"
    , y = initMotion arc.y "px"
    , radiusX = initMotion arc.x "px"
    , radiusY = initMotion arc.y "px"
    , xAxisRotation = initMotion arc.xAxisRotation "deg"
    , sweep = sweep
    , large = large
    }


pointsProp : List ( Float, Float ) -> List ( Motion, Motion )
pointsProp pnts =
    List.map (\( x, y ) -> ( initMotion x "", initMotion y "" )) pnts


{-| Rendered as an attribute because it can't be represented as a style.
-}
points : List ( Float, Float ) -> Property
points pnts =
    Points <|
        pointsProp <|
            alignStartingPoint pnts


fill : Color -> Property
fill color =
    colorProp "fill" color


stroke : Color -> Property
stroke color =
    colorProp "stroke" color


{-| Given two lists of coordinates, rotate the list so that the lowest coordinate is first.

This is to align polygon coordinates so that they can morph smoothely into each other.
-}
alignStartingPoint : List ( Float, Float ) -> List ( Float, Float )
alignStartingPoint points =
    let
        sums =
            List.map (\( x, y ) -> x + y) points

        maybeMin =
            List.minimum sums

        indexOfLowestPoint =
            case maybeMin of
                Nothing ->
                    Nothing

                Just min ->
                    List.head <|
                        List.filterMap identity <|
                            List.indexedMap
                                (\i val ->
                                    if val == min then
                                        Just i
                                    else
                                        Nothing
                                )
                                sums
    in
        case indexOfLowestPoint of
            Nothing ->
                points

            Just i ->
                (List.drop i points) ++ (List.take i points)


{-| Ensure that two lists of points have the same number
of points by duplicating the last point of the smaller list.

-}
matchPoints : List ( Motion, Motion ) -> List ( Motion, Motion ) -> ( List ( Motion, Motion ), List ( Motion, Motion ) )
matchPoints points1 points2 =
    let
        diff =
            List.length points1 - List.length points2
    in
        if diff > 0 then
            case List.head <| List.reverse points2 of
                Nothing ->
                    ( points1, points2 )

                Just last2 ->
                    ( points1
                    , points2 ++ (List.repeat (abs diff) last2)
                    )
        else if diff < 0 then
            case List.head <| List.reverse points1 of
                Nothing ->
                    ( points1, points2 )

                Just last1 ->
                    ( points1 ++ (List.repeat (abs diff) last1)
                    , points2
                    )
        else
            ( points1, points2 )



-------------------------
-- Rendering
-------------------------


{-| Render style properties into the style attribute and render other attributes as needed for svg.

Combine "transform" based properties into a single css property.
-}
render : State msg -> List (Html.Attribute msg)
render (State model) =
    let
        ( attrProps, styleProps ) =
            List.partition isAttr model.style

        ( style, transforms ) =
            List.foldl
                (\prop ( style, transforms ) ->
                    if isTransformation prop then
                        ( style, transforms ++ [ prop ] )
                    else
                        ( style ++ [ prop ], transforms )
                )
                ( [], [] )
                styleProps

        renderedStyle =
            List.map (\prop -> ( propertyName prop, propertyValue prop " " )) style

        styleAttr =
            if List.length transforms == 0 then
                Html.Attributes.style <| List.concatMap prefix renderedStyle
            else
                Html.Attributes.style <|
                    List.concatMap prefix <|
                        ( "transform"
                        , String.concat <|
                            List.map
                                (\prop ->
                                    propertyName prop ++ "(" ++ (propertyValue prop ", ") ++ ")"
                                )
                                transforms
                        )
                            :: renderedStyle

        otherAttrs =
            List.filterMap renderAttrs attrProps
    in
        styleAttr :: otherAttrs


renderAttrs : Property -> Maybe (Html.Attribute msg)
renderAttrs prop =
    case prop of
        Points pts ->
            Just <| Svg.Attributes.points <| propertyValue (Points pts) " "

        Path cmds ->
            Just <| Svg.Attributes.d <| propertyValue (Path cmds) " "

        _ ->
            Nothing


isTransformation : Property -> Bool
isTransformation prop =
    List.member (propertyName prop)
        [ "rotate"
        , "rotate3d"
        , "transform"
        , "transform3d"
        , "translateX"
        , "translateY"
        , "scale"
        , "scale3d"
        , "scaleX"
        , "scaleY"
        , "scaleZ"
        , "skew"
        , "skewX"
        , "skewY"
        , "perspective"
        ]


iePrefix : String
iePrefix =
    "-ms-"


webkitPrefix : String
webkitPrefix =
    "-webkit-"


{-| Add a prefix to a name/value pair, if needed.
-}
prefix : ( String, String ) -> List ( String, String )
prefix stylePair =
    let
        propName =
            fst stylePair

        propValue =
            snd stylePair
    in
        case propName of
            "transform" ->
                [ stylePair
                , ( iePrefix ++ propName, propValue )
                , ( webkitPrefix ++ propName, propValue )
                ]

            "transform-origin" ->
                [ stylePair
                , ( iePrefix ++ propName, propValue )
                , ( webkitPrefix ++ propName, propValue )
                ]

            _ ->
                [ stylePair ]


{-| This property can only be represented as an html attribute
-}
isAttr : Property -> Bool
isAttr prop =
    case prop of
        Points _ ->
            True

        Path _ ->
            True

        _ ->
            False


propertyName : Property -> String
propertyName prop =
    case prop of
        ExactProperty name _ ->
            name

        ColorProperty name _ _ _ _ ->
            name

        ShadowProperty name _ _ ->
            name

        Property name _ ->
            name

        Property2 name _ _ ->
            name

        Property3 name _ _ _ ->
            name

        AngleProperty name _ ->
            name

        Points _ ->
            "points"

        Path _ ->
            "path"


displayModeName : DisplayMode -> String
displayModeName mode =
    case mode of
        None ->
            "none"

        Inline ->
            "inline"

        InlineBlock ->
            "inline-block"

        Block ->
            "block"

        Flex ->
            "flex"

        InlineFlex ->
            "inline-flex"

        ListItem ->
            "list-item"


propertyValue : Property -> String -> String
propertyValue prop delim =
    case prop of
        ExactProperty _ value ->
            value

        ColorProperty _ r g b a ->
            "rgba("
                ++ toString (round r.position)
                ++ ","
                ++ toString (round g.position)
                ++ ","
                ++ toString (round b.position)
                ++ ","
                ++ toString a.position
                ++ ")"

        ShadowProperty _ inset shadow ->
            (if inset then
                "inset "
             else
                ""
            )
                ++ toString shadow.offsetX.position
                ++ "px"
                ++ " "
                ++ toString shadow.offsetY.position
                ++ "px"
                ++ " "
                ++ toString shadow.blur.position
                ++ "px"
                ++ " "
                ++ toString shadow.size.position
                ++ "px"
                ++ " "
                ++ "rgba("
                ++ toString (round shadow.red.position)
                ++ ", "
                ++ toString (round shadow.green.position)
                ++ ", "
                ++ toString (round shadow.blue.position)
                ++ ", "
                ++ toString shadow.alpha.position
                ++ ")"

        Property _ x ->
            toString x.position ++ x.unit

        Property2 _ x y ->
            toString x.position
                ++ x.unit
                ++ delim
                ++ toString y.position
                ++ y.unit

        Property3 _ x y z ->
            toString x.position
                ++ x.unit
                ++ delim
                ++ toString y.position
                ++ y.unit
                ++ delim
                ++ toString z.position
                ++ z.unit

        AngleProperty _ x ->
            toString x.position ++ x.unit

        Points coords ->
            String.join " " <|
                List.map
                    (\( x, y ) ->
                        toString x.position ++ "," ++ toString y.position
                    )
                    coords

        Path cmds ->
            String.join " " <|
                List.map pathCmdValue cmds


pathCmdValue : PathCommand -> String
pathCmdValue cmd =
    let
        renderPoints coords =
            String.join " " <|
                List.map
                    (\( x, y ) ->
                        toString x.position ++ "," ++ toString y.position
                    )
                    coords
    in
        case cmd of
            Move x y ->
                "m " ++ toString x.position ++ "," ++ toString y.position

            MoveTo x y ->
                "M " ++ toString x.position ++ "," ++ toString y.position

            Line x y ->
                "l " ++ toString x.position ++ "," ++ toString y.position

            LineTo x y ->
                "L " ++ toString x.position ++ "," ++ toString y.position

            Horizontal a ->
                "h " ++ toString a.position

            HorizontalTo a ->
                "H " ++ toString a.position

            Vertical a ->
                "v " ++ toString a.position

            VerticalTo a ->
                "V " ++ toString a.position

            Curve points ->
                "c " ++ renderPoints points

            CurveTo points ->
                "C " ++ renderPoints points

            Quadratic points ->
                "q " ++ renderPoints points

            QuadraticTo points ->
                "Q " ++ renderPoints points

            SmoothQuadratic points ->
                "t " ++ renderPoints points

            SmoothQuadraticTo points ->
                "T " ++ renderPoints points

            Smooth points ->
                "s " ++ renderPoints points

            SmoothTo points ->
                "S " ++ renderPoints points

            ArcCmd arc ->
                "a "
                    ++ toString arc.radiusX.position
                    ++ ","
                    ++ toString arc.radiusY.position
                    ++ " "
                    ++ toString arc.xAxisRotation.position
                    ++ " "
                    ++ (if arc.large then
                            "1"
                        else
                            "0"
                       )
                    ++ " "
                    ++ (if arc.sweep then
                            "1"
                        else
                            "0"
                       )
                    ++ " "
                    ++ toString arc.x.position
                    ++ ","
                    ++ toString arc.y.position

            ArcTo arc ->
                "A "
                    ++ toString arc.radiusX.position
                    ++ ","
                    ++ toString arc.radiusY.position
                    ++ " "
                    ++ toString arc.xAxisRotation.position
                    ++ " "
                    ++ (if arc.large then
                            "1"
                        else
                            "0"
                       )
                    ++ " "
                    ++ (if arc.sweep then
                            "1"
                        else
                            "0"
                       )
                    ++ " "
                    ++ toString arc.x.position
                    ++ ","
                    ++ toString arc.y.position

            Close ->
                "z"
