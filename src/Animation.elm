module Animation
    exposing
        ( render
        , interrupt
        , queue
        , wait
        , subscription
        , State
        , to
        , tick
        , style
        , opacity
        , top
        , left
        , right
        , bottom
        , color
        , fill
        , backgroundColor
        , borderColor
        , rotateTo
        , translate
        , translateY
        , translateX
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
        , deg
        , isRunning
        )

import Color exposing (Color)
import Time exposing (Time, second)
import AnimationFrame
import String
import Html
import Html.Attributes
import Svg.Attributes


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


{-| Combine "transform" based properties into a single css property.

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
                        ( style, prop :: transforms )
                    else
                        ( prop :: style, transforms )
                )
                ( [], [] )
                styleProps

        renderedStyle =
            List.map (\prop -> ( propertyName prop, propertyValue prop " " )) style

        styleAttr =
            if List.length transforms == 0 then
                Html.Attributes.style renderedStyle
            else
                Html.Attributes.style <|
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


{-|

-}
type Step msg
    = To (List Property)
    | Step
    | Wait Time
    | Send msg
    | Repeat Int (List (Step msg))
    | Loop (List (Step msg))


type Interpolation
    = Spring
        { stiffness : Float
        , damping : Float
        }
    | Easing
        { progress : Float
        , duration : Time
        , ease : Float -> Float
        }


type alias Style =
    List Property


{-| For each 'value' we track position, velocity, and target.
-}
type Property
    = Display DisplayMode
    | ColorProperty String Motion Motion Motion Motion
    | FloatProperty String Motion
    | LengthProperty String Motion LengthUnit
    | LengthProperty2 String Motion Motion LengthUnit LengthUnit
    | LengthProperty3 String Motion Motion Motion LengthUnit LengthUnit LengthUnit
    | AngleProperty String Motion AngleUnit
    | Points (List ( Motion, Motion ))
    | Path (List PathCommand)


type alias Motion =
    { position : Float
    , velocity : Float
    , target : Float
    , interpolation : Interpolation
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


defaultInterpolation : Interpolation
defaultInterpolation =
    Spring
        { stiffness = 170
        , damping = 26
        }


setDefaultInterpolation : Property -> Property
setDefaultInterpolation prop =
    let
        interp =
            defaultInterpolationByProperty prop
    in
        setInterpolation interp prop


defaultInterpolationByProperty : Property -> Interpolation
defaultInterpolationByProperty prop =
    let
        spring =
            Spring
                { stiffness = 170
                , damping = 26
                }

        linear duration =
            Easing
                { progress = 0
                , duration = duration
                , ease = identity
                }
    in
        case prop of
            Display _ ->
                spring

            ColorProperty _ _ _ _ _ ->
                linear (1 * second)

            FloatProperty _ _ ->
                spring

            LengthProperty _ _ _ ->
                spring

            LengthProperty2 _ _ _ _ _ ->
                spring

            LengthProperty3 _ _ _ _ _ _ _ ->
                spring

            AngleProperty _ _ _ ->
                linear (2 * second)

            Points _ ->
                spring

            Path _ ->
                spring


propertyName : Property -> String
propertyName prop =
    case prop of
        Display _ ->
            "display"

        ColorProperty name _ _ _ _ ->
            name

        FloatProperty name _ ->
            name

        LengthProperty name _ _ ->
            name

        LengthProperty2 name _ _ _ _ ->
            name

        LengthProperty3 name _ _ _ _ _ _ ->
            name

        AngleProperty name _ _ ->
            name

        Points _ ->
            "points"

        Path _ ->
            "path"


propertyValue : Property -> String -> String
propertyValue prop delim =
    case prop of
        Display mode ->
            displayModeName mode

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

        FloatProperty _ x ->
            toString x.position

        LengthProperty _ x unit ->
            toString x.position ++ lengthUnitName unit

        LengthProperty2 _ x y unit1 unit2 ->
            toString x.position
                ++ lengthUnitName unit1
                ++ delim
                ++ toString y.position
                ++ lengthUnitName unit2

        LengthProperty3 _ x y z unit1 unit2 unit3 ->
            toString x.position
                ++ lengthUnitName unit1
                ++ delim
                ++ toString y.position
                ++ lengthUnitName unit2
                ++ delim
                ++ toString z.position
                ++ lengthUnitName unit3

        AngleProperty _ x unit ->
            toString x.position ++ angleUnitName unit

        Points coords ->
            String.join " " <|
                List.map
                    (\( x, y ) ->
                        toString x.position ++ "," ++ toString y.position
                    )
                    coords

        Path cmds ->
            String.join " " <|
                List.map cmdValue cmds


cmdValue : PathCommand -> String
cmdValue cmd =
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


wait : Time -> Step msg
wait till =
    Wait till


to : List Property -> Step msg
to props =
    To props


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
subscription : State msg -> (Time -> msg) -> Sub msg
subscription (State model) msg =
    if model.running then
        AnimationFrame.times msg
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


tick : Time -> State msg -> ( State msg, List msg )
tick now (State model) =
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

        -- if there are more than one matching interruptions,
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
        , sentMessages
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


{-| Property is done?

TODO: What about interlaced property animations?

-}
isDone : Property -> Bool
isDone property =
    let
        motionDone motion =
            motion.velocity == 0 && motion.position == motion.target
    in
        case property of
            Display _ ->
                True

            ColorProperty _ m1 m2 m3 m4 ->
                List.all motionDone [ m1, m2, m3, m4 ]

            FloatProperty _ m1 ->
                motionDone m1

            LengthProperty _ m1 _ ->
                motionDone m1

            LengthProperty2 _ m1 m2 _ _ ->
                motionDone m1 && motionDone m2

            LengthProperty3 _ m1 m2 m3 _ _ _ ->
                List.all motionDone [ m1, m2, m3 ]

            AngleProperty _ m1 _ ->
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
                    Just <| setTarget (default target) target
        )
        (zipPropertiesGreedy current target)


setInterpolation : Interpolation -> Property -> Property
setInterpolation interp prop =
    case prop of
        Display mode ->
            Display mode

        ColorProperty name m1 m2 m3 m4 ->
            ColorProperty name
                { m1 | interpolation = interp }
                { m2 | interpolation = interp }
                { m3 | interpolation = interp }
                { m4 | interpolation = interp }

        FloatProperty name m1 ->
            FloatProperty name
                { m1 | interpolation = interp }

        LengthProperty name m1 unit ->
            LengthProperty name
                { m1 | interpolation = interp }
                unit

        LengthProperty2 name m1 m2 unit1 unit2 ->
            LengthProperty2 name
                { m1 | interpolation = interp }
                { m2 | interpolation = interp }
                unit1
                unit2

        LengthProperty3 name m1 m2 m3 unit1 unit2 unit3 ->
            LengthProperty3 name
                { m1 | interpolation = interp }
                { m2 | interpolation = interp }
                { m3 | interpolation = interp }
                unit1
                unit2
                unit3

        AngleProperty name m1 unit ->
            AngleProperty name
                { m1 | interpolation = interp }
                unit

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


setTarget : Property -> Property -> Property
setTarget current newTarget =
    case current of
        Display mode ->
            Display mode

        ColorProperty name m1 m2 m3 m4 ->
            case newTarget of
                ColorProperty _ t1 t2 t3 t4 ->
                    ColorProperty name
                        { m1 | target = t1.position }
                        { m2 | target = t2.position }
                        { m3 | target = t3.position }
                        { m4 | target = t4.position }

                _ ->
                    current

        FloatProperty name m1 ->
            case newTarget of
                FloatProperty _ t1 ->
                    FloatProperty name
                        { m1 | target = t1.position }

                _ ->
                    current

        LengthProperty name m1 unit ->
            case newTarget of
                LengthProperty _ t1 _ ->
                    LengthProperty name
                        { m1 | target = t1.position }
                        unit

                _ ->
                    current

        LengthProperty2 name m1 m2 unit1 unit2 ->
            case newTarget of
                LengthProperty2 _ t1 t2 _ _ ->
                    LengthProperty2 name
                        { m1 | target = t1.position }
                        { m2 | target = t2.position }
                        unit1
                        unit2

                _ ->
                    current

        LengthProperty3 name m1 m2 m3 unit1 unit2 unit3 ->
            case newTarget of
                LengthProperty3 _ t1 t2 t3 _ _ _ ->
                    LengthProperty3 name
                        { m1 | target = t1.position }
                        { m2 | target = t2.position }
                        { m3 | target = t3.position }
                        unit1
                        unit2
                        unit3

                _ ->
                    current

        AngleProperty name m1 unit ->
            case newTarget of
                AngleProperty _ t1 _ ->
                    AngleProperty name
                        { m1 | target = t1.position }
                        unit

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
                                    ( { x1 | target = x2.position }
                                    , { y1 | target = y2.position }
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


{-| We match two sets of properties that have any degree of overlap.

We do a fold over the maximum number of combinations it could be
(the lengths of boths lists together).

Order matters.


-}
zipPropertiesGreedy : List Property -> List Property -> List ( Maybe Property, Maybe Property )
zipPropertiesGreedy listA listB =
    let
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


propertyMatch : Property -> Property -> Bool
propertyMatch prop1 prop2 =
    propertyName prop1 == propertyName prop2


step : Time -> List Property -> List Property
step dt props =
    let
        stepProp property =
            case property of
                Display mode ->
                    Display mode

                FloatProperty name motion ->
                    FloatProperty name (stepSpring dt motion)

                LengthProperty name motion unit ->
                    LengthProperty name (stepSpring dt motion) unit

                AngleProperty name motion unit ->
                    AngleProperty name (stepSpring dt motion) unit

                LengthProperty2 name motion1 motion2 unit1 unit2 ->
                    LengthProperty2 name
                        (stepSpring dt motion1)
                        (stepSpring dt motion2)
                        unit1
                        unit2

                LengthProperty3 name motion1 motion2 motion3 unit1 unit2 unit3 ->
                    LengthProperty3 name
                        (stepSpring dt motion1)
                        (stepSpring dt motion2)
                        (stepSpring dt motion3)
                        unit1
                        unit2
                        unit3

                ColorProperty name red green blue alpha ->
                    ColorProperty name
                        (stepSpring dt red)
                        (stepSpring dt green)
                        (stepSpring dt blue)
                        (stepSpring dt alpha)

                Points points ->
                    Points <|
                        List.map
                            (\( x, y ) ->
                                ( stepSpring dt x
                                , stepSpring dt y
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
                    ( stepSpring dt x
                    , stepSpring dt y
                    )
                )
                coords
    in
        case cmd of
            Move m1 m2 ->
                Move
                    (stepSpring dt m1)
                    (stepSpring dt m2)

            MoveTo m1 m2 ->
                MoveTo
                    (stepSpring dt m1)
                    (stepSpring dt m2)

            Line m1 m2 ->
                Line
                    (stepSpring dt m1)
                    (stepSpring dt m2)

            LineTo m1 m2 ->
                LineTo
                    (stepSpring dt m1)
                    (stepSpring dt m2)

            Horizontal motion ->
                Horizontal
                    (stepSpring dt motion)

            HorizontalTo motion ->
                HorizontalTo
                    (stepSpring dt motion)

            Vertical motion ->
                Vertical
                    (stepSpring dt motion)

            VerticalTo motion ->
                VerticalTo
                    (stepSpring dt motion)

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
                        | x = stepSpring dt arc.x
                        , y = stepSpring dt arc.y
                        , radiusX = stepSpring dt arc.radiusX
                        , radiusY = stepSpring dt arc.radiusY
                        , xAxisRotation = stepSpring dt arc.xAxisRotation
                    }

            ArcTo arc ->
                ArcTo <|
                    { arc
                        | x = stepSpring dt arc.x
                        , y = stepSpring dt arc.y
                        , radiusX = stepSpring dt arc.radiusX
                        , radiusY = stepSpring dt arc.radiusY
                        , xAxisRotation = stepSpring dt arc.xAxisRotation
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
stepSpring : Time -> Motion -> Motion
stepSpring dtms motion =
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

        Easing { progress, duration, ease } ->
            let
                newProgress =
                    if (dtms / duration) + progress < 1 then
                        (dtms / duration) + progress
                    else
                        1

                dEased =
                    (ease newProgress) - (ease progress)

                distance =
                    motion.target - motion.position

                newValue =
                    (dEased * distance) + motion.position

                sprungMotion =
                    stepSpring dtms
                        { motion
                            | target = newValue
                            , interpolation =
                                Spring { stiffness = 500, damping = 0 }
                        }

                updatedProgress =
                    Easing
                        { progress = newProgress
                        , duration = duration
                        , ease = ease
                        }
            in
                { sprungMotion
                    | target = motion.target
                    , interpolation = updatedProgress
                }



-- resolveInterruption : Maybe ( Time, Animation msg )
-- Do any interruptions take effect?
-- If so, implement them
-- Resolve steps.  This could mean multiple steps need to be resolved.
-- Send, Wait 0, and Set all resolve immediately.
--
-- Steps keep resolving until an unresolvable step is encountered
--     Such as Repeat n, Loop, or To.


{-| Given a property, return the same property with the value set to a default.

TODO: Path property could have a more intelligent default
-}
default : Property -> Property
default property =
    case property of
        Display mode ->
            Display Block

        ColorProperty name _ _ _ _ ->
            Debug.log (name ++ " has no initial value.  Defaulting to transparent white.") <|
                colorProp name (Color.rgba 255 255 255 0)

        FloatProperty name _ ->
            unitless name 0

        LengthProperty name _ unit ->
            length name ( 0, unit )

        LengthProperty2 name _ _ unit1 unit2 ->
            length2 name ( 0, unit1 ) ( 0, unit2 )

        LengthProperty3 name _ _ _ unit1 unit2 unit3 ->
            length3 name ( 0, unit1 ) ( 0, unit2 ) ( 0, unit3 )

        AngleProperty name _ unit ->
            angleProp name ( 0, unit )

        Points pnts ->
            Points <| List.map (\_ -> ( initMotion 0, initMotion 0 )) pnts

        Path cmds ->
            Path []


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
The reason for this is that later on we handle units separately from a 'float' value.

See the 'Motion' type.

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


initMotion : Float -> Motion
initMotion position =
    { position = position
    , velocity = 0
    , target = position
    , interpolation = defaultInterpolation
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


unitless : String -> Float -> Property
unitless name x =
    LengthProperty name (initMotion x) NoUnit


length : String -> Length -> Property
length name ( x, len ) =
    LengthProperty name (initMotion x) len


length2 : String -> Length -> Length -> Property
length2 name ( x, len ) ( x2, len2 ) =
    LengthProperty2 name
        (initMotion x)
        (initMotion x2)
        len
        len2


length3 : String -> Length -> Length -> Length -> Property
length3 name ( x, len ) ( x2, len2 ) ( x3, len3 ) =
    LengthProperty3 name
        (initMotion x)
        (initMotion x2)
        (initMotion x3)
        len
        len2
        len3


angleProp : String -> Angle -> Property
angleProp name ( x, ang ) =
    AngleProperty name (initMotion x) ang


{-| We convert the rgb channels to a float because that allows us to use the motion type without parametricity.
When rendering we convert them back to ints because CSS does not recognize them otherwise.

-}
colorProp : String -> Color -> Property
colorProp name color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        ColorProperty name
            (initMotion <| toFloat red)
            (initMotion <| toFloat green)
            (initMotion <| toFloat blue)
            (initMotion alpha)


opacity : Float -> Property
opacity x =
    unitless "opacity" x


height : Length -> Property
height len =
    length "height" len


width : Length -> Property
width len =
    length "width" len


left : Length -> Property
left len =
    length "left" len


top : Length -> Property
top len =
    length "top" len


right : Length -> Property
right len =
    length "right" len


bottom : Length -> Property
bottom len =
    length "bottom" len


maxHeight : Length -> Property
maxHeight len =
    length "max-height" len


maxWidth : Length -> Property
maxWidth len =
    length "max-width" len


minHeight : Length -> Property
minHeight len =
    length "min-height" len


minWidth : Length -> Property
minWidth len =
    length "min-width" len


padding : Length -> Property
padding len =
    length "padding" len


paddingLeft : Length -> Property
paddingLeft len =
    length "padding-left" len


paddingRight : Length -> Property
paddingRight len =
    length "padding-right" len


paddingTop : Length -> Property
paddingTop len =
    length "padding-top" len


paddingBottom : Length -> Property
paddingBottom len =
    length "padding-bottom" len


margin : Length -> Property
margin len =
    length "margin" len


marginLeft : Length -> Property
marginLeft len =
    length "margin-left" len


marginRight : Length -> Property
marginRight len =
    length "margin-right" len


marginTop : Length -> Property
marginTop len =
    length "margin-top" len


marginBottom : Length -> Property
marginBottom len =
    length "margin-bottom" len


borderWidth : Length -> Property
borderWidth len =
    length "border-width" len


borderRadius : Length -> Property
borderRadius len =
    length "border-radius" len


borderTopLeftRadius : Length -> Property
borderTopLeftRadius len =
    length "border-top-left-radius" len


borderTopRightRadius : Length -> Property
borderTopRightRadius len =
    length "border-top-right-radius" len


borderBottomLeftRadius : Length -> Property
borderBottomLeftRadius len =
    length "border-bottom-left-radius" len


borderBottomRightRadius : Length -> Property
borderBottomRightRadius len =
    length "border-bottom-right-radius" len


letterSpacing : Length -> Property
letterSpacing len =
    length "letter-spacing" len


lineHeight : Length -> Property
lineHeight len =
    length "line-height" len


backgroundPosition : Length -> Length -> Property
backgroundPosition x y =
    length2 "background-position" x y


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
transformOrigin x y z =
    length3 "transform-origin" x y z


translate : Length -> Length -> Property
translate x y =
    length2 "translate" x y


translate3d : Length -> Length -> Length -> Property
translate3d x y z =
    length3 "translate3d" x y z


translateX : Length -> Property
translateX len =
    length "translate-x" len


translateY : Length -> Property
translateY len =
    length "translate-y" len


scale : Float -> Property
scale x =
    unitless "scale" x



-- scale3d : Float -> Float -> Float -> Property
-- scale3d x y z =
--     length3 "scale3d" x y z


scaleX : Float -> Property
scaleX x =
    unitless "scale-x" x


scaleY : Float -> Property
scaleY x =
    unitless "scale-y" x


scaleZ : Float -> Property
scaleZ x =
    unitless "scale-z" x


rotateTo : Angle -> Property
rotateTo angle =
    angleProp "rotate" angle



-- rotate3d : Float -> Float -> Float -> Angle -> Property


rotateX : Angle -> Property
rotateX angle =
    angleProp "rotate-x" angle


rotateY : Angle -> Property
rotateY angle =
    angleProp "rotate-y" angle


rotateZ : Angle -> Property
rotateZ angle =
    angleProp "rotate-z" angle



-- skew : Angle -> Angle -> Property


skewX : Angle -> Property
skewX angle =
    angleProp "skew-x" angle


skewY : Angle -> Property
skewY angle =
    angleProp "skew-y" angle


perspective : Float -> Property
perspective x =
    unitless "perspective" x



-- SVG properties


x : Float -> Property
x x =
    unitless "x" x


y : Float -> Property
y y =
    unitless "y" y


cx : Float -> Property
cx x =
    unitless "cx" x


cy : Float -> Property
cy y =
    unitless "cy" y


radius : Float -> Property
radius r =
    unitless "r" r


radiusX : Float -> Property
radiusX rx =
    unitless "rx" rx


radiusY : Float -> Property
radiusY ry =
    unitless "ry" ry


path : List (PathCommand) -> Property
path commands =
    Path commands


move : Float -> Float -> PathCommand
move x y =
    Move (initMotion x) (initMotion y)


moveTo : Float -> Float -> PathCommand
moveTo x y =
    MoveTo (initMotion x) (initMotion y)


line : Float -> Float -> PathCommand
line x y =
    Line (initMotion x) (initMotion y)


lineTo : Float -> Float -> PathCommand
lineTo x y =
    LineTo (initMotion x) (initMotion y)


horizontal : Float -> PathCommand
horizontal x =
    Horizontal (initMotion x)


horizontalTo : Float -> PathCommand
horizontalTo x =
    HorizontalTo (initMotion x)


vertical : Float -> PathCommand
vertical x =
    Vertical (initMotion x)


verticalTo : Float -> PathCommand
verticalTo x =
    VerticalTo (initMotion x)


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


setPathTarget : PathCommand -> PathCommand -> PathCommand
setPathTarget cmd targetCmd =
    case cmd of
        Move m1 m2 ->
            case targetCmd of
                Move t1 t2 ->
                    Move
                        { m1 | target = t1.position }
                        { m2 | target = t2.position }

                _ ->
                    cmd

        MoveTo m1 m2 ->
            case targetCmd of
                MoveTo t1 t2 ->
                    MoveTo
                        { m1 | target = t1.position }
                        { m2 | target = t2.position }

                _ ->
                    cmd

        Line m1 m2 ->
            case targetCmd of
                Line t1 t2 ->
                    Line
                        { m1 | target = t1.position }
                        { m2 | target = t2.position }

                _ ->
                    cmd

        LineTo m1 m2 ->
            case targetCmd of
                LineTo t1 t2 ->
                    LineTo
                        { m1 | target = t1.position }
                        { m2 | target = t2.position }

                _ ->
                    cmd

        Horizontal motion ->
            case targetCmd of
                Horizontal target ->
                    Horizontal
                        { motion | target = target.position }

                _ ->
                    cmd

        HorizontalTo motion ->
            case targetCmd of
                HorizontalTo target ->
                    HorizontalTo
                        { motion | target = target.position }

                _ ->
                    cmd

        Vertical motion ->
            case targetCmd of
                Vertical target ->
                    Vertical
                        { motion | target = target.position }

                _ ->
                    cmd

        VerticalTo motion ->
            case targetCmd of
                VerticalTo target ->
                    VerticalTo
                        { motion | target = target.position }

                _ ->
                    cmd

        Curve coords ->
            case targetCmd of
                Curve targetCoords ->
                    Curve <|
                        List.map2
                            (\( x1, y1 ) ( x2, y2 ) ->
                                ( { x1 | target = x2.position }
                                , { y1 | target = y2.position }
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
                                ( { x1 | target = x2.position }
                                , { y1 | target = y2.position }
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
                                ( { x1 | target = x2.position }
                                , { y1 | target = y2.position }
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
                                ( { x1 | target = x2.position }
                                , { y1 | target = y2.position }
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
                                ( { x1 | target = x2.position }
                                , { y1 | target = y2.position }
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
                                ( { x1 | target = x2.position }
                                , { y1 | target = y2.position }
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
                                ( { x1 | target = x2.position }
                                , { y1 | target = y2.position }
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
                                ( { x1 | target = x2.position }
                                , { y1 | target = y2.position }
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
                                | x = { x | target = target.x.position }
                                , y = { y | target = target.y.position }
                                , radiusX = { radiusX | target = target.radiusX.position }
                                , radiusY = { radiusY | target = target.radiusY.position }
                                , xAxisRotation = { xAxis | target = target.xAxisRotation.position }
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
                                | x = { x | target = target.x.position }
                                , y = { y | target = target.y.position }
                                , radiusX = { radiusX | target = target.radiusX.position }
                                , radiusY = { radiusY | target = target.radiusY.position }
                                , xAxisRotation = { xAxis | target = target.xAxisRotation.position }
                            }

                _ ->
                    cmd

        Close ->
            Close


initArcMotion : Arc -> Bool -> Bool -> ArcMotion
initArcMotion arc large sweep =
    { x = initMotion arc.x
    , y = initMotion arc.y
    , radiusX = initMotion arc.x
    , radiusY = initMotion arc.y
    , xAxisRotation = initMotion arc.xAxisRotation
    , sweep = sweep
    , large = large
    }


pointsProp : List ( Float, Float ) -> List ( Motion, Motion )
pointsProp pnts =
    List.map (\( x, y ) -> ( initMotion x, initMotion y )) pnts


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
