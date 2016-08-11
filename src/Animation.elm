module Animation exposing (interrupt, queue, subscription)

import Color exposing (Color)
import AnimationFrame


type State msg
    = State
        { animationQueue : Animation msg
        , currentStyle : List ( Spring, Property, Target )
        , time : Timing
        , running : Bool
        , interruption : Maybe ( Time, Animation msg )
        }


type Timing
    = Timing
        { current : Time
        , dt : Time
        }


type alias Animation msg =
    List (Step msg)


type Target
    = Target Motion
    | Target2 Motion Motion
    | Target3 Motion Motion Motion
    | Target4 Motion Motion Motion Motion


type Motion
    = Motion
        { velocity : Float
        , target : Float
        }


initMotion : Float -> Motion
initMotion target =
    Motion
        { velocity = 0
        , target = target
        }


makeTarget : Property -> Target
makeTarget property =
    case property of
        ColorProperty _ color ->
            let
                { red, green, blue, alpha } =
                    Color.toRgb color
            in
                Target4
                    (initMotion red)
                    (initMotion green)
                    (initMotion blue)
                    (initMotion alpha)

        FloatProperty _ x ->
            Target <| initMotion x

        LengthProperty _ x ->
            Target <| initMotion x

        LengthProperty2 _ x y ->
            Target2 (initMotion x) (initMotion y)

        LengthProperty3 _ x y z ->
            Target3 (initMotion x) (initMotion y) (initMotion z)

        AngleProperty _ x ->
            Target <| initMotion x


type Step msg
    = To (List Property)
    | Wait Time
    | Send msg
    | Repeat Int (List (Step msg))
    | Loop (List (Step msg))


type Spring
    = Spring { stiffness : Float, damping : Float }
    | Easing Time (Float -> Float)


type Style
    = Style (List ( Spring, Property ))


defaultSpring : Spring
defaultSpring =
    Spring
        { stiffness = 170
        , damping = 26
        }


type Property
    = ColorProperty String Color
    | FloatProperty String Float
    | LengthProperty String Length
    | LengthProperty2 String Length Length
    | LengthProperty3 String Length Length Length
    | AngleProperty String Angle


style : List Property -> Style
style props =
    Style <| List.map (\prop -> ( defaultSpring, prop, makeTarget prop )) props


styleWith : Options -> List Property -> Style
styleWith spring props =
    Style <| List.map (\prop -> ( spring, prop, makeTarget prop )) props


styleWithEach : List ( Options, Property ) -> Style
styleWithEach props =
    Style <| List.map (\( opts, prop ) -> ( opts, prop, makeTarget prop )) props


queue : Animation msg -> State msg -> State msg
queue steps state =
    { state
        | animationQueue = state.animationQueue ++ steps
        , running = True
    }


interrupt : Animation msg -> State msg -> State msg
interrupt steps state =
    let
        ( wait, remainingSteps ) =
            extractInitialWait steps
    in
        if wait == 0 then
            { state
                | animationQueue = remainingSteps
                , running = True
            }
        else
            case state.interruption of
                Nothing ->
                    { state
                        | interruption = ( wait, remainingSteps )
                        , running = True
                    }

                Just ( interruptionTime, interruptionAnim ) ->
                    if interruptionTime < wait then
                        state
                    else
                        { state
                            | interruption = ( wait, remainingSteps )
                            , running = True
                        }


{-| Sums all leading `Wait` steps and removes them from the animation.

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
                            initialWait (List.drop 1 steps)
                    in
                        ( wait + additionalTime, remainingSteps )

                _ ->
                    ( 0, steps )


{-| Create a subscription to AnimationFrame.times. It is throttled based on whether the current animation is running or not.

-}
subscription : State msg -> msg -> Sub msg
subscription state msg =
    if state.running then
        AnimationFrame.times msg
    else
        Sub.none



-- type State msg
--     = State
--         { animationQueue : Animation msg
--         , currentStyle : List ( Spring, Property, Target )
--         , timeOffset : Time
--         , running : Bool
--         , interruption : Maybe ( Time, Animation msg )
--         }


setTiming : Time -> State msg -> State msg
setTiming now (State model) =
    let
        dt =
            now - model.times.current

        -- dt is set to 0 if it is a large dt,
        -- because that usually means that the user
        -- left the browser window and came back.
        newTimes =
            { current = now
            , dt =
                if dt > 300 then
                    0.0
                else
                    dt
            }
    in
        State
            { model
                | times = newTimes
            }


tick : Time -> State msg -> ( State msg, List msg )
tick now (State model) =
    let
        timing =
            setTiming now (State model)

        ( interruption, queue ) =
            case model.interruption of
                Nothing ->
                    ( Nothing, model.animationQueue )

                Just ( countDown, anim ) ->
                    if countDown <= 0 then
                        ( Nothing, model.interruption )
                    else
                        ( Just ( countDown - timing.dt, anim )
                        , model.animationQueue
                        )
    in
        State model



-- resolveInterruption : Maybe ( Time, Animation msg )
-- Do any interruptions take effect?
-- If so, implement them
-- Resolve steps.  This could mean multiple steps need to be resolved.
-- Send, Wait 0, and Set all resolve immediately.
--
-- Steps keep resolving until an unresolvable step is encountered
--     Such as Repeat n, Loop, or To.


{-| Given a property, return the same property with the value set to a default.

-}
default : Property -> Property
default property =
    case property of
        ColorProperty name color ->
            Debug.log (name ++ " has no initial value.  Defaulting to transparent white.") <|
                ColorProperty name (Color.rgba 255 255 255 0)

        FloatProperty name _ ->
            FloatProperty name 0

        LengthProperty name _ ->
            LengthProperty name (Px 0)

        LengthProperty2 name _ _ ->
            LengthProperty name (Px 0) (Px 0)

        LengthProperty3 name _ _ _ ->
            LengthProperty name (Px 0) (Px 0) (Px 0)

        AngleProperty name _ ->
            AngleProperty name (Deg 0)


type Length
    = Px Float
    | Percent Float
    | Rem Float
    | Em Float
    | Ex Float
    | Ch Float
    | Vh Float
    | Vw Float
    | Vmin Float
    | Vmax Float
    | Mm Float
    | Cm Float
    | In Float
    | Pt Float
    | Pc Float


type Angle
    = Deg Float


deg : Float -> Angle
deg a =
    Deg a


grad : Float -> Angle
grad a =
    Deg <| (a / 400) * 360


rad : Float -> Angle
rad a =
    Deg <| (a / (2 * pi)) * 360


turn : Float -> Angle
turn a =
    Deg (a * 360)



-- Lengths


px : Float -> Length
px x =
    Px x


percent : Float -> Length
percent x =
    Percent x


rem : Float -> Length
rem x =
    Rem x


em : Float -> Length
em x =
    Em x


ex : Float -> Length
ex x =
    Ex x


ch : Float -> Length
ch x =
    Ch x


vh : Float -> Length
vh x =
    Vh x


vw : Float -> Length
vw x =
    Vw x


vmin : Float -> Length
vmin x =
    Vmin x


vmax : Float -> Length
vmax x =
    Vmax x


mm : Float -> Length
mm x =
    Mm x


cm : Float -> Length
cm x =
    Cm x


inches : Float -> Length
inches x =
    In x


pt : Float -> Length
pt x =
    Pt x


pc : Float -> Length
pc x =
    Pc x



---- Properties


opacity : Float -> Property
opacity x =
    FloatProperty "opacity" x


height : Length -> Property
height len =
    LengthProperty "height" len


width : Length -> Property
width len =
    LengthProperty "width" len


left : Length -> Property
left len =
    LengthProperty "left" len


top : Length -> Property
top len =
    LengthProperty "top" len


right : Length -> Property
right len =
    LengthProperty "right" len


bottom : Length -> Property
bottom len =
    LengthProperty "bottom" len


maxHeight : Length -> Property
maxHeight len =
    LengthProperty "max-height" len


maxWidth : Length -> Property
maxWidth len =
    LengthProperty "max-width" len


minHeight : Length -> Property
minHeight len =
    LengthProperty "min-height" len


minWidth : Length -> Property
minWidth len =
    LengthProperty "min-width" len


padding : Length -> Property
padding len =
    LengthProperty "padding" len


paddingLeft : Length -> Property
paddingLeft len =
    LengthProperty "padding-left" len


paddingRight : Length -> Property
paddingRight len =
    LengthProperty "padding-right" len


paddingTop : Length -> Property
paddingTop len =
    LengthProperty "padding-top" len


paddingBottom : Length -> Property
paddingBottom len =
    LengthProperty "padding-bottom" len


margin : Length -> Property
margin len =
    LengthProperty "margin" len


marginLeft : Length -> Property
marginLeft len =
    LengthProperty "margin-left" len


marginRight : Length -> Property
marginRight len =
    LengthProperty "margin-right" len


marginTop : Length -> Property
marginTop len =
    LengthProperty "margin-top" len


marginBottom : Length -> Property
marginBottom len =
    LengthProperty "margin-bottom" len


borderWidth : Length -> Property
borderWidth len =
    LengthProperty "border-width" len


borderRadius : Length -> Property
borderRadius len =
    LengthProperty "border-radius" len


borderTopLeftRadius : Length -> Property
borderTopLeftRadius len =
    LengthProperty "border-top-left-radius" len


borderTopRightRadius : Length -> Property
borderTopRightRadius len =
    LengthProperty "border-top-right-radius" len


borderBottomLeftRadius : Length -> Property
borderBottomLeftRadius len =
    LengthProperty "border-bottom-left-radius" len


borderBottomRightRadius : Length -> Property
borderBottomRightRadius len =
    LengthProperty "border-bottom-right-radius" len


letterSpacing : Length -> Property
letterSpacing len =
    LengthProperty "letter-spacing" len


lineHeight : Length -> Property
lineHeight len =
    LengthProperty "line-height" len


backgroundPosition : Length -> Length -> Property
backgroundPosition x y =
    LengthProperty2 "background-position" x y


color : Color -> Property
color c =
    ColorProperty "color" c


backgroundColor : Color -> Property
backgroundColor c =
    ColorProperty "background-color" c


borderColor : Color -> Property
borderColor c =
    ColorProperty "border-color" c


transformOrigin : Length -> Length -> Length -> Property
transformOrigin x y z =
    LengthProperty3 "transform-origin" x y z


translate : Length -> Length -> Property
translate x y =
    LengthProperty2 "translate" x y


translate3d : Length -> Length -> Length -> Property
translate3d x y z =
    LengthProperty3 "translate3d" x y z


translateX : Length -> Property
translateX len =
    LengthProperty "translate-x" len


translateY : Length -> Property
translateY len =
    LengthProperty "translate-y" len


scale : Float -> Property
scale x =
    FloatProperty "scale" x


scale3d : Float -> Float -> Float -> Property
scale3d x y z =
    LengthProperty3 "scale3d" x y z


scaleX : Float -> Property
scaleX x =
    FloatProperty "scale-x" x


scaleY : Float -> Property
scaleY x =
    FloatProperty "scale-y" x


scaleZ : Float -> Property
scaleZ x =
    FloatProperty "scale-z" x


rotate : Angle -> Property
rotate angle =
    AngleProperty "rotate" angle


rotate3d : Float -> Float -> Float -> Angle -> Property


rotateX : Angle -> Property
rotateX angle =
    AngleProperty "rotate-x" angle


rotateY : Angle -> Property
rotateY angle =
    AngleProperty "rotate-y" angle


rotateZ : Angle -> Property
rotateZ angle =
    AngleProperty "rotate-z" angle



-- skew : Angle -> Angle -> Property


skewX : Angle -> Property
skewX angle =
    AngleProperty "skew-x" angle


skewY : Angle -> Property
skewY angle =
    AngleProperty "skew-y" angle


perspective : Float -> Property
perspective x =
    FloatProperty "perspective" x



-- SVG properties


x : Float -> Property
x x =
    FloatProperty "x" x


y : Float -> Property
y y =
    FloatProperty "y" x


cx : Float -> Property
cx x =
    FloatProperty "cx" x


cy : Float -> Property
cy y =
    FloatProperty "cy" y


radius : Float -> Property
radius r =
    FloatProperty "r" r


radiusX : Float -> Property
radiusX rx =
    FloatProperty "rx" rx


radiusY : Float -> Property
radiusY ry =
    FloatProperty "ry" ry



-- d : List (PathCommand a)
-- points : List ( Float, Float ) -> Property


fill : Color -> Property
fill color =
    ColorProperty "fill" color


stroke : Color -> Property
stroke color =
    ColorProperty "stroke" color
