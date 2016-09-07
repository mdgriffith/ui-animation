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
        , toWith
        , toWithEach
        , set
        , repeat
        , loop
        , update
        , style
        , styleWith
        , styleWithEach
        , spring
        , easing
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
        , textShadow
        , insetShadow
        , fill
        , stroke
        , strokeWidth
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
        , curve2
        , curve2To
        , filterUrl
        , blur
        , brightness
        , contrast
        , grayscale
        , greyscale
        , hueRotate
        , invert
        , saturate
        , sepia
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
        )

{-| A library for animations.

# Setting up an animation
@docs State, subscription, Msg, render

# Creating an animation
@docs  interrupt, queue, wait, to, toWith, toWithEach, set, repeat, loop, update, style, styleWith, styleWithEach, spring, easing

# Animatable Properties
@docs opacity, display, inline, inlineBlock, flex, inlineFlex, block, none, top, left, right, bottom, width, height, padding, paddingLeft, paddingRight, paddingTop, paddingBottom, margin, marginLeft, marginRight, marginTop, marginBottom, color, backgroundColor, borderColor, borderWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderBottomWidth, borderRadius, borderTopLeftRadius, borderTopRightRadius, borderBottomLeftRadius, borderBottomRightRadius, shadow, textShadow, insetShadow, fill, stroke, strokeWidth, scale, scaleX, scaleY, scaleZ, rotate, rotateX, rotateY, rotateZ, translate, translateX, translateY, translateZ, points, path, move, moveTo, close, curve, curveTo, curve2, curve2To, filterUrl, blur, brightness, contrast, grayscale, greyscale, hueRotate, invert, saturate, sepia

# Units
@docs px, percent, em, rem, turn, deg, grad, rad

# Advanced
@docs custom, exactly


-}

import Color exposing (Color)
import Time exposing (Time, second)
import AnimationFrame
import String
import Html
import Html.Attributes
import Svg.Attributes
import Task
import Animation.Model exposing (..)


{-| -}
type alias State =
    Animation Never


{-| -}
type alias Msg =
    Tick



---------------------------
-- Setting Defaults
--------------------------


{-| Specify a custom Spring to animate with.  To be used in conjunction with `StyleWith`, `StyleWithEach`, `toWith`, and `toWithEach`.

-}
spring : { stiffness : Float, damping : Float } -> Interpolation
spring settings =
    Spring settings


{-| Specify a custom Easing to animate with.  To be used in conjunction with `StyleWith`, `StyleWithEach`, `toWith`, and `toWithEach`.

-}
easing : { duration : Time, ease : Float -> Float } -> Interpolation
easing { duration, ease } =
    Easing
        { progress = 1
        , duration = duration
        , start = 0
        , ease = ease
        }


setDefaultInterpolation : Property -> Property
setDefaultInterpolation prop =
    let
        interp =
            defaultInterpolationByProperty prop
    in
        mapToMotion (\m -> { m | interpolation = interp }) prop


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



--------------------
-- Animation Steps
-------------------


{-| -}
wait : Time -> Step msg
wait till =
    Wait till


{-| Animate to a set of target values, using the default interpolation.

-}
to : List Property -> Step msg
to props =
    To props


{-| Animate to a set of target values. Use a temporary interpolation instead of the default.
The interpolation will revert back to default after this step.
-}
toWith : Interpolation -> List Property -> Step msg
toWith interp props =
    ToWith <|
        List.map
            (mapToMotion (\m -> { m | interpolation = interp }))
            props


{-| Animate to a set of target values. Use a temporary interpolation for each property instead of the default.
The interpolation will revert back to default after this step.
-}
toWithEach : List ( Interpolation, Property ) -> Step msg
toWithEach interpProps =
    ToWith <|
        List.map
            (\( interp, prop ) -> mapToMotion (\m -> { m | interpolation = interp }) prop)
            interpProps



--{-| Animate two properties along a relative curve
---}
--along : List (Float, Float) -> (Property, Property) -> Step msg


{-| Immediately set properties to a value.
-}
set : List Property -> Step msg
set props =
    Set props


{-| Repeat a number of steps `n` times.
-}
repeat : Int -> List (Step msg) -> Step msg
repeat n steps =
    Repeat n steps


{-| Repeat a number of steps until interrupted.
-}
loop : List (Step msg) -> Step msg
loop steps =
    Loop steps


initialState : Style -> Animation msg
initialState current =
    Animation
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
style : List Property -> Animation msg
style props =
    initialState <| List.map setDefaultInterpolation props


{-| Set an initial style for an animation and override the standard default for interpolation.

-}
styleWith : Interpolation -> List Property -> Animation msg
styleWith interp props =
    initialState <| List.map (mapToMotion (\m -> { m | interpolation = interp })) props


{-| Set an initial style for an animation and specify the interpolation to be used for each property.

Any property not listed will receive interpolation based on the standard defaults.
-}
styleWithEach : List ( Interpolation, Property ) -> Animation msg
styleWithEach props =
    initialState <| List.map (\( interp, prop ) -> mapToMotion (\m -> { m | interpolation = interp }) prop) props


{-| Add an animation to the queue, execiting once the current animation finishes

-}
queue : List (Step msg) -> Animation msg -> Animation msg
queue steps (Animation model) =
    Animation
        { model
            | steps = model.steps ++ steps
            , running = True
        }


{-| Interrupt any running animations with the following animation.

-}
interrupt : List (Step msg) -> Animation msg -> Animation msg
interrupt steps (Animation model) =
    Animation
        { model
            | interruption = extractInitialWait steps :: model.interruption
            , running = True
        }


{-| Sums all leading `Wait` steps and removes them from the animation.

This is used because the wait at the start of an interruption works differently than a normal wait.


-}
extractInitialWait : List (Step msg) -> ( Time, List (Step msg) )
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
subscription : List (Animation msgA) -> (Msg -> msgB) -> Sub msgB
subscription states msg =
    if List.any isRunning states then
        Sub.map msg (AnimationFrame.times Tick)
    else
        Sub.none


isRunning : Animation msg -> Bool
isRunning (Animation model) =
    model.running


{-|
-}
debug : Animation msg -> List ( String, Motion, Time )
debug (Animation model) =
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


{-|
-}
update : Msg -> Animation msg -> Animation msg
update tick animation =
    fst <| updateAnimation tick animation



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
    = Rad


angleUnitName : AngleUnit -> String
angleUnitName unit =
    case unit of
        Rad ->
            "rad"


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
    , interpolationOverride = Nothing
    }


{-| -}
deg : Float -> Angle
deg a =
    ( (a / 360) * (2 * pi), Rad )


{-| -}
grad : Float -> Angle
grad a =
    ( (a / 400) * (2 * pi), Rad )


{-| -}
rad : Float -> Angle
rad a =
    ( a, Rad )


{-| -}
turn : Float -> Angle
turn a =
    ( a * (2 * pi), Rad )


{-| -}
px : Float -> Length
px x =
    ( x, Px )


{-| -}
percent : Float -> Length
percent x =
    ( x, Percent )


{-| -}
rem : Float -> Length
rem x =
    ( x, Rem )


{-| -}
em : Float -> Length
em x =
    ( x, Em )


{-| -}
ex : Float -> Length
ex x =
    ( x, Ex )


{-| -}
ch : Float -> Length
ch x =
    ( x, Ch )


{-| -}
vh : Float -> Length
vh x =
    ( x, Vh )


{-| -}
vw : Float -> Length
vw x =
    ( x, Vw )


{-| -}
vmin : Float -> Length
vmin x =
    ( x, Vmin )


{-| -}
vmax : Float -> Length
vmax x =
    ( x, Vmax )


{-| -}
mm : Float -> Length
mm x =
    ( x, Mm )


{-| -}
cm : Float -> Length
cm x =
    ( x, Cm )


{-| -}
inches : Float -> Length
inches x =
    ( x, In )


{-| -}
pt : Float -> Length
pt x =
    ( x, Pt )


{-| -}
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


{-| Advanced: Animate a custom property by providing it's name, a float value, and the units it should have.

-}
custom : String -> Float -> String -> Property
custom name value unit =
    Property name (initMotion value unit)


{-| Advanced: Set a non-numerical to an exact value.  For example

```
Animation.set
    [ Animation.exactly "border-style" "dashed"
    ]
```

-}
exactly : String -> String -> Property
exactly name value =
    ExactProperty name value


{-| -}
opacity : Float -> Property
opacity x =
    custom "opacity" x ""


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


{-| -}
display : DisplayMode -> Property
display mode =
    ExactProperty "display" (displayModeName mode)


{-| -}
none : DisplayMode
none =
    None


{-| -}
inline : DisplayMode
inline =
    Inline


{-| -}
inlineBlock : DisplayMode
inlineBlock =
    InlineBlock


{-| -}
block : DisplayMode
block =
    Block


{-| -}
flex : DisplayMode
flex =
    Flex


{-| -}
inlineFlex : DisplayMode
inlineFlex =
    InlineFlex


{-| -}
listItem : DisplayMode
listItem =
    ListItem


{-| -}
height : Length -> Property
height ( x, len ) =
    length "height" x (lengthUnitName len)


{-| -}
width : Length -> Property
width ( x, len ) =
    length "width" x (lengthUnitName len)


{-| -}
left : Length -> Property
left ( x, len ) =
    length "left" x (lengthUnitName len)


{-| -}
top : Length -> Property
top ( x, len ) =
    length "top" x (lengthUnitName len)


{-| -}
right : Length -> Property
right ( x, len ) =
    length "right" x (lengthUnitName len)


{-| -}
bottom : Length -> Property
bottom ( x, len ) =
    length "bottom" x (lengthUnitName len)


{-| -}
maxHeight : Length -> Property
maxHeight ( x, len ) =
    length "max-height" x (lengthUnitName len)


{-| -}
maxWidth : Length -> Property
maxWidth ( x, len ) =
    length "max-width" x (lengthUnitName len)


{-| -}
minHeight : Length -> Property
minHeight ( x, len ) =
    length "min-height" x (lengthUnitName len)


{-| -}
minWidth : Length -> Property
minWidth ( x, len ) =
    length "min-width" x (lengthUnitName len)


{-| -}
padding : Length -> Property
padding ( x, len ) =
    length "padding" x (lengthUnitName len)


{-| -}
paddingLeft : Length -> Property
paddingLeft ( x, len ) =
    length "padding-left" x (lengthUnitName len)


{-| -}
paddingRight : Length -> Property
paddingRight ( x, len ) =
    length "padding-right" x (lengthUnitName len)


{-| -}
paddingTop : Length -> Property
paddingTop ( x, len ) =
    length "padding-top" x (lengthUnitName len)


{-| -}
paddingBottom : Length -> Property
paddingBottom ( x, len ) =
    length "padding-bottom" x (lengthUnitName len)


{-| -}
margin : Length -> Property
margin ( x, len ) =
    length "margin" x (lengthUnitName len)


{-| -}
marginLeft : Length -> Property
marginLeft ( x, len ) =
    length "margin-left" x (lengthUnitName len)


{-| -}
marginRight : Length -> Property
marginRight ( x, len ) =
    length "margin-right" x (lengthUnitName len)


{-| -}
marginTop : Length -> Property
marginTop ( x, len ) =
    length "margin-top" x (lengthUnitName len)


{-| -}
marginBottom : Length -> Property
marginBottom ( x, len ) =
    length "margin-bottom" x (lengthUnitName len)


{-| -}
borderWidth : Length -> Property
borderWidth ( x, len ) =
    length "border-width" x (lengthUnitName len)


{-| -}
borderLeftWidth : Length -> Property
borderLeftWidth ( x, len ) =
    length "border-left-width" x (lengthUnitName len)


{-| -}
borderRightWidth : Length -> Property
borderRightWidth ( x, len ) =
    length "border-right-width" x (lengthUnitName len)


{-| -}
borderTopWidth : Length -> Property
borderTopWidth ( x, len ) =
    length "border-top-width" x (lengthUnitName len)


{-| -}
borderBottomWidth : Length -> Property
borderBottomWidth ( x, len ) =
    length "border-bottom-width" x (lengthUnitName len)


{-| -}
borderRadius : Length -> Property
borderRadius ( x, len ) =
    length "border-radius" x (lengthUnitName len)


{-| -}
borderTopLeftRadius : Length -> Property
borderTopLeftRadius ( x, len ) =
    length "border-top-left-radius" x (lengthUnitName len)


{-| -}
borderTopRightRadius : Length -> Property
borderTopRightRadius ( x, len ) =
    length "border-top-right-radius" x (lengthUnitName len)


{-| -}
borderBottomLeftRadius : Length -> Property
borderBottomLeftRadius ( x, len ) =
    length "border-bottom-left-radius" x (lengthUnitName len)


{-| -}
borderBottomRightRadius : Length -> Property
borderBottomRightRadius ( x, len ) =
    length "border-bottom-right-radius" x (lengthUnitName len)


{-| -}
letterSpacing : Length -> Property
letterSpacing ( x, len ) =
    length "letter-spacing" x (lengthUnitName len)


{-| -}
lineHeight : Length -> Property
lineHeight ( x, len ) =
    length "line-height" x (lengthUnitName len)


{-| -}
backgroundPosition : Length -> Length -> Property
backgroundPosition ( x, len1 ) ( y, len2 ) =
    length2 "background-position" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 )


{-| -}
color : Color -> Property
color c =
    colorProp "color" c


{-| -}
backgroundColor : Color -> Property
backgroundColor c =
    colorProp "background-color" c


{-| -}
borderColor : Color -> Property
borderColor c =
    colorProp "border-color" c


{-| -}
transformOrigin : Length -> Length -> Length -> Property
transformOrigin ( x, len1 ) ( y, len2 ) ( z, len3 ) =
    length3 "transform-origin" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 ) ( z, lengthUnitName len3 )


{-| -}
translate : Length -> Length -> Property
translate ( x, len1 ) ( y, len2 ) =
    length2 "translate" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 )


{-| -}
translate3d : Length -> Length -> Length -> Property
translate3d ( x, len1 ) ( y, len2 ) ( z, len3 ) =
    length3 "translate3d" ( x, lengthUnitName len1 ) ( y, lengthUnitName len2 ) ( z, lengthUnitName len3 )


{-| -}
translateX : Length -> Property
translateX ( x, len ) =
    length "translateX" x (lengthUnitName len)


{-| -}
translateY : Length -> Property
translateY ( x, len ) =
    length "translateY" x (lengthUnitName len)


{-| -}
translateZ : Length -> Property
translateZ ( x, len ) =
    length "translateZ" x (lengthUnitName len)


{-| -}
scale : Float -> Property
scale x =
    custom "scale" x ""



-- scale3d : Float -> Float -> Float -> Property
-- scale3d x y z =
--     length3 "scale3d" x y z


{-| -}
scaleX : Float -> Property
scaleX x =
    custom "scaleX" x ""


{-| -}
scaleY : Float -> Property
scaleY x =
    custom "scaleY" x ""


{-| -}
scaleZ : Float -> Property
scaleZ x =
    custom "scaleZ" x ""



-- Internally, angles are always in degrees which is why we throw away the angleUnit here.  It was already checked.


{-| -}
rotate : Angle -> Property
rotate ( x, angle ) =
    AngleProperty "rotate" (initMotion x "rad")



-- rotate3d : Float -> Float -> Float -> Angle -> Property


{-| -}
rotateX : Angle -> Property
rotateX ( x, angle ) =
    AngleProperty "rotateX" (initMotion x "rad")


{-| -}
rotateY : Angle -> Property
rotateY ( x, angle ) =
    AngleProperty "rotateY" (initMotion x "rad")


{-| -}
rotateZ : Angle -> Property
rotateZ ( x, angle ) =
    AngleProperty "rotateZ" (initMotion x "rad")



-- skew : Angle -> Angle -> Property


{-| -}
skewX : Angle -> Property
skewX ( x, angle ) =
    AngleProperty "skewX" (initMotion x "rad")


{-| -}
skewY : Angle -> Property
skewY ( x, angle ) =
    AngleProperty "skewY" (initMotion x "rad")


{-| -}
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


{-| -}
textShadow : Shadow -> Property
textShadow shade =
    let
        { red, green, blue, alpha } =
            Color.toRgb shade.color
    in
        ShadowProperty
            "text-shadow"
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


{-| -}
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


{-| -}
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


{-| -}
x : Float -> Property
x x =
    custom "x" x ""


{-| -}
y : Float -> Property
y y =
    custom "y" y ""


{-| -}
cx : Float -> Property
cx x =
    custom "cx" x ""


{-| -}
cy : Float -> Property
cy y =
    custom "cy" y ""


{-| -}
radius : Float -> Property
radius r =
    custom "r" r ""


{-| -}
radiusX : Float -> Property
radiusX rx =
    custom "rx" rx ""


{-| -}
radiusY : Float -> Property
radiusY ry =
    custom "ry" ry ""


{-| -}
path : List (PathCommand) -> Property
path commands =
    Path commands


{-| -}
move : Float -> Float -> PathCommand
move x y =
    Move (initMotion x "") (initMotion y "")


{-| -}
moveTo : Float -> Float -> PathCommand
moveTo x y =
    MoveTo (initMotion x "") (initMotion y "")


{-| -}
line : Float -> Float -> PathCommand
line x y =
    Line (initMotion x "") (initMotion y "")


{-| -}
lineTo : Float -> Float -> PathCommand
lineTo x y =
    LineTo (initMotion x "") (initMotion y "")


{-| -}
horizontal : Float -> PathCommand
horizontal x =
    Horizontal (initMotion x "")


{-| -}
horizontalTo : Float -> PathCommand
horizontalTo x =
    HorizontalTo (initMotion x "")


{-| -}
vertical : Float -> PathCommand
vertical x =
    Vertical (initMotion x "")


{-| -}
verticalTo : Float -> PathCommand
verticalTo x =
    VerticalTo (initMotion x "")


type alias CubicCurve =
    { control1 : ( Float, Float )
    , control2 : ( Float, Float )
    , point : ( Float, Float )
    }


type alias QuadraticCurve =
    { control : ( Float, Float )
    , point : ( Float, Float )
    }


{-| Create a relative Curve with 2 control points and a target point.
This is a Cubic Curve in the svg spec.

-}
curve2 : CubicCurve -> PathCommand
curve2 { control1, control2, point } =
    Curve
        { control1 =
            ( initMotion (fst control1) ""
            , initMotion (snd control1) ""
            )
        , control2 =
            ( initMotion (fst control2) ""
            , initMotion (snd control2) ""
            )
        , point =
            ( initMotion (fst point) ""
            , initMotion (snd point) ""
            )
        }


{-| Create an absolute Curve with 2 control points and a target point.
This is a Cubic Curve in the svg spec.

-}
curve2To : CubicCurve -> PathCommand
curve2To { control1, control2, point } =
    CurveTo
        { control1 =
            ( initMotion (fst control1) ""
            , initMotion (snd control1) ""
            )
        , control2 =
            ( initMotion (fst control2) ""
            , initMotion (snd control2) ""
            )
        , point =
            ( initMotion (fst point) ""
            , initMotion (snd point) ""
            )
        }


{-| Create a relative curve with 1 control point and a target point.
This is a Quadratic curve in teh svg spec.
-}
curve : QuadraticCurve -> PathCommand
curve { control, point } =
    Quadratic
        { control =
            ( initMotion (fst control) ""
            , initMotion (snd control) ""
            )
        , point =
            ( initMotion (fst point) ""
            , initMotion (snd point) ""
            )
        }


{-| Create an absolute curve with 1 control point and a target point.
This is a Quadratic curve in teh svg spec.
-}
curveTo : QuadraticCurve -> PathCommand
curveTo { control, point } =
    QuadraticTo
        { control =
            ( initMotion (fst control) ""
            , initMotion (snd control) ""
            )
        , point =
            ( initMotion (fst point) ""
            , initMotion (snd point) ""
            )
        }



--continueCurve : QuadraticCurve -> PathCommand
--continueCurve points =
--    Smooth <| pointsProp points
--continueCurveTo : QuadraticCurve -> PathCommand
--continueCurveTo points =
--    SmoothTo <| pointsProp points
--continueQuadratic : List ( Float, Float ) -> PathCommand
--continueQuadratic points =
--    SmoothQuadratic <| pointsProp points
--continueQuadraticTo : List ( Float, Float ) -> PathCommand
--continueQuadraticTo points =
--    SmoothQuadraticTo <| pointsProp points


type alias Arc =
    { x : Float
    , y : Float
    , radius : Float
    , startAngle : Float
    , endAngle : Float
    , clockwise : Bool
    }


{-| Create an simple arc by specifying
    * x
    * y
    * radius
    * startAngle - specified in degrees
    * endAngle - specified in degrees
    * clockwise - boolean
-}
arc : Arc -> PathCommand
arc arc =
    if arc.clockwise then
        ClockwiseArc
            { x = initMotion arc.x ""
            , y = initMotion arc.y ""
            , radius = initMotion arc.radius ""
            , startAngle = initMotion arc.startAngle ""
            , endAngle = initMotion arc.endAngle ""
            }
    else
        AntiClockwiseArc
            { x = initMotion arc.x ""
            , y = initMotion arc.y ""
            , radius = initMotion arc.radius ""
            , startAngle = initMotion arc.startAngle ""
            , endAngle = initMotion arc.endAngle ""
            }


{-| Close a Path
-}
close : PathCommand
close =
    Close


{-| Create a CSS filter-url
-}
filterUrl : String -> Property
filterUrl url =
    exactly "filter-url" url


{-| Create a CSS blur filter, these stack with other filters.
-}
blur : Length -> Property
blur ( x, len ) =
    length "blur" x (lengthUnitName len)


{-| Create a CSS brightness filter, these stack with other filters.
-}
brightness : Float -> Property
brightness x =
    custom "brightness" x "%"


{-| Create a CSS contrast filter, these stack with other filters.
-}
contrast : Float -> Property
contrast x =
    custom "contrast" x "%"


{-| Create a CSS grayscale filter, these stack with other filters.
-}
grayscale : Float -> Property
grayscale x =
    custom "grayscale" x "%"


{-| Create a CSS grayscale filter, these stack with other filters.  This is a spelling adjusment.
-}
greyscale : Float -> Property
greyscale x =
    grayscale x


{-| Create a CSS hue-rotation filter, these stack with other filters.
-}
hueRotate : Angle -> Property
hueRotate ( x, angle ) =
    AngleProperty "hue-rotate" (initMotion x "rad")


{-| Create a CSS invert filter, these stack with other filters.
-}
invert : Float -> Property
invert x =
    custom "invert" x "%"


{-| Create a CSS saturate filter, these stack with other filters.
-}
saturate : Float -> Property
saturate x =
    custom "saturate" x "%"


{-| Create a CSS sepia filter, these stack with other filters.
-}
sepia : Float -> Property
sepia x =
    custom "sepia" x "%"


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


{-| -}
fill : Color -> Property
fill color =
    colorProp "fill" color


{-| -}
stroke : Color -> Property
stroke color =
    colorProp "stroke" color


{-| -}
strokeWidth : Float -> Property
strokeWidth x =
    length "stroke-width" x ""


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



-------------------------
-- Rendering
-------------------------


{-| Render style properties into the style attribute and render other attributes as needed for svg.

Combine "transform" based properties into a single css property.

Combine "filter" based properties into a single css property.
-}
render : Animation msgA -> List (Html.Attribute msgB)
render (Animation model) =
    let
        ( attrProps, styleProps ) =
            List.partition isAttr model.style

        ( style, transforms, filters ) =
            List.foldl
                (\prop ( style, transforms, filters ) ->
                    if isTransformation prop then
                        ( style
                        , transforms ++ [ prop ]
                        , filters
                        )
                    else if isFilter prop then
                        ( style
                        , transforms
                        , filters ++ [ prop ]
                        )
                    else
                        ( style ++ [ prop ]
                        , transforms
                        , filters
                        )
                )
                ( [], [], [] )
                styleProps

        renderedStyle =
            List.map (\prop -> ( propertyName prop, propertyValue prop " " )) style

        renderedTransforms =
            if List.isEmpty transforms then
                []
            else
                [ ( "transform"
                  , String.concat <|
                        List.map
                            (\prop ->
                                propertyName prop ++ "(" ++ (propertyValue prop ", ") ++ ")"
                            )
                            transforms
                  )
                ]

        renderedFilters =
            if List.isEmpty filters then
                []
            else
                [ ( "filter"
                  , String.join " " <|
                        List.map
                            (\prop ->
                                let
                                    name =
                                        propertyName prop
                                in
                                    if name == "filter-url" then
                                        "url(\"" ++ (propertyValue prop ", ") ++ "\")"
                                    else
                                        propertyName prop ++ "(" ++ (propertyValue prop ", ") ++ ")"
                            )
                            filters
                  )
                ]

        styleAttr =
            Html.Attributes.style <|
                List.concatMap prefix <|
                    (renderedTransforms ++ renderedFilters ++ renderedStyle)

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
        , "rotateX"
        , "rotateY"
        , "rotateZ"
        , "rotate3d"
        , "transform"
        , "transform3d"
        , "translateX"
        , "translateY"
        , "translateZ"
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


isFilter : Property -> Bool
isFilter prop =
    List.member (propertyName prop)
        [ "filter-url"
        , "blur"
        , "brightness"
        , "contrast"
        , "grayscale"
        , "hue-rotate"
        , "invert"
        , "saturate"
        , "sepia"
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

            "filter" ->
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

            Curve { control1, control2, point } ->
                "c "
                    ++ (toString <| fst control1)
                    ++ " "
                    ++ (toString <| fst control1)
                    ++ ", "
                    ++ (toString <| fst control2)
                    ++ " "
                    ++ (toString <| fst control2)
                    ++ ", "
                    ++ (toString <| fst point)
                    ++ " "
                    ++ (toString <| fst point)
                    ++ " "

            CurveTo { control1, control2, point } ->
                "C "
                    ++ (toString <| fst control1)
                    ++ " "
                    ++ (toString <| fst control1)
                    ++ ", "
                    ++ (toString <| fst control2)
                    ++ " "
                    ++ (toString <| fst control2)
                    ++ ", "
                    ++ (toString <| fst point)
                    ++ " "
                    ++ (toString <| fst point)
                    ++ " "

            Quadratic { control, point } ->
                "q "
                    ++ (toString <| fst control)
                    ++ " "
                    ++ (toString <| fst control)
                    ++ ", "
                    ++ (toString <| fst point)
                    ++ " "
                    ++ (toString <| fst point)
                    ++ " "

            QuadraticTo { control, point } ->
                "Q "
                    ++ (toString <| fst control)
                    ++ " "
                    ++ (toString <| fst control)
                    ++ ", "
                    ++ (toString <| fst point)
                    ++ " "
                    ++ (toString <| fst point)
                    ++ " "

            SmoothQuadratic points ->
                "t " ++ renderPoints points

            SmoothQuadraticTo points ->
                "T " ++ renderPoints points

            Smooth points ->
                "s " ++ renderPoints points

            SmoothTo points ->
                "S " ++ renderPoints points

            ClockwiseArc arc ->
                let
                    deltaAngle =
                        arc.endAngle.position - arc.startAngle.position
                in
                    if deltaAngle > (360 - 1.0e-6) then
                        let
                            dx =
                                arc.radius.position * cos (degrees arc.startAngle.position)

                            dy =
                                arc.radius.position * sin (degrees arc.startAngle.position)
                        in
                            "A "
                                ++ toString arc.radius.position
                                ++ ","
                                ++ toString arc.radius.position
                                ++ ",0,1,1,"
                                ++ toString (arc.x.position - dx)
                                ++ ","
                                ++ toString (arc.y.position - dy)
                                ++ " A "
                                ++ toString arc.radius.position
                                ++ ","
                                ++ toString arc.radius.position
                                ++ ",0,1,1,"
                                ++ toString (arc.x.position + dx)
                                ++ ","
                                ++ toString (arc.y.position + dy)
                    else
                        "A "
                            ++ toString arc.radius.position
                            ++ ","
                            ++ toString arc.radius.position
                            ++ " 0 "
                            ++ (if deltaAngle >= 180 then
                                    "1"
                                else
                                    "0"
                               )
                            ++ " "
                            ++ "1"
                            ++ " "
                            ++ toString (arc.x.position + (arc.radius.position * (cos <| degrees arc.endAngle.position)))
                            ++ ","
                            ++ toString (arc.y.position + (arc.radius.position * (sin <| degrees arc.endAngle.position)))

            AntiClockwiseArc arc ->
                let
                    deltaAngle =
                        arc.endAngle.position - arc.startAngle.position
                in
                    if deltaAngle > (360 - 1.0e-6) then
                        let
                            dx =
                                arc.radius.position * cos (degrees arc.startAngle.position)

                            dy =
                                arc.radius.position * sin (degrees arc.startAngle.position)
                        in
                            "A "
                                ++ toString arc.radius.position
                                ++ ","
                                ++ toString arc.radius.position
                                ++ ",0,1,0,"
                                ++ toString (arc.x.position - dx)
                                ++ ","
                                ++ toString (arc.y.position - dy)
                                ++ " A "
                                ++ toString arc.radius.position
                                ++ ","
                                ++ toString arc.radius.position
                                ++ ",0,1,1,"
                                ++ toString (arc.x.position + dx)
                                ++ ","
                                ++ toString (arc.y.position + dy)
                    else
                        "A "
                            ++ toString arc.radius.position
                            ++ ","
                            ++ toString arc.radius.position
                            ++ " 0 "
                            ++ (if arc.startAngle.position - arc.endAngle.position >= 180 then
                                    "1"
                                else
                                    "0"
                               )
                            ++ " "
                            ++ "0"
                            ++ " "
                            ++ toString (arc.x.position + (arc.radius.position * (cos arc.endAngle.position)))
                            ++ ","
                            ++ toString (arc.y.position + (arc.radius.position * (sin arc.endAngle.position)))

            Close ->
                "z"
