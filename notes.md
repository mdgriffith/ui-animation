# Notes


## Interruptions
When you want an interrupt that starts with a 'wait',
the desired effect is to not interrupt until after that wait is finished.
This prevents the implementation from just replacing the animation queue in the interrupt function.


## Renames
Property | name
`r`      | radius
`cx`     | centerX
`cy`     | centerY
`in`     | inches
`%`      | percent


## Rotation units
Rotation units could be optional because they can be interconverted via multiplication.

Naked units would default to degrees.

In the same way that time units work, you can do the following
```
rotate (1 * turn)
rotate (360 * degrees)
rotate (5 * rads)
```

Right now, rotation takes the same format as length Properties

```
height (px 200)
width (percent 50)
rotate (turn 5)
```


## Possibly too clever potential Rotation Assumptions

```
rotate (degrees 360)
```
Means rotateBy 360 degrees



```
rotateTo (degrees 50)
rotateTo (degrees 0)  -- rotate clockwise to 0 degrees
rotateTo (degrees -360) -- rotate counterClockwise to 0 degrees
rotateTo (degrees 90) -- rotate clockwise to 90 degrees
rotateTo (degrees -90) -- rotate counter clockwise to 90 degrees

```
Means go to that rotation.


## Html.Transform vs Svg.Transform
Performing a transform in svg uses svg's standard 'unit',
while in html it expects an actual unit specification.

Currently length units are always required, but are ignored when applied to an svg element.

The alternative is to  just require unitless values and only render as px values.


## Things that can't be animated but might want to be set as a step within an animation
 * display - setting 'display' to none after fading something out is very common.
 * border-style - Maybe setting a border style to 'dashed' or some such?  Maybe not.


## Support for filter or mask?
https://developer.mozilla.org/en-US/docs/Web/CSS/filter


## Are Transform.Matrices needed at all?
Are there any operations that can only be expressed through matrices
that can't be more succinctly created through a transformation stack.
I.e. translate 2 200 |> rotate (turn 5) |> scale 1.2



## Maintaining State

A unique position, velocity, and target needs to be maintained for each _value_ in each property.

So, properties with more than one value, such as `translate (px 5) (px 10)`, will have a different position, target and velocity for both the x and y components.

This also needs to be accounted for in all the channels of a color property.  Each channel has a position, a velocity and a target.



## Do Interruptions Stack?
If two interruptions are called, does the first one that goes into effect negate the second one?


## Steps for Properties instead of Property Groups.

If a style with two properties:
    opacity
    left

An animation is created
    opacity -> 1

Before it finishes,
    left -> 20

If They are both using Duration/Easing, then they will have different start and end times.

If they are using Springs, they will proceed as necessary.


## What Times are Necessary.

 - Step starting time is necessary for Easing/Duration.
 - Absolute time has been used for interruptions (because starting time changes with every step.)
    - It could be relative time that is decremented every tick.  Decrement requires dt.
    - This is also what is collected by the `wait` step.
 - Dt is used to update a spring.


Current Time


Does using DT make the animation nonreversable for something like the time traveling debugger?
