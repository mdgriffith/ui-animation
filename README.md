# Elm-Style-Animation Rewrite
This code will eventually be released as a major version to [elm-style-animation](https://github.com/mdgriffith/elm-style-animation).

The main purpose of this rewrite was to change the API to use a list based api vs the pipes based api that was used previously.  I also wanted to take the time to add some polish.

I've filed a number of issues on this repo on design points that I would love feedback on.  Please reply to them if you have any thoughts or submit an issue if you've thought of something I've missed!


## Basic Animation

To get started, there are a few things that need to happen.


__Set an initial style__ in your model.

```elm
import Animation exposing (px)

init : Model
init =
    { style = 
        Animation.style 
            [ Animation.left (px 0.0)
            , Animation.opacity 1.0
            ]
    }
```

__Subscribe to Animation's subscription.__  This will animate using AnimationFrame when something is running, and stop giving updates when there is no animation. 
```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription [model.style] Animate

```


__Set up an update `Msg`__ in your update function.
```elm
    Animate animMsg ->
        { model
            | animation = Animation.update animMsg model.style
        }
                
```


__Render our animation__ at the necessary element in your view.  Not all animated properties are style properties.  Notable examples are for animating svg paths and polygon points.  `Html.Attributes.style` stacks, so we can still add style properties!
```elm
    div
        (Animation.render model.style
            ++ [ style
                    [ ( "position", "absolute" )
                    , ( "border-style", "dotted" )
                    ]
               ]
        )
        [ text "This is being Animated!" ]
```




__Start an animation__ in your update statement.

```elm
case msgs of
    Show ->
        let 
            newStyle = 
                Animation.interrupt
                    [ Animation.to 
                        [ Animation.left (px 0.0)
                        , Animation.opacity 1.0
                        ]
                    ]
                    model.style
        in
            { model
                | style = newStyle
            }
```

Here's generally how we compose animations.

 * Choose `Animation.queue` or `Animation.interrupt`, both of which take a list of steps and your animation model.
 * Steps can be
    * `Animation.to` - Animate to a target style
    * `Animation.set` - Set a animation to a style immediately.
    * `Animation.wait (5 * second)` - wait for some amount of time
    * `Animation.repeat x [..list of steps to repeat]` - Repeat a list of steps x times.
    * `Animation.loop [..list of steps to repeat]` - Loop a list of steps forever/until interrupted.







