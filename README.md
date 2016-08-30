# Elm-Style-Animation Rewrite
This code will eventually be released as a major version to [elm-style-animation](https://github.com/mdgriffith/elm-style-animation).

The main purpose of this rewrite was to change the API to use a list based api vs the pipes based api that was used previously.  I also wanted to take the time to really polish the API.

I've filed a number of issues on this repo on design points that I would love feedback on.  Please reply to them if you have any thoughts or submit an issue if you've thought of something I've missed!


## Modules
 * __Animation__ - Everything you need to create an animation
 * __Animation.List__ - Some convenience functions for handling a list of animations.  In this case a list of animations means the animations/style for multiple separate html elements.
 * __Animation.Dict__ - Same for organizing your animations into a `Dict`.



## Basic Animation

To get started, there are a few things that need to happen.


__Set an initial style__ in your model.

```elm
init : ( Model, Cmd Msg )
init =
    ( { style = 
        Animation.style 
            [ Animation.left (Animation.px 0.0)
            , Animation.opacity 1.0
            ]
      }
    , Cmd.none
    )

```

__Subscribe to Animation's subscription.__  This will animate using AnimationFrame when something is running, and stop giving updates when there is no animation.  We have `Animate (Animation.State msg)` in our `Msg` type.
```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription model.style Animate

```


__Set up a tick `Msg`.__  The animation can send messages for you, which is why it returns a `cmd`.
```elm
        Animate time ->
            let
                ( anim, cmd ) =
                    Animation.tick time model.style
            in
                ( { model
                    | animation = anim
                  }
                , cmd
                )
```


__Render our animation__ at the necessary element in our view.  Note that not all animated properties are style properties.  Notable examples are for animating svg paths and polygon points.  `Html.Attributes.style` will(hopefully) be combinable in the future, allowing for additional style properties to be added.
```elm
    div
        ( Animation.render model.style )
        [ h1 [] [ text "Hidden Menu" ]
        , ul []
            [ li [] [ text "Some things" ]
            , li [] [ text "in a list" ]
            ]
        ]
```



And finally __specify our animation__ in our update statement.

```elm
    case msgs of
        Show ->
            ( { model
                | style =
                    Animation.interrupt
                        [ Animation.to 
                            [ Animation.left (Animation.px 0.0)
                            , Animation.opacity 1.0
                            ]
                        ]
                        model.style
              }
            , Cmd.none
            )
```

Here's geerally how we compose animations.

 * Choose `Animation.queue` or `Animation.interrupt`, both of which take a list of steps and then your animation model.
 * Steps can be
    * `Animation.to` - Animate to a target style
    * `Animation.set` - Set a animation to a style immediately.
    * `Animation.wait (5 * second)` - wait for some amount of time
    * `Animation.send YourMsg` - Send a custom `Msg` when this point is reached.  Very useful for doing `onFinished` style work, fire a msg when something fades out, etc.
    * `Animation.repeat x [..list of steps to repeat]` - Repeat a list of steps x times.
    * `Animation.loop [..list of steps to repeat]` - Loop a list of steps forever/until interrupted.


## Examples

There are a number of examples