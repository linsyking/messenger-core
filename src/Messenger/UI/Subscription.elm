module Messenger.UI.Subscription exposing (subscriptions)

{-|


# Game Subscriptions

The subscriptions for the game

@docs subscriptions

-}

import Audio exposing (AudioData)
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp, onMouseDown, onMouseMove, onMouseUp, onResize, onVisibilityChange)
import Json.Decode as Decode
import Messenger.Base exposing (WorldEvent(..))
import Messenger.Model exposing (Model)
import Messenger.UserConfig exposing (TimeInterval(..), UserConfig)
import Time


{-| The subscriptions for the game.
-}
subscriptions : UserConfig userdata scenemsg -> AudioData -> Model userdata scenemsg -> Sub WorldEvent
subscriptions config _ _ =
    let
        timeSub =
            case config.timeInterval of
                Fixed time ->
                    Time.every time WTick

                Animation ->
                    onAnimationFrame WTick
    in
    Sub.batch
        [ timeSub
        , onKeyDown
            (Decode.map2
                (\x rep ->
                    if not rep then
                        WKeyDown x

                    else
                        NullEvent
                )
                (Decode.field "keyCode" Decode.int)
                (Decode.field "repeat" Decode.bool)
            )
        , onKeyUp
            (Decode.map2
                (\x rep ->
                    if not rep then
                        WKeyUp x

                    else
                        NullEvent
                )
                (Decode.field "keyCode" Decode.int)
                (Decode.field "repeat" Decode.bool)
            )
        , onResize (\w h -> NewWindowSize ( toFloat w, toFloat h ))
        , onVisibilityChange (\v -> WindowVisibility v)
        , onMouseDown (Decode.map3 (\b x y -> WMouseDown b ( x, y )) (Decode.field "button" Decode.int) (Decode.field "clientX" Decode.float) (Decode.field "clientY" Decode.float))
        , onMouseUp (Decode.map3 (\b x y -> WMouseUp b ( x, y )) (Decode.field "button" Decode.int) (Decode.field "clientX" Decode.float) (Decode.field "clientY" Decode.float))
        , onMouseMove (Decode.map2 (\x y -> MouseMove ( x, y )) (Decode.field "clientX" Decode.float) (Decode.field "clientY" Decode.float))
        , config.ports.promptReceiver (\p -> WPrompt p.name p.result)
        ]
