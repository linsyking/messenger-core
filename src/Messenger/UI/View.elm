module Messenger.UI.View exposing (view, audio)

{-|


# Game Update

View the game via Canvas

@docs view, audio

-}

import Audio exposing (Audio, AudioData)
import Canvas
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as Decode
import Messenger.Audio.Internal exposing (getAudio)
import Messenger.Base exposing (WorldEvent(..))
import Messenger.Model exposing (Model)
import Messenger.Resources.Base exposing (getTexture)
import Messenger.UI.Input exposing (Input)


{-| View function of the game.
-}
view : Input userdata scenemsg -> AudioData -> Model userdata scenemsg -> Html WorldEvent
view input _ model =
    let
        resources =
            input.resources

        config =
            input.config

        gd =
            model.env.globalData

        canvas =
            Canvas.toHtmlWith
                { width = floor gd.internalData.realWidth
                , height = floor gd.internalData.realHeight
                , textures = getTexture resources
                }
                ([ style "left" (String.fromFloat gd.internalData.startLeft)
                 , style "top" (String.fromFloat gd.internalData.startTop)
                 , style "position" "fixed"
                 ]
                    ++ gd.canvasAttributes
                )
                (config.background gd :: model.canvasRenderable)
    in
    Html.div [ on "wheel" (Decode.map WMouseWheel (Decode.field "deltaY" Decode.int)) ]
        (case gd.extraHTML of
            Just x ->
                [ canvas, x ]

            Nothing ->
                [ canvas ]
        )


{-| Audio view function

The audio argument needed in the main model.

-}
audio : AudioData -> Model userdata scenemsg -> Audio
audio _ model =
    Audio.group (getAudio model.env.globalData.internalData.audioRepo)
        |> Audio.scaleVolume model.env.globalData.volume
