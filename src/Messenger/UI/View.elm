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
import Messenger.Audio.Audio exposing (getAudio)
import Messenger.Base exposing (WorldEvent(..))
import Messenger.Model exposing (Model)
import Messenger.Resources.Base exposing (getTexture)
import Messenger.Scene.Scene exposing (unroll)
import Messenger.Scene.Transition exposing (makeTransition)
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

        transitionData =
            Maybe.map Tuple.first model.transition

        canvas =
            Canvas.toHtmlWith
                { width = floor model.currentGlobalData.internalData.realWidth
                , height = floor model.currentGlobalData.internalData.realHeight
                , textures = getTexture resources
                }
                ([ style "left" (String.fromFloat model.currentGlobalData.internalData.startLeft)
                 , style "top" (String.fromFloat model.currentGlobalData.internalData.startTop)
                 , style "position" "fixed"
                 ]
                    ++ model.currentGlobalData.canvasAttributes
                )
                [ config.background model.currentGlobalData
                , makeTransition model.currentGlobalData transitionData <| (unroll model.currentScene).view { globalData = model.currentGlobalData, commonData = () }
                ]
    in
    Html.div [ on "wheel" (Decode.map WMouseWheel (Decode.field "deltaY" Decode.int)) ]
        (case model.currentGlobalData.extraHTML of
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
    Audio.group (getAudio model.currentGlobalData.internalData.audioRepo)
        |> Audio.scaleVolume model.currentGlobalData.volume
