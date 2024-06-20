module Messenger.UI.View exposing (view, audio)

{-|


# Game Update

View the game via Canvas

@docs view, audio

-}

import Audio exposing (Audio, AudioData)
import Canvas exposing (Renderable)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as Decode
import Messenger.Audio.Internal exposing (getAudio)
import Messenger.Base exposing (WorldEvent(..))
import Messenger.Component.GlobalComponent exposing (combinePP)
import Messenger.GeneralModel exposing (viewModelList)
import Messenger.Model exposing (Model)
import Messenger.Resources.Base exposing (getTexture)
import Messenger.Scene.Scene exposing (unroll)
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

        sceneView =
            (unroll model.env.commonData).view { globalData = model.env.globalData, commonData = () }

        gcView =
            viewModelList model.env model.globalComponents

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
                ([ config.background gd.internalData
                 , postProcess sceneView <| combinePP model.globalComponents
                 ]
                    ++ gcView
                )
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


postProcess : Renderable -> List (Renderable -> Renderable) -> Renderable
postProcess x xs =
    List.foldl (\le uni -> le uni) x xs
