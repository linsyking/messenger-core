module Messenger.UI.View exposing (..)

{-| view

Canvas viewer
You can change the mouse style here.

-}

import Audio exposing (AudioData)
import Canvas
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as Decode
import Messenger.Base exposing (WorldEvent(..))
import Messenger.Model exposing (Model)
import Messenger.Resources.Base exposing (getTexture)
import Messenger.Scene.Scene exposing (unroll)
import Messenger.Scene.Transition exposing (makeTransition)
import Messenger.UserConfig exposing (UserConfig)


view : UserConfig userdata scenemsg -> AudioData -> Model userdata scenemsg -> Html WorldEvent
view config _ model =
    let
        transitiondata =
            Maybe.map Tuple.first model.transition

        canvas =
            Canvas.toHtmlWith
                { width = floor model.currentGlobalData.internalData.realWidth
                , height = floor model.currentGlobalData.internalData.realHeight
                , textures = getTexture config
                }
                [ style "left" (String.fromFloat model.currentGlobalData.internalData.startLeft)
                , style "top" (String.fromFloat model.currentGlobalData.internalData.startTop)
                , style "position" "fixed"
                ]
                [ config.background model.currentGlobalData
                , makeTransition model.currentGlobalData transitiondata <| (unroll model.currentScene).view { globalData = model.currentGlobalData, commonData = () }
                ]
    in
    Html.div [ on "wheel" (Decode.map MouseWheel (Decode.field "deltaY" Decode.int)) ]
        (case model.currentGlobalData.extraHTML of
            Just x ->
                [ canvas, x ]

            Nothing ->
                [ canvas ]
        )
