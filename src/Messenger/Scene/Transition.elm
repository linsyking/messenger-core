module Messenger.Scene.Transition exposing (..)

{-|


# Transtition Library

@docs makeTransition

-}

import Canvas exposing (Renderable)
import Messenger.Base exposing (GlobalData)
import Messenger.Scene.Transitions.Base exposing (Transition)


{-| Generate transition from transition data
-}
makeTransition : GlobalData a -> Maybe (Transition a) -> Renderable -> Renderable
makeTransition gd trans ren =
    case trans of
        Just data ->
            if data.currentTransition < data.outT then
                -- Fade out
                data.fadeout gd ren (toFloat data.currentTransition / toFloat data.outT)

            else if data.currentTransition < data.outT + data.inT then
                -- Fade in
                data.fadein gd ren (toFloat (data.currentTransition - data.outT) / toFloat data.inT)

            else
                ren

        Nothing ->
            ren
