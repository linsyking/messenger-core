module Messenger.RecursionList exposing (updateObjects)

{-|


# RecursionList

List implementation for the recursion algorithm

@docs updateObjects

-}

import Messenger.Recursion exposing (RecBody)


{-| Recursively update all the objects in the List
-}
updateObjects : RecBody a b c d -> c -> b -> List a -> ( List a, List b, c )
updateObjects rec env msg objs =
    let
        ( newObjs, ( newMsgUnfinished, newMsgFinished ), newEnv ) =
            updateOnce rec env msg objs
    in
    updateRemain rec newEnv ( newMsgUnfinished, newMsgFinished ) newObjs



-- Below are all helper functions


updateOnce : RecBody a b c d -> c -> b -> List a -> ( List a, ( List ( d, b ), List b ), c )
updateOnce rec env msg objs =
    List.foldr
        (\ele ( lastObjs, ( lastMsgUnfinished, lastMsgFinished ), lastEnv ) ->
            let
                ( newObj, newMsg, newEnv ) =
                    rec.update ele lastEnv msg

                finishedMsg =
                    List.filterMap
                        (\( x, y ) ->
                            if rec.super x then
                                Just y

                            else
                                Nothing
                        )
                        newMsg

                unfinishedMsg =
                    List.filter (\( x, _ ) -> not (rec.super x)) newMsg
            in
            ( newObj :: lastObjs, ( unfinishedMsg ++ lastMsgUnfinished, finishedMsg ++ lastMsgFinished ), newEnv )
        )
        ( [], ( [], [] ), env )
        objs



{-
   Recursively update remaining objects
-}


updateRemain : RecBody a b c d -> c -> ( List ( d, b ), List b ) -> List a -> ( List a, List b, c )
updateRemain rec env ( unfinishedMsg, finishedMsg ) objs =
    if List.isEmpty unfinishedMsg then
        ( objs, finishedMsg, env )

    else
        let
            ( newObjs, ( newUnfinishedMsg, newFinishedMsg ), newEnv ) =
                List.foldr
                    (\ele ( lastObjs, ( lastMsgUnfinished, lastMsgFinished ), lastEnv ) ->
                        let
                            msgMatched =
                                List.filterMap
                                    (\( tar, msg ) ->
                                        if rec.match ele tar then
                                            Just msg

                                        else
                                            Nothing
                                    )
                                    unfinishedMsg
                        in
                        if List.isEmpty msgMatched then
                            -- No need to update
                            ( ele :: lastObjs, ( lastMsgUnfinished, lastMsgFinished ), lastEnv )

                        else
                            -- Need update
                            let
                                -- Update the object with all messages in msgMatched
                                ( newObj, ( newMsgUnfinished, newMsgFinished ), newEnv2 ) =
                                    List.foldr
                                        (\msg ( lastObj2, ( lastMsgUnfinished2, lastMsgFinished2 ), lastEnv2 ) ->
                                            let
                                                ( newEle, newMsgs, newEnv3 ) =
                                                    rec.update lastObj2 lastEnv2 msg

                                                finishedMsgs =
                                                    List.filterMap
                                                        (\( x, y ) ->
                                                            if rec.super x then
                                                                Just y

                                                            else
                                                                Nothing
                                                        )
                                                        newMsgs

                                                unfinishedMsgs =
                                                    List.filter (\( x, _ ) -> not (rec.super x)) newMsgs
                                            in
                                            ( newEle, ( unfinishedMsgs ++ lastMsgUnfinished2, finishedMsgs ++ lastMsgFinished2 ), newEnv3 )
                                        )
                                        ( ele, ( [], [] ), env )
                                        msgMatched
                            in
                            ( newObj :: lastObjs, ( newMsgUnfinished ++ lastMsgUnfinished, newMsgFinished ++ lastMsgFinished ), newEnv2 )
                    )
                    ( [], ( [], [] ), env )
                    objs
        in
        updateRemain rec newEnv ( newUnfinishedMsg, newFinishedMsg ++ finishedMsg ) newObjs
