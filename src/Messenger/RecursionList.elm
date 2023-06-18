module Messenger.RecursionList exposing
    ( updateObjects, updateObjectsWithTarget
    , getObjectByIndex, getObjectIndices, getObjectIndex, getObjects, getObject
    )

{-|


# RecursionList

List implementation for the recursion algorithm

@docs updateObjects, updateObjectsWithTarget


## Tools

@docs getObjectByIndex, getObjectIndices, getObjectIndex, getObjects, getObject

-}

import List.Extra
import Messenger.Recursion exposing (Matcher, RecBody)


{-| Recursively update all the objects in the List
-}
updateObjects : RecBody a b c d -> c -> List a -> ( List a, List b, c )
updateObjects rec env objs =
    let
        ( newObjs, ( newMsgUnfinished, newMsgFinished ), newEnv ) =
            updateOnce rec env objs
    in
    updateRemain rec (rec.clean newEnv) ( newMsgUnfinished, newMsgFinished ) newObjs


{-| Recursively update all the objects in the List, but also uses target
-}
updateObjectsWithTarget : RecBody a b c d -> c -> List ( d, b ) -> List a -> ( List a, List b, c )
updateObjectsWithTarget rec env msgs objs =
    updateRemain rec env ( msgs, [] ) objs



-- Below are some helper functions


updateOnce : RecBody a b c d -> c -> List a -> ( List a, ( List ( d, b ), List b ), c )
updateOnce rec env objs =
    List.foldr
        (\ele ( lastObjs, ( lastMsgUnfinished, lastMsgFinished ), lastEnv ) ->
            let
                ( newObj, newMsg, newEnv ) =
                    rec.update ele lastEnv

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
            ( newObj :: lastObjs, ( lastMsgUnfinished ++ unfinishedMsg, lastMsgFinished ++ finishedMsg ), newEnv )
        )
        ( [], ( [], [] ), env )
        objs


{-| Recursively update remaining objects
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
                                    List.foldl
                                        (\msg ( lastObj2, ( lastMsgUnfinished2, lastMsgFinished2 ), lastEnv2 ) ->
                                            let
                                                ( newEle, newMsgs, newEnv3 ) =
                                                    rec.updaterec lastObj2 lastEnv2 msg

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
                                            ( newEle, ( lastMsgUnfinished2 ++ unfinishedMsgs, lastMsgFinished2 ++ finishedMsgs ), newEnv3 )
                                        )
                                        ( ele, ( [], [] ), env )
                                        msgMatched
                            in
                            ( newObj :: lastObjs, ( lastMsgUnfinished ++ newMsgUnfinished, lastMsgFinished ++ newMsgFinished ), newEnv2 )
                    )
                    ( [], ( [], [] ), env )
                    objs
        in
        updateRemain rec newEnv ( newUnfinishedMsg, finishedMsg ++ newFinishedMsg ) newObjs


{-| Get the object by index
-}
getObjectByIndex : Int -> List a -> Maybe a
getObjectByIndex index objs =
    if index < 0 then
        Nothing

    else
        List.head (List.drop index objs)


{-| Get the indices of the objects that match the target
-}
getObjectIndices : Matcher a d -> d -> List a -> List Int
getObjectIndices matcher tar objs =
    List.Extra.findIndices (\x -> matcher x tar) objs


{-| Get the index of the object that matches the target (the first one)
-}
getObjectIndex : Matcher a d -> d -> List a -> Maybe Int
getObjectIndex matcher tar objs =
    List.Extra.findIndex (\x -> matcher x tar) objs


{-| Get the objects that match the target
-}
getObjects : Matcher a d -> d -> List a -> List a
getObjects matcher tar objs =
    List.filter (\x -> matcher x tar) objs


{-| Get the object that matches the target (the first one)
-}
getObject : Matcher a d -> d -> List a -> Maybe a
getObject matcher tar objs =
    List.Extra.find (\x -> matcher x tar) objs
