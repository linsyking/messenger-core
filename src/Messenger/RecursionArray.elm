module Messenger.RecursionArray exposing
    ( updateObjects
    , getObjectIndices, getObjectIndex, getObjects, getObject, updateObjectByIndex, updateObjectsByTarget
    )

{-|


# RecursionArray

Array implementation for the recursion algorithm

@docs updateObjects


## Tools

@docs getObjectIndices, getObjectIndex, getObjects, getObject, updateObjectByIndex, updateObjectsByTarget

-}

import Array exposing (Array)
import Array.Extra exposing (insertAt)
import Messenger.Recursion exposing (Matcher, RecBody)


{-| Recursively update all the objects in the List
-}
updateObjects : RecBody a b c d -> c -> b -> Array a -> ( Array a, List b, c )
updateObjects rec env msg objs =
    let
        ( newObjs, ( newMsgUnfinished, newMsgFinished ), newEnv ) =
            updateOnce rec env msg objs
    in
    updateRemain rec newEnv ( newMsgUnfinished, newMsgFinished ) newObjs



-- Below are all helper functions


updateOnce : RecBody a b c d -> c -> b -> Array a -> ( Array a, ( List ( d, b ), List b ), c )
updateOnce rec env msg objs =
    Array.foldr
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
            ( insertAt 0 newObj lastObjs, ( lastMsgUnfinished ++ unfinishedMsg, lastMsgFinished ++ finishedMsg ), newEnv )
        )
        ( Array.empty, ( [], [] ), env )
        objs



{-
   Recursively update remaining objects
-}


updateRemain : RecBody a b c d -> c -> ( List ( d, b ), List b ) -> Array a -> ( Array a, List b, c )
updateRemain rec env ( unfinishedMsg, finishedMsg ) objs =
    if List.isEmpty unfinishedMsg then
        ( objs, finishedMsg, env )

    else
        let
            ( newObjs, ( newUnfinishedMsg, newFinishedMsg ), newEnv ) =
                Array.foldr
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
                            ( insertAt 0 ele lastObjs, ( lastMsgUnfinished, lastMsgFinished ), lastEnv )

                        else
                            -- Need update
                            let
                                -- Update the object with all messages in msgMatched
                                ( newObj, ( newMsgUnfinished, newMsgFinished ), newEnv2 ) =
                                    List.foldl
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
                                            ( newEle, ( lastMsgUnfinished2 ++ unfinishedMsgs, lastMsgFinished2 ++ finishedMsgs ), newEnv3 )
                                        )
                                        ( ele, ( [], [] ), env )
                                        msgMatched
                            in
                            ( insertAt 0 newObj lastObjs, ( lastMsgUnfinished ++ newMsgUnfinished, lastMsgFinished ++ newMsgFinished ), newEnv2 )
                    )
                    ( Array.empty, ( [], [] ), env )
                    objs
        in
        updateRemain rec newEnv ( newUnfinishedMsg, finishedMsg ++ newFinishedMsg ) newObjs


{-| locate
Locate an element by index in an array.
-}
locate : (a -> Bool) -> Array a -> List Int
locate f xs =
    let
        b =
            List.range 0 (Array.length xs - 1)

        res =
            List.filter
                (\i ->
                    case Array.get i xs of
                        Just x ->
                            if f x then
                                True

                            else
                                False

                        Nothing ->
                            False
                )
                b
    in
    res


{-| Get the indices of the objects that match the target
-}
getObjectIndices : Matcher a d -> d -> Array a -> List Int
getObjectIndices matcher tar objs =
    locate (\x -> matcher x tar) objs


{-| Get the index of the object that matches the target (the first one)

TODO: use better algorithm

-}
getObjectIndex : Matcher a d -> d -> Array a -> Maybe Int
getObjectIndex matcher tar objs =
    List.head <| getObjectIndices matcher tar objs


{-| Get the objects that match the target
-}
getObjects : Matcher a d -> d -> Array a -> Array a
getObjects matcher tar objs =
    Array.filter (\x -> matcher x tar) objs


{-| Get the object that matches the target (the first one)
-}
getObject : Matcher a d -> d -> Array a -> Maybe a
getObject matcher tar objs =
    Array.get 0 <| getObjects matcher tar objs


{-| Update the object by index
-}
updateObjectByIndex : RecBody a b c d -> c -> b -> Int -> Array a -> ( Array a, List ( d, b ), c )
updateObjectByIndex rec env msg index objs =
    case Array.get index objs of
        Nothing ->
            ( objs, [], env )

        Just obj ->
            let
                ( newObj, newMsg, newEnv ) =
                    rec.update obj env msg
            in
            ( Array.set index newObj objs, newMsg, newEnv )


{-| Update all the objects that match the target
-}
updateObjectsByTarget : RecBody a b c d -> c -> b -> d -> Array a -> ( Array a, List ( d, b ), c )
updateObjectsByTarget rec env msg tar objs =
    let
        ( newObjs, newMsg, newEnv ) =
            Array.foldl
                (\obj ( lastObjs, lastMsg, lastEnv ) ->
                    if rec.match obj tar then
                        let
                            ( newObj, newMsg2, newEnv2 ) =
                                rec.update obj lastEnv msg
                        in
                        ( Array.push newObj lastObjs, lastMsg ++ newMsg2, newEnv2 )

                    else
                        ( Array.push obj lastObjs, lastMsg, lastEnv )
                )
                ( Array.empty, [], env )
                objs
    in
    ( newObjs, newMsg, newEnv )
