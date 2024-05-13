module Messenger.Recursion exposing (updateObjects, updateObjectsWithTarget)

{-|


# RecursionList

List implementation for the recursion algorithm

@docs updateObjects, updateObjectsWithTarget

-}

import List exposing (reverse)
import Messenger.GeneralModel exposing (AbstractGeneralModel, Msg(..), MsgBase, unroll)


{-| Recursively update all the objects in the List
-}
updateObjects : env -> event -> List (AbstractGeneralModel env event tar msg ren bdata sommsg) -> ( List (AbstractGeneralModel env event tar msg ren bdata sommsg), List (MsgBase msg sommsg), ( env, Bool ) )
updateObjects env evt objs =
    let
        ( newObjs, ( newMsgUnfinished, newMsgFinished ), ( newEnv, newBlock ) ) =
            updateOnce env evt objs

        ( resObj, resMsg, resEnv ) =
            updateRemain newEnv ( newMsgUnfinished, newMsgFinished ) newObjs
    in
    ( resObj, resMsg, ( resEnv, newBlock ) )


{-| Recursively update all the objects in the List, but also uses target
-}
updateObjectsWithTarget : env -> List (Msg tar msg sommsg) -> List (AbstractGeneralModel env event tar msg ren bdata sommsg) -> ( List (AbstractGeneralModel env event tar msg ren bdata sommsg), List (MsgBase msg sommsg), env )
updateObjectsWithTarget env msgs objs =
    updateRemain env ( msgs, [] ) objs



-- Below are some helper functions


updateOne : env -> event -> List (AbstractGeneralModel env event tar msg ren bdata sommsg) -> List (AbstractGeneralModel env event tar msg ren bdata sommsg) -> List (Msg tar msg sommsg) -> List (MsgBase msg sommsg) -> ( List (AbstractGeneralModel env event tar msg ren bdata sommsg), ( List (Msg tar msg sommsg), List (MsgBase msg sommsg) ), ( env, Bool ) )
updateOne lastEnv evt objs lastObjs lastMsgUnfinished lastMsgFinished =
    case objs of
        ele :: restObjs ->
            let
                ( newObj, newMsg, ( newEnv, block ) ) =
                    (unroll ele).update lastEnv evt

                finishedMsg =
                    List.filterMap
                        (\m ->
                            case m of
                                Parent x ->
                                    Just x

                                _ ->
                                    Nothing
                        )
                        newMsg

                unfinishedMsg =
                    List.filter
                        (\m ->
                            case m of
                                Parent _ ->
                                    False

                                _ ->
                                    True
                        )
                        newMsg
            in
            if block then
                ( reverse restObjs ++ newObj :: lastObjs, ( lastMsgUnfinished ++ unfinishedMsg, lastMsgFinished ++ finishedMsg ), ( lastEnv, block ) )

            else
                updateOne newEnv evt restObjs (newObj :: lastObjs) (lastMsgUnfinished ++ unfinishedMsg) (lastMsgFinished ++ finishedMsg)

        [] ->
            ( lastObjs, ( lastMsgUnfinished, lastMsgFinished ), ( lastEnv, False ) )


updateOnce : env -> event -> List (AbstractGeneralModel env event tar msg ren bdata sommsg) -> ( List (AbstractGeneralModel env event tar msg ren bdata sommsg), ( List (Msg tar msg sommsg), List (MsgBase msg sommsg) ), ( env, Bool ) )
updateOnce env evt objs =
    updateOne env evt (reverse objs) [] [] []


{-| Recursively update remaining objects
-}
updateRemain : env -> ( List (Msg tar msg sommsg), List (MsgBase msg sommsg) ) -> List (AbstractGeneralModel env event tar msg ren bdata sommsg) -> ( List (AbstractGeneralModel env event tar msg ren bdata sommsg), List (MsgBase msg sommsg), env )
updateRemain env ( unfinishedMsg, finishedMsg ) objs =
    if List.isEmpty unfinishedMsg then
        ( objs, finishedMsg, env )

    else
        let
            ( newObjs, ( newUnfinishedMsg, newFinishedMsg ), newEnv ) =
                List.foldl
                    (\ele ( lastObjs, ( lastMsgUnfinished, lastMsgFinished ), lastEnv ) ->
                        let
                            msgMatched =
                                List.filterMap
                                    (\ufmsg ->
                                        case ufmsg of
                                            Parent _ ->
                                                Nothing

                                            Other tar msg ->
                                                if (unroll ele).matcher tar then
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
                                                    (unroll lastObj2).updaterec lastEnv2 msg

                                                finishedMsgs =
                                                    List.filterMap
                                                        (\nmsg ->
                                                            case nmsg of
                                                                Parent pmsg ->
                                                                    Just pmsg

                                                                Other _ _ ->
                                                                    Nothing
                                                        )
                                                        newMsgs

                                                unfinishedMsgs =
                                                    List.filter
                                                        (\nmsg ->
                                                            case nmsg of
                                                                Parent _ ->
                                                                    False

                                                                Other _ _ ->
                                                                    True
                                                        )
                                                        newMsgs
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
        updateRemain newEnv ( newUnfinishedMsg, finishedMsg ++ newFinishedMsg ) newObjs
