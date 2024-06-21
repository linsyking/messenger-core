module GlobalComponents.Transition.Model exposing (InitOption, genGC)

{-|


# Transition

@docs InitOption, genGC

-}

import Canvas exposing (Renderable)
import GlobalComponents.Transition.Transitions.Base exposing (Transition)
import GlobalComponents.Transition.VSR exposing (VSR, updateVSR, viewVSR)
import Json.Encode exposing (null)
import Messenger.Base exposing (UserEvent(..), removeCommonData)
import Messenger.Component.GlobalComponent exposing (genGlobalComponent)
import Messenger.GeneralModel exposing (Msg(..), MsgBase(..))
import Messenger.Scene.Scene exposing (AbstractScene(..), ConcreteGlobalComponent, GCTarget, GlobalComponentInit, GlobalComponentStorage, GlobalComponentUpdate, GlobalComponentUpdateRec, GlobalComponentView, MAbstractScene, SceneOutputMsg(..), updateResultRemap)


{-| Options
-}
type alias InitOption scenemsg =
    { transition : Transition
    , scene : ( String, Maybe scenemsg )
    , filterSOM : Bool
    }


type alias Data userdata scenemsg =
    { preScene : Maybe (MAbstractScene userdata scenemsg)
    , transition : Transition
    , scene : ( String, Maybe scenemsg )
    , filterSOM : Bool
    , vsr : Maybe (VSR userdata scenemsg)
    }


init : InitOption scenemsg -> GlobalComponentInit userdata scenemsg (Data userdata scenemsg)
init opts _ _ =
    ( { preScene = Nothing
      , transition = opts.transition
      , scene = opts.scene
      , filterSOM = opts.filterSOM
      , vsr = Nothing
      }
    , { dead = False
      , postProcessor = []
      }
    )


remap : ( List (SceneOutputMsg scenemsg userdata), env ) -> ( List (SceneOutputMsg scenemsg userdata), env )
remap ( som, env ) =
    ( List.filter
        (\msg ->
            case msg of
                SOMChangeScene _ _ ->
                    False

                SOMLoadGC _ ->
                    False

                _ ->
                    True
        )
        som
    , env
    )


update : GlobalComponentUpdate userdata scenemsg (Data userdata scenemsg)
update env evnt data bdata =
    let
        trans0 =
            data.transition

        ( env1, data1 ) =
            if data.filterSOM then
                -- Disable SOM messages
                let
                    newScene =
                        updateResultRemap remap env.commonData
                in
                ( { env | commonData = newScene }, { data | filterSOM = False } )

            else
                ( env, data )
    in
    if trans0.options.mix then
        updateMix env1 evnt data1 bdata

    else
        updateNoMix env1 evnt data1 bdata


max1 : Float -> Float
max1 a =
    if a > 1 then
        1

    else
        a


updateMix : GlobalComponentUpdate userdata scenemsg (Data userdata scenemsg)
updateMix env evnt data bdata =
    let
        trans0 =
            data.transition

        ( scene, scenemsg ) =
            data.scene

        ( data1, soms ) =
            case data.vsr of
                Nothing ->
                    ( data, [] )

                Just vsr ->
                    let
                        ( newVSR, lsoms ) =
                            updateVSR vsr evnt
                    in
                    ( { data | vsr = Just newVSR }, List.map (\s -> Parent <| SOMMsg s) lsoms )
    in
    case evnt of
        Tick delta ->
            let
                newTime =
                    trans0.currentTransition + delta

                ( data1_1, soms1 ) =
                    case data.vsr of
                        Nothing ->
                            -- Save the current scene
                            ( { data | vsr = Just (VSR (removeCommonData env) env.commonData) }
                            , [ Parent <| SOMMsg <| SOMChangeScene scenemsg scene ]
                            )

                        Just _ ->
                            ( data1, soms )

                data2 =
                    { data1_1 | transition = { trans0 | currentTransition = newTime } }
            in
            if newTime >= max trans0.inT trans0.outT then
                -- End
                ( ( data2, { bdata | dead = True } ), soms1, ( env, False ) )

            else
                let
                    oldSceneView =
                        case data2.vsr of
                            Nothing ->
                                Canvas.empty

                            Just vsr ->
                                viewVSR vsr

                    outProgress =
                        max1 <| toFloat data.transition.currentTransition / toFloat data.transition.outT

                    inProgress =
                        max1 <| toFloat data.transition.currentTransition / toFloat data.transition.inT

                    pp : Renderable -> Renderable
                    pp ren =
                        Canvas.group []
                            [ data.transition.inTrans env.globalData.internalData ren inProgress
                            , data.transition.outTrans env.globalData.internalData oldSceneView outProgress
                            ]
                in
                ( ( data2, { bdata | postProcessor = [ pp ] } ), soms1, ( env, False ) )

        _ ->
            ( ( data1, bdata ), soms, ( env, False ) )


updateNoMix : GlobalComponentUpdate userdata scenemsg (Data userdata scenemsg)
updateNoMix env evnt data bdata =
    let
        trans0 =
            data.transition
    in
    case evnt of
        Tick delta ->
            let
                newTime =
                    trans0.currentTransition + delta

                data2 =
                    { data | transition = { trans0 | currentTransition = newTime } }
            in
            if newTime >= trans0.inT + trans0.outT then
                -- End
                ( ( data, { bdata | dead = True } ), [], ( env, False ) )

            else if trans0.currentTransition <= trans0.outT then
                let
                    progress =
                        max1 <| toFloat trans0.currentTransition / toFloat trans0.outT

                    outPP : Renderable -> Renderable
                    outPP ren =
                        trans0.outTrans env.globalData.internalData ren progress
                in
                if newTime >= trans0.outT then
                    -- Needs to change scene
                    let
                        ( scene, scenemsg ) =
                            data.scene

                        progress2 =
                            max1 <| toFloat (newTime - trans0.outT) / toFloat trans0.inT

                        inPP : Renderable -> Renderable
                        inPP ren =
                            trans0.inTrans env.globalData.internalData ren progress2
                    in
                    ( ( data2, { bdata | postProcessor = [ inPP ] } ), [ Parent <| SOMMsg (SOMChangeScene scenemsg scene) ], ( env, False ) )

                else
                    ( ( data2, { bdata | postProcessor = [ outPP ] } ), [], ( env, False ) )

            else
                -- Implies trans0.outT + trans0.inT > trans0.currentTransition > trans0.outT
                let
                    progress =
                        max1 <| toFloat (trans0.currentTransition - trans0.outT) / toFloat trans0.inT

                    inPP : Renderable -> Renderable
                    inPP ren =
                        trans0.inTrans env.globalData.internalData ren progress
                in
                ( ( data2, { bdata | postProcessor = [ inPP ] } ), [], ( env, False ) )

        _ ->
            ( ( data, bdata ), [], ( env, False ) )


updaterec : GlobalComponentUpdateRec userdata scenemsg (Data userdata scenemsg)
updaterec env _ data bdata =
    ( ( data, bdata ), [], env )


view : GlobalComponentView userdata scenemsg (Data userdata scenemsg)
view _ _ _ =
    Canvas.empty


gcCon : InitOption scenemsg -> ConcreteGlobalComponent (Data userdata scenemsg) userdata scenemsg
gcCon opt =
    { init = init opt
    , update = update
    , updaterec = updaterec
    , view = view
    , id = "transition"
    }


{-| Generate a global component.
-}
genGC : InitOption scenemsg -> Maybe GCTarget -> GlobalComponentStorage userdata scenemsg
genGC opt =
    genGlobalComponent (gcCon opt) null
