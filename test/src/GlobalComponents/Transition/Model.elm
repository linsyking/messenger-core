module GlobalComponents.Transition.Model exposing (InitOption, genGC)

{-|


# Transition

@docs InitOption, genGC

-}

import Canvas exposing (Renderable)
import GlobalComponents.Transition.Transitions.Base exposing (Transition)
import Json.Encode exposing (null)
import Messenger.Base exposing (Env, UserEvent(..))
import Messenger.Component.GlobalComponent exposing (genGlobalComponent)
import Messenger.GeneralModel exposing (Msg(..), MsgBase(..))
import Messenger.Scene.Scene exposing (AbstractScene(..), ConcreteGlobalComponent, GCTarget, GlobalComponentInit, GlobalComponentStorage, GlobalComponentUpdate, GlobalComponentUpdateRec, GlobalComponentView, MAbstractScene, SceneOutputMsg(..), unroll)


{-| Options
-}
type alias InitOption scenemsg =
    ( Transition, String, Maybe scenemsg )


type alias Data userdata scenemsg =
    { preScene : Maybe (MAbstractScene userdata scenemsg)
    , transition : Transition
    , scene : ( String, Maybe scenemsg )
    }


init : InitOption scenemsg -> GlobalComponentInit userdata scenemsg (Data userdata scenemsg)
init ( tran, scene, msg ) _ _ =
    ( { preScene = Nothing
      , transition = tran
      , scene = ( scene, msg )
      }
    , { dead = False
      , postProcessor = []
      }
    )


update : GlobalComponentUpdate userdata scenemsg (Data userdata scenemsg)
update env evnt data bdata =
    case evnt of
        Tick delta ->
            let
                trans0 =
                    data.transition

                newTime =
                    trans0.currentTransition + delta

                ( scene, scenemsg ) =
                    data.scene

                data2 =
                    { data | transition = { trans0 | currentTransition = newTime } }
            in
            if newTime >= trans0.inT + trans0.outT then
                -- End
                ( ( data, { bdata | dead = True } ), [], ( env, False ) )

            else if data.transition.options.mix then
                -- TODO
                ( ( data, bdata ), [], ( env, False ) )

            else if trans0.currentTransition < trans0.outT then
                let
                    progress_ =
                        toFloat data.transition.currentTransition / toFloat data.transition.outT

                    progress =
                        if progress_ > 1 then
                            1

                        else
                            progress_

                    outPP : Renderable -> Renderable
                    outPP ren =
                        data.transition.outTrans env.globalData.internalData ren progress
                in
                if newTime >= trans0.outT then
                    -- Needs to change scene
                    ( ( data2, { bdata | postProcessor = [ outPP ] } ), [ Parent <| SOMMsg (SOMChangeScene scenemsg scene) ], ( env, False ) )

                else
                    ( ( data2, { bdata | postProcessor = [ outPP ] } ), [], ( env, False ) )

            else
                -- Implies trans0.outT + trans0.inT > trans0.currentTransition >= trans0.outT
                let
                    progress_ =
                        toFloat (data.transition.currentTransition - data.transition.outT) / toFloat data.transition.inT

                    progress =
                        if progress_ > 1 then
                            1

                        else
                            progress_

                    inPP : Renderable -> Renderable
                    inPP ren =
                        data.transition.inTrans env.globalData.internalData ren progress
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
