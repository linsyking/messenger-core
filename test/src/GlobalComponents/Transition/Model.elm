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


type TransitionState
    = In
    | BeforeIn
    | Out
    | BeforeOut


{-| Options
-}
type alias InitOption scenemsg =
    ( Transition, String, Maybe scenemsg )


type alias Data userdata scenemsg =
    { preScene : Maybe (MAbstractScene userdata scenemsg)
    , transition : Transition
    , scene : ( String, Maybe scenemsg )
    , state : TransitionState
    }


init : InitOption scenemsg -> GlobalComponentInit userdata scenemsg (Data userdata scenemsg)
init ( tran, scene, msg ) _ _ =
    ( { preScene = Nothing
      , transition = tran
      , scene = ( scene, msg )
      , state = BeforeOut
      }
    , { dead = False
      }
    )


type alias SceneView userdata =
    Env () userdata -> Renderable


changeSceneView : MAbstractScene userdata scenemsg -> (SceneView userdata -> SceneView userdata) -> MAbstractScene userdata scenemsg
changeSceneView scene f =
    let
        old =
            unroll scene

        new =
            { old | view = f old.view }
    in
    Roll new


inViewReplace : Data userdata scenemsg -> SceneView userdata -> SceneView userdata
inViewReplace data old env =
    let
        progress_ =
            toFloat data.transition.currentTransition / toFloat data.transition.inT

        progress =
            if progress_ > 1 then
                1

            else
                progress_

        oldView =
            old env
    in
    data.transition.inTrans env.globalData.internalData oldView progress


outViewReplace : Data userdata scenemsg -> SceneView userdata -> SceneView userdata
outViewReplace data old env =
    let
        progress_ =
            toFloat data.transition.currentTransition / toFloat data.transition.outT

        progress =
            if progress_ > 1 then
                1

            else
                progress_

        oldView =
            old env
    in
    data.transition.outTrans env.globalData.internalData oldView progress


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
            in
            if data.state == In && newTime >= trans0.inT then
                -- End
                ( ( data, { bdata | dead = True } ), [], ( env, False ) )

            else if data.transition.options.mix then
                -- TODO
                ( ( data, bdata ), [], ( env, False ) )

            else if data.state == BeforeOut then
                -- Change view of current scene
                let
                    currentScene =
                        env.commonData

                    newScene =
                        changeSceneView currentScene (outViewReplace data)
                in
                ( ( { data | state = Out }, bdata ), [], ( { env | commonData = newScene }, False ) )

            else if data.state == Out && newTime >= trans0.outT then
                let
                    trans1 =
                        { trans0 | currentTransition = 0 }
                in
                ( ( { data | state = BeforeIn, transition = trans1 }, bdata ), [ Parent <| SOMMsg (SOMChangeScene scenemsg scene) ], ( env, False ) )

            else if data.state == BeforeIn then
                -- Change view of current scene
                let
                    currentScene =
                        env.commonData

                    newScene =
                        changeSceneView currentScene (inViewReplace data)
                in
                ( ( { data | state = In }, bdata ), [], ( { env | commonData = newScene }, False ) )

            else
                ( ( data, bdata ), [], ( env, False ) )

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
