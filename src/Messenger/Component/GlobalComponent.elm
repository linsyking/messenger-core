module Messenger.Component.GlobalComponent exposing
    ( genGlobalComponent
    , filterAliveGC
    , combinePP
    )

{-|


# Global Component

@docs genGlobalComponent
@docs filterAliveGC
@docs combinePP

-}

import Messenger.GeneralModel as GM
import Messenger.Scene.Scene exposing (AbstractGlobalComponent, ConcreteGlobalComponent, GCBaseData, GCCommonData, GCMsg, GCTarget, GlobalComponentStorage, MConcreteGeneralModel)
import REGL exposing (Renderable)


{-| Generate abstract global component from concrete global component.
-}
genGlobalComponent : ConcreteGlobalComponent data userdata scenemsg -> GCMsg -> Maybe GCTarget -> GlobalComponentStorage userdata scenemsg
genGlobalComponent conpcomp gcmsg gctar =
    GM.abstract (gcTransform conpcomp gctar) <| gcmsg


{-| Turn global component into a general model.
-}
gcTransform : ConcreteGlobalComponent data userdata scenemsg -> Maybe GCTarget -> MConcreteGeneralModel data (GCCommonData userdata scenemsg) userdata GCTarget GCMsg GCBaseData scenemsg
gcTransform concomp gctar =
    let
        id =
            case gctar of
                Just t ->
                    t

                Nothing ->
                    concomp.id
    in
    { init = \env msg -> concomp.init env msg
    , update =
        \env evt data bdata ->
            let
                ( resData, resMsg, resEnv ) =
                    concomp.update env evt data bdata
            in
            ( resData, resMsg, resEnv )
    , updaterec =
        \env msg data bdata ->
            let
                ( resData, resMsg, resEnv ) =
                    concomp.updaterec env msg data bdata
            in
            ( resData, resMsg, resEnv )
    , view = \env data bdata -> concomp.view env data bdata
    , matcher = \_ _ tar -> tar == id
    }


{-| Filter out dead global components.
-}
filterAliveGC : List (AbstractGlobalComponent userdata scenemsg) -> List (AbstractGlobalComponent userdata scenemsg)
filterAliveGC xs =
    List.filter (\x -> not (GM.unroll x).baseData.dead) xs


{-| Combine post processors of all global components.
-}
combinePP : List (AbstractGlobalComponent userdata scenemsg) -> List (Renderable -> Renderable)
combinePP xs =
    List.map (\gc -> (GM.unroll gc).baseData.postProcessor) xs
