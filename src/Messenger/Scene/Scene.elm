module Messenger.Scene.Scene exposing (..)

import Canvas exposing (Renderable)
import Messenger.Audio.Base exposing (AudioOption)
import Messenger.Base exposing (Env, WorldEvent)
import Messenger.Scene.Transitions.Base exposing (Transition)


type alias ConcreteScene data env event ren scenemsg userdata =
    { init : env -> Maybe scenemsg -> data
    , update : env -> event -> data -> ( data, List (SceneOutputMsg scenemsg userdata), env )
    , view : env -> data -> ren
    }


type alias UnrolledAbstractScene env event ren scenemsg userdata =
    { update : env -> event -> ( AbstractScene env event ren scenemsg userdata, List (SceneOutputMsg scenemsg userdata), env )
    , view : env -> ren
    }


type AbstractScene env event ren scenemsg userdata
    = Roll (UnrolledAbstractScene env event ren scenemsg userdata)


type alias MConcreteScene data userdata scenemsg =
    ConcreteScene data (Env () userdata) WorldEvent Renderable scenemsg userdata


type alias MAbstractScene userdata scenemsg =
    AbstractScene (Env () userdata) WorldEvent Renderable scenemsg userdata


unroll : AbstractScene env event ren scenemsg userdata -> UnrolledAbstractScene env event ren scenemsg userdata
unroll (Roll un) =
    un


abstract : ConcreteScene data env event ren scenemsg userdata -> env -> Maybe scenemsg -> AbstractScene env event ren scenemsg userdata
abstract conmodel initEnv initMsg =
    let
        abstractRec data =
            let
                updates : env -> event -> ( AbstractScene env event ren scenemsg userdata, List (SceneOutputMsg scenemsg userdata), env )
                updates env event =
                    let
                        ( new_d, new_m, new_e ) =
                            conmodel.update env event data
                    in
                    ( abstractRec new_d, new_m, new_e )

                views : env -> ren
                views env =
                    conmodel.view env data
            in
            Roll
                { update = updates
                , view = views
                }
    in
    abstractRec (conmodel.init initEnv initMsg)


{-| Remove common data from environment.

Useful when sending message to a component.

-}
noCommonData : Env cdata userdata -> Env () userdata
noCommonData env =
    { globalData = env.globalData
    , commonData = ()
    }


{-| Add the common data back.
-}
addCommonData : cdata -> Env () userdata -> Env cdata userdata
addCommonData commonData env =
    { globalData = env.globalData
    , commonData = commonData
    }


type SceneOutputMsg scenemsg userdata
    = SOMChangeScene ( Maybe scenemsg, String, Maybe (Transition userdata) )
    | SOMPlayAudio String String AudioOption -- audio name, audio url, audio option
    | SOMAlert String
    | SOMStopAudio String
    | SOMSetVolume Float
    | SOMPrompt String String -- name, title
    | SOMSaveUserData
