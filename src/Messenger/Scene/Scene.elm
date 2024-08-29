module Messenger.Scene.Scene exposing
    ( AbstractScene(..)
    , MConcreteScene, MAbstractScene
    , unroll, abstract
    , SceneOutputMsg(..)
    , SceneStorage, AllScenes
    , MMsg, MMsgBase
    , MConcreteGeneralModel, MAbstractGeneralModel
    , updateResultRemap
    , GCCommonData, GCBaseData, GCMsg, GCTarget
    , AbstractGlobalComponent, ConcreteGlobalComponent
    , GlobalComponentInit, GlobalComponentUpdate, GlobalComponentUpdateRec, GlobalComponentView
    , GlobalComponentStorage
    )

{-|


# Scene Base

Gerneral Model and Basic types for Scenes

@docs AbstractScene
@docs MConcreteScene, MAbstractScene
@docs unroll, abstract
@docs SceneOutputMsg
@docs SceneStorage, AllScenes
@docs MMsg, MMsgBase
@docs MConcreteGeneralModel, MAbstractGeneralModel


## Scene Result Remapper

@docs updateResultRemap


# Global Component

@docs GCCommonData, GCBaseData, GCMsg, GCTarget
@docs AbstractGlobalComponent, ConcreteGlobalComponent
@docs GlobalComponentInit, GlobalComponentUpdate, GlobalComponentUpdateRec, GlobalComponentView
@docs GlobalComponentStorage

-}

import Audio exposing (Audio)
import Canvas exposing (Renderable)
import Dict
import Json.Decode
import Messenger.Audio.Base exposing (AudioOption, AudioTarget)
import Messenger.Base exposing (Env, UserEvent)
import Messenger.GeneralModel exposing (AbstractGeneralModel, ConcreteGeneralModel, Msg, MsgBase)


{-| Concrete Scene Model

Users deal with the fields in concrete model.

  - Note: Since there should only be one scene running at a time, a scene needed to receive messages from other, so updateRec is unnecessary.
    However, as we introduce GlobalComponent, this assumption is weakened. Also matcher is also unnecessary since we may use name as matcher.
    Therefore, we should only determine init, update and view function.

-}
type alias ConcreteScene data env event ren scenemsg userdata =
    { init : env -> Maybe scenemsg -> data
    , update : env -> event -> data -> ( data, List (SceneOutputMsg scenemsg userdata), env )
    , view : env -> data -> ren
    }


{-| Unrolled Abstract Scene Model

The unrolled abstract model. Used internally.

  - Note: The init function will only be called once when the object is created, so there is no need to store it in actual running models. Also the data is stored in globalData, so there is no need to store data.

-}
type alias UnrolledAbstractScene env event ren scenemsg userdata =
    { update : env -> event -> ( AbstractScene env event ren scenemsg userdata, List (SceneOutputMsg scenemsg userdata), env )
    , view : env -> ren
    }


{-| Rolled Abstract Scene Model.

Cannot be directedly modified.
Used for storage.

-}
type AbstractScene env event ren scenemsg userdata
    = Roll (UnrolledAbstractScene env event ren scenemsg userdata)


{-| Specialized Concrete scene for Messenger
-}
type alias MConcreteScene data userdata scenemsg =
    ConcreteScene data (Env () userdata) UserEvent Renderable scenemsg userdata


{-| Specialized Abstract scene for Messenger
-}
type alias MAbstractScene userdata scenemsg =
    AbstractScene (Env () userdata) UserEvent Renderable scenemsg userdata


{-| Unroll a rolled abstract scene.
-}
unroll : AbstractScene env event ren scenemsg userdata -> UnrolledAbstractScene env event ren scenemsg userdata
unroll (Roll un) =
    un


{-| Abstract a concrete scene to an abstract scene.

Initialize it with env and msg.

-}
abstract : ConcreteScene data env event ren scenemsg userdata -> Maybe scenemsg -> env -> AbstractScene env event ren scenemsg userdata
abstract conmodel initMsg initEnv =
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


{-| Scene Output Msg is the message that directedly handled by the top-level core.

`scenemsg` is a custom type which represents the message type users wants
to send to a scene when switching scenes.

  - `SOMChangeScene (Maybe scenemsg) name` is used to change to a target scene by giving the initial message and name of the scene.
      - Note: initial message is defined in Scene.elm or Scenebase.elm, depending on whether you use sceneproto or not.
  - `SOMPlayAudio channel name option` is used to play an audio resource by giving **channel name option**
      - Note: See the docs in Audio.elm for more information on options.
  - `SOMStopAudio` is used to stop a playing audio by giving the desired AudioTarget(see the docs on AudioTarget)
  - `SOMSetVolume` is used to set the volume with a value **from 0 to 1**
  - `SOMAlert` makes an alert
  - `SOMPrompt name title` makes a prompt with name and title. This means the system will eject a some textbox with the given title and name.
      - Note: The returning message from the user will be given as a worldevent.
  - `SOMSaveGlobalData` saves the global by encode funtion given in UserConfig
      - Note: At the beginning of the game messenger will load the stored data into userdata (which is also the only chance to load it)
        We urge you to store these data elsewhere and use userData as a local storage "in game", and only update the userData whenever the user save data.
  - The GC section is under construction.

-}
type SceneOutputMsg scenemsg userdata
    = SOMChangeScene (Maybe scenemsg) String
    | SOMAlert String
    | SOMPrompt String String
    | SOMPlayAudio Int String AudioOption
    | SOMTransformAudio AudioTarget (Audio -> Audio)
    | SOMStopAudio AudioTarget
    | SOMSetVolume Float
    | SOMSaveGlobalData
    | SOMLoadGC (GlobalComponentStorage userdata scenemsg)
    | SOMUnloadGC GCTarget
    | SOMCallGC ( GCTarget, GCMsg )


{-| The type used to store the scene data.
You should not handle this by yourself.
-}
type alias SceneStorage userdata scenemsg =
    Maybe scenemsg -> Env () userdata -> MAbstractScene userdata scenemsg


{-| All scenes type
-}
type alias AllScenes userdata scenemsg =
    Dict.Dict String (SceneStorage userdata scenemsg)


{-| Messsenger MsgBase
This message base make concrete MMsg type from abstract Msg type. By using MMsgBase, messenger can know what is SOMMsg and make reaction to it accordingly.
Users should use Msg at most of the time.
-}
type alias MMsgBase othermsg scenemsg userdata =
    MsgBase othermsg (SceneOutputMsg scenemsg userdata)


{-| Messenger Msg
-}
type alias MMsg othertar msg scenemsg userdata =
    Msg othertar msg (SceneOutputMsg scenemsg userdata)


{-| Specialized Concrete Model for Messenger
-}
type alias MConcreteGeneralModel data common userdata tar msg bdata scenemsg =
    ConcreteGeneralModel data (Env common userdata) UserEvent tar msg Renderable bdata (SceneOutputMsg scenemsg userdata)


{-| Specialized Abstract Model for Messenger
-}
type alias MAbstractGeneralModel common userdata tar msg bdata scenemsg =
    AbstractGeneralModel (Env common userdata) UserEvent tar msg Renderable bdata (SceneOutputMsg scenemsg userdata)


{-| Change the `update` function to remap the result and return the changed abstract scene.
-}
updateResultRemap : (( List (SceneOutputMsg scenemsg userdata), env ) -> ( List (SceneOutputMsg scenemsg userdata), env )) -> AbstractScene env event ren scenemsg userdata -> AbstractScene env event ren scenemsg userdata
updateResultRemap f model =
    let
        change : AbstractScene env event ren scenemsg userdata -> AbstractScene env event ren scenemsg userdata
        change m =
            let
                um =
                    unroll m

                newUpdate : env -> event -> ( AbstractScene env event ren scenemsg userdata, List (SceneOutputMsg scenemsg userdata), env )
                newUpdate env evnt =
                    let
                        ( oldr, oldmsg, oldres ) =
                            um.update env evnt

                        ( newmsg, newres ) =
                            f ( oldmsg, oldres )
                    in
                    ( change oldr, newmsg, newres )
            in
            Roll { um | update = newUpdate }
    in
    change model



--- Global Component


{-| Global component common data.
-}
type alias GCCommonData userdata scenemsg =
    MAbstractScene userdata scenemsg


{-| Global component base data.
-}
type alias GCBaseData =
    { dead : Bool
    , postProcessor : Renderable -> Renderable
    }


{-| Global component message type.
-}
type alias GCMsg =
    Json.Decode.Value


{-| Global component target type.
-}
type alias GCTarget =
    String


{-| init type sugar
-}
type alias GlobalComponentInit userdata scenemsg data =
    Env (GCCommonData userdata scenemsg) userdata -> GCMsg -> ( data, GCBaseData )


{-| update type sugar.
In a layered scene, the job is done by Messenger, so you don't have to write this. However, in a raw scene, the logic is up to you to decide, so you are supposed to imply this.
-}
type alias GlobalComponentUpdate userdata scenemsg data =
    Env (GCCommonData userdata scenemsg) userdata -> UserEvent -> data -> GCBaseData -> ( ( data, GCBaseData ), List (MMsg GCTarget GCMsg scenemsg userdata), ( Env (GCCommonData userdata scenemsg) userdata, Bool ) )


{-| updaterec type sugar
-}
type alias GlobalComponentUpdateRec userdata scenemsg data =
    Env (GCCommonData userdata scenemsg) userdata -> GCMsg -> data -> GCBaseData -> ( ( data, GCBaseData ), List (MMsg GCTarget GCMsg scenemsg userdata), Env (GCCommonData userdata scenemsg) userdata )


{-| view type sugar
-}
type alias GlobalComponentView userdata scenemsg data =
    Env (GCCommonData userdata scenemsg) userdata -> data -> GCBaseData -> Renderable


{-| GlobalComponent Storage
-}
type alias GlobalComponentStorage userdata scenemsg =
    Env (GCCommonData userdata scenemsg) userdata -> AbstractGlobalComponent userdata scenemsg


{-| Concrete GlobalComponent Model
-}
type alias ConcreteGlobalComponent data userdata scenemsg =
    { init : GlobalComponentInit userdata scenemsg data
    , update : GlobalComponentUpdate userdata scenemsg data
    , updaterec : GlobalComponentUpdateRec userdata scenemsg data
    , view : GlobalComponentView userdata scenemsg data
    , id : GCTarget
    }


{-| Abstract GlobalComponent Model.

Cannot be directedly modified.
Used for storage.

-}
type alias AbstractGlobalComponent userdata scenemsg =
    MAbstractGeneralModel (GCCommonData userdata scenemsg) userdata GCTarget GCMsg GCBaseData scenemsg
