module Messenger.GeneralModel exposing
    ( Msg(..), MsgBase(..), filterSOM
    , ConcreteGeneralModel, AbstractGeneralModel(..)
    , unroll, abstract
    , viewModelList
    , Matcher
    , updateResultRemap, updaterecResultRemap
    )

{-|


# General Model

General model is designed to be an abstract interface of layers, components, etc..

A General model has the ability to:

  - specialize its own data types
  - share some data types with objects in the same type
  - be initialized by some inputs
  - be updated by event and msgs
  - be updated in a list of same type objects
  - send msg to the objects in the same type
  - send msg to the parent object
  - render itself
  - identify itself by a matcher

@docs Msg, MsgBase, filterSOM
@docs ConcreteGeneralModel, AbstractGeneralModel
@docs unroll, abstract
@docs viewModelList
@docs Matcher


## Result Remap

@docs updateResultRemap, updaterecResultRemap

-}

import REGL exposing (Renderable)
import Messenger.Base exposing (Env, UserEvent)


{-| MsgBase

Used when sending a msg to parent object.

Using **SOMMsg** when sending a `SceneOutputMsg`, which will be directly handled by Top-level.
Use **OtherMsg** when sending a normal message to parent.

Examples:

  - Parent <| SOMMsg <| SOMChangeScene (Just NullSceneMsg) "SelectLevel" -- SOMMsg
  - Parent <| OtherMsg <| genMeteor env.commonData.playerPos env.globalData.globalStartTime -- In a component. This genMeteor message will be handled in the layer containing the component.

-}
type MsgBase othermsg sommsg
    = SOMMsg sommsg
    | OtherMsg othermsg


{-| Filter SOMMsg from list of MsgBase.

  - Not very likely to be used. The messenger template will handle this for you automatically.

-}
filterSOM : List (MsgBase othermsg sommsg) -> List sommsg
filterSOM xs =
    List.filterMap
        (\x ->
            case x of
                SOMMsg som ->
                    Just som

                _ ->
                    Nothing
        )
        xs


{-| The Basic Msg Model.

Using **Other** when sending msg to objects in the same type.
Make sure the `othertar` can pass the matcher of target object.

Examples:

  - Parent <| SOMMsg <| SOMChangeScene (Just NullSceneMsg) "SelectLevel" -- Parent
  - Other <| ( Type "Camera", CameraShakeMsg 1 200 ) -- Note here Type "Camera" is of Matcher Type (Defined by the user) which indicates the target of the message, and the desired message is the second entry of the tuple.

-}
type Msg othertar msg sommsg
    = Parent (MsgBase msg sommsg)
    | Other ( othertar, msg )


{-| Concrete General Model.

Users deal with the fields in concrete model. That is to say, you should implement the init,update,updaterec,view and matcher function in the model.

-}
type alias ConcreteGeneralModel data env event tar msg ren bdata sommsg =
    { init : env -> msg -> ( data, bdata )
    , update : env -> event -> data -> bdata -> ( ( data, bdata ), List (Msg tar msg sommsg), ( env, Bool ) )
    , updaterec : env -> msg -> data -> bdata -> ( ( data, bdata ), List (Msg tar msg sommsg), env )
    , view : env -> data -> bdata -> ren
    , matcher : data -> bdata -> tar -> Bool
    }


{-| Unrolled Abstract General Model.

the unrolled abstract model. Used internally, but it the actual model for storaging data for models. It is sealed by roll in most of the time. See the manual and unroll function for more information.

-}
type alias UnrolledAbstractGeneralModel env event tar msg ren bdata sommsg =
    { update : env -> event -> ( AbstractGeneralModel env event tar msg ren bdata sommsg, List (Msg tar msg sommsg), ( env, Bool ) )
    , updaterec : env -> msg -> ( AbstractGeneralModel env event tar msg ren bdata sommsg, List (Msg tar msg sommsg), env )
    , view : env -> ren
    , matcher : tar -> Bool
    , baseData : bdata
    }


{-| Rolled Abstract General Model.

Cannot be directedly modified.
Used for storage.

-}
type AbstractGeneralModel env event tar msg ren bdata sommsg
    = Roll (UnrolledAbstractGeneralModel env event tar msg ren bdata sommsg)


{-| Unroll a rolled abstract model. In this way, you retrieve the data from the storage.
This is useful when you want to manipulate and calculate the reactions between components, like in collision handling process.
For example, the following function called by the layer can read the position of from AbstractComponent.
let
x\_data = unroll x
posX=x\_data.baseData.position
in
...
-}
unroll : AbstractGeneralModel env event tar msg ren bdata sommsg -> UnrolledAbstractGeneralModel env event tar msg ren bdata sommsg
unroll (Roll un) =
    un


{-| Abstract a concrete model to an abstract model.

Initialize it with env and msg.
Messenger will handle this for you.

-}
abstract : ConcreteGeneralModel data env event tar msg ren bdata sommsg -> msg -> env -> AbstractGeneralModel env event tar msg ren bdata sommsg
abstract conmodel initMsg initEnv =
    let
        abstractRec : data -> bdata -> AbstractGeneralModel env event tar msg ren bdata sommsg
        abstractRec data base =
            let
                updates : env -> event -> ( AbstractGeneralModel env event tar msg ren bdata sommsg, List (Msg tar msg sommsg), ( env, Bool ) )
                updates env event =
                    let
                        ( ( new_d, new_bd ), new_m, new_e ) =
                            conmodel.update env event data base
                    in
                    ( abstractRec new_d new_bd, new_m, new_e )

                updaterecs : env -> msg -> ( AbstractGeneralModel env event tar msg ren bdata sommsg, List (Msg tar msg sommsg), env )
                updaterecs env msg =
                    let
                        ( ( new_d, new_bd ), new_m, new_e ) =
                            conmodel.updaterec env msg data base
                    in
                    ( abstractRec new_d new_bd, new_m, new_e )

                views : env -> ren
                views env =
                    conmodel.view env data base

                matchers : tar -> Bool
                matchers =
                    conmodel.matcher data base

                baseDatas : bdata
                baseDatas =
                    base
            in
            Roll
                { update = updates
                , updaterec = updaterecs
                , view = views
                , matcher = matchers
                , baseData = baseDatas
                }

        ( init_d, init_bd ) =
            conmodel.init initEnv initMsg
    in
    abstractRec init_d init_bd


{-| View model list.

  - Not very likely to be used. The messenger template will handle this for you automatically.

-}
viewModelList : Env common userdata -> List (AbstractGeneralModel (Env common userdata) UserEvent tar msg Renderable bdata sommsg) -> List Renderable
viewModelList env models =
    List.map (\model -> (unroll model).view env) models


{-| A general matcher type sugar

Similar to the matcher function implemented by user in the components and layers.

-}
type alias Matcher data tar =
    data -> tar -> Bool


{-| Change the `update` function to remap the result and return the changed abstract general model.

  - Not very likely to be used. The messenger template will handle this for you automatically.

-}
updateResultRemap : (( List (Msg tar msg sommsg), ( env, Bool ) ) -> ( List (Msg tar msg sommsg), ( env, Bool ) )) -> AbstractGeneralModel env event tar msg ren bdata sommsg -> AbstractGeneralModel env event tar msg ren bdata sommsg
updateResultRemap f model =
    let
        change : AbstractGeneralModel env event tar msg ren bdata sommsg -> AbstractGeneralModel env event tar msg ren bdata sommsg
        change m =
            let
                um =
                    unroll m

                newUpdate : env -> event -> ( AbstractGeneralModel env event tar msg ren bdata sommsg, List (Msg tar msg sommsg), ( env, Bool ) )
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


{-| Change the `updaterec` function to remap the result and return the changed abstract general model.

  - Not very likely to be used. The messenger template will handle this for you automatically.

-}
updaterecResultRemap : (( List (Msg tar msg sommsg), env ) -> ( List (Msg tar msg sommsg), env )) -> AbstractGeneralModel env event tar msg ren bdata sommsg -> AbstractGeneralModel env event tar msg ren bdata sommsg
updaterecResultRemap f model =
    let
        change : AbstractGeneralModel env event tar msg ren bdata sommsg -> AbstractGeneralModel env event tar msg ren bdata sommsg
        change m =
            let
                um =
                    unroll m

                newUpdateRec : env -> msg -> ( AbstractGeneralModel env event tar msg ren bdata sommsg, List (Msg tar msg sommsg), env )
                newUpdateRec env msg =
                    let
                        ( oldr, oldmsg, oldres ) =
                            um.updaterec env msg

                        ( newmsg, newres ) =
                            f ( oldmsg, oldres )
                    in
                    ( change oldr, newmsg, newres )
            in
            Roll { um | updaterec = newUpdateRec }
    in
    change model
