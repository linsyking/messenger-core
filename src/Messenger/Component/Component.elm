module Messenger.Component.Component exposing
    ( AbstractComponent
    , ConcreteUserComponent
    , genComponent
    , updateComponents, updateComponentsWithBlock
    , updateComponentsWithTarget
    , genComponentsRenderList, viewComponentsRenderList
    , viewComponents
    , ComponentInit, ComponentUpdate, ComponentUpdateRec, ComponentView, ComponentMatcher
    , ComponentStorage, LevelComponentStorage
    )

{-|


# Component

A component is an object that you may put in your layers.


## User components

These are components that you users can create with custom **basedata**.

Basedata is the data that components with the same type can share.

For example, you may want a game component to have some common properties like position, velocity, etc.

In this case, your basedata would be a record with these properties.

@docs AbstractComponent
@docs ConcreteUserComponent
@docs genComponent
@docs updateComponents, updateComponentsWithBlock
@docs updateComponentsWithTarget


## View components

@docs genComponentsRenderList, viewComponentsRenderList
@docs viewComponents


# Type sugar

@docs ComponentInit, ComponentUpdate, ComponentUpdateRec, ComponentView, ComponentMatcher
@docs ComponentStorage, LevelComponentStorage

-}

import Canvas exposing (Renderable, group)
import Messenger.Base exposing (Env, UserEvent)
import Messenger.GeneralModel exposing (AbstractGeneralModel, ConcreteGeneralModel, abstract, unroll)
import Messenger.Recursion exposing (updateObjects, updateObjectsWithTarget)
import Messenger.Scene.Scene exposing (MMsg, MMsgBase, SceneOutputMsg)


{-| ConcreteUserComponent

Not very likely to be used. The messenger template will handle this for you automatically.

-}
type alias ConcreteUserComponent data cdata userdata tar msg bdata scenemsg =
    ConcreteGeneralModel data (Env cdata userdata) UserEvent tar msg ( Renderable, Int ) bdata (SceneOutputMsg scenemsg userdata)


{-| Component init type sugar.

Receives the environment and the initializing message (from which you can extract the parameters used to initialize the object, for example position),
and returns a tuple of (Data, BaseData).

-}
type alias ComponentInit cdata userdata msg data bdata =
    Env cdata userdata -> msg -> ( data, bdata )


{-| Component update type sugar

Determines the behaviour when a UserEvent is sent to the component. This could include, but not limited to, a mouse click, a keyboard press, or most commonly a time tick
(which means a specific amount of time has passed, see the manual)

Receives the environment env, the UserEvent evnt, the currect data and basedata ,and returns a structure in ((Data,BaseData),List Msg, (Env, Bool)).

  - Note: The last bool determines if the message would be blocked from the next component. If it is set to be True, then all the components behind it will
    not receive this UserEvent. This is useful when you want the action of the player only influence the front component, but not the conponents behind them.

-}
type alias ComponentUpdate cdata data userdata scenemsg tar msg bdata =
    Env cdata userdata -> UserEvent -> data -> bdata -> ( ( data, bdata ), List (MMsg tar msg scenemsg userdata), ( Env cdata userdata, Bool ) )


{-| Component updaterec type sugar

Determines the behaviour when other components, layers or scene sends messages to the component. For example, a knife component may send a message to the monster component to decrease its blood.

Receives the environment env, the component messages (defined in ComponentBase.elm), the currect data and basedata ,and returns a structure in ((Data,BaseData),List Msg, Env).

-}
type alias ComponentUpdateRec cdata data userdata scenemsg tar msg bdata =
    Env cdata userdata -> msg -> data -> bdata -> ( ( data, bdata ), List (MMsg tar msg scenemsg userdata), Env cdata userdata )


{-| Component view type sugar

Renders the component. If you want to render multiple things, you should use Canvas.group to assemble them into one renderable.
The second entry of the returning tuple is the "z-index" of the component. For example, if you want to put something important to the toppest layer of the window (Like the mouse cursor),
you can set it to a very high value(like 1000).

Receives environment, data, and basedata.

-}
type alias ComponentView cdata userdata data bdata =
    Env cdata userdata -> data -> bdata -> ( Renderable, Int )


{-| Component storage type sugar

  - Not very likely to be used. The messenger template will handle this for you automatically.

-}
type alias ComponentStorage cdata userdata tar msg bdata scenemsg =
    msg -> LevelComponentStorage cdata userdata tar msg bdata scenemsg


{-| Level component storage type sugar

  - Not very likely to be used. The messenger template will handle this for you automatically.

-}
type alias LevelComponentStorage cdata userdata tar msg bdata scenemsg =
    Env cdata userdata -> AbstractComponent cdata userdata tar msg bdata scenemsg


{-| Abstract component

  - Not very likely to be used. The messenger template will handle this for you automatically.

-}
type alias AbstractComponent cdata userdata tar msg bdata scenemsg =
    AbstractGeneralModel (Env cdata userdata) UserEvent tar msg ( Renderable, Int ) bdata (SceneOutputMsg scenemsg userdata)


{-| Component matcher type sugar
The function that is used to judge if this component is the target of a incoming component msg (sent by OtherTar). It will return true if the incoming
componentMatcher matches this component.

It receives data, basedata, and the componentMatcher value of the incoming message.

Here is an example of it:

matcher : ComponentMatcher Data BaseData ComponentTarget
matcher data basedata tar = (tar == Type basedata.ty || tar == Id basedata.id)

Where the matcher will return true if the id or the name matches the incoming message.

-}
type alias ComponentMatcher data bdata tar =
    data -> bdata -> tar -> Bool


{-| Generate abstract user component from concrete component.

  - Not very likely to be used. The messenger template will handle this for you automatically.

-}
genComponent : ConcreteUserComponent data cdata userdata tar msg bdata scenemsg -> ComponentStorage cdata userdata tar msg bdata scenemsg
genComponent concomp =
    abstract concomp


{-| Update a list of abstract user components.
-}
updateComponents : Env cdata userdata -> UserEvent -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> ( List (AbstractComponent cdata userdata tar msg bdata scenemsg), List (MMsgBase msg scenemsg userdata), ( Env cdata userdata, Bool ) )
updateComponents env evt comps =
    updateObjects env evt comps


{-| Update a list of abstract user components with block indicator.

It block is True, this function will not update anything.

  - Not very likely to be used. The messenger template will handle this for you automatically.

-}
updateComponentsWithBlock : Env cdata userdata -> UserEvent -> Bool -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> ( List (AbstractComponent cdata userdata tar msg bdata scenemsg), List (MMsgBase msg scenemsg userdata), ( Env cdata userdata, Bool ) )
updateComponentsWithBlock env evt block comps =
    if block then
        ( comps, [], ( env, True ) )

    else
        updateObjects env evt comps


{-| Update a list of abstract user components with targeted msgs.

  - Not very likely to be used. The messenger template will handle this for you automatically.

-}
updateComponentsWithTarget : Env cdata userdata -> List ( tar, msg ) -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> ( List (AbstractComponent cdata userdata tar msg bdata scenemsg), List (MMsgBase msg scenemsg userdata), Env cdata userdata )
updateComponentsWithTarget env msgs comps =
    updateObjectsWithTarget env msgs comps


{-| Generate render list for one list of components.

Useful when there are several component lists.

The output should be used as the input of `viewComponentsRenderList`.

-}
genComponentsRenderList : Env cdata userdata -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> List ( Renderable, Int )
genComponentsRenderList env compls =
    List.map (\comp -> (unroll comp).view env) compls


{-| View the render list of components.

Useful when there are several component lists.

The input should be generated by several `genComponentsRenderList`.

-}
viewComponentsRenderList : List ( Renderable, Int ) -> Renderable
viewComponentsRenderList previews =
    group [] <|
        List.map (\( r, _ ) -> r) <|
            List.sortBy (\( _, n ) -> n) previews


{-| View one list of abstract components.

Used when there is only one list of components

-}
viewComponents : Env cdata userdata -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> Renderable
viewComponents env compls =
    viewComponentsRenderList <| genComponentsRenderList env compls
