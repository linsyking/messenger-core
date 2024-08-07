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
-}
type alias ConcreteUserComponent data cdata userdata tar msg bdata scenemsg =
    ConcreteGeneralModel data (Env cdata userdata) UserEvent tar msg ( Renderable, Int ) bdata (SceneOutputMsg scenemsg userdata)


{-| Component init type sugar
-}
type alias ComponentInit cdata userdata msg data bdata =
    Env cdata userdata -> msg -> ( data, bdata )


{-| Component update type sugar
-}
type alias ComponentUpdate cdata data userdata scenemsg tar msg bdata =
    Env cdata userdata -> UserEvent -> data -> bdata -> ( ( data, bdata ), List (MMsg tar msg scenemsg userdata), ( Env cdata userdata, Bool ) )


{-| Component updaterec type sugar
-}
type alias ComponentUpdateRec cdata data userdata scenemsg tar msg bdata =
    Env cdata userdata -> msg -> data -> bdata -> ( ( data, bdata ), List (MMsg tar msg scenemsg userdata), Env cdata userdata )


{-| Component view type sugar

The second entry is the "z-index" of the component.

-}
type alias ComponentView cdata userdata data bdata =
    Env cdata userdata -> data -> bdata -> ( Renderable, Int )


{-| Component storage type sugar
-}
type alias ComponentStorage cdata userdata tar msg bdata scenemsg =
    msg -> LevelComponentStorage cdata userdata tar msg bdata scenemsg


{-| Level component storage type sugar
-}
type alias LevelComponentStorage cdata userdata tar msg bdata scenemsg =
    Env cdata userdata -> AbstractComponent cdata userdata tar msg bdata scenemsg


{-| Abstract component
-}
type alias AbstractComponent cdata userdata tar msg bdata scenemsg =
    AbstractGeneralModel (Env cdata userdata) UserEvent tar msg ( Renderable, Int ) bdata (SceneOutputMsg scenemsg userdata)


{-| Component matcher type sugar
-}
type alias ComponentMatcher data bdata tar =
    data -> bdata -> tar -> Bool


{-| Generate abstract user component from concrete component.
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

-}
updateComponentsWithBlock : Env cdata userdata -> UserEvent -> Bool -> List (AbstractComponent cdata userdata tar msg bdata scenemsg) -> ( List (AbstractComponent cdata userdata tar msg bdata scenemsg), List (MMsgBase msg scenemsg userdata), ( Env cdata userdata, Bool ) )
updateComponentsWithBlock env evt block comps =
    if block then
        ( comps, [], ( env, True ) )

    else
        updateObjects env evt comps


{-| Update a list of abstract user components with targeted msgs.
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
