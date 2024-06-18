module GlobalComponents.FPS.Model exposing (Msg, encode, genGC)

{-| Global component configuration module

A Global Component to show FPS at the corner

@docs Msg, encode, genGC

-}

import Color
import Json.Decode as D
import Json.Encode as E
import Messenger.Base exposing (UserEvent(..))
import Messenger.Render.Text exposing (renderTextWithColor)
import Messenger.Scene.Scene exposing (ConcreteGlobalComponent, GCMsg, GCTarget, GlobalComponentInit, GlobalComponentStorage, GlobalComponentUpdate, GlobalComponentUpdateRec, GlobalComponentView, genGlobalComponent)


type alias Msg =
    { fontSize : Float
    }


decode : GCMsg -> Msg
decode gcmsg =
    let
        decoder =
            D.field "size" D.float

        num =
            Result.withDefault 20 <| D.decodeValue decoder gcmsg
    in
    Msg num


{-| Encode custom message into GCMsg.
-}
encode : Msg -> GCMsg
encode msg =
    E.object
        [ ( "size", E.float msg.fontSize )
        ]


type alias Data =
    { lastTenTime : List Int
    , fps : Float
    , size : Float
    }


init : GlobalComponentInit userdata scenemsg Data
init _ gcmsg =
    let
        msg =
            decode gcmsg
    in
    { lastTenTime = []
    , fps = 0
    , size = msg.fontSize
    }


update : GlobalComponentUpdate userdata scenemsg Data
update env evnt data =
    case evnt of
        Tick delta ->
            let
                lastTimes =
                    (if List.length data.lastTenTime == 10 then
                        Maybe.withDefault [] <| List.tail data.lastTenTime

                     else
                        data.lastTenTime
                    )
                        ++ [ delta ]

                sum =
                    toFloat (List.sum lastTimes)

                fps =
                    toFloat (List.length lastTimes) / sum * 1000
            in
            ( { data | lastTenTime = lastTimes, fps = fps }, [], ( env, False ) )

        _ ->
            ( data, [], ( env, False ) )


updaterec : GlobalComponentUpdateRec userdata scenemsg Data
updaterec env _ data =
    ( data, [], env )


view : GlobalComponentView userdata scenemsg Data
view env data =
    renderTextWithColor env.globalData.internalData data.size ("FPS: " ++ String.fromInt (floor data.fps)) "Arial" Color.gray ( 0, 0 )


gcCon : ConcreteGlobalComponent Data userdata scenemsg
gcCon =
    { init = init
    , update = update
    , updaterec = updaterec
    , view = view
    , matcher = "fps"
    }


{-| Generate a global component.
-}
genGC : Maybe GCMsg -> Maybe GCTarget -> GlobalComponentStorage userdata scenemsg
genGC gcMsg =
    genGlobalComponent gcCon <| Maybe.withDefault E.null gcMsg
