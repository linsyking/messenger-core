module GlobalComponents.GC1.Model exposing (Msg, encode, genGC)

{-| A Global Component to show FPS
-}

import Color
import Json.Decode as D
import Json.Encode as E
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
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


init : GlobalComponentInit UserData SceneMsg Data
init _ gcmsg =
    let
        msg =
            decode gcmsg
    in
    { lastTenTime = []
    , fps = 0
    , size = msg.fontSize
    }


update : GlobalComponentUpdate UserData SceneMsg Data
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


updaterec : GlobalComponentUpdateRec UserData SceneMsg Data
updaterec env _ data =
    ( data, [], env )


view : GlobalComponentView UserData SceneMsg Data
view env data =
    renderTextWithColor env.globalData data.size ("FPS: " ++ String.fromInt (floor data.fps)) "Arial" Color.gray ( 0, 0 )


gcCon : ConcreteGlobalComponent Data UserData SceneMsg
gcCon =
    { init = init
    , update = update
    , updaterec = updaterec
    , view = view
    , matcher = "fps"
    }


genGC : Maybe GCMsg -> Maybe GCTarget -> GlobalComponentStorage UserData SceneMsg
genGC gcmsg =
    genGlobalComponent gcCon <| Maybe.withDefault E.null gcmsg
