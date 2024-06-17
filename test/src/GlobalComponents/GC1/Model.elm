module GlobalComponents.GC1.Model exposing (genGC)

{-| A Global Component to show FPS
-}

import Canvas
import Color
import Json.Encode
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Base exposing (UserEvent(..))
import Messenger.Render.Text exposing (renderTextWithColor)
import Messenger.Scene.Scene exposing (ConcreteGlobalComponent, GCTarget, GlobalComponentInit, GlobalComponentStorage, GlobalComponentUpdate, GlobalComponentUpdateRec, GlobalComponentView, genGlobalComponent)


type alias Data =
    { lastTenTime : List Int
    , fps : Float
    }


init : GlobalComponentInit UserData SceneMsg Data
init _ _ =
    { lastTenTime = []
    , fps = 0
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
updaterec env msg data =
    ( data, [], env )


view : GlobalComponentView UserData SceneMsg Data
view env data =
    renderTextWithColor env.globalData 10 ("FPS: " ++ String.fromFloat data.fps) "Arial" Color.gray ( 0, 0 )


gcCon : ConcreteGlobalComponent Data UserData SceneMsg
gcCon =
    { init = init
    , update = update
    , updaterec = updaterec
    , view = view
    , matcher = "fps"
    }


genGC : Maybe GCTarget -> GlobalComponentStorage UserData SceneMsg
genGC =
    genGlobalComponent gcCon Json.Encode.null
