module GlobalComponents.FPS.Model exposing (InitOption, genGC)

{-| Global component configuration module

A Global Component to show FPS at the corner

@docs InitOption, genGC

-}

import Color
import Json.Encode as E
import Messenger.Base exposing (UserEvent(..))
import Messenger.Component.GlobalComponent exposing (genGlobalComponent)
import Messenger.Render.Text exposing (renderTextWithColor)
import Messenger.Scene.Scene exposing (ConcreteGlobalComponent, GCTarget, GlobalComponentInit, GlobalComponentStorage, GlobalComponentUpdate, GlobalComponentUpdateRec, GlobalComponentView)


{-| Init Options
-}
type alias InitOption =
    { fontSize : Float
    }


type alias Data =
    { lastTenTime : List Int
    , fps : Float
    , size : Float
    }


init : InitOption -> GlobalComponentInit userdata scenemsg Data
init opt _ _ =
    ( { lastTenTime = []
      , fps = 0
      , size = opt.fontSize
      }
    , { dead = False
      , postProcessor = []
      }
    )


update : GlobalComponentUpdate userdata scenemsg Data
update env evnt data bdata =
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
            ( ( { data | lastTenTime = lastTimes, fps = fps }, bdata ), [], ( env, False ) )

        _ ->
            ( ( data, bdata ), [], ( env, False ) )


updaterec : GlobalComponentUpdateRec userdata scenemsg Data
updaterec env _ data bdata =
    ( ( data, bdata ), [], env )


view : GlobalComponentView userdata scenemsg Data
view env data _ =
    renderTextWithColor env.globalData.internalData data.size ("FPS: " ++ String.fromInt (floor data.fps)) "Arial" Color.gray ( 0, 0 )


gcCon : InitOption -> ConcreteGlobalComponent Data userdata scenemsg
gcCon opt =
    { init = init opt
    , update = update
    , updaterec = updaterec
    , view = view
    , id = "fps"
    }


{-| Generate a global component.
-}
genGC : InitOption -> Maybe GCTarget -> GlobalComponentStorage userdata scenemsg
genGC opt =
    genGlobalComponent (gcCon opt) E.null
