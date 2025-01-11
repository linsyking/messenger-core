module Scenes.Audio.Model exposing (scene)

{-| Scene configuration module

@docs scene

-}

import Audio exposing (LoopConfig, scaleVolume, scaleVolumeAt)
import Color
import Duration
import Lib.Base exposing (SceneMsg)
import Lib.UserData exposing (UserData)
import Messenger.Audio.Audio exposing (audioDuration)
import Messenger.Audio.Base exposing (AudioCommonOption, AudioOption(..), AudioTarget(..))
import Messenger.Base exposing (UserEvent(..))
import Messenger.GeneralModel exposing (Msg(..), MsgBase(..))
import Messenger.Scene.RawScene exposing (RawSceneInit, RawSceneUpdate, RawSceneView, genRawScene)
import Messenger.Scene.Scene exposing (MConcreteScene, SceneOutputMsg(..), SceneStorage)
import REGL
import REGL.BuiltinPrograms as P
import Time


type alias Data =
    {}


init : RawSceneInit Data UserData SceneMsg
init env msg =
    {}


update : RawSceneUpdate Data UserData SceneMsg
update env msg data =
    let
        length =
            audioDuration env.globalData.internalData "test"
    in
    case msg of
        KeyDown 49 ->
            ( data
            , [ SOMStopAudio <| AudioName 0 "test", SOMPlayAudio 0 "test" <| AOnce Nothing ]
            , env
            )

        KeyDown 50 ->
            ( data
            , [ SOMStopAudio <| AudioName 0 "test", SOMPlayAudio 0 "test" <| AOnce (Just <| AudioCommonOption 0.5 (Duration.seconds 1)) ]
            , env
            )

        KeyDown 51 ->
            ( data
            , [ SOMStopAudio <| AudioName 0 "test", SOMPlayAudio 0 "test" <| ALoop Nothing (Just <| LoopConfig (Duration.seconds 1) (Duration.seconds 2)) ]
            , env
            )

        KeyDown 52 ->
            ( data
            , [ SOMStopAudio <| AudioName 0 "test", SOMPlayAudio 0 "test" <| ALoop Nothing (Just <| LoopConfig (Duration.seconds 1) (Maybe.withDefault (Duration.seconds 0) length)) ]
            , env
            )

        KeyDown 53 ->
            ( data
            , [ SOMStopAudio <| AudioName 0 "test", SOMPlayAudio 0 "test" <| ALoop Nothing Nothing, SOMTransformAudio (AudioName 0 "test") (scaleVolume 0.5) ]
            , env
            )

        KeyDown 54 ->
            let
                ts =
                    env.globalData.currentTimeStamp

                nts =
                    Time.millisToPosix <| floor ts + 2000

                lts =
                    Time.millisToPosix <| floor ts + 6000
            in
            ( data
            , [ SOMStopAudio <| AudioName 0 "test", SOMPlayAudio 0 "test" <| ALoop Nothing Nothing, SOMTransformAudio (AudioName 0 "test") (scaleVolumeAt [ ( Time.millisToPosix <| floor env.globalData.currentTimeStamp, 0 ), ( nts, 2 ), ( lts, 0 ) ]) ]
            , env
            )

        _ ->
            ( data, [], env )


view : RawSceneView UserData Data
view env data =
    REGL.group []
        [ P.clear (Color.rgb 1.0 0.0 0.0)
        , P.textbox ( 0, 30 ) 50 "Mode:\n1: Play once\n2. Play once with 0.5 speed and some offset\n3. Play loop with 1 to 2 seconds\n4. Play loop with 1 to the end\n5. Scale audio to 0.5\n6. Audio fading out and in" "arial" Color.black
        ]


scenecon : MConcreteScene Data UserData SceneMsg
scenecon =
    { init = init
    , update = update
    , view = view
    }


{-| Scene generator
-}
scene : SceneStorage UserData SceneMsg
scene =
    genRawScene scenecon
