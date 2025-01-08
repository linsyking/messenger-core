module Messenger.Audio.Internal exposing
    ( playAudio
    , stopAudio
    , getAudio
    , updateAudio
    , AudioRepo, emptyRepo
    )

{-|


# Audio

This module is used to manage audios.

**Note. This module may only be used within Messenger core**

@docs playAudio
@docs stopAudio
@docs getAudio
@docs updateAudio
@docs AudioRepo, emptyRepo

-}

import Audio
import Dict exposing (Dict)
import Duration
import Messenger.Audio.Base exposing (AudioOption(..), AudioTarget(..))
import Time


{-| Play audio by name.
-}
playAudio : AudioRepo -> Int -> String -> AudioOption -> Float -> AudioRepo
playAudio rawrepo channel name opt t =
    let
        repo =
            removeFinishedAudio rawrepo t

        playing =
            repo.playing

        audio =
            Dict.get name repo.audio
    in
    case audio of
        Just ( source, duration ) ->
            case opt of
                ALoop comopt loopopt ->
                    let
                        rawDefault =
                            Audio.audioDefaultConfig

                        config1 =
                            Maybe.withDefault Audio.audioDefaultConfig (Maybe.map (\topt -> { rawDefault | startAt = topt.start, playbackRate = topt.rate }) comopt)

                        loopConfig =
                            Maybe.withDefault (Audio.LoopConfig (Duration.seconds 0) duration) loopopt

                        audioWC =
                            Audio.audioWithConfig { config1 | loop = Just loopConfig } source (Time.millisToPosix (floor t))

                        newPA =
                            { channel = channel
                            , name = name
                            , audio = audioWC
                            , opt = opt
                            , duration = duration
                            , startTime = t
                            }
                    in
                    { repo | playing = newPA :: playing }

                AOnce comopt ->
                    let
                        rawDefault =
                            Audio.audioDefaultConfig

                        config1 =
                            Maybe.withDefault Audio.audioDefaultConfig (Maybe.map (\topt -> { rawDefault | startAt = topt.start, playbackRate = topt.rate }) comopt)

                        audioWC =
                            Audio.audioWithConfig config1 source (Time.millisToPosix <| floor t)

                        newPA =
                            { channel = channel
                            , name = name
                            , audio = audioWC
                            , opt = opt
                            , duration = duration
                            , startTime = t
                            }
                    in
                    { repo | playing = newPA :: playing }

        Nothing ->
            repo


{-| Check if audio option is loop.
-}
audioLoop : AudioOption -> Bool
audioLoop ao =
    case ao of
        ALoop _ _ ->
            True

        AOnce _ ->
            False


{-| Remove finished audio.
-}
removeFinishedAudio : AudioRepo -> Float -> AudioRepo
removeFinishedAudio repo t =
    let
        playing =
            repo.playing

        newPlaying =
            List.filter
                (\pa ->
                    audioLoop pa.opt || t - pa.startTime < toFloat (ceiling (Duration.inMilliseconds pa.duration))
                )
                playing
    in
    { repo | playing = newPlaying }


{-| Stop an audio by id.
-}
stopAudio : AudioRepo -> Float -> AudioTarget -> AudioRepo
stopAudio rawrepo t target =
    let
        repo =
            removeFinishedAudio rawrepo t

        playing =
            repo.playing

        newPlaying =
            List.filter
                (\pa ->
                    not <|
                        case target of
                            AllAudio ->
                                True

                            AudioChannel c ->
                                pa.channel == c

                            AudioName id name ->
                                pa.channel == id && pa.name == name
                )
                playing
    in
    { repo | playing = newPlaying }


{-| Change audio with config to real audio.
-}
getAudio : AudioRepo -> List Audio.Audio
getAudio repo =
    List.map
        (\pa -> pa.audio)
        repo.playing


{-| Information about the playing audio.
-}
type alias PlayingAudio =
    { channel : Int
    , name : String
    , audio : Audio.Audio
    , opt : AudioOption
    , duration : Duration.Duration
    , startTime : Float
    }


{-| Audio repository that stores all the audios.
-}
type alias AudioRepo =
    { audio : Dict String ( Audio.Source, Duration.Duration )
    , playing : List PlayingAudio
    }


{-| An empty audio repository.
-}
emptyRepo : AudioRepo
emptyRepo =
    { audio = Dict.empty
    , playing = []
    }


{-| Update audio based on transformation.
-}
updateAudio : AudioRepo -> AudioTarget -> (Audio.Audio -> Audio.Audio) -> AudioRepo
updateAudio repo target f =
    let
        playing =
            repo.playing

        newPlaying =
            List.map
                (\pa ->
                    case target of
                        AllAudio ->
                            { pa | audio = f pa.audio }

                        AudioChannel c ->
                            if pa.channel == c then
                                { pa | audio = f pa.audio }

                            else
                                pa

                        AudioName id name ->
                            if pa.channel == id && pa.name == name then
                                { pa | audio = f pa.audio }

                            else
                                pa
                )
                playing
    in
    { repo | playing = newPlaying }
