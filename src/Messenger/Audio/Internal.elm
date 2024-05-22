module Messenger.Audio.Internal exposing
    ( playAudio
    , stopAudio
    , getAudio
    , AudioRepo, emptyRepo
    )

{-|


# Audio

This module is used to manage audios.

**Note. This module may only be used within Messenger core**

@docs playAudio
@docs stopAudio
@docs getAudio
@docs AudioRepo, emptyRepo

-}

import Audio
import Dict exposing (Dict)
import Duration
import Messenger.Audio.Base exposing (AudioOption(..))
import Time


{-| Play audio by name.
-}
playAudio : AudioRepo -> Int -> String -> AudioOption -> Time.Posix -> AudioRepo
playAudio rawrepo channel name opt t =
    let
        repo =
            removeFinishedAudio rawrepo t

        _ =
            Debug.log "audios" <| List.length repo.playing

        playing =
            repo.playing

        audio =
            Dict.get name repo.audio
    in
    case audio of
        Just ( source, duration ) ->
            case opt of
                ALoop ->
                    let
                        defaultConfig =
                            Audio.audioDefaultConfig

                        audioWC =
                            Audio.audioWithConfig { defaultConfig | loop = Just (Audio.LoopConfig (Duration.seconds 0) duration) } source t

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

                AOnce ->
                    let
                        newPA =
                            { channel = channel
                            , name = name
                            , audio = Audio.audio source t
                            , opt = opt
                            , duration = duration
                            , startTime = t
                            }
                    in
                    { repo | playing = newPA :: playing }

        Nothing ->
            repo


removeFinishedAudio : AudioRepo -> Time.Posix -> AudioRepo
removeFinishedAudio repo t =
    let
        playing =
            repo.playing

        newPlaying =
            List.filter
                (\pa ->
                    pa.opt == ALoop || Time.posixToMillis t - Time.posixToMillis pa.startTime < ceiling (Duration.inMilliseconds pa.duration)
                )
                playing
    in
    { repo | playing = newPlaying }


{-| Stop an audio by id.
-}
stopAudio : AudioRepo -> Time.Posix -> Int -> AudioRepo
stopAudio rawrepo t s =
    let
        repo =
            removeFinishedAudio rawrepo t

        playing =
            repo.playing

        newPlaying =
            List.filter (\pa -> pa.channel /= s) playing
    in
    { repo | playing = newPlaying }


{-| Change audio with config to real audio.
-}
getAudio : AudioRepo -> List Audio.Audio
getAudio repo =
    List.map
        (\pa -> pa.audio)
        repo.playing


type alias PlayingAudio =
    { channel : Int
    , name : String
    , audio : Audio.Audio
    , opt : AudioOption
    , duration : Duration.Duration
    , startTime : Time.Posix
    }


{-| Audio repository that stores all the audios.
-}
type alias AudioRepo =
    { audio : Dict String ( Audio.Source, Duration.Duration )
    , playing : List PlayingAudio
    }


emptyRepo : AudioRepo
emptyRepo =
    { audio = Dict.empty
    , playing = []
    }
