module Messenger.Audio.Audio exposing
    ( loadAudio
    , stopAudio
    , getAudio
    , AudioRepo
    )

{-|


# Audio

This module is used to manage audios.

**Note. This module may only be used within Messenger core**

@docs loadAudio
@docs stopAudio
@docs getAudio
@docs AudioRepo

-}

import Audio exposing (AudioData)
import Dict exposing (Dict)
import Duration
import Messenger.Audio.Base exposing (AudioOption(..))
import Time


{-| Load audio by name.
-}
loadAudio : AudioRepo -> String -> Audio.Source -> AudioOption -> Time.Posix -> AudioRepo
loadAudio repo name source opt t =
    let
        filterrepo =
            List.filter (\( n, _, _ ) -> n /= name) repo
    in
    filterrepo ++ [ ( name, source, ( opt, t ) ) ]


{-| Stop an audio by id.
-}
stopAudio : AudioRepo -> String -> AudioRepo
stopAudio repo s =
    List.filter (\( name, _, _ ) -> name /= s) repo


{-| Change audio with config to real audio.
-}
getAudio : AudioData -> AudioRepo -> List Audio.Audio
getAudio ad repo =
    List.map
        (\( _, sound, ( opt, s ) ) ->
            case opt of
                ALoop ->
                    let
                        default =
                            Audio.audioDefaultConfig
                    in
                    Audio.audioWithConfig { default | loop = Just (Audio.LoopConfig (Duration.seconds 0) (Audio.length ad sound)) } sound s

                AOnce ->
                    Audio.audio sound s
        )
        repo


{-| Audio repository that stores all the audios.
-}
type alias AudioRepo =
    { audio : Dict String Audio.Source
    , playing : List ( String, Audio.Audio )
    }
