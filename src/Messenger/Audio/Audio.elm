module Messenger.Audio.Audio exposing
    ( loadAudio
    , stopAudio
    , getAudio
    )

{-|


# Audio

This module is used to manage audios.

@docs loadAudio
@docs stopAudio
@docs getAudio

-}

import Audio exposing (AudioData)
import Duration
import Messenger.Audio.Base exposing (AudioOption(..), AudioRepo)
import Time


{-| loadAudio

Load audio by name

-}
loadAudio : AudioRepo -> String -> Audio.Source -> AudioOption -> Time.Posix -> AudioRepo
loadAudio repo name source opt t =
    let
        filterrepo =
            List.filter (\( n, _, _ ) -> n /= name) repo
    in
    filterrepo ++ [ ( name, source, ( opt, t ) ) ]


{-| stopAudio

Stop an audio by id

-}
stopAudio : AudioRepo -> String -> AudioRepo
stopAudio repo s =
    List.filter (\( name, _, _ ) -> name /= s) repo


{-| getAudio
Change audio with config to real audio
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
