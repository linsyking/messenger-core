module Messenger.Audio.Audio exposing
    ( newAudioChannel
    , audioDuration
    )

{-|


# Audio Module

You can play audio by emitting `SOMPlayAudio` message with the audio ID, a channel, and an option.

The channel is an integer that is used to identify the audio channel.

You **can** play different audio on the same channel at the same time. The previous audio will not be stopped.

However, when you stop the channel, all audio on the channel will be stopped.

If an audio is finished playing, it will be removed from the playing channel.

@docs newAudioChannel
@docs audioDuration

-}

import Dict
import Duration exposing (Duration)
import List exposing (maximum)
import Messenger.Base exposing (InternalData)


{-| Generate a new unique audio channel number.
-}
newAudioChannel : InternalData -> Int
newAudioChannel idata =
    let
        playingChannels =
            List.map (\pl -> pl.channel) idata.audioRepo.playing
    in
    case maximum playingChannels of
        Just maxChannel ->
            maxChannel + 1

        Nothing ->
            0


{-| Get the duration of an audio by its ID.
-}
audioDuration : InternalData -> String -> Maybe Duration
audioDuration internalData audioId =
    case Dict.get audioId internalData.audioRepo.audio of
        Just ( _, duration ) ->
            Just duration

        _ ->
            Nothing
