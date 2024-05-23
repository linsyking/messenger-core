module Messenger.Audio.Audio exposing (newAudioChannel)

{-|


# Audio Module

You can play audio by emitting `SOMPlayAudio` message with the audio ID, a channel, and an option.

The channel is an integer that is used to identify the audio channel.

You **can** play different audio on the same channel at the same time. The previous audio will not be stopped.

However, when you stop the channel, all audio on the channel will be stopped.

If an audio is finished playing, it will be removed from the playing channel.

@docs newAudioChannel

-}

import List exposing (maximum)
import Messenger.Base exposing (GlobalData)


{-| Generate a new unique audio channel number.
-}
newAudioChannel : GlobalData userdata -> Int
newAudioChannel globalData =
    let
        playingChannels =
            List.map (\pl -> pl.channel) globalData.internalData.audioRepo.playing
    in
    case maximum playingChannels of
        Just maxChannel ->
            maxChannel + 1

        Nothing ->
            0
