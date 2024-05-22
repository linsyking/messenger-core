module Messenger.Audio.Audio exposing (newAudioChannel)

{-|


# Audio Module

@docs newAudioChannel

-}

import List exposing (maximum)
import Messenger.Base exposing (GlobalData)


{-| Generate a new audio channel number.
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
