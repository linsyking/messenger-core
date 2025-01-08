module Messenger.Audio.Base exposing (AudioOption(..), AudioTarget(..), AudioCommonOption)

{-|


# Audio Base

@docs AudioOption, AudioTarget, AudioCommonOption

-}

import Audio exposing (LoopConfig)
import Duration exposing (Duration)


{-| Audio common options.

  - rate refers to the playing speed of the audio. For example, setting rate to 2 will make Messenger play the music at 2x Speed.
  - start is the starting point where messenger starts to play music. It is a Duration value, and can be transferred from float by Duration.seconds

Example: AudioCommonOption 1.5 <| Duration.seconds 30 -- The audio will be played at 1.5x speed and start from 30.0s.

-}
type alias AudioCommonOption =
    { rate : Float
    , start : Duration
    }


{-| The type used to specify the target of the AudioOptions, for example in SOMStopAudio.

  - AllAudio sets the target to be all of the audios.
  - AudioChannel sets the target to be a specific channel, which is determined by the Int following it.
  - AudioName sets the target to be a specific audio piece by stating the Channel(Int) and the name(String) of the audio piece.

-}
type AudioTarget
    = AllAudio
    | AudioChannel Int
    | AudioName Int String


{-| You can play one audio once or loop it.

  - ALoop means the audio will be played repeatedly until a SOMStopAudio is called.
  - AOnce means the audio will be played only once.

-}
type AudioOption
    = ALoop (Maybe AudioCommonOption) (Maybe LoopConfig)
    | AOnce (Maybe AudioCommonOption)
