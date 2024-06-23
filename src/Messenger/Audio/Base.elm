module Messenger.Audio.Base exposing (AudioOption(..), AudioTarget(..))

{-|


# Audio Base

@docs AudioOption, AudioTarget

-}

import Audio exposing (LoopConfig)
import Duration exposing (Duration)


{-| Audio common options.
-}
type alias AudioCommonOption =
    { rate : Float
    , start : Duration
    }


{-| Audo target.
-}
type AudioTarget
    = AllAudio
    | AudioChannel Int
    | AudioName Int String


{-| You can play one audio once or loop it.
-}
type AudioOption
    = ALoop (Maybe AudioCommonOption) (Maybe LoopConfig)
    | AOnce (Maybe AudioCommonOption)
