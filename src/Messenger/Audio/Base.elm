module Messenger.Audio.Base exposing
    ( AudioOption(..)
    , AudioRepo
    )

{-|


# Audio Base

@docs AudioOption
@docs AudioRepo

-}

import Audio
import Time


{-| AudioOption

You can play one audio once or loop it.

-}
type AudioOption
    = ALoop
    | AOnce


{-| AudioRepo

Audio repository that stores all the audios.

-}
type alias AudioRepo =
    List ( String, Audio.Source, ( AudioOption, Time.Posix ) )
