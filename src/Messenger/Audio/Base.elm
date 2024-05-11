module Messenger.Audio.Base exposing (AudioOption(..))

{-|


# Audio Base

@docs AudioOption

-}


{-| AudioOption

You can play one audio once or loop it.

-}
type AudioOption
    = ALoop
    | AOnce
