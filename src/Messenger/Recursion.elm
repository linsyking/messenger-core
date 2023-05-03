module Messenger.Recursion exposing (RecBody)

{-|


# Recursion

This module provides the signature for the updater

@docs RecBody

-}

{- The updater
   a: message sender (object)
   b: message
   c: environment messages
   d: target
-}


type alias Updater a b c d =
    a -> c -> b -> ( a, List ( d, b ), c )



{-
   Return true if the target is the sender (second argument)
-}


type alias Matcher a d =
    a -> d -> Bool



{-
   Return true if the target is the parent
-}


type alias Super d =
    d -> Bool


{-| RecBody type.

Pass this as an argument to the updater

-}
type alias RecBody a b c d =
    { update : Updater a b c d
    , match : Matcher a d
    , super : Super d
    }
