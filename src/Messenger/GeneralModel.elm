module Messenger.GeneralModel exposing (NamedGeneralModel, NamelessGeneralModel)

{-|


# General Model

General model is designed to be an abstract interface of scenes, layers, components, game components, etc..

  - a: data type
  - b: environment type
  - c: message type
  - d: render type

@docs NamedGeneralModel, NamelessGeneralModel

-}


{-| Named General Model.

This has a name field.

-}
type alias NamedGeneralModel a b c d =
    { name : String
    , data : a
    , init : b -> c -> a
    , update : b -> c -> a -> ( a, b )
    , view : b -> a -> d
    }


{-| General Model without name field.
-}
type alias NamelessGeneralModel a b c d =
    { data : a
    , init : b -> c -> a
    , update : b -> c -> a -> ( a, b )
    , view : b -> a -> d
    }
