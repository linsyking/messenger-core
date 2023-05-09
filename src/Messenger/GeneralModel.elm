module Messenger.GeneralModel exposing (GeneralModel)

{-|


# General Model

General model is designed to be an abstract interface of scenes, layers, components, game components, etc..

  - a: data type
  - b: environment type
  - c: init type
  - d: message type
  - e: target type
  - f: render type

@docs GeneralModel

-}


{-| General Model.

This has a name field.

-}
type alias GeneralModel a b c d e f =
    { name : String
    , data : a
    , init : b -> c -> a
    , update : b -> d -> a -> ( a, List ( e, d ), b )
    , view : b -> a -> f
    }
