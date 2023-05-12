module Messenger.GeneralModel exposing
    ( GeneralModel
    , viewModelList, viewModelArray
    )

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
@docs viewModelList, viewModelArray

-}

import Array exposing (Array)


{-| General Model.

This has a name field.

-}
type alias GeneralModel a b d e f =
    { name : String
    , data : a
    , update : b -> d -> a -> ( a, List ( e, d ), b )
    , view : b -> a -> f
    }


{-| View model list.
-}
viewModelList : b -> List (GeneralModel a b d e f) -> List f
viewModelList env models =
    List.map (\model -> model.view env model.data) models


{-| View model array.
-}
viewModelArray : b -> Array (GeneralModel a b d e f) -> List f
viewModelArray env models =
    Array.toList models
        |> List.map (\model -> model.view env model.data)
