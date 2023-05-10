module Messenger.GeneralModel exposing
    ( GeneralModel
    , viewModelList, viewModelArray
    )

{-|


# General Model

General model is designed to be an abstract interface of scenes, layers, components, game components, etc..

  - a: data type
  - b: environment type
  - c: message type
  - d: target type
  - e: render type

@docs GeneralModel
@docs viewModelList, viewModelArray

-}

import Array exposing (Array)


{-| General Model.

This has a name field.

-}
type alias GeneralModel a b c d e =
    { name : String
    , data : a
    , update : b -> c -> a -> ( a, List ( d, c ), b )
    , view : b -> a -> e
    }


{-| View model list.
-}
viewModelList : b -> List (GeneralModel a b c d e) -> List e
viewModelList env models =
    List.map (\model -> model.view env model.data) models


{-| View model array.
-}
viewModelArray : b -> Array (GeneralModel a b c d e) -> List e
viewModelArray env models =
    Array.toList models
        |> List.map (\model -> model.view env model.data)
