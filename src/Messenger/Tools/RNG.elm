module Messenger.Tools.RNG exposing (genRandomInt, genRandomListInt)

{-|


# RNG module

Sample RNG module.

The seed is often given by the current tick.

@docs genRandomInt, genRandomListInt

-}

import Random


{-| Generate a random int in the range [a, b] using the given seed.
-}
genRandomInt : Int -> ( Int, Int ) -> Int
genRandomInt t ( a, b ) =
    Tuple.first (Random.step (genInt ( a, b )) (seed t))


genInt : ( Int, Int ) -> Random.Generator Int
genInt ( a, b ) =
    Random.int a b


{-| Generate a random list of ints in the range [a, b] using the given seed.
-}
genRandomListInt : Int -> Int -> ( Int, Int ) -> List Int
genRandomListInt t n ( a, b ) =
    Tuple.first (Random.step (genListInt ( a, b ) n) (seed t))


genListInt : ( Int, Int ) -> Int -> Random.Generator (List Int)
genListInt ( a, b ) n =
    Random.list n (genInt ( a, b ))


seed : Int -> Random.Seed
seed t =
    Random.initialSeed t
