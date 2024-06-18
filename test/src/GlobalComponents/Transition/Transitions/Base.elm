module GlobalComponents.Transition.Transitions.Base exposing
    ( Transition, SingleTrans, TransStorage
    , genTransition, nullTransition
    , colorDec, colorEnc
    )

{-|


# Transition Base

@docs Transition, SingleTrans, TransStorage
@docs genTransition, nullTransition
@docs colorDec, colorEnc

-}

import Canvas exposing (Renderable)
import Color exposing (Color)
import Duration exposing (Duration)
import Json.Decode as D
import Json.Encode as E
import Messenger.Base exposing (InternalData)
import Messenger.Scene.Scene exposing (GCMsg)


{-| Single Transition
-}
type alias SingleTrans =
    InternalData -> Renderable -> Float -> Renderable


{-| Trnasition Storage
-}
type alias TransStorage =
    GCMsg -> SingleTrans


{-| Null Transition
-}
nullTransition : SingleTrans
nullTransition _ r _ =
    r


{-| Transition has two stages:

1.  From the old scene to the transition scene
2.  From the transition scene to the new scene

-}
type alias Transition =
    { currentTransition : Int
    , outT : Int
    , inT : Int
    , outTrans : SingleTrans
    , inTrans : SingleTrans
    }


type alias TransitionOption =
    { mix : Bool
    }


{-| Generate new transition
-}
genTransition : ( ( String, GCMsg ), Duration ) -> ( ( String, GCMsg ), Duration ) -> TransitionOption -> GCMsg
genTransition ( ( outName, outTrans ), outT ) ( ( inName, inTrans ), inT ) opt =
    let
        outTR =
            ceiling <| Duration.inMilliseconds outT

        inTR =
            ceiling <| Duration.inMilliseconds inT
    in
    E.object
        [ ( "outName", E.string outName )
        , ( "outTrans", outTrans )
        , ( "outT", E.int outTR )
        , ( "inName", E.string inName )
        , ( "inTrans", inTrans )
        , ( "inT", E.int inTR )
        , ( "mix", E.bool opt.mix )
        ]


{-| Decoder for Color
-}
colorDec : GCMsg -> Color
colorDec msg =
    let
        r =
            D.field "r" D.float

        g =
            D.field "g" D.float

        b =
            D.field "b" D.float

        a =
            D.field "a" D.float

        dec =
            D.map4 Color.rgba r g b a
    in
    Result.withDefault Color.black <| D.decodeValue dec msg


{-| Encoder for Color
-}
colorEnc : Color -> GCMsg
colorEnc col =
    let
        rgba =
            Color.toRgba col
    in
    E.object
        [ ( "r", E.float rgba.red )
        , ( "g", E.float rgba.green )
        , ( "b", E.float rgba.blue )
        , ( "a", E.float rgba.alpha )
        ]
