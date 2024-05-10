module Messenger.Render.Sprite exposing
    ( renderSprite, renderSpriteWithRev
    , renderSpriteCropped
    )

{-|


# Sprite Rendering

@docs renderSprite, renderSpriteWithRev
@docs renderSpriteCropped

-}

import Canvas exposing (Renderable, empty, texture)
import Canvas.Settings exposing (Setting)
import Canvas.Settings.Advanced exposing (scale, transform, translate)
import Canvas.Texture exposing (Texture, dimensions, sprite)
import Messenger.Base exposing (GlobalData)
import Messenger.Coordinate.Coordinates exposing (lengthToReal, posToReal)
import Messenger.Resources.Base exposing (igetSprite)


{-| Render a single sprite.
-}
renderSprite : GlobalData a -> List Setting -> ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
renderSprite gd ls p size name =
    let
        dst =
            gd.internalData.sprites
    in
    case igetSprite name dst of
        Just t ->
            renderSprite_ gd ls p size t

        Nothing ->
            empty


{-| Render a single sprite with crop.
-}
renderSpriteCropped : GlobalData a -> List Setting -> ( Float, Float ) -> ( Float, Float ) -> { x : Float, y : Float, width : Float, height : Float } -> String -> Renderable
renderSpriteCropped gd ls p size spconf name =
    let
        dst =
            gd.internalData.sprites
    in
    case igetSprite name dst of
        Just t ->
            renderSprite_ gd ls p size (sprite spconf t)

        Nothing ->
            empty


renderSprite_ : GlobalData a -> List Setting -> ( Float, Float ) -> ( Float, Float ) -> Texture -> Renderable
renderSprite_ gd ls p ( w, h ) t =
    let
        text_dim =
            dimensions t

        rw =
            lengthToReal gd w

        rh =
            lengthToReal gd h

        text_width =
            text_dim.width

        text_height =
            text_dim.height

        width_s =
            rw / text_width

        height_s =
            rh / text_height

        ( newx, newy ) =
            posToReal gd p
    in
    if w > 0 && h > 0 then
        texture
            (transform
                [ translate newx newy
                , scale width_s height_s
                ]
                :: ls
            )
            ( 0, 0 )
            t

    else if w > 0 && h <= 0 then
        texture
            (transform
                [ translate newx newy
                , scale width_s width_s
                ]
                :: ls
            )
            ( 0, 0 )
            t

    else if w <= 0 && h > 0 then
        texture
            (transform
                [ translate newx newy
                , scale height_s height_s
                ]
                :: ls
            )
            ( 0, 0 )
            t

    else
        -- All <= 0
        texture
            ls
            ( newx, newy )
            t


{-| renderSpriteWithRev

Render a single sprite with (possible) reverse.

The first argument is the reverse flag. Sent true to make the sprite being rendered in reverse.

-}
renderSpriteWithRev : Bool -> GlobalData a -> List Setting -> ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
renderSpriteWithRev rev gd ls p size name =
    if not rev then
        renderSprite gd ls p size name

    else
        case igetSprite name gd.internalData.sprites of
            Just t ->
                renderSpriteWithRev_ gd ls p size t

            Nothing ->
                empty


renderSpriteWithRev_ : GlobalData a -> List Setting -> ( Float, Float ) -> ( Float, Float ) -> Texture -> Renderable
renderSpriteWithRev_ gd ls p ( w, h ) t =
    let
        text_dim =
            dimensions t

        rw =
            lengthToReal gd w

        rh =
            lengthToReal gd h

        text_width =
            text_dim.width

        text_height =
            text_dim.height

        width_s =
            rw / text_width

        height_s =
            rh / text_height

        ( newx, newy ) =
            posToReal gd p
    in
    if w > 0 && h > 0 then
        texture
            (transform
                [ translate newx newy
                , scale -width_s height_s
                , translate -text_width 0
                ]
                :: ls
            )
            ( 0, 0 )
            t

    else if w > 0 && h <= 0 then
        texture
            (transform
                [ translate newx newy
                , scale -width_s width_s
                , translate -text_width 0
                ]
                :: ls
            )
            ( 0, 0 )
            t

    else if w <= 0 && h > 0 then
        texture
            (transform
                [ translate newx newy
                , scale -height_s height_s
                , translate -text_width 0
                ]
                :: ls
            )
            ( 0, 0 )
            t

    else
        -- All <= 0
        texture
            ls
            ( newx, newy )
            t
