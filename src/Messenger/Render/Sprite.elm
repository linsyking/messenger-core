module Messenger.Render.Sprite exposing
    ( renderSprite, renderSpriteWithRev
    , renderSpriteCropped
    , textureDim
    )

{-|


# Sprite Rendering

@docs renderSprite, renderSpriteWithRev
@docs renderSpriteCropped
@docs textureDim

-}

import Canvas exposing (Renderable, empty, texture)
import Canvas.Settings exposing (Setting)
import Canvas.Settings.Advanced exposing (scale, transform, translate)
import Canvas.Texture exposing (Texture, dimensions, sprite)
import Messenger.Base exposing (InternalData)
import Messenger.Coordinate.Coordinates exposing (lengthToReal, posToReal)
import Messenger.Resources.Base exposing (igetSprite)


{-| Render a single sprite.
Usage: renderSprite gd settings position size name

  - gd is the internal data type. You should always put globaldata.internaldata as your first parameter.
  - settings is a list of Setting. For the usage of settings, see the elm package on Canvas.Settings and Canvas.Settings.Extra.
  - The position is the start point of the sprite from the left-top corner in virtual coordinates.
      - Note: The renderSprite automatically does posToReal and lengthToReal for you. No need to call them again.
  - size is the size of the sprite.
      - Note: You can leave one or two of the size field to be empty. For the usage of it, please read the manual. In short, leaving the length/width to
        zero will let Messenger deduce the argument based on the existing argument and the initial length-width ratio of the sprite. Leaving both to be zero
        yields a picture that is in initial size.
  - Name is the name of the sprite image configed in resources.elm.

-}
renderSprite : InternalData -> List Setting -> ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
renderSprite gd settings position size name =
    let
        dst =
            gd.sprites
    in
    case igetSprite name dst of
        Just t ->
            renderSprite_ gd settings position size t

        Nothing ->
            empty


{-| Render a single sprite with crop.
-}
renderSpriteCropped : InternalData -> List Setting -> ( Float, Float ) -> ( Float, Float ) -> { x : Float, y : Float, width : Float, height : Float } -> String -> Renderable
renderSpriteCropped gd settings position size spconf name =
    let
        dst =
            gd.sprites
    in
    case igetSprite name dst of
        Just t ->
            renderSprite_ gd settings position size (sprite spconf t)

        Nothing ->
            empty


renderSprite_ : InternalData -> List Setting -> ( Float, Float ) -> ( Float, Float ) -> Texture -> Renderable
renderSprite_ gd settings position ( w, h ) t =
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
            posToReal gd position
    in
    if w > 0 && h > 0 then
        texture
            (transform
                [ translate newx newy
                , scale width_s height_s
                ]
                :: settings
            )
            ( 0, 0 )
            t

    else if w > 0 && h <= 0 then
        texture
            (transform
                [ translate newx newy
                , scale width_s width_s
                ]
                :: settings
            )
            ( 0, 0 )
            t

    else if w <= 0 && h > 0 then
        texture
            (transform
                [ translate newx newy
                , scale height_s height_s
                ]
                :: settings
            )
            ( 0, 0 )
            t

    else
        -- All <= 0
        texture
            settings
            ( newx, newy )
            t


{-| Render a single sprite with (possible) reverse.

The first argument is the reverse flag. Sent true to make the sprite being rendered in reverse.

-}
renderSpriteWithRev : Bool -> InternalData -> List Setting -> ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
renderSpriteWithRev rev gd settings position size name =
    if not rev then
        renderSprite gd settings position size name

    else
        case igetSprite name gd.sprites of
            Just t ->
                renderSpriteWithRev_ gd settings position size t

            Nothing ->
                empty


renderSpriteWithRev_ : InternalData -> List Setting -> ( Float, Float ) -> ( Float, Float ) -> Texture -> Renderable
renderSpriteWithRev_ gd settings position ( w, h ) t =
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
            posToReal gd position
    in
    if w > 0 && h > 0 then
        texture
            (transform
                [ translate newx newy
                , scale -width_s height_s
                , translate -text_width 0
                ]
                :: settings
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
                :: settings
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
                :: settings
            )
            ( 0, 0 )
            t

    else
        -- All <= 0
        texture
            settings
            ( newx, newy )
            t


{-| Get the width and height of a sprite.
-}
textureDim : InternalData -> String -> Maybe ( Float, Float )
textureDim gd name =
    Maybe.map
        (\t ->
            let
                dim =
                    dimensions t
            in
            ( dim.width, dim.height )
        )
        (igetSprite name gd.sprites)
