module Messenger.Resources.Base exposing
    ( saveSprite
    , getTexture
    , igetSprite
    )

{-|


# Resource

There are many assets (images) in our game, so it's important to manage them.

In elm-canvas, we have to preload all the images before the game starts.

The game will only start when all resources are loaded.

If some asset is not found, the game will panic and throw an error (alert).

After the resources are loaded, we can get those data from globaldata.sprites.

@docs saveSprite
@docs getTexture
@docs igetSprite

-}

import Canvas.Texture as Texture exposing (Texture)
import Dict exposing (Dict)
import Messenger.Base exposing (WorldEvent(..))
import Messenger.UserConfig exposing (UserConfig)


{-| Return all the textures.
-}
getTexture : UserConfig userdata scenemsg -> List (Texture.Source WorldEvent)
getTexture config =
    List.map (\( x, y ) -> Texture.loadFromImageUrl y (TextureLoaded x)) config.allTexture


{-| Save the sprite.
-}
saveSprite : Dict String Texture -> String -> Texture -> Dict String Texture
saveSprite dst name text =
    Dict.insert name text dst


{-| Get the texture by name.
-}
igetSprite : String -> Dict String Texture -> Maybe Texture
igetSprite name dst =
    Dict.get name dst
