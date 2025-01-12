module Messenger.Resources.Base exposing
    ( saveSprite
    , igetSprite
    , ResourceDef(..), ResourceDefs, resourceNum
    )

{-|


# Resource

There are many assets (images) in our game, so it's important to manage them.

In elm-canvas, we have to preload all the images before the game starts.

The game will only start when all resources are loaded.

If some asset is not found, the game will panic and throw an error (alert).

After the resources are loaded, we can get those data from globaldata.sprites.

@docs saveSprite
@docs igetSprite
@docs ResourceDef, ResourceDefs, resourceNum

-}

import Dict exposing (Dict)
import REGL exposing (Texture)
import REGL.Program exposing (REGLProgram)


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


type ResourceDef
    = TextureRes String (Maybe REGL.TextureOptions)
    | AudioRes String
    | FontRes String String
    | ProgramRes REGLProgram


type alias ResourceDefs =
    List ( String, ResourceDef )


{-| The number of sprites in the game.
-}
resourceNum : ResourceDefs -> Int
resourceNum =
    List.length
