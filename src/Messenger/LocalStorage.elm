port module Messenger.LocalStorage exposing (..)

{-| This is the port for sending the data to localstorage.
-}


port sendInfo : String -> Cmd msg
