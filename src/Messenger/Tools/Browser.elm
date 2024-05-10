port module Messenger.Tools.Browser exposing (alert, prompt, promptReceiver)

{-| This module contains functions for working with the browser.

@docs alert, prompt, promptReceiver

-}


{-| This is the alert function in JS.
-}
port alert : String -> Cmd msg


{-| This is the prompt function in JS.
-}
port prompt : { name : String, title : String } -> Cmd msg


{-| Prompt Receiver
-}
port promptReceiver : ({ name : String, result : String } -> msg) -> Sub msg
