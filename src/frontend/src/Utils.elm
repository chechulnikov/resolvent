module Utils exposing (..)


flip : (a -> b -> c) -> (b -> a -> c)
flip function b a =
    function a b