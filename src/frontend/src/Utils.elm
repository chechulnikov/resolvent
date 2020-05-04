module Utils exposing (..)


flip : (a -> b -> c) -> (b -> a -> c)
flip function b a =
    function a b


ifTrueThenUpdate : (a -> a) -> a -> Bool -> a
ifTrueThenUpdate fn a shouldBeDone =
    if shouldBeDone then
        fn a

    else
        a

boolToMaybe : Bool -> a -> Maybe a
boolToMaybe bool a =
    if bool then
        Just a

    else
        Nothing