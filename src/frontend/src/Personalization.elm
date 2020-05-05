module Personalization exposing (..)


import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, css)
import Html.Styled.Events exposing (..)
import Json.Decode as Decode
import List.Extra as List


type Theme
    = Light
    | Dark


type Style
    = Primary
    | Secondary
    | Accent
    | Information
    | Warning
    | Error


colors =
    [ "#ef476f"
    , "#ffd166"
    , "#06d6a0"
    , "#118ab2"
    , "#118ab2"
    , "#073b4c"
    ]