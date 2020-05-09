module Controls exposing (..)


import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)



viewToggle : String -> a -> (a -> msg) -> (a -> Bool) -> Html msg
viewToggle label data fn checkFn =
    div
        [ onClick (fn data)
        , css
            [ cursor pointer
            ]
        ]
        [ span [] [ text label ]
        , div
            [ css
                [ border3 (rem 0.1) solid (rgb 4 4 4)
                , borderRadius (rem 1)
                , height (rem 1)
                , width (rem 2)
                ]
            ]
            [ div
                [ css
                    [ borderRadius (pct 50)
                    , backgroundColor (rgb 4 4 4)
                    , height (rem 1)
                    , width (rem 1)
                    , if checkFn data then float right else float left
                    ]
                ]
                []
            ]
        ]