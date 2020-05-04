module ChartTest exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import LineChart



-- MODEL


type alias Point =
    { x : Float
    , y : Float
    }



-- VIEW


view =
    div
        [ css
            [ margin (rem 1)
            ]
        ]
        [ Html.Styled.fromUnstyled
            ( LineChart.view1 .x .y
                [ Point 0 2, Point 5 5, Point 10 10 ]
            )
        ]