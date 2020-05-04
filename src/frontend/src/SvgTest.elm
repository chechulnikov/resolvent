module SvgTest exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Svg as Svg exposing (..)
import Svg.Attributes as SA exposing (..)



-- VIEW


view =
    div
        [ css
            [ margin (rem 1)
            ]
        ]
        [ Html.Styled.fromUnstyled
            ( Svg.svg
                [ SA.width "1000"
                , SA.height "300"
                , SA.viewBox "0 0 1000 300"
                ]
                [ Svg.circle
                    [ SA.cx "50"
                    , SA.cy "50"
                    , SA.r "50"
                    , SA.fill "red"
                    ]
                    []
                , Svg.line
                    [ SA.x1 "100"
                    , SA.y1 "50"
                    , SA.x2 "200"
                    , SA.y2 "50"
                    , SA.stroke "black"
                    ]
                    []
                , Svg.circle
                    [ SA.cx "250"
                    , SA.cy "50"
                    , SA.r "50"
                    , SA.fill "green"
                    ]
                    []
                ]
            )
        ]