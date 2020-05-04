module ProcessDesigner exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Utils exposing (flip)


type Msg
    = NewItem ProcessItem
    | NewSubProcess SubProcess


type Mode
    = Viewer
    | Editor


type alias Process =
    { name : String
    , items : List ProcessItem
    , subProcesses : List SubProcess
    }

type alias SubProcess =
    { name : String
    , items : List ProcessItem
    }

type alias ProcessItem =
    { name : String
    , description : String
    }



-- VEW


view : Mode -> Process -> Html Msg
view mode process =
    let
        viewProcessItems =
            let items = process.items |> List.map viewProcessItem in
            if mode == Editor then
                List.append items [ viewAddItemButton ]

            else
                items

        --viewSubProcesses =

    in
    div
        [ css
            [ displayFlex
            , overflowX scroll
            , whiteSpace noWrap
            ]
        ]
        viewProcessItems


viewProcessItem : ProcessItem -> Html Msg
viewProcessItem processItem =
    let
        viewName =
            div
                [ css
                    [ textAlign center
                    ]
                ]
                [ div [] [ text processItem.name ]
                , div [] [ text processItem.description ]
                ]
    in
    div
        [ css
            [ border3 (rem 0.1) solid (rgb 4 4 4)
            , margin (rem 0.5)
            , padding (rem 0.25)
            , minWidth (rem 10)
            , height (rem 5)
            , overflowX hidden
            , whiteSpace noWrap
            , hover
                [ backgroundColor (rgb 216 255 211)
                , color (rgb 6 74 6)
                , cursor pointer
                ]
            , active
                [ border3 (rem 0.1) solid (rgb 26 171 26)
                , backgroundColor (rgb 216 255 211)
                , color (rgb 26 171 26)
                , cursor pointer
                ]
            ]
        ]
        [ div
            []
            [ viewName
            ]
        ]


viewAddItemButton =
    div
        [ css
            [ border3 (rem 0.1) dashed (rgb 4 4 4)
            , margin (rem 0.5)
            , padding (rem 0.25)
            , minWidth (rem 10)
            , height (rem 5)
            , overflowX hidden
            , whiteSpace noWrap
            , opacity (num 0.25)
            , hover
                [ border3 (rem 0.1) solid (rgb 15 103 15)
                , backgroundColor (rgb 216 255 211)
                , color (rgb 13 110 13)
                , cursor pointer
                , opacity (num 0.75)
                ]
            , active
                [ border3 (rem 0.1) solid (rgb 26 171 26)
                , backgroundColor (rgb 216 255 211)
                , color (rgb 26 171 26)
                , cursor pointer
                , opacity (num 1.0)
                ]
            ]
        , onClick (NewItem { name = "New item", description = "Just added item" })
        ]
        [ div
            []
            [ text "+" ]
        ]