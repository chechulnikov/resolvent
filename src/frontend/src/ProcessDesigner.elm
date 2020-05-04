module ProcessDesigner exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, css)
import Html.Styled.Events exposing (..)
import Json.Decode as Decode
import List.Extra as List
import Utils exposing (ifTrueThenUpdate)


type Msg
    = Idle
    | NewProcess Process
    | NewItem Process ProcessItem Int
    | NewItemSlot Process
    | NewSubProcess Process SubProcess
    | DragProcessItem DraggingProcessItemState
    | DropProcessItem DropProcessItemTarget
    | DragEmptyItem DraggingEmptyItemState
    | DropEmptyItem


type alias Model =
    { processes : List Process
    , mode : Mode
    , draggingProcessItemState : Maybe DraggingProcessItemState
    , draggingEmptyItemState : Maybe DraggingEmptyItemState
    }


type alias Process =
    { id : String
    , name : String
    , items : List (Maybe ProcessItem)
    , subProcesses : List SubProcess
    }


type alias SubProcess =
    { id : String
    , name : String
    , parent : ProcessItem
    , items : List ProcessItem
    }


type alias ProcessItem =
    { id : String
    , name : String
    , description : String
    }


type alias DraggingProcessItemState =
    { process : Process
    , item : ProcessItem
    , itemIndex : Int
    }


type alias DraggingEmptyItemState =
    { process : Process
    , itemIndex : Int
    }


type Mode
    = Viewer
    | Editor


type DropProcessItemTarget
    = DropOnEmptySlot Int
    | DropOnNewSlot
    | DropInBin



-- UPDATE


addProcess : Process -> Model -> Model
addProcess process model =
    { model | processes = List.append model.processes [ process ] }


addItemToProcess : Int -> ProcessItem -> Process -> Model -> Model
addItemToProcess itemIndex item process model =
    let
        update p =
            { p | items = p.items |> List.updateAt itemIndex (\_ -> Just item) }

        updatedProcesses =
            model.processes |> List.map (\p -> ifTrueThenUpdate update p (p.name == process.name))
    in
    { model | processes = updatedProcesses }


addSubProcessToProcess : SubProcess -> Process -> Model -> Model
addSubProcessToProcess subProcess process model =
    let
        update p =
            { p | subProcesses = List.append p.subProcesses [ subProcess ] }

        updatedProcesses =
            model.processes |> List.map (\p -> ifTrueThenUpdate update p (p.id == process.id))
    in
    { model | processes = updatedProcesses }


removeItemFromProcess : Int -> Process -> Model -> Model
removeItemFromProcess itemIndex process model =
    let
        update p =
            { p | items = p.items |> List.updateAt itemIndex (\_ -> Nothing) }

        updatedProcesses =
            model.processes
                |> List.map (\p -> ifTrueThenUpdate update p (p.id == process.id))
    in
    { model | processes = updatedProcesses }


dragAndDropProcessItem : DropProcessItemTarget -> Model -> Model
dragAndDropProcessItem target model =
    model.draggingProcessItemState
        |> Maybe.map
            (\{ process, item, itemIndex } ->
                case target of
                    DropOnNewSlot ->
                        model
                            |> addItemSlot process
                            |> addItemToProcess (List.length process.items) item process
                            |> removeItemFromProcess itemIndex process

                    DropInBin ->
                        removeItemFromProcess itemIndex process model

                    DropOnEmptySlot targetItemIndex ->
                        model
                            |> addItemToProcess targetItemIndex item process
                            |> removeItemFromProcess itemIndex process
            )
        |> Maybe.withDefault model
        |> (\m -> { m | draggingProcessItemState = Nothing })


addItemSlot : Process -> Model -> Model
addItemSlot process model =
    let
        update p =
            { p | items = List.append p.items [ Nothing ] }

        updatedProcesses =
            model.processes |> List.map (\p -> ifTrueThenUpdate update p (p.name == process.name))
    in
    { model | processes = updatedProcesses }


removeEmptySlot : Int -> Process -> Model -> Model
removeEmptySlot itemIndex process model =
    let
        update p =
            { p | items = p.items |> List.removeAt itemIndex }

        updatedProcesses =
            model.processes
                |> List.map (\p -> ifTrueThenUpdate update p (p.id == process.id))
    in
    { model | processes = updatedProcesses }


dragAndDropEmptyItemToBin : Model -> Model
dragAndDropEmptyItemToBin model =
    model.draggingEmptyItemState
        |> Maybe.map (\{ process, itemIndex } -> removeEmptySlot itemIndex process model)
        |> Maybe.withDefault model
        |> (\m -> { m | draggingEmptyItemState = Nothing })



-- VIEW


view : Model -> Html Msg
view model =
    model.processes
        |> List.map (viewProcess model.mode)
        |> (\ps ->
            if model.mode == Editor then
                List.append ps [ div [ css [ displayFlex ] ] [ viewAddProcessButton model, viewBin model ] ]

            else
                ps
        )
        |> div
            [ css
                [ overflowX scroll
                , whiteSpace noWrap
                ]
            ]

viewProcess mode process =
    let
        viewProcessItems  =
            let
                items =
                    process.items
                        |> List.indexedMap
                            ( \itemIndex item ->
                                item
                                    |> Maybe.map (viewProcessItem process)
                                    |> Maybe.withDefault (viewEmptyItem process itemIndex)
                            )
            in
            if mode == Editor then
                List.append items [ viewNewSlotButton process ]

            else
                items
    in
    div
        [ css
            [ displayFlex
            ]
        ]
        viewProcessItems


viewProcessItem process processItem =
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

        itemIndex =
            process.items
                |> List.elemIndex (Just processItem)
                |> Maybe.withDefault 0  -- TODO !!!!!!!!!!
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
                [ border3 (rem 0.1) solid (rgb 10 124 10)
                , backgroundColor (rgb 216 255 211)
                , color (rgb 10 124 10)
                , cursor pointer
                ]
            , active
                [ border3 (rem 0.1) solid (rgb 26 171 26)
                , backgroundColor (rgb 216 255 211)
                , color (rgb 26 171 26)
                , cursor pointer
                ]
            ]
        , on "dragstart" (Decode.succeed (DragProcessItem (DraggingProcessItemState process processItem itemIndex)))
        , attribute "draggable" "true"
        ]
        [ div
            []
            [ viewName
            ]
        ]


viewEmptyItem process itemIndex =
    let
        newItemName =
            "New item " ++ (String.fromInt (itemIndex + 1))
    in
    div
        [ css
            [ border3 (rem 0.1) solid (rgb 255 255 255)
            , margin (rem 0.5)
            , padding (rem 0.25)
            , minWidth (rem 10)
            , height (rem 5)
            , opacity (num 0.25)
            , hover
                [ border3 (rem 0.1) solid (rgb 15 103 15)
                , backgroundColor (rgb 216 255 211)
                , color (rgb 13 110 13)
                , cursor pointer
                , opacity (num 0.5)
                ]
            , active
                [ border3 (rem 0.1) solid (rgb 26 171 26)
                , backgroundColor (rgb 216 255 211)
                , color (rgb 26 171 26)
                , cursor pointer
                , opacity (num 0.85)
                ]
            ]
        , onClick (NewItem process { id = newItemName, name = newItemName, description = "" } itemIndex)
        , preventDefaultOn "dragover" (Decode.succeed (Idle, True))
        , on "drop" (Decode.succeed (DropProcessItem (DropOnEmptySlot itemIndex)))
        , on "dragstart" (Decode.succeed (DragEmptyItem { process = process, itemIndex = itemIndex }))
        , attribute "draggable" "true"
        ]
        [ div [] [ text "EMPTY" ] ]


viewNewSlotButton process =
    div
        [ css
            [ border3 (rem 0.1) dashed (rgb 4 4 4)
            , margin (rem 0.5)
            , padding (rem 0.25)
            , minWidth (rem 10)
            , height (rem 5)
            , opacity (num 0.25)
            , hover
                [ backgroundColor (rgb 216 255 211)
                , color (rgb 13 110 13)
                , cursor pointer
                , opacity (num 0.5)
                ]
            , active
                [ border3 (rem 0.1) solid (rgb 26 171 26)
                , backgroundColor (rgb 216 255 211)
                , color (rgb 26 171 26)
                , cursor pointer
                , opacity (num 0.85)
                ]
            ]
        , onClick (NewItemSlot process)
        , preventDefaultOn "dragover" (Decode.succeed (Idle, True))
        , on "drop" (Decode.succeed (DropProcessItem DropOnNewSlot))
        ]
        [ div [] [ text "➕" ] ]


viewAddProcessButton model =
    let
        processesCount =
            List.length model.processes

        newProcessName =
            "New item " ++ (String.fromInt (processesCount + 1))
    in
    div
        [ css
            [ border3 (rem 0.1) dashed (rgb 4 4 4)
            , margin (rem 0.5)
            , padding (rem 0.25)
            , width (rem 20)
            , height (rem 5)
            , opacity (num 0.25)
            , hover
                [ border3 (rem 0.1) solid (rgb 15 103 15)
                , backgroundColor (rgb 216 255 211)
                , color (rgb 13 110 13)
                , cursor pointer
                , opacity (num 0.5)
                ]
            , active
                [ border3 (rem 0.1) solid (rgb 26 171 26)
                , backgroundColor (rgb 216 255 211)
                , color (rgb 26 171 26)
                , cursor pointer
                , opacity (num 0.85)
                ]
            ]
        , onClick (NewProcess (Process newProcessName newProcessName [] []))
        ]
        [ div [] [ text "➕" ] ]


viewBin model =
    let
        getDropMsg m =
            if m.draggingProcessItemState /= Nothing then
                DropProcessItem DropInBin

            else if m.draggingEmptyItemState /= Nothing then
                DropEmptyItem

            else
                Idle
    in
    div
        [ css
            [ border3 (rem 0.1) dashed (rgb 4 4 4)
            , margin (rem 0.5)
            , padding (rem 0.25)
            , width (rem 20)
            , height (rem 5)
            , opacity (num 0.25)
            , hover
                [ border3 (rem 0.1) solid (rgb 105 0 24)
                , backgroundColor (rgb 255 211 216)
                , color (rgb 105 0 24)
                , cursor default
                , opacity (num 0.5)
                ]
            ]
        , preventDefaultOn "dragover" (Decode.succeed (Idle, True))
        , on "drop" (Decode.succeed (getDropMsg model))
        ]
        [ div [] [ text "🗑️" ] ]