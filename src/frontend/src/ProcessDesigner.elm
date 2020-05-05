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
    | DragProcessItemStart DraggingProcessItemState
    | DragProcessItemEnd
    | DropProcessItem DropProcessItemTarget
    | DragEmptyItemStart DraggingEmptyItemState
    | DragEmptyItemEnd
    | DropEmptyItem
    | DragProcessStart DraggingProcessState
    | DragProcessEnd
    | DropProcess


type alias Model =
    { processes : List Process
    , mode : Mode
    , draggingProcessItemState : Maybe DraggingProcessItemState
    , draggingEmptyItemState : Maybe DraggingEmptyItemState
    , draggingProcessState : Maybe DraggingProcessState
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


type alias DraggingProcessState =
    { process : Process
    }


type Mode
    = Viewer
    | Editor


type DropProcessItemTarget
    = DropOnEmptySlot Process Int
    | DropOnNewSlot Process
    | DropInBin
    | DropOnAnotherProcessItem Process ProcessItem Int


type DroppableAreaMode
    = Normal
    | ReadyToReceiveProcessItem
    | ReadyToReceiveEmptyItem
    | ReadyToReceiveProcess



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


dropProcessItemOn : DropProcessItemTarget -> Model -> Model
dropProcessItemOn target model =
    model.draggingProcessItemState
        |> Maybe.map
            (\{ process, item, itemIndex } ->
                case target of
                    DropOnNewSlot targetProcess ->
                        model
                            |> addItemSlot targetProcess
                            |> addItemToProcess (List.length targetProcess.items) item targetProcess
                            |> removeItemFromProcess itemIndex process

                    DropInBin ->
                        removeItemFromProcess itemIndex process model

                    DropOnEmptySlot targetProcess targetItemIndex ->
                        model
                            |> addItemToProcess targetItemIndex item targetProcess
                            |> removeItemFromProcess itemIndex process

                    DropOnAnotherProcessItem targetProcess targetItem targetItemIndex ->
                        model
                            |> addItemToProcess itemIndex targetItem process
                            |> addItemToProcess targetItemIndex item targetProcess
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


removeProcess : Process -> Model -> Model
removeProcess process model =
    { model | processes = model.processes |> List.remove process }


dragAndDropProcessToBin : Model -> Model
dragAndDropProcessToBin model =
    model.draggingProcessState
        |> Maybe.map (\{ process } -> removeProcess process model)
        |> Maybe.withDefault model
        |> (\m -> { m | draggingProcessState = Nothing })



-- VIEW


view : Model -> Html Msg
view model =
    let
        droppableAreaMode =
            if model.draggingProcessItemState /= Nothing then
                ReadyToReceiveProcessItem

            else if model.draggingEmptyItemState /= Nothing then
                ReadyToReceiveEmptyItem

            else if model.draggingProcessState /= Nothing then
                ReadyToReceiveProcess

            else
                Normal
    in
    model.processes
        |> List.map (viewProcess model.mode droppableAreaMode)
        |> (\ps ->
            if model.mode == Editor then
                List.append ps [ div [ css [ displayFlex ] ] [ viewAddProcessButton model, viewBin droppableAreaMode ] ]

            else
                ps
        )
        |> div
            [ css
                [ overflowX scroll
                , whiteSpace noWrap
                ]
            ]

viewProcess mode droppableAreaMode process =
    let
        viewProcessItems  =
            let
                items =
                    process.items
                        |> List.indexedMap
                            ( \itemIndex item ->
                                item
                                    |> Maybe.map (viewProcessItem process)
                                    |> Maybe.withDefault (viewEmptyItem droppableAreaMode process itemIndex)
                            )
            in
            if mode == Editor then
                List.append items [ viewNewSlotButton droppableAreaMode process ]

            else
                items
    in
    div
        [ css
            [ displayFlex
            , before
                [ width (rem 2)
                , margin (rem 0.25)
                , property "background" "repeating-linear-gradient(45deg, #606dbc, #606dbc 10px, #465298 10px, #465298 20px)"
                , property "content" "''"
                , cursor move
                ]
            ]
        , on "dragstart" (Decode.succeed (DragProcessStart { process = process }))
        , on "dragend" (Decode.succeed DragProcessEnd)
        , attribute "draggable" "true"
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
        , attribute "draggable" "true"
        , on "dragstart" (Decode.succeed (DragProcessItemStart (DraggingProcessItemState process processItem itemIndex)))
        , on "dragend" (Decode.succeed DragProcessItemEnd)
        , on "drop" (Decode.succeed (DropProcessItem (DropOnAnotherProcessItem process processItem itemIndex)))
        , preventDefaultOn "dragover" (Decode.succeed (Idle, True))
        ]
        [ div
            []
            [ viewName
            ]
        ]


viewEmptyItem droppableAreaMode process itemIndex =
    let
        newItemName =
            "New item " ++ (String.fromInt (itemIndex + 1))

        droppableStyles =
            case droppableAreaMode of
                ReadyToReceiveProcessItem ->
                    Css.batch
                        [ border3 (rem 0.1) dashed (rgb 15 103 15)
                        , backgroundColor (rgb 216 255 211)
                        , color (rgb 13 110 13)
                        , opacity (num 0.5)
                        ]

                _ ->
                    Css.batch
                        [ border3 (rem 0.1) solid (rgb 255 255 255)
                        , opacity (num 0.25)
                        ]
    in
    div
        [ css
            [ droppableStyles
            , margin (rem 0.5)
            , padding (rem 0.25)
            , minWidth (rem 10)
            , height (rem 5)
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
        , on "drop" (Decode.succeed (DropProcessItem (DropOnEmptySlot process itemIndex)))
        , on "dragstart" (Decode.succeed (DragEmptyItemStart { process = process, itemIndex = itemIndex }))
        , on "dragend" (Decode.succeed DragEmptyItemEnd)
        , attribute "draggable" "true"
        ]
        [ div [] [ text "EMPTY" ] ]


viewNewSlotButton droppableAreaMode process =
    let
        droppableStyles =
            case droppableAreaMode of
                ReadyToReceiveProcessItem ->
                    Css.batch
                        [ border3 (rem 0.1) dashed (rgb 15 103 15)
                        , backgroundColor (rgb 216 255 211)
                        , color (rgb 13 110 13)
                        , opacity (num 0.5)
                        ]

                _ ->
                    Css.batch
                        [ border3 (rem 0.1) dashed (rgb 4 4 4)
                        , opacity (num 0.25)
                        ]
    in
    div
        [ css
            [ droppableStyles
            , margin (rem 0.5)
            , padding (rem 0.25)
            , minWidth (rem 10)
            , height (rem 5)
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
        , on "drop" (Decode.succeed (DropProcessItem (DropOnNewSlot process)))
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


viewBin : DroppableAreaMode -> Html Msg
viewBin droppableAreaMode =
    let
        droppableStyles =
            let
                droppableReadyStyles =
                    Css.batch
                        [ border3 (rem 0.1) dashed (rgb 105 0 24)
                        , backgroundColor (rgb 255 211 216)
                        , color (rgb 105 0 24)
                        , opacity (num 0.5)
                        , display block
                        ]
            in
            case droppableAreaMode of
                ReadyToReceiveProcessItem ->
                    droppableReadyStyles

                ReadyToReceiveEmptyItem ->
                    droppableReadyStyles

                ReadyToReceiveProcess->
                    droppableReadyStyles

                _ ->
                    Css.batch
                        [ border3 (rem 0.1) dashed (rgb 4 4 4)
                        , opacity (num 0.25)
                        , display none
                        ]

        dropMsg =
            case droppableAreaMode of
                Normal ->
                    Idle

                ReadyToReceiveProcessItem ->
                    DropProcessItem DropInBin

                ReadyToReceiveEmptyItem ->
                    DropEmptyItem

                ReadyToReceiveProcess ->
                    DropProcess
    in
    div
        [ css
            [ droppableStyles
            , position fixed
            , bottom (rem 0)
            , right (rem 0)
            , margin (rem 0.5)
            , padding (rem 0.25)
            , width (vw 25)
            , height (vh 25)
            --, hover
            --    [ border3 (rem 0.1) solid (rgb 105 0 24)
            --    , backgroundColor (rgb 255 211 216)
            --    , color (rgb 105 0 24)
            --    , cursor default
            --    , opacity (num 0.5)
            --    ]
            ]
        , preventDefaultOn "dragover" (Decode.succeed (Idle, True))
        , on "drop" (Decode.succeed dropMsg)
        ]
        [ div [] [ text "🗑️" ] ]