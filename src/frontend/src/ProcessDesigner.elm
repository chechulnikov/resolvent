module ProcessDesigner exposing (..)

import Controls
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, css)
import Html.Styled.Events exposing (..)
import List.Extra as List
import Utils exposing (boolToString, updateIfTrue, onStd, onWithoutDef, onWithoutProp)


type Msg
    = Idle
    | ToggleMode Mode
    | TempIdRequested (Int -> Msg)
    | NewProcess Process Int
    | NewItem Process ProcessItem Int Int
    | NewItemSlot Process
    | DragStart DraggingState
    | DragEnd
    | Drop DropProcessItemTarget
    | DragTargetOnDraggableArea Bool


type alias Model =
    { processes : List Process
    , mode : Mode
    , draggingState : Maybe { state : DraggingState, hasTargeted : Bool}
    }


type alias Process =
    { id : String
    , name : String
    , items : List (Maybe ProcessItem)
    }


type alias ProcessItem =
    { id : String
    , name : String
    , description : String
    }


type DraggingState
    = DraggingProcessItemState
        { process : Process
        , item : ProcessItem
        , itemIndex : Int
        }
    | DraggingEmptyItemState
        { process : Process
        , itemIndex : Int
        }
    | DraggingProcessState
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


toggleMode : Mode -> Model -> Model
toggleMode mode model =
    { model | mode = mode }


newProcess : Int -> Process -> Model -> Model
newProcess tempId process model =
    addProcess { process | id = String.fromInt tempId } model


addProcess : Process -> Model -> Model
addProcess process model =
    { model | processes = List.append model.processes [ process ] }


newItemToProcess : Int -> Int -> ProcessItem -> Process -> Model -> Model
newItemToProcess tempId itemIndex item process model =
    addItemToProcess itemIndex { item | id = String.fromInt tempId } process model


addItemToProcess : Int -> ProcessItem -> Process -> Model -> Model
addItemToProcess itemIndex item process model =
    let
        update p =
            { p | items = p.items |> List.updateAt itemIndex (\_ -> Just item) }

        updatedProcesses =
            model.processes |> List.map (\p -> updateIfTrue update p (p.id == process.id))
    in
    { model | processes = updatedProcesses }


removeItemFromProcess : Int -> Process -> Model -> Model
removeItemFromProcess itemIndex process model =
    let
        update p =
            { p | items = p.items |> List.updateAt itemIndex (\_ -> Nothing) }

        updatedProcesses =
            model.processes
                |> List.map (\p -> updateIfTrue update p (p.id == process.id))
    in
    { model | processes = updatedProcesses }


dropOn : DropProcessItemTarget -> Model -> Model
dropOn target model =
    model.draggingState
        |> Maybe.map
            (\{ state } ->
                case state of
                    DraggingProcessItemState { process, item, itemIndex } ->
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

                    DraggingEmptyItemState { process, itemIndex } ->
                        removeEmptySlot itemIndex process model

                    DraggingProcessState { process } ->
                        removeProcess process model
            )
        |> Maybe.withDefault model
        |> clearDraggingState


addItemSlot : Process -> Model -> Model
addItemSlot process model =
    let
        update p =
            { p | items = List.append p.items [ Nothing ] }

        updatedProcesses =
            model.processes |> List.map (\p -> updateIfTrue update p (p.id == process.id))
    in
    { model | processes = updatedProcesses }


removeEmptySlot : Int -> Process -> Model -> Model
removeEmptySlot itemIndex process model =
    let
        update p =
            { p | items = p.items |> List.removeAt itemIndex }

        updatedProcesses =
            model.processes
                |> List.map (\p -> updateIfTrue update p (p.id == process.id))
    in
    { model | processes = updatedProcesses }


removeProcess : Process -> Model -> Model
removeProcess process model =
    { model | processes = model.processes |> List.remove process }


setDraggingState : DraggingState -> Model -> Model
setDraggingState draggingState model =
    { model | draggingState = Just { state = draggingState, hasTargeted = False } }


hasDragTargeted : Model -> Bool
hasDragTargeted model =
    model.draggingState
        |> Maybe.map .hasTargeted
        |> Maybe.withDefault False


clearDraggingState : Model -> Model
clearDraggingState model =
    { model | draggingState = Nothing }


toggleTargetingOfDraggingState : Bool -> Model -> Model
toggleTargetingOfDraggingState hasTargeted model =
    { model | draggingState = model.draggingState |> Maybe.map (\s -> { s | hasTargeted = hasTargeted}) }


-- ATTRS


attrDraggable mode =
    attribute "draggable" (boolToString (mode == Editor))


attrEditable mode =
    attribute "contenteditable" (boolToString (mode == Editor))


-- VIEW


view : Model -> Html Msg
view model =
    let
        droppableAreaMode =
            case model.draggingState of
                Just { state } ->
                    case state of
                        DraggingProcessItemState _ ->
                            ReadyToReceiveProcessItem

                        DraggingEmptyItemState _ ->
                            ReadyToReceiveEmptyItem

                        DraggingProcessState _ ->
                            ReadyToReceiveProcess
                Nothing ->
                    Normal

        toggleModeButton =
            let
                (label, newMode) =
                    case model.mode of
                        Viewer ->
                            ("Edit", Editor)

                        Editor ->
                            ("Back to viewer", Viewer)
            in
            Controls.viewToggle label newMode ToggleMode ((==) Viewer)
    in
    model.processes
        |> List.map (viewProcess model.mode droppableAreaMode)
        |> (\ps ->
            if model.mode == Editor then
                List.append ps [ div [ css [ displayFlex ] ] [ viewAddProcessButton model, viewBin droppableAreaMode ] ]

            else
                ps
        )
        |> List.append [ toggleModeButton ]
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
                        |> List.filter ((/=) Nothing >> (||) (mode == Editor))
                        |> List.indexedMap
                            ( \itemIndex item ->
                                item
                                    |> Maybe.map (viewProcessItem mode process)
                                    |> Maybe.withDefault (viewEmptyItem mode droppableAreaMode process itemIndex)
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
                , minWidth (rem 2)
                , margin (rem 0.25)
                , backgroundColor (hex "#606dbc")
                , property "content" "''"
                , cursor (if mode == Editor then move else default)
                ]
            , hover
                [ backgroundColor (rgba 128 128 128 0.05)
                ]
            ]
        , onWithoutProp "dragstart" (DragStart (DraggingProcessState { process = process }))
        , onWithoutProp "dragend" DragEnd
        , attrDraggable mode
        ]
        viewProcessItems


viewProcessItem mode process processItem =
    let
        viewName =
            div
                [ css []
                ]
                [ div
                    [ attrEditable mode
                    , css
                        [ focus [ outline none ]
                        , whiteSpace noWrap
                        ]
                    ]
                    [ text processItem.name ]
                ]

        itemIndex =
            process.items
                |> List.elemIndex (Just processItem)
                |> Maybe.withDefault 0  -- unattainable result
    in
    div
        [ css
            [ border3 (rem 0.1) solid (rgb 4 4 4)
            , margin (rem 0.5)
            , padding (rem 0.25)
            , minWidth (rem 10)
            , height (rem 3)
            , overflowX hidden
            , backgroundColor (rgb 255 255 255)
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
        , attrDraggable mode
        , onWithoutProp "dragstart" (DragStart (DraggingProcessItemState { process = process, item = processItem, itemIndex = itemIndex }))
        , onWithoutProp "dragend" DragEnd
        , onWithoutProp "dragenter" (DragTargetOnDraggableArea True)
        , onWithoutProp "dragleave" (DragTargetOnDraggableArea False)
        , onWithoutProp "drop" (Drop (DropOnAnotherProcessItem process processItem itemIndex))
        , onWithoutDef "dragover" Idle
        ]
        [ div
            []
            [ viewName
            ]
        ]


viewEmptyItem mode droppableAreaMode process itemIndex =
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
            , height (rem 3)
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
        , onClick (TempIdRequested (NewItem process { id = "", name = newItemName, description = "" } itemIndex))
        , onWithoutDef "dragover" Idle
        , attrDraggable mode
        , onStd "drop" (Drop (DropOnEmptySlot process itemIndex))
        , onWithoutProp "dragstart" (DragStart (DraggingEmptyItemState { process = process, itemIndex = itemIndex }))
        , onWithoutProp "dragend" DragEnd
        , onStd "dragenter" (DragTargetOnDraggableArea True)
        , onStd "dragleave" (DragTargetOnDraggableArea False)
        ]
        [ text "EMPTY" ]


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
            , height (rem 3)
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
        , onWithoutDef "dragover" Idle
        , onStd "drop" (Drop (DropOnNewSlot process))
        , onStd "dragenter" (DragTargetOnDraggableArea True)
        , onStd "dragleave" (DragTargetOnDraggableArea False)
        ]
        [ text "➕" ]


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
            , height (rem 3)
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
        , onClick (TempIdRequested (NewProcess (Process newProcessName newProcessName [])))
        ]
        [ text "➕" ]


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

                _ ->
                    Drop DropInBin
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
            ]
        , onWithoutDef "dragover" Idle
        , onStd "drop" dropMsg
        , onStd "dragenter" (DragTargetOnDraggableArea True)
        , onStd "dragleave" (DragTargetOnDraggableArea False)
        ]
        [ text "🗑️" ]
