module ProcessDesigner exposing (..)

import Controls
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, css)
import Html.Styled.Events exposing (..)
import List.Extra as List
import Utils exposing (boolToMaybe, boolToString, onStd, onWithoutDef, onWithoutProp, updateIfTrue)


type Msg
    = Idle
    | ToggleMode Mode
    | TempIdRequested (Int -> Msg)
    | NewProcess WorkflowKind Process Int
    | NewItem WorkflowKind Process ProcessItem Int Int
    | NewItemSlot WorkflowKind Process
    | DragStart DraggingState
    | DragEnd
    | Drop DropProcessItemTarget
    | DragTargetOnDraggableArea Bool
    | ToggleProcessItemSelection Process ProcessItem


type alias Model =
    { processes : List Process
    , subWorkflow : Maybe SubWorkflowData
    , mode : Mode
    , draggingState : Maybe { state : DraggingState, hasTargeted : Bool}
    }


type alias Workflow a =
    { a | processes : List Process }


type alias SubWorkflowData =
    { parentProcessItem : ProcessItem
    , parentProcess : Process
    , processes : List Process
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
    = ViewerMode
    | EditorMode


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


type WorkflowKind
    = MainWorkflow
    | SubWorkflow



-- UPDATE


toggleMode : Mode -> Model -> Model
toggleMode mode model =
    { model | mode = mode }


updateWorkflow : WorkflowKind -> (Model-> Model) -> (SubWorkflowData -> SubWorkflowData) -> Model -> Model
updateWorkflow workflowType updateMainWorkflow updateSecondaryWorkflow model =
    case workflowType of
        MainWorkflow ->
            let _ = Debug.log "update main workflow" {} in
            updateMainWorkflow model

        SubWorkflow ->
            let _ = Debug.log "update sub workflow" {} in
            { model | subWorkflow = model.subWorkflow |> Maybe.map updateSecondaryWorkflow }


newProcess : Int -> Process -> Workflow a -> Workflow a
newProcess tempId process workflow =
    addProcess { process | id = String.fromInt tempId } workflow


addProcess : Process -> Workflow a -> Workflow a
addProcess process workflow =
    { workflow | processes = List.append workflow.processes [ process ] }


newItemToProcess : Int -> Int -> ProcessItem -> Process -> Workflow a -> Workflow a
newItemToProcess tempId itemIndex item process model =
    addItemToProcess itemIndex { item | id = String.fromInt tempId } process model


addItemToProcess : Int -> ProcessItem -> Process -> Workflow a -> Workflow a
addItemToProcess itemIndex item process workflow =
    let
        update p =
            { p | items = p.items |> List.updateAt itemIndex (\_ -> Just item) }

        updatedProcesses =
            workflow.processes |> List.map (\p -> updateIfTrue update p (p.id == process.id))
    in
    { workflow | processes = updatedProcesses }


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


addItemSlot : Process -> Workflow a -> Workflow a
addItemSlot process workflow =
    let
        update p =
            { p | items = List.append p.items [ Nothing ] }

        updatedProcesses =
            workflow.processes |> List.map (\p -> updateIfTrue update p (p.id == process.id))
    in
    { workflow | processes = updatedProcesses }


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


toggleSubProcess : Process -> ProcessItem -> List Process -> Model -> Model
toggleSubProcess parentProcess parentProcessItem processes model =
    model.subWorkflow
        |> Maybe.map (.parentProcessItem >> (==) parentProcessItem)
        |> Maybe.map (always { model | subWorkflow = Nothing })
        |> Maybe.withDefault
            { model
            | subWorkflow =
                Just
                    { parentProcessItem = parentProcessItem
                    , parentProcess = parentProcess
                    , processes = processes
                    }
            }



-- ATTRS


attrDraggable mode =
    attribute "draggable" (boolToString (mode == EditorMode))


attrEditable mode =
    attribute "contenteditable" (boolToString (mode == EditorMode))


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

        mode =
            model.mode

        toggleModeButton =
            let
                (label, newMode) =
                    case model.mode of
                        ViewerMode ->
                            ("Edit", EditorMode)

                        EditorMode ->
                            ("Back to viewer", ViewerMode)
            in
            Controls.viewToggle label newMode ToggleMode ((==) ViewerMode)

        addWorkflowControls workflowKind workflow ps =
            mode == EditorMode
                |> boolToMaybe
                    ( List.append
                        ps
                        [ div
                            [ css [ displayFlex ] ]
                            [ viewAddProcessButton workflowKind workflow.processes, viewBin droppableAreaMode ]
                        ]
                    )
                |> Maybe.withDefault ps

        viewMainWorkflow =
            model.processes
                |> List.filter isUsefulProcess
                |> List.map (viewProcess MainWorkflow mode droppableAreaMode)
                |> addWorkflowControls MainWorkflow model
                |> List.append [ toggleModeButton ]
                |> div
                    [ css
                        [ overflowX scroll
                        , whiteSpace noWrap
                        , height (vh 75)
                        ]
                    ]

        viewSubWorkflow workflow =
            workflow.processes
                |> List.map (viewProcess SubWorkflow mode droppableAreaMode)
                |> addWorkflowControls SubWorkflow workflow
                |> div
                    [ css
                        [ bottom (vh 0)
                        , right (rem 0)
                        , padding (rem 0.25)
                        , width (pct 100)
                        , height (vh 25)
                        , borderTop3 (rem 0.1) solid (rgb 4 4 4)
                        ]
                    ]

        isUsefulProcess p =
            (model.mode == EditorMode)
            || ((model.mode == ViewerMode) && (p.items /= [] && List.any ((/=) Nothing) p.items))
    in
    div
        []
        [ viewMainWorkflow
        , model.subWorkflow
            |> Maybe.map viewSubWorkflow
            |> Maybe.withDefault (text "")
        ]


viewProcess workflowKind mode droppableAreaMode process =
    let
        viewProcessItems  =
            let
                items =
                    process.items
                        |> List.filter ((/=) Nothing >> (||) (mode == EditorMode))
                        |> List.indexedMap
                            ( \itemIndex item ->
                                item
                                    |> Maybe.map (viewProcessItem mode process)
                                    |> Maybe.withDefault (viewEmptyItem workflowKind mode droppableAreaMode process itemIndex)
                            )
            in
            if mode == EditorMode then
                List.append items [ viewNewSlotButton workflowKind droppableAreaMode process ]

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
                , cursor (if mode == EditorMode then move else default)
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
        , onClick (ToggleProcessItemSelection process processItem)
        ]
        [ div
            []
            [ viewName
            ]
        ]


viewEmptyItem workflowKind mode droppableAreaMode process itemIndex =
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
        , onClick (TempIdRequested (NewItem workflowKind process { id = "", name = newItemName, description = "" } itemIndex))
        , onWithoutDef "dragover" Idle
        , attrDraggable mode
        , onStd "drop" (Drop (DropOnEmptySlot process itemIndex))
        , onWithoutProp "dragstart" (DragStart (DraggingEmptyItemState { process = process, itemIndex = itemIndex }))
        , onWithoutProp "dragend" DragEnd
        , onStd "dragenter" (DragTargetOnDraggableArea True)
        , onStd "dragleave" (DragTargetOnDraggableArea False)
        ]
        [ text "EMPTY" ]


viewNewSlotButton workflowKind droppableAreaMode process =
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
        , onClick (NewItemSlot workflowKind process)
        , onWithoutDef "dragover" Idle
        , onStd "drop" (Drop (DropOnNewSlot process))
        , onStd "dragenter" (DragTargetOnDraggableArea True)
        , onStd "dragleave" (DragTargetOnDraggableArea False)
        ]
        [ text "➕" ]


viewAddProcessButton : WorkflowKind -> List Process -> Html Msg
viewAddProcessButton workflowKind processes =
    let
        processesCount =
            List.length processes

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
        , onClick (TempIdRequested (NewProcess workflowKind (Process newProcessName newProcessName [])))
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
            , bottom (vh 0)
            , right (rem 0)
            , margin (rem 0.5)
            , padding (rem 0.25)
            , width (vw 50)
            , height (vh 50)
            ]
        , onWithoutDef "dragover" Idle
        , onStd "drop" dropMsg
        , onStd "dragenter" (DragTargetOnDraggableArea True)
        , onStd "dragleave" (DragTargetOnDraggableArea False)
        ]
        [ text "🗑️" ]
