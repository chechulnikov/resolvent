port module Main exposing (..)

import BoardDesigner
import Browser
import Browser.Navigation exposing (Key)
import Header
import Html exposing (..)
import Html.Styled
import ProcessDesigner exposing (..)
import Random
import TestData
import Url exposing (Url)
import Utils exposing (boolToMaybe)




-- PORTS


port localStorageGetItem : String -> Cmd msg
port localStorageSetItem : (String, String) -> Cmd msg
port localStorageGetItemResponse : ((String, String) -> msg) -> Sub msg



-- MAIN


main =
    Browser.application
        { init = init
        , view = document
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


document model =
    { title = "Resolvent"
    , body = [ view model ]
    }


init : () -> Url -> Key -> (Model, Cmd msg)
init _ _ _ =
    ( { body =
        ProcessDesignerModel
            { processes = TestData.testProcessDesignerProcesses  -- TODO !!!!!! TEST DATA !!!!!!
            , subWorkflow = Nothing
            , mode = EditorMode
            , draggingState = Nothing
            }
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { body : BodyModel
    }


type BodyModel
    = ProcessDesignerModel ProcessDesigner.Model
    | BoardDesigner BoardDesigner.Model



-- UPDATE


type Msg
  = Idle
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url
  | HeaderMsg Header.Msg
  | ProcessDesignerMsg ProcessDesigner.Model ProcessDesigner.Msg
  | BoardDesignerMsg BoardDesigner.Model BoardDesigner.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        updateBodyModel : (bodyModel -> BodyModel) -> bodyModel -> Model
        updateBodyModel bodyModelWrapper bodyModel =
            { model | body = bodyModel |> bodyModelWrapper }
    in
    case msg of
        Idle ->
            (model, Cmd.none)

        LinkClicked _ ->
            (model, Cmd.none)

        UrlChanged _ ->
            (model, Cmd.none)

        HeaderMsg headerMsg ->
            case headerMsg of
                Header.Idle ->
                    (model, Cmd.none)

        ProcessDesignerMsg designerModel processEditorMsg ->
            let
                updateDesignerModel fn =
                    designerModel |> fn |> updateBodyModel ProcessDesignerModel
            in
            case processEditorMsg of
                ProcessDesigner.Idle ->
                    (model, Cmd.none)

                ProcessDesigner.TempIdRequested message ->
                    (model, Random.generate (message >> (ProcessDesignerMsg designerModel)) (Random.int 10000 100000))

                ProcessDesigner.ToggleMode mode ->
                    (toggleMode mode |> updateDesignerModel, Cmd.none)

                ProcessDesigner.NewProcess workflowKind process tempId ->
                    let updateFn = newProcess tempId process in
                    (updateWorkflow workflowKind updateFn updateFn |> updateDesignerModel, Cmd.none)

                ProcessDesigner.NewItem workflowKind process item itemIndex tempId ->
                    let updateFn = newItemToProcess tempId itemIndex item process in
                    (updateWorkflow workflowKind updateFn updateFn |> updateDesignerModel, Cmd.none)

                ProcessDesigner.NewItemSlot workflowKind process ->
                    let updateFn = addItemSlot process in
                    (updateWorkflow workflowKind updateFn updateFn |> updateDesignerModel, Cmd.none)

                ProcessDesigner.DragStart draggingState ->
                    (setDraggingState draggingState |> updateDesignerModel, Cmd.none)

                ProcessDesigner.DragEnd ->
                    ( designerModel
                        |> hasDragTargeted
                        |> boolToMaybe model
                        |> Maybe.withDefault (updateDesignerModel clearDraggingState)
                    , Cmd.none
                    )

                ProcessDesigner.Drop workflowKind target ->
                    let updateFn = dropOn target designerModel in
                    (updateWorkflow workflowKind updateFn updateFn >> clearDraggingState |> updateDesignerModel, Cmd.none)

                ProcessDesigner.DragTargetOnDraggableArea hasTargeted ->
                    (toggleTargetingOfDraggingState hasTargeted |> updateDesignerModel, Cmd.none)

                ProcessDesigner.ToggleProcessItemSelection process processItem ->
                    -- TODO !!!!!! TEST DATA !!!!!!
                    (toggleSubWorkflow process processItem (TestData.testSubProcesses processItem.id) |> updateDesignerModel, Cmd.none)

        BoardDesignerMsg designerModel boardDesignerMsg ->
            case boardDesignerMsg of
                BoardDesigner.Idle ->
                    (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        header =
            Header.view
                |> Html.Styled.toUnstyled
                |> Html.map HeaderMsg

        body m =
            case m of
                ProcessDesignerModel designerModel ->
                    ProcessDesigner.view designerModel
                        |> Html.Styled.toUnstyled
                        |> Html.map (ProcessDesignerMsg designerModel)

                BoardDesigner designerModel ->
                    BoardDesigner.view designerModel
                        |> Html.Styled.toUnstyled
                        |> Html.map (BoardDesignerMsg designerModel)
    in
    div
        []
        [ header
        , body model.body
        ]
