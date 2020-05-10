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
            (ProcessDesigner.Model TestData.testProcessDesignerModel Editor Nothing Nothing Nothing)
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

                ProcessDesigner.NewProcess process tempId ->
                    (newProcess tempId process |> updateDesignerModel, Cmd.none)

                ProcessDesigner.NewItem process item itemIndex tempId ->
                    (newItemToProcess tempId itemIndex item process |> updateDesignerModel, Cmd.none)

                ProcessDesigner.NewItemSlot process ->
                    (addItemSlot process |> updateDesignerModel, Cmd.none)

                ProcessDesigner.DragProcessItemStart draggingState ->
                    (setDraggingProcessItemState draggingState |> updateDesignerModel, Cmd.none)

                ProcessDesigner.DragProcessItemEnd ->
                    ( designerModel
                        |> hasProcessItemTargeted
                        |> boolToMaybe model
                        |> Maybe.withDefault (updateDesignerModel clearDraggingProcessItemState)
                    , Cmd.none
                    )

                ProcessDesigner.DropProcessItem target ->
                    (dropProcessItemOn target |> updateDesignerModel, Cmd.none)

                ProcessDesigner.DragEmptyItemStart draggingState ->
                    (setDraggingEmptyItemState draggingState |> updateDesignerModel, Cmd.none)

                ProcessDesigner.DragEmptyItemEnd ->
                    ( designerModel
                        |> hasEmptyItemTargeted
                        |> boolToMaybe model
                        |> Maybe.withDefault (updateDesignerModel clearDraggingEmptyItemState)
                    , Cmd.none
                    )

                ProcessDesigner.DropEmptyItem ->
                    (dropEmptyItemToBin |> updateDesignerModel, Cmd.none)

                ProcessDesigner.DragProcessStart draggingState ->
                    (setDraggingProcessState draggingState |> updateDesignerModel, Cmd.none)

                ProcessDesigner.DragProcessEnd ->
                    ( designerModel
                        |> hasProcessTargeted
                        |> boolToMaybe model
                        |> Maybe.withDefault (updateDesignerModel clearDraggingProcessState)
                    , Cmd.none
                    )

                ProcessDesigner.DropProcess ->
                    (dropProcessToBin |> updateDesignerModel, Cmd.none)

                ProcessDesigner.DragTargetOnDraggableArea hasTargeted ->
                    (toggleTargetingOfDraggingState hasTargeted |> updateDesignerModel, Cmd.none)

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
