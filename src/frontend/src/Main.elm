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
            (ProcessDesigner.Model TestData.testProcessDesignerModel Viewer Nothing Nothing Nothing)
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
            case processEditorMsg of
                ProcessDesigner.Idle ->
                    (model, Cmd.none)

                ProcessDesigner.TempIdRequested message ->
                    (model, Random.generate (message >> (ProcessDesignerMsg designerModel)) (Random.int 10000 100000))

                ProcessDesigner.ToggleMode mode ->
                    ({ model | body = designerModel |> toggleMode mode |> ProcessDesignerModel }, Cmd.none)

                ProcessDesigner.NewProcess process tempId ->
                    ({ model | body = designerModel |> newProcess tempId process |> ProcessDesignerModel }, Cmd.none)

                ProcessDesigner.NewItem process item itemIndex tempId ->
                    ({ model | body = designerModel |> newItemToProcess tempId itemIndex item process |> ProcessDesignerModel }, Cmd.none)

                ProcessDesigner.NewItemSlot process ->
                    ({ model | body = designerModel |> addItemSlot process |> ProcessDesignerModel }, Cmd.none)

                ProcessDesigner.DragProcessItemStart draggingState ->
                    ({ model | body = ProcessDesignerModel { designerModel | draggingProcessItemState = Just draggingState } }, Cmd.none)

                ProcessDesigner.DragProcessItemEnd ->
                    ({ model | body = ProcessDesignerModel { designerModel | draggingProcessItemState = Nothing } }, Cmd.none)

                ProcessDesigner.DropProcessItem target ->
                    ({ model | body = designerModel |> dropProcessItemOn target |> ProcessDesignerModel }, Cmd.none)

                ProcessDesigner.DragEmptyItemStart draggingState ->
                    ({ model | body = ProcessDesignerModel { designerModel | draggingEmptyItemState = Just draggingState } }, Cmd.none)

                ProcessDesigner.DragEmptyItemEnd ->
                    ({ model | body = ProcessDesignerModel { designerModel | draggingEmptyItemState = Nothing } }, Cmd.none)

                ProcessDesigner.DropEmptyItem ->
                    ({ model | body = designerModel |> dragAndDropEmptyItemToBin |> ProcessDesignerModel }, Cmd.none)

                ProcessDesigner.DragProcessStart draggingProcessState ->
                    ({ model | body = ProcessDesignerModel { designerModel | draggingProcessState = Just draggingProcessState } }, Cmd.none)

                ProcessDesigner.DragProcessEnd ->
                    ({ model | body = ProcessDesignerModel { designerModel | draggingProcessState = Nothing } }, Cmd.none)

                ProcessDesigner.DropProcess ->
                    ({ model | body = designerModel |> dragAndDropProcessToBin |> ProcessDesignerModel }, Cmd.none)

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

        body =
            case model.body of
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
        , body
        ]
