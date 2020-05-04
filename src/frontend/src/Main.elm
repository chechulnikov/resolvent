port module Main exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Styled
import ProcessDesigner exposing (..)
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
    ( ProcessDesignerModel (ProcessDesigner.Model TestData.testProcessDesignerModel Editor Nothing Nothing)
    , Cmd.none
    )



-- MODEL


type Model
    = ProcessDesignerModel ProcessDesigner.Model



-- UPDATE


type Msg
  = Idle
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url
  | ProcessDesignerMsg ProcessDesigner.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Idle ->
            (model, Cmd.none)

        LinkClicked _ ->
            (model, Cmd.none)

        UrlChanged _ ->
            (model, Cmd.none)

        ProcessDesignerMsg processEditorMsg ->
            let
                (ProcessDesignerModel designer) = model
            in
            case processEditorMsg of
                ProcessDesigner.Idle ->
                    (model, Cmd.none)

                ProcessDesigner.NewProcess process ->
                    ( designer |> addProcess process |> ProcessDesignerModel
                    , Cmd.none
                    )

                ProcessDesigner.NewItem process processItem itemIndex ->
                    ( designer |> addItemToProcess itemIndex processItem process |> ProcessDesignerModel
                    , Cmd.none
                    )

                ProcessDesigner.NewItemSlot process ->
                    ( designer |> addItemSlot process |> ProcessDesignerModel
                    , Cmd.none
                    )

                ProcessDesigner.NewSubProcess process subProcess ->
                    ( designer |> addSubProcessToProcess subProcess process |> ProcessDesignerModel
                    , Cmd.none
                    )

                ProcessDesigner.DragProcessItem draggingState ->
                    ( ProcessDesignerModel { designer | draggingProcessItemState = Just draggingState }
                    , Cmd.none
                    )

                ProcessDesigner.DropProcessItem target ->
                    ( designer |> dragAndDropProcessItem target |> ProcessDesignerModel
                    , Cmd.none
                    )

                ProcessDesigner.DragEmptyItem draggingState ->
                    ( ProcessDesignerModel { designer | draggingEmptyItemState = Just draggingState }
                    , Cmd.none
                    )

                ProcessDesigner.DropEmptyItem ->
                    ( designer |> dragAndDropEmptyItemToBin |> ProcessDesignerModel
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
         ProcessDesignerModel designerModel ->
            div []
                [ ProcessDesigner.view designerModel |> Html.Styled.toUnstyled |> Html.map ProcessDesignerMsg
                ]
