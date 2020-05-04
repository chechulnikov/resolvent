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
    (ProcessDesignerModel Editor TestData.testProcess, Cmd.none)



-- MODEL


type Model
    = ProcessDesignerModel Mode Process



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
                (ProcessDesignerModel mode process) = model
            in
            case processEditorMsg of
                ProcessDesigner.NewItem item ->
                    let
                        updatedProcess =
                            { process | items = List.append process.items [ item ] }
                    in
                    (ProcessDesignerModel mode updatedProcess, Cmd.none)

                ProcessDesigner.NewSubProcess subProcess ->
                    let
                        updatedProcess =
                            { process | subProcesses = List.append process.subProcesses [ subProcess ] }
                    in
                    (ProcessDesignerModel mode updatedProcess, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
         ProcessDesignerModel mode process ->
            div []
                [ ProcessDesigner.view mode process |> Html.Styled.toUnstyled |> Html.map ProcessDesignerMsg
                ]
