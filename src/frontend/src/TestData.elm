module TestData exposing (..)

import Dict
import ProcessDesigner


testProcessDesignerProcesses =
    [ { id = "1"
      , name = "Main process"
      , items =
            [ ProcessDesigner.ProcessItem "1" "Idea" "Get an idea from storage" |> Just
            , ProcessDesigner.ProcessItem "2" "Analysis" "Analyse the idea" |> Just
            , Nothing
            , ProcessDesigner.ProcessItem "3" "Decomposition" "Decompose the idea to several features" |> Just
            , ProcessDesigner.ProcessItem "4" "Specification" "Specify every feature" |> Just
            , ProcessDesigner.ProcessItem "5" "Specification review" "Review of specification" |> Just
            ]
      }
    , { id = "2"
      , name = "Hotfix process"
      , items =
            [ ProcessDesigner.ProcessItem "1" "TO DO" "" |> Just
            , ProcessDesigner.ProcessItem "2" "In progress" "" |> Just
            , ProcessDesigner.ProcessItem "3" "Done" "" |> Just
            ]
      }
    ]


testSubProcesses processItemId =
    Dict.fromList
        [ ( "2"
          , [ { id = "3"
              , name = "Tech research"
              , items =
                    [ ProcessDesigner.ProcessItem "10" "Idea" "Get an idea from storage" |> Just
                    , ProcessDesigner.ProcessItem "11" "Analysis" "Analyse the idea" |> Just
                    ]
              }
            , { id = "4"
              , name = "Acceptance bugs"
              , items =
                    [ ProcessDesigner.ProcessItem "12" "TO DO" "" |> Just
                    , ProcessDesigner.ProcessItem "13" "Fix" "" |> Just
                    , ProcessDesigner.ProcessItem "14" "Ready for testing" "" |> Just
                    , ProcessDesigner.ProcessItem "15" "Testing" "" |> Just
                    , ProcessDesigner.ProcessItem "16" "Done" "" |> Just
                    ]
              }
            ]
          )
        ]
        |> Dict.get processItemId
        |> Maybe.withDefault []
