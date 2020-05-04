module TestData exposing (..)

import ProcessDesigner

testProcessDesignerModel =
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
      , subProcesses = []
      }
    ,  { id = "2"
       , name = "Hotfix process"
       , items =
            [ ProcessDesigner.ProcessItem "1" "TO DO" "" |> Just
            , ProcessDesigner.ProcessItem "2" "In progress" "" |> Just
            , ProcessDesigner.ProcessItem "3" "Done" "" |> Just
            ]
       , subProcesses = []
       }
    ]
