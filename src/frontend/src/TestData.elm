module TestData exposing (..)

import ProcessDesigner exposing (Process, ProcessItem)

testProcess : Process
testProcess =
    { name = "Main process"
    , items =
        [ ProcessItem "Idea" "Get an idea from storage"
        , ProcessItem "Analysis" "Analyse the idea"
        , ProcessItem "Decomposition" "Decompose the idea to several features"
        , ProcessItem "Specification" "Specify every feature"
        , ProcessItem "Specification review" "Review of specification"
        ]
    , subProcesses = []
    }