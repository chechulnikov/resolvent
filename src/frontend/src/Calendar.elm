module Calendar exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Time exposing (Posix)



-- MSG


type Msg
    = ChangeMode Mode



-- MODEL


type alias Model =
    { currentTime : Posix
    , mode : Mode
    , schedule : Schedule
    }


type Mode
    = DayMode
    | WeekMode
    | MonthMode
    | YearMode


type alias Layout =
    {

    }


type alias Schedule =
    { items : List ScheduleItem
    }


type alias ScheduleItem =
    { title : String
    , description : String
    , start : Posix
    , end : Posix
    }



-- VIEW


viewDay : List ScheduleItem -> Posix -> Html Msg
viewDay scheduleItems currentTime =
    div [] []


viewMonth : List ScheduleItem -> Posix -> Html Msg
viewMonth scheduleItems currentTime =
    div [] []


viewYear : List ScheduleItem -> Posix -> Html Msg
viewYear scheduleItems currentTime =
    div [] []


view : Model -> Html Msg
view model =
    div [] []
