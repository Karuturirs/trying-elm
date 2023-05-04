module Calendar exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Time exposing (Posix, toMonth, toYear, Month, Weekday(..))
import List


-- TYPES --

type alias Model =
    { year : Int
    , month : Int
    , selectedDate : Maybe Posix
    , visible : Bool
    }


-- MSG --

type Msg
    = PrevMonth
    | NextMonth
    | SelectDate Int
    | Done


-- INIT --

init : Model
init =
    { year = toYear Time.now
    , month = toMonth Time.now |> Month.toNumber
    , selectedDate = Nothing
    , visible = False
    }


-- UPDATE --

update : Msg -> Model -> Model
update msg model =
    case msg of
        PrevMonth ->
            let
                (newYear, newMonth) =
                    if model.month == 1 then
                        (model.year - 1, 12)
                    else
                        (model.year, model.month - 1)
            in
            { model | year = newYear, month = newMonth, selectedDate = Nothing }

        NextMonth ->
            let
                (newYear, newMonth) =
                    if model.month == 12 then
                        (model.year + 1, 1)
                    else
                        (model.year, model.month + 1)
            in
            { model | year = newYear, month = newMonth, selectedDate = Nothing }

        SelectDate day ->
            let
                selectedDate =
                    fromCalendarDate model.year model.month day
                        |> Time.toPosix
            in
            { model | selectedDate = Just selectedDate }

        Done ->
            { model | selectedDate = Nothing , visible=False }


-- VIEW --

view : Model -> (Posix -> msg) -> Html msg
view model onDone =
    let
        daysInMonth =
            fromCalendarDate model.year model.month 1
                |> Time.monthLength

        days =
            List.range 1 daysInMonth
                |> List.map (\day -> (day, weekDayForDate model.day day))

        weekDayForDate selectedDay day =
            let
                dayOfWeek =
                    fromCalendarDate model.year model.month day
                        |> Time.toWeekday
                        |> weekdayToInt
            in
            if day == selectedDay then
                "selected"
            else if dayOfWeek == 6 || dayOfWeek == 0 then
                "weekend"
            else
                ""

        dateString =
            case model.selectedDate of
                Just posix ->
                    posix
                        |> fromPosix
                        |> Time.toIsoString
                        |> String.left 10

                Nothing ->
                    ""

        timeString =
            case model.selectedDate of
                Just posix ->
                    posix
                        |> fromPosix
                        |> Time.toIsoString
                        |> String.slice 11 16

                Nothing ->
                    "00:00"
    in
    div []
        [ div [ class "calendar-header" ]
            [ button [ class "calendar-button", onClick PrevMonth ] [ text "<" ]
            , div [ class "calendar-title" ] [ text <| monthName model.month ++ " " ++ String.fromInt model.year ]
            , button [ class "calendar-button", onClick NextMonth ] [ text ">" ]
            ]
        , table [ class "calendar-table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Sun" ]
                    , th [] [ text "Mon"
                    , th [] [ text "Tue" ]
                    , th [] [ text "Wed" ]
                    , th [] [ text "Thu" ]
                    , th [] [ text "Fri" ]
                    , th [] [ text "Sat" ]
                    ]
                ]
            , tbody []
                (List.chunksOf 7 days
                    |> List.map (\row ->
                        tr []
                            (List.map (\(day, className) ->
                                let
                                    dayClass =
                                        if day == model.selectedDate |> Maybe.map fromPosix == Just (fromCalendarDate model.year model.month day) then
                                            "selected"
                                        else
                                            className
                                in
                                td [ class dayClass, onClick <| SelectDate day ] [ text <| String.fromInt day ]
                            ) row)
                    )
                )
            ]
        , div []
            [ text "Date: "
            , text dateString
            ]
        , div []
            [ text "Time: "
            , input [ type_ "time", value timeString, onInput (onDone << Maybe.withDefault 0 << Time.fromString "%H:%M") ] []
            ]
        , button [ class "calendar-done-button", disabled (model.selectedDate == Nothing), onClick Done ] [ text "Done" ]
        ]
        ]


-- HELPERS --

monthName : Int -> String
monthName monthNumber =
    case Month.fromNumber monthNumber of
        Just month ->
            Month.toString month

        Nothing ->
            ""


weekdayToInt : Weekday -> Int
weekdayToInt weekday =
    case weekday of
        Monday -> 1
        Tuesday -> 2
        Wednesday -> 3
        Thursday -> 4
        Friday -> 5
        Saturday -> 6
        Sunday -> 0

fromPosix : Time.Posix -> Time
fromPosix posix =
    Time.millisToPosix posix
        |> Time.millisToUtcTime
        |> Time.utcToZoned Time.utc
        |> Time.toLocalTime

fromCalendarDate : Int -> Int -> Int -> Time.Zone -> Int
fromCalendarDate year month day zone =
    Date.fromParts year (Month.toInt month) day
        |> Date.toTime zone

