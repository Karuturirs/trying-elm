module Main exposing (main)

import Html exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time)
import Time.Extra exposing (weekdayToInt, fromCalendarDate, fromPosix)
import Calendar exposing (Calendar, Model, Msg, view, update)


type alias Model =
    { 
      zone : Time.Zone
    , time : Time.Posix
    , calendarModel : Calendar.Model
    , inputText : String
    }

main : Program () Model Msg
main =
   Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) { year = toYear Time.now
    , month = toMonth Time.now |> Month.toNumber
    , selectedDate = Nothing
    , visible = False
    }  "2023/05/31 22:25" 
  , Task.perform AdjustTimeZone Time.here
  )


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | CalendarMsg Msg
    | SelectDate


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )
        CalendarMsg subMsg ->
            ( { model | calendarModel = Calendar.update subMsg model.calendarModel }, Cmd.none )

        SelectDate ->
            let
                selectedDateTime =
                    model.calendarModel.selection
                        |> Time.Extra.fromCalendarDate Time.utc
                        |> Time.toPosix
                        |> Time.Extra.fromPosix

                newInputText =
                    selectedDateTime
                        |> Time.format "%Y-%m-%d %H:%M:%S"
            in
                ( { model | inputText = newInputText }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        year   = String.fromInt ( Time.toYear model.zone model.time)
        month   = ( Time.toMonth model.zone model.time)
        day   = String.fromInt ( Time.toDay model.zone model.time)
        hour   = String.fromInt (Time.toHour   model.zone model.time)
        minute = String.fromInt (Time.toMinute model.zone model.time)
        second = String.fromInt (Time.toSecond model.zone model.time)
    in
        div []
            [ div [] [
                    h1 [] [ text (year ++ "/" ++ month ++ "/" ++ day ++ " "++hour ++ ":" ++ minute ++ ":" ++ second) ] 
                ]
            , input [ type_ "text", value model.inputText ] []
            , button [ onClick SelectDate ] [ text "Select Date & Time" ]
            , if model.calendarModel.visible then
                    let
                        calendarView =
                            Calendar.view model.calendarModel CalendarMsg
                    in
                    div [ class "calendar-popup" ]
                        [ calendarView ]

            else
                    text ""
            ]



subscriptions : Model -> Sub Msg
subscriptions model =
        Time.every 1000 Tick