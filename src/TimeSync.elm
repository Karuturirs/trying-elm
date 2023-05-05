module TimeSync exposing (..)

import Browser
import Time
import Task 
import Html
import Html exposing (node, Html, button, input, div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (..)
import Date exposing (fromCalendarDate, fromPosix)
import Time exposing (millisToPosix, utc, Month(..))
import Html.Attributes exposing (value, size, placeholder, align, attribute)







--Main

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

--Model

-- date & timestamp format May 5, 2023 9:00 am
type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , year : Int
  , month : Int
  , day :  Int  
  , hh : Int
  , mm: Int 
  , am : String 
  , utcmill : Int
  }


type AmPm = AM | PM



init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) 2023 12 30  8 30  "AM" 0
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | ShowCalendar
  | AdjustTimeZone Time.Zone
  | YearUpdate String 
  | MonthUpdate  String
  | DayUpdate  String
  | HourUpdate String 
  | MinUpdate String 
  | TimeUpdate String
  | Share



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

    ShowCalendar ->
      ( model
      , Cmd.none
      )

    YearUpdate newYear-> 
        ( { model | year = Maybe.withDefault 0 (String.toInt newYear) }
        , Cmd.none
        )
    MonthUpdate newMonth ->
        ( { model | month = Maybe.withDefault 0 (String.toInt newMonth) }
        , Cmd.none
        )
    DayUpdate newDay ->
        ( { model | day = Maybe.withDefault 0 (String.toInt newDay) }
        , Cmd.none
        )
    HourUpdate newhh ->
        ( { model | hh = Maybe.withDefault 0 (String.toInt newhh) }
        , Cmd.none
        )
    MinUpdate newmm ->
        ( { model | mm = Maybe.withDefault 0 (String.toInt newmm) }
        , Cmd.none
        )
    TimeUpdate newtime ->
        ( { model | am =  newtime }
        , Cmd.none
        )
    Share -> 
        
            (  model  
            , Cmd.none
            )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
        Time.every 1000 Tick

-- VIEW


view : Model -> Html Msg
view model =
  let
    dateString = Date.toIsoString (fromPosix model.zone model.time)
    hour   = Time.toHour   model.zone model.time
    minute = Time.toMinute model.zone model.time
    second = Time.toSecond model.zone model.time

  in
    div[align "center"][
       -- h3 [] [ Html.text (dateString ++ " " ++  String.fromInt hour ++ ":" ++  String.fromInt minute ++ ":" ++  String.fromInt second) ],
          div [] [
             svg
                [ viewBox "0 0 400 400"
                , width "400"
                , height "400"
                ]
                [ circle [ cx "200", cy "200", r "120", fill "#1293D8" ] []
                , viewHand 6 60 (toFloat hour/12)
                , viewHand 6 90 (toFloat minute/60)
                , viewHand 3 90 (toFloat second/60)
                ,   Svg.text_
                    [   Svg.Attributes.x "50%", Svg.Attributes.y "25%", Svg.Attributes.textAnchor "middle", 
                        fontFamily "Gill Sans", 
                        fontSize "20",
                        textAnchor "middle",
                        fill "white", 
                        Svg.Attributes.style "-webkit-user-select" -- make text unselectable by browser (seems to work, though hard to test with certainty)

                    ] [Svg.text( String.fromInt (Date.year (fromPosix model.zone model.time)) )]
                ,  Svg.text_
                    [   Svg.Attributes.x "50%", Svg.Attributes.y "70%", Svg.Attributes.textAnchor "middle", 
                        fontFamily "Gill Sans", 
                        fontSize "20",
                        textAnchor "middle",
                        fill "white", 
                        Svg.Attributes.style "-webkit-user-select" -- make text unselectable by browser (seems to work, though hard to test with certainty)

                    ] [Svg.text (  (Date.month (fromPosix model.zone model.time)) |> monthToName )]
                ,  Svg.text_
                    [   Svg.Attributes.x "50%", Svg.Attributes.y "75%", Svg.Attributes.textAnchor "middle", 
                        fontFamily "Gill Sans", 
                        fontSize "20",
                        textAnchor "middle",
                        fill "white", 
                        Svg.Attributes.style "-webkit-user-select" -- make text unselectable by browser (seems to work, though hard to test with certainty)

                    ] [Svg.text (String.fromInt (Date.day (fromPosix model.zone model.time))  )]
                ]
            ]

        , input [ type_ "text", size 2,  placeholder "YYYY", value (String.fromInt model.year) , onInput YearUpdate  ] []
        , Html.text "-"
        , input [ type_ "text", size 1, placeholder "MM", value (String.fromInt model.month) , onInput MonthUpdate ] []
        , Html.text "-"
        , input [ type_ "text", size 1, placeholder "DD", value (String.fromInt model.day) , onInput DayUpdate ] []
        , Html.text "         "
        , input [ type_ "text", size 1, placeholder "HH", value (String.fromInt model.hh), onInput HourUpdate ] []
        , Html.text ":"
        , input [ type_ "text", size 1, placeholder "MM", value (String.fromInt model.mm), onInput MinUpdate] []
        , Html.text "  "
        , input [ type_ "text", size 1, placeholder "AM", value  model.am , onInput TimeUpdate ] []
        , button [ onClick Share ] [ Html.text "Share" ]
        , dateToMillsec  model.year model.month model.day model.hh model.mm
    ]
    


--

-- viewInput : String -> String -> (Time.Month, Int, Int ) -> (Int, Int , AmPm ) -> msg -> Html msg
-- viewInput t p v1 v2 toMsg =
--     let 
--             v = toString 
--     in
--         input [ type_ t, placeholder p, value v, onInput ShowCalendar ] []


-- yyyy-mm-dd HH:mm
-- toPosix : Time.Zone -> String -> Int
-- toPosix zone dateTimeString =
--     let
--         dateTime = String.split " " dateTimeString
--         date = List.head dateTime
--         time = List.head <| List.tail dateTime
--         [ year, month, day ] = String.split "-" date
--         [ hours, minutes ] = String.split ":" time
--         posix = Time.posix <| Time.millisToPosix 0
--             |> Time.setYear (String.toInt year |> Result.withDefault 0)
--             |> Time.setMonth (String.toInt month |> Result.withDefault 1)
--             |> Time.setDay (String.toInt day |> Result.withDefault 1)
--             |> Time.setHour (String.toInt hours |> Result.withDefault 0)
--             |> Time.setMinute (String.toInt minutes |> Result.withDefault 0)
--             |> Time.inZone zone
--             |> Time.toMillis
--     in
--     posix

dateToMillsec : Int -> Int -> Int -> Int-> Int -> Html Msg
dateToMillsec year month day hh mm =
  Html.node "time-millisec"
    [ attribute "year" (String.fromInt year)
    , attribute "month" (String.fromInt month)
    , attribute "day" (String.fromInt day)
    , attribute "hh" (String.fromInt hh)
    , attribute "mm" (String.fromInt mm)
    ]
    []


viewHand : Int -> Float -> Float -> Svg Msg
viewHand width length turns =
  let
    t = 2 * pi * (turns - 0.25)
    x = 200 + length * cos t
    y = 200 + length * sin t
  in
  line
    [ x1 "200"
    , y1 "200"
    , x2 (String.fromFloat x)
    , y2 (String.fromFloat y)
    , stroke "white"
    , strokeWidth (String.fromInt width)
    , strokeLinecap "round"
    ]
    []

monthToName : Month -> String
monthToName m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"
