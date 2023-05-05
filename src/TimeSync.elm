module TimeSync exposing (..)

import Browser
import Html exposing (..)
import Time
import Html.Events exposing (..)
import Task 
import Html.Attributes  exposing (..)

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
  }


type AmPm = AM | PM



init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) 2023 12 30  8 30  "AM"
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




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
        Time.every 1000 Tick

-- VIEW


view : Model -> Html Msg
view model =
  
    div [] [
        
        , input [ type_ "text", size 2,  placeholder "YYYY", value (String.fromInt model.year) , onInput YearUpdate  ] []
        , input [ type_ "text", size 1, placeholder "MM", value (String.fromInt model.month) , onInput MonthUpdate ] []
        , input [ type_ "text", size 1, placeholder "DD", value (String.fromInt model.day) , onInput DayUpdate ] []
        , input [ type_ "text", size 1, placeholder "HH", value (String.fromInt model.hh), onInput HourUpdate ] []
        , input [ type_ "text", size 1, placeholder "MM", value (String.fromInt model.mm), onInput MinUpdate] []
        , input [ type_ "text", size 1, placeholder "AM", value  model.am , onInput TimeUpdate ] []
        
    ]


--

-- viewInput : String -> String -> (Time.Month, Int, Int ) -> (Int, Int , AmPm ) -> msg -> Html msg
-- viewInput t p v1 v2 toMsg =
--     let 
--             v = toString 
--     in
--         input [ type_ t, placeholder p, value v, onInput ShowCalendar ] []

