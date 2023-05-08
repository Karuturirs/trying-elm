module TimeSync exposing (..)

import Browser
import Time
import Task 
import Html
import Url exposing (..)
import Browser.Navigation as Nav
import Html exposing (node, Html, button, input, div, br)

import Date exposing (fromCalendarDate, fromPosix)
import Time exposing (millisToPosix, utc, Month(..))
import Html.Attributes exposing (value, size, placeholder, align, attribute)
import List exposing (sort)
import Html.Attributes exposing (id)


--Main


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }

--Model

-- date & timestamp format May 5, 2023 9:00 am
type alias Model =
  { 
    key : Nav.Key
    , url : Url.Url
    , zone : Time.Zone
    , time : Time.Posix
    , slots : List Slots
    , ms : List (String, Int) 
    , link : Bool
  }

type alias Slots =
    {
        id : Int
        , year : Int
        , month : Int
        , day :  Int  
        , hh : Int
        , mm: Int 
    }


init : ()-> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
  let
    queryString =  Maybe.withDefault "" (Url.percentDecode (Maybe.withDefault "" url.query))
         getValueOfmsKey (splitQueryString queryString )
          
  in
    ( Model key url Time.utc (Time.millisToPosix 0) [Slots 0 0 0 0 0 0]  qrpm False , Task.perform AdjustTimeZone Time.here )



-- UPDATE





update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )
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
        ( { model | year = Maybe.withDefault 0 (String.toInt newYear) , link = False}
        , Cmd.none
        )
    MonthUpdate newMonth ->
        ( { model | month = Maybe.withDefault 0 (String.toInt newMonth), link = False }
        , Cmd.none
        )
    DayUpdate newDay ->
        ( { model | day = Maybe.withDefault 0 (String.toInt newDay) , link = False}
        , Cmd.none
        )
    HourUpdate newhh ->
        ( { model | hh = Maybe.withDefault 0 (String.toInt newhh), link = False }
        , Cmd.none
        )
    MinUpdate newmm ->
        ( { model | mm = Maybe.withDefault 0 (String.toInt newmm), link = False }
        , Cmd.none
        )
    TimeUpdate newtime ->
        ( { model | ms =  newtime , link = False }
        , Cmd.none
        )
    Share -> 
        (  { model | link =  True }
        , Cmd.none
        )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
        Time.every 1000 Tick

-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    dateString = Date.toIsoString (fromPosix model.zone model.time)
    hour   = Time.toHour   model.zone model.time
    minute = Time.toMinute model.zone model.time
    second = Time.toSecond model.zone model.time

  in
  { title = "Free time matcher with timezone converter.",
    body = [
       text "The current URL is: "
      , Html.b [] [ text (  model.url.path) ]
      , Html.ul []
          [ viewLink "Home" "/home"
          , viewLink "Shared" "/Shared"
          ]
      , div[align "center"][
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
            , br [][]
            , div[][ dateToMillsec  model.slots.year model.slots.month model.slots.day model.slots.hh model.slots.mm model.link ]
        ]
    ]
  }

displaySlots : Model -> Html Msg
displaySlots model = 
    if model.ms.length > 0 then
        List.map (\msavalue -> Time.millisToPosix msavalue ) model.ms  
            |> List.map (\posixvalue -> slotMaker model posixvalue )
            |> List.map (\slot -> timeSlotElement model )
      
        

    else 
        timeSlotElement model 1

slotMaker : Model -> Time.Posix -> Slots
slotMaker model posix =
        Slots (Time.toYear model.zone  posix) (Time.toMonth model.zone  posix) (Time.toDay model.zone  posix) (Time.toHour model.zone  posix) (Time.toMinute model.zone  posix)


timeSlotElement : Model -> Int -> Html Msg
timeSlotElement model index =
    div [Html.id index] [
        input [ type_ "text", size 2,  placeholder "YYYY", value (String.fromInt model.slots.year) , onInput YearUpdate  ] []
        , Html.text "-"
        , input [ type_ "text", size 1, placeholder "MM", value (String.fromInt model.slots.month) , onInput MonthUpdate ] []
        , Html.text "-"
        , input [ type_ "text", size 1, placeholder "DD", value (String.fromInt model.slots.day) , onInput DayUpdate ] []
        , Html.text "         "
        , input [ type_ "text", size 1, placeholder "HH", value (String.fromInt model.slots.hh), onInput HourUpdate ] []
        , Html.text ":"
        , input [ type_ "text", size 1, placeholder "MM", value (String.fromInt model.slots.mm), onInput MinUpdate] []
        , Html.text "  "
        , input [ type_ "text", size 1, placeholder "AM", value  model.am , onInput TimeUpdate ] []
    ]
viewLink : String -> String -> Html msg
viewLink text hrefPath =
  Html.li [] [ Html.a [ Html.Attributes.href hrefPath ] [ Html.text text ] ]

{-|
 A function to split a string into key-value pairs
 -}
splitQueryString : String -> List (String, String)
splitQueryString queryString =
    let
        pairs = String.split "&" queryString
                    |> sort
    in
    List.map
        (\pair ->
            case String.split "=" pair of
                [ key, value ] ->
                    ( key, value )

                _ ->
                    ("", "")
        )
        pairs

getValueByKey : List (a, b) -> a -> Maybe b
getValueByKey myList key =
    List.filter (\(k, _) -> k == key) myList
        |> List.head
        |> Maybe.map Tuple.second

getValueOfmsKey : List (String, String)  -> List Int
getValueOfmsKey list  =
     List.filter (\(key, _) -> String.startsWith key "ms") list
        |>  List.map (\(_, value) ->  String.toInt(value) )

getQueryValueForKey :  String -> String -> Maybe String
getQueryValueForKey queryString key =
    queryString
        |> splitQueryString
        |> getValueByKey key

{-|

Fetch value from List given Index
-}
fetchValue : List a -> Int -> Maybe a
fetchValue list index =
   getValueByKey (List.indexedMap Tuple.pair list) index


dateToMillsec : Int -> Int -> Int -> Int-> Int -> Bool -> Html Msg
dateToMillsec year month day hh mm link =
    if link then
        Html.node "time-millisec"
            [ attribute "year" (String.fromInt year)
            , attribute "month" (String.fromInt month)
            , attribute "day" (String.fromInt day)
            , attribute "hh" (String.fromInt hh)
            , attribute "mm" (String.fromInt mm)
            ]
            []
    else 
        Html.a [][]


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
