module Main exposing (..)

import Task
import Url exposing (..)
import Browser exposing (..)
import Browser.Navigation as Nav
import Html exposing (node, Html, button, input, div, br)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (..)
import Date exposing (fromCalendarDate, fromPosix)
import Time exposing (..)
import Html.Attributes exposing (value, size, placeholder, align, attribute, id)
import List exposing (sort)
import Json.Decode exposing (bool)
import Dict exposing (Dict)

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


type alias Model =
  { 
    key : Nav.Key
    , url : Url.Url
    , zone : Time.Zone
    , time : Time.Posix
    , ms : Dict String Slot
    , link : Bool
  }

type alias Slot =
    {   
        millsec :Int
        , year : Int
        , month : Int
        , day :  Int  
        , hh : Int
        , mm: Int 
    }

init : ()-> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    let
        mslist = Dict.fromList [ ("msa", (Slot 1675191540000 2023 1 31 13 59))]  
    in
        ( Model key url Time.utc (Time.millisToPosix 0) mslist  False 
        , Task.perform AdjustTimeZone Time.here )


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | YearUpdate String String
  | Share


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
    
    YearUpdate key newYear-> 
        let
            newms = Dict.update key (Maybe.map (\slot -> { slot | year = (Maybe.withDefault 0 (String.toInt newYear)) })) model.ms

        in
            ( { model | ms = newms , link = False}
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

view : Model -> Browser.Document Msg
view model =
  let
    hour   = Time.toHour model.zone model.time
    minute = Time.toMinute model.zone model.time
    second = Time.toSecond model.zone model.time

  in
  { title = "Free time matcher with auto timezone converter.",
    body = [
       text "The current URL is: "
      , Html.b [] [ text (  model.url.path) ]
      , Html.ul [][]
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
            , displaySlots model
            , button [ onClick Share ] [ Html.text "Share" ]
            , br [][]
           -- , div[][ dateToMillsecSlot  model.ms model.link ]
        ]
    ]
  }


displaySlots : Model -> Html Msg
displaySlots model = 
        List.map (\(key, slot) -> timeSlotElement slot key ) (Dict.toList model.ms)
            |> div []  
            
      
    

timeSlotElement : Slot -> String -> Html Msg
timeSlotElement slot qkey =
    div [Html.Attributes.id qkey] [
        input [ type_ "text", size 2,  placeholder "YYYY", value (String.fromInt slot.year) , onInput (YearUpdate qkey) ] []
    ]

-- updateMs : String -> String -> Model -> Dict String Slot
-- updateMs key newyear model =
--     Dict.update key (\slot -> Just { slot | year = newyear }) model.ms



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

dateToMillsecSlot : List (String, Int, Slot) -> Bool -> Html Msg
dateToMillsecSlot mslist link =
            mslist 
               |> List.map (\(k, _ , s) -> (dateToMillsec k s.year s.month s.day s.hh s.mm link) )
               |> div [] 

dateToMillsec : String -> Int -> Int -> Int -> Int-> Int -> Bool -> Html Msg
dateToMillsec nodeid year month day hh mm link =
    if link then
        Html.node "time-millisec"
            [ attribute "id" nodeid
            , attribute "year" (String.fromInt year)
            , attribute "month" (String.fromInt month)
            , attribute "day" (String.fromInt day)
            , attribute "hh" (String.fromInt hh)
            , attribute "mm" (String.fromInt mm)
            ]
            []
    else 
        Html.a [][]
