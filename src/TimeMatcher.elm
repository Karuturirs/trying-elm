module TimeMatcher exposing (..)

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
import Html.Attributes exposing (value, size, placeholder, align, attribute, id, style)
import List exposing (sort)
import Json.Decode exposing (bool)
import Html.Attributes exposing (disabled)
import Html exposing (img)



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
    , ms : List (String, Int , Slot)
    , link : Bool
    , ts :Int
    , copyText :String
  }

type alias Slot =
    {
        year : Int
        , month : Int
        , day :  Int  
        , hh : Int
        , mm: Int 
    }

{-|
qrStr =(Maybe.withDefault "" (Url.percentDecode (Maybe.withDefault "" url.query)))
        if !String.isEmpty qrStr then
            mslist = qrStr
                        |> splitQueryString
                        |> filtermsKey
                        |> convertQueryToMS Time.utc

-}
init : ()-> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    let
        mslist = buildInitMs url utc
    in
        ( Model key url Time.utc (Time.millisToPosix 0) mslist  False 0 ""
        , Task.perform AdjustTimeZone Time.here )

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | YearUpdatea String String
  | MonthUpdatea  String String
  | DayUpdatea  String String
  | HourUpdatea String String
  | MinUpdatea String String
  | AddSlot
  | RemoveSlot String
  | Share
  | Copy




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
        let
           mslist = buildInitMs model.url newZone
        in
        
            ( { model | zone = newZone , ms = mslist, copyText = "" }
            , Cmd.none
            )

    AddSlot ->
        let
            newms = [ ((picknextMs model), 0 , (Slot  2025 1 5 13 59))]  
        in 
            ( { model | ms = (List.append model.ms newms) , link = False,  copyText = "" }
            , Cmd.none
            )
    RemoveSlot qkey ->
        let
            newms = removeSlotReorder model qkey
                                                       
        in 
            ( { model | ms = newms , link = False,  copyText = "" }
            , Cmd.none
            )

    YearUpdatea key newYear -> 
        let
            newms = model.ms 
                        |> List.map (\(k, ms, slot) -> (updateSlotYear (k, ms, slot) key (Maybe.withDefault 0 (String.toInt newYear)) ))
        in
            ( { model | ms = newms , link = False,  copyText = ""}
            , Cmd.none
            )
    MonthUpdatea key newMonth ->
       let
            newms = model.ms 
                        |> List.map (\(k, ms, slot) -> (updateSlotMonth (k, ms, slot) key (Maybe.withDefault 0 (String.toInt newMonth)) ))
        in
            ( { model | ms = newms , link = False,  copyText = ""}
            , Cmd.none
            )
    DayUpdatea key newDay ->
        let
            newms = model.ms 
                        |> List.map (\(k, ms, slot) -> (updateSlotDay (k, ms, slot) key (Maybe.withDefault 0 (String.toInt newDay)) ))
        in
            ( { model | ms = newms , link = False,  copyText = ""}
            , Cmd.none
            )
    HourUpdatea key newhh ->
        let
            newms = model.ms 
                        |> List.map (\(k, ms, slot) -> (updateSlotHH (k, ms, slot) key (Maybe.withDefault 0 (String.toInt newhh)) ))
        in
            ( { model | ms = newms , link = False,  copyText = ""}
            , Cmd.none
            )
    MinUpdatea key newmm ->
        let
            newms = model.ms 
                        |> List.map (\(k, ms, slot) -> (updateSlotMM (k, ms, slot) key (Maybe.withDefault 0 (String.toInt newmm)) ))
        in
            ( { model | ms = newms , link = False,  copyText = ""}
            , Cmd.none
            )
    Share ->
        ( { model | link =  True ,  copyText = ""}
            , Cmd.none
            )
    Copy -> 
        ( { model | link =  True, copyText = "copied" }
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
    hour   = Time.toHour model.zone model.time
    minute = Time.toMinute model.zone model.time
    second = Time.toSecond model.zone model.time

  in
  { title = "Free time matcher with auto timezone converter.",
    body = [
    --    text "The current URL is: "
    --   , Html.b [] [ text (  model.url.path) ]
    --   , Html.ul []
    --       [ viewLink "Home" "/home"
    --       , viewLink "Shared" "/Shared"
    --       ]
        div[align "center"][
         Html.h3 [Html.Attributes.style "font-family" "Gill Sans"] [ Html.text "Stop bothering about timezones convertion, share your availabilty in your local time to anyone around the world." ],
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
            , div [ Html.Attributes.style "width" "70%"
                                , Html.Attributes.style "min-width" "204px"
                                , Html.Attributes.style "padding-bottom" "20px"] [
                                    div [ Html.Attributes.style "width" "calc(100%-4px)"
                                        , Html.Attributes.style "min-width" "200px"
                                        , Html.Attributes.style "margin-left" "10px"
                                        , Html.Attributes.style  "title" "Put local date here"
                                        , Html.Attributes.style "font-family" "Gill Sans" ] [text "Enter Local YYYY - MM - DD  HH:MM"
                                        ,button [ Html.Attributes.style "margin-left" "10px", disabled ((List.length model.ms) >=3) ,onClick AddSlot ]  [ Html.text "➕" ]
                                    ]
                ]
            , displaySlots model
            , div[ Html.Attributes.style "padding-bottom" "20px"] [ button [ Html.Attributes.style "margin-left" "10px"
                       , Html.Attributes.style "font-family" "Gill Sans"
                       , onClick Share ] [ Html.text "Generate URl to Share 🚀" ] ]
            , dateToMillsecSlot  model.ms model.link model.url 
            , copyUrl model.link (checkCopyText model)
            , copyUrlText model
            
           
            
        ]
    ]
  }

-- HELPING FUNCTIONS
  
checkCopyText : Model -> String
checkCopyText model=
    if model.copyText /= "" then
        "✔️ Copied"
    else
        "📋 Copy Url"

copyUrl : Bool -> String-> Html Msg
copyUrl enable textval =
    if enable  then
        button [ Html.Attributes.style "margin-left" "10px"
                       , Html.Attributes.style "font-family" "Gill Sans"
                       , onClick Copy ] [ Html.text textval ] 
    else
        div[][]


buildInitMs : Url ->Zone -> List (String, Int, Slot)
buildInitMs url zone =
    let
        query = (Maybe.withDefault "" url.query)
    in 
        if query /= "" then
            splitQueryString query
                    |> filtermsKey
                    |> convertQueryToMS zone 
        else
            [ ("msa", 0 , (Slot  2023 1 31 13 59))]
        

removeSlotReorder : Model -> String-> List (String, Int, Slot)
removeSlotReorder model qkey =
        List.filter (\(k,_,_) -> k /= qkey ) model.ms
                                |> List.indexedMap Tuple.pair 
                                |> List.map (\(index, (k, p, r)) -> case index of
                                                                            0 -> ("msa", p, r)
                                                                            1 -> ("msb", p, r)
                                                                            2 -> ("msc", p, r) 
                                                                            _ -> ("msd", p, r) 
                                                                            ) 

picknextMs : Model -> String
picknextMs model =
    --keyslist = List.map (\(k, _ , _) -> k ) model.ms
    case (List.length model.ms) of
        0 -> "msa"
        1 -> "msb"  
        2 -> "msc"
        _ -> "msd"

updateSlotYear : (String, Int , Slot) -> String  -> Int -> (String, Int , Slot)
updateSlotYear (p, q, r) key  newValue =
    if p == key then
        (p, q, { r | year = newValue })
    else
        (p, q, r)
    

updateSlotMonth : (String, Int , Slot) -> String -> Int  -> (String, Int , Slot)
updateSlotMonth (p, q, r) key newValue =
    if p == key then
        (p, q, { r | month = newValue })
    else
        (p, q, r)

updateSlotDay : (String, Int , Slot) -> String -> Int  -> (String, Int , Slot)
updateSlotDay (p, q, r) key newValue =
    if p == key then
        (p, q, { r | day = newValue })
    else
        (p, q, r)

updateSlotHH : (String, Int , Slot) -> String -> Int  -> (String, Int , Slot)
updateSlotHH (p, q, r) key newValue =
    if p == key then
        (p, q, { r | hh = newValue })
    else
        (p, q, r)

updateSlotMM : (String, Int , Slot) -> String -> Int  -> (String, Int , Slot)
updateSlotMM (p, q, r) key newValue =
    if p == key then
        (p, q, { r | mm = newValue })
    else
        (p, q, r)

displaySlots : Model -> Html Msg
displaySlots model = 
        List.map (\(key, _, slot) -> timeSlotElement slot key ) model.ms
            |> div [Html.Attributes.id "34rr"]  
            
      
    

timeSlotElement : Slot -> String -> Html Msg
timeSlotElement slot qkey =
    div [Html.Attributes.id qkey, Html.Attributes.style "padding-bottom" "20px", Html.Attributes.style "font-family" "Gill Sans"][
        input [ type_ "text", size 4,  placeholder "YYYY", value (String.fromInt slot.year) , onInput (YearUpdatea qkey) , Html.Attributes.style "font-family" "Gill Sans"] []
        , Html.text "-"
        , input [ type_ "text", size 2, placeholder "MM", value (String.fromInt slot.month) , onInput (MonthUpdatea qkey) , Html.Attributes.style "font-family" "Gill Sans"] []
        , Html.text "-"
        , input [ type_ "text", size 2, placeholder "DD", value (String.fromInt slot.day) , onInput (DayUpdatea qkey) , Html.Attributes.style "font-family" "Gill Sans"] []
        , Html.text "         "
        , input [ type_ "text", size 2, placeholder "HH", value (String.fromInt slot.hh), onInput (HourUpdatea qkey) , Html.Attributes.style "font-family" "Gill Sans"] []
        , Html.text ":"
        , input [ type_ "text", size 2, placeholder "MM", value (String.fromInt slot.mm), onInput (MinUpdatea qkey) , Html.Attributes.style "font-family" "Gill Sans"] []
        , Html.text "         "
        , button [ onClick (RemoveSlot qkey)]  [ Html.text "🗑️" ]
                            
    ]

          
             
viewLink : String -> String -> Html Msg
viewLink text hrefPath =
  Html.li [] [ Html.a [ Html.Attributes.href hrefPath ] [ Html.text text ] ]


splitQueryString : String -> List(String, String)
splitQueryString queryString =
    let
        pairs = queryString
                    |> String.split "&"
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

getAllValueOfmsKey : List (String, String)  -> List Int
getAllValueOfmsKey list  =
     List.filter (\(key, _) -> String.startsWith "ms" key) list
        |>  List.map (\(_, value) ->  (Maybe.withDefault 0 (String.toInt value )))

filtermsKey : List (String, String)  -> List (String, String) 
filtermsKey list  =
     List.filter (\(key, _) -> String.startsWith "ms"  key) list
      
convertQueryToMS : Time.Zone ->  List (String, String) -> List (String, Int, Slot)
convertQueryToMS  zone  list=
        list
          |> List.map(\(k,v) -> (k, Maybe.withDefault 0 (String.toInt v), posixToSlot (Time.millisToPosix (Maybe.withDefault 0 (String.toInt v))) zone ))

posixToSlot : Time.Posix -> Time.Zone -> Slot
posixToSlot posix zone =
    Slot (Time.toYear zone posix ) ( Date.monthToNumber (Time.toMonth zone posix)) (Time.toDay zone posix) (Time.toHour zone posix) (Time.toMinute zone posix)

dateToMillsecSlot : List (String, Int, Slot) -> Bool -> Url -> Html Msg
dateToMillsecSlot mslist link url =
        if link then
             ((textCustom ( toStringUrl url) ) ::  dateToMillsecList mslist)
                      |> div [ Html.Attributes.id "genUrl", Html.Attributes.style  "border-style" "ridge", Html.Attributes.style "display" "inline" ]

        else 
            div[][]

textCustom : String -> Html Msg
textCustom url =
    Html.text url

toStringUrl : Url -> String
toStringUrl url =
  let
    http =
      case url.protocol of
        Http ->
          "http://"

        Https ->
          "https://"
  in
  addPort url.port_ (http ++ url.host) ++ url.path

addPort : Maybe Int -> String -> String
addPort maybePort starter =
  case maybePort of
    Nothing ->
      starter

    Just port_ ->
      starter ++ ":" ++ String.fromInt port_


dateToMillsecList : List (String, Int, Slot)  -> List (Html Msg)
dateToMillsecList mslist =
    List.map (\(k, _ , s) -> (dateToMillsec k s.year s.month s.day s.hh s.mm ) ) mslist         

dateToMillsec : String -> Int -> Int -> Int -> Int-> Int -> Html Msg
dateToMillsec nodeid year month day hh mm  =
        Html.node "time-millisec"
            [ attribute "id" nodeid
            , attribute "year" (String.fromInt year)
            , attribute "month" (String.fromInt month)
            , attribute "day" (String.fromInt day)
            , attribute "hh" (String.fromInt hh)
            , attribute "mm" (String.fromInt mm)
            ]
            [ ]

copyUrlText : Model ->  Html Msg
copyUrlText model=
    if model.copyText /= "" then
        Html.node "copy-url"
                [ attribute "id" "genUrl"] []
    else
        div[][]


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
