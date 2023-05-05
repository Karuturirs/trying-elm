module Main-better exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Time exposing (Posix)
import Url


type alias Model =
    { selectedDate : Posix
    , selectedTime : Posix
    }


type Msg
    = SetSelectedDate Posix
    | SetSelectedTime Posix


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initialModel : Model
initialModel =
    { selectedDate = Time.millisToPosix 0
    , selectedTime = Time.millisToPosix 0
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Select a date and time:" ]
        , datePicker model.selectedDate SetSelectedDate
        , timePicker model.selectedTime SetSelectedTime
        , button [ onClick ShareClicked ] [ text "Share URL" ]
        ]


datePicker : Posix -> (Posix -> Msg) -> Html Msg
datePicker date onDateChange =
    -- TODO: Implement a date picker component


timePicker : Posix -> (Posix -> Msg) -> Html Msg
timePicker time onTimeChange =
    -- TODO: Implement a time picker component


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSelectedDate date ->
            ( { model | selectedDate = date }, Cmd.none )

        SetSelectedTime time ->
            ( { model | selectedTime = time }, Cmd.none )


type UrlData
    = UrlData Posix Posix


urlDataEncoder : UrlData -> Url.Builder
urlDataEncoder (UrlData date time) =
    Url.string "date" (Time.format "%Y-%m-%d" date)
        |> Url.string "time" (Time.format "%H:%M:%S" time)


urlDataDecoder : Url.Parser UrlData
urlDataDecoder =
    Url.map2 UrlData
        (Url.s "date" Url.date)
        (Url.s "time" Url.time)


parseUrlData : String -> Maybe UrlData
parseUrlData url =
    Url.fromString url
        |> Maybe.andThen (Url.parse urlDataDecoder)


shareUrl : Model -> String
shareUrl model =
    Url.Builder.crossOrigin "anonymous" "//example.com"
        |> Url.addPathSegments [ "app" ]
        |> Url.addQueryParams [ urlDataEncoder (UrlData model.selectedDate model.selectedTime) ]
        |> Url.build
        |> Maybe.withDefault "#"


ShareClicked : Msg
ShareClicked =
    let
        url = shareUrl initialModel
    in
    case Url.fromString url of
        Just _ ->
            Browser.Navigation.load url

        Nothing ->
            -- TODO: Handle invalid URL case
            ( initialModel, Cmd.none )
