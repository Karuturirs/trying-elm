module DieRandom exposing (..)

-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--

import Browser
import Html exposing (..)
import Html exposing (Html, text)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (align, src )
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Debug exposing (toString)

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias Model =
    {
        diesFace : Int
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1 ,
    Random.generate Newface (Random.int 1 6)
  )



-- UPDATE


type Msg
  = Roll |
    Newface Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate Newface (Random.int 1 6)
      )
    Newface x ->
      ( Model x
      , Cmd.none)
    


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [align "center"]
    [ h1 [] [Html.text (toString model.diesFace)] 
    , svg
            [ width "120", height "120", viewBox "0 0 120 120" ]
            ( List.append
                [ rect [  width "100", height "100", rx "10", ry "10" ] [] ]
                [  g [fill "white"] 
                ( dieSvg model.diesFace )
                ]
            )
    , br [] []
    , button [ onClick Roll ] [ Html.text "Roll" ]
    ]

dieSvg :Int -> List ( Svg Msg)
dieSvg  num =
    case num of
        1 -> 
           [ circle [cx "50", cy "50", r "8"] [] ]
           
        
        2 -> 
            [ circle [cx "20", cy "20", r "8"] []
            , circle [cx "80", cy "80", r "8"] [] 
            ]
        3 -> 
            [ circle [cx "80", cy "20", r "8"] []
            , circle [cx "50", cy "50", r "8"] [] 
            , circle [cx "20", cy "80", r "8"] []
            ]
        4 -> 
            [ circle [cx "20", cy "20", r "8"] []
            , circle [cx "80", cy "20", r "8"] [] 
            , circle [cx "20", cy "80", r "8"] []
            , circle [cx "80", cy "80", r "8"] []
            ]
        5 -> 
            [ circle [cx "20", cy "20", r "8"] []
            , circle [cx "50", cy "50", r "8"] [] 
            , circle [cx "80", cy "20", r "8"] []
            , circle [cx "20", cy "80", r "8"] []
            , circle [cx "80", cy "80", r "8"] []
            ]
        6 -> 
            [ circle [cx "20", cy "20", r "8"] []
            , circle [cx "50", cy "20", r "8"] [] 
            , circle [cx "80", cy "20", r "8"] []
            , circle [cx "20", cy "80", r "8"] []
            , circle [cx "50", cy "80", r "8"] []
            , circle [cx "80", cy "80", r "8"] []
            ]
        _ -> 
            []
           
        

dieImg : Int -> String
dieImg num  =
    case num of 
        1 -> 
            "../docs/1.png"
        2 ->
            "../docs/2.png"
        3 -> 
            "../docs/3.png"
        4 ->
            "../docs/4.png"
        5 ->
            "../docs/5.png"
        6 ->
            "../docs/6.png"
        _ ->
            "../docs/1.png"
