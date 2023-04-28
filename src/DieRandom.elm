module DieRandom exposing (..)

-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--

import Browser
import Html exposing (..)
import Html exposing (Html, text, img)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (align, src)


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
        diesFace : String
    
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" ,
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
      (Model (dieImg x)
      , Cmd.none)
    


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [align "center"]
    [ img [ src  model.diesFace  ] []
    , br [] []
    , button [ onClick Roll ] [ text "Roll" ]
    ]



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
