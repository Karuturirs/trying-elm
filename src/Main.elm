module Main exposing (main)

import Browser
import Html exposing (..)
import List exposing (..)
import Random exposing (..)

-- Define the image URLs
imageUrls =
    [ "https://picsum.photos/200/300?random=1"
    , "https://picsum.photos/200/300?random=2"
    , "https://picsum.photos/200/300?random=3"
    , "https://picsum.photos/200/300?random=4"
    , "https://picsum.photos/200/300?random=5"
    ]

-- Define the model
type alias Model =
    { images : List String
    }

-- Define the initial model
initModel : Model
initModel =
    { images = [] }

-- Define the Msg type
type Msg
    = NewImages (List String)

-- Define the update function
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewImages newImages ->
            ( { model | images = newImages }, Cmd.none )

-- Define the view function
view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GetImages ] [ text "Show Random Images" ]
        , div [] (List.map (\imgUrl -> img [src imgUrl] []) model.images)
        ]

-- Define the GetImages message and the function to generate a random selection of images
type GetImages = GetImages

getImages : Cmd Msg
getImages =
    let
        getRandomImages =
            getRandomList 3 (0, length imageUrls - 1)
                |> map (\indices -> List.map (\index -> imageUrls !! index) indices)
    in
    Task.perform NewImages getRandomImages

-- Define the update function for GetImages
updateGetImages : Msg -> Model -> ( Model, Cmd Msg )
updateGetImages GetImages model =
    ( model, getImages )

-- Define the main function
main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, getImages )
        , view = view
        , update = updateGetImages
        , subscriptions = \_ -> Sub.none
        }
