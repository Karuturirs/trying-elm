module Forms exposing (..)

import Browser
import Html exposing (Html, div, input, text, form)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import ReverseString exposing (Msg)
import String exposing (any)
import Char exposing (isUpper)
import Char exposing (isLower)
import Char exposing (isDigit)
-- MAIN

main = 
    Browser.sandbox { init = init, update = update, view = view}

-- MODEL

type alias Model = 
    {
        name: String
        , password : String
        , passwordAgain : String
    }

init : Model
init = 
    Model "" "" ""

-- UPDATE
type Msg 
    = Name String
    | Password String
    | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name -> 
            { model | name = name }

        Password password ->
            { model | password= password }

        PasswordAgain password -> 
            { model | passwordAgain = password }

-- VIEW

view : Model -> Html Msg
view model =
    div [] 
    [ viewInput  "text" "Enter Name" model.name Name 
    , viewInput "password" "Enter password" model.password Password 
    , viewInput "password" "Re-Enter password" model.passwordAgain  PasswordAgain
    , viewValidation model
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    viewValidateLength model
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]


viewValidateLength : Model -> Html msg
viewValidateLength model = 
    if String.length model.password <= 8 then
         div [ style "color" "red" ] [ text "Passwords Should be more than 8 Charaters" ]
    else
        viewValidateAllCases model

viewValidateAllCases : Model -> Html msg    
viewValidateAllCases model = 
    if hasUpperCase model.password then
        if hasLowerCase model.password then
            if hasNumeric model.password then
                div [ style "color" "green" ] [ text "OK" ]
            else
                div [ style "color" "red" ] [ text "Passwords Should be atleast one Numeric" ] 
        else
           div [ style "color" "red" ] [ text "Passwords Should be atleast one LowerCase" ]  
    else
       div [ style "color" "red" ] [ text "Passwords Should be atleast one UpperCase" ] 




hasUpperCase : String -> Bool
hasUpperCase str =
    any isUpper str


hasLowerCase : String -> Bool
hasLowerCase str =
    any isLower str

hasNumeric : String -> Bool
hasNumeric str =
    any isDigit str
