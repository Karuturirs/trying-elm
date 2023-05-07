module Main exposing (..)


type alias MyTuple = (String, Int)

myList : List MyTuple
myList =
    [ ( "msa", 1 )
    , ( "b", 2 )
    , ( "msc", 3 )
    ]


getValueOfmsKey : List (String, String)  -> List String
getValueOfmsKey list  =
     List.filter (\(key, _) -> String.startsWith key "ms") list
        |>  List.map (\(_, value) ->  value )