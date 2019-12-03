module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, string, string, list, map3)

main = Browser.element
  { init = startIt
  , update = updateIt
  , subscriptions = noSubscriptions
  , view = showIt
  }

type alias Person =
  { id: Int
  , firstName: String
  , lastName: String

  }

type alias Model =
  { person: Maybe Person
  , msg: String
  }

type Msg
  = FetchPerson1
  | GotPerson (Result Http.Error Person)


startIt: () -> (Model, Cmd Msg)
startIt _ = (Model Nothing "OK", Cmd.none)

updateIt:  Msg -> Model -> (Model, Int -> Cmd Msg)
updateIt message model =
  case message of
    --FetchPerson -> (model, fetchPerson1)

    FetchPerson1 -> (model, fetchPerson1)

    GotPerson (Ok p) -> ({ model | person = Just p },Cmd.none)

    GotPerson (Err err) -> ({ model | msg = (printError err) }, Cmd.none)

printError: Http.Error -> String
printError error =
  case error of
    Http.BadBody m -> "Bad body "++m
    Http.BadUrl u -> "Bad URL: "++u
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "Network panic"
    Http.BadStatus i -> "Bad Status: "++(String.fromInt i)

        
fetchPerson: Cmd Msg
fetchPerson =
    Http.get
        {url = "http://localhost:5000/getmember/1"
        , expect = Http.expectJson GotPerson personDecoder }

-- hvad med fetchPerson: Int -> Cmd Msg
fetchPerson1: Int -> Cmd Msg
fetchPerson1 int =
    Http.get
        {url = "http://localhost:5000/getmember/" ++ String.fromInt(int)
        , expect = Http.expectJson GotPerson personDecoder }


personDecoder: Decoder Person
personDecoder =
    map3 Person
        (field "id" int)
        (field "firstName" string)
        (field "lastName" string)

noSubscriptions: Model -> Sub Msg
noSubscriptions model =
  Sub.none

showIt: Model -> Html Msg
showIt model =
  div []
    [ input [ placeholder "Insert ID"] []
    , button [onClick FetchPerson1] [text "Fetch person"]
    , br [] []
    , text ("Person is "++(getName model.person))
    , br [] []
    , text model.msg
    , hr [] []
   
    ]

getName: Maybe Person -> String
getName mp =
    case mp of 
        Just person -> person.firstName++" "++(person.lastName)
        Nothing -> "..."

