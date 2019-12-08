module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (Decoder, field, int, string, map3, string)



-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- USER
type alias Member =
  {
      id: Int
      ,firstName: String
      ,lastName: String
  }

type alias ListModel =
  {
    memberList: List Member
  , msg: String
  }

type alias Value =
  { 
      value : String
  }

-- MODEL
type Model
  = Failure
  | Loading
  | Success Member
  | SuccessAll (List Member)



init : () -> (Model, Cmd Msg)
init _ =
    (Loading, getSingleMember("2"))
  


-- UPDATE
type Msg
  = MorePlease
  | SingleMemberButton
  | GetAllMembersButton
  | GotMember (Result Http.Error Member)
  | GotAllMembersList (Result Http.Error (List Member))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getSingleMember("2"))

    SingleMemberButton ->
      (Loading, getSingleMember("2"))
    
    GetAllMembersButton ->
      (Loading, getAllMembers)

    GotMember result ->
      case result of
        Ok url ->
          (Success url, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)

    GotAllMembersList result ->
      case result of
        Ok url ->
          (SuccessAll url, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ hr [] []
    , searchMember model
    , viewTable model
    ]

searchMember : Model -> Html Msg
searchMember model = 
    div []
        [ 
        h5 [] [text "Get all members"]
        , button [ onClick GetAllMembersButton] [ text "Get all" ]
        , br [] []
        , h5 [] [text "Get member by id"]
        , input [type_ "text", placeholder "id" ] []
        , br [] []
        , button [ onClick SingleMemberButton] [ text "Get member" ]
        , hr [] []
        ]

viewTable : Model -> Html Msg
viewTable model =
  case model of
    Failure ->
      div []
        [ text "I could not load any member. " ]

    Loading ->
      text "Loading..."

    Success user ->
      div []
        [ 
        table [] 
            [
            tr []
                [
                    th[] [text "ID "]
                    , th[] [text "Firstname "]
                    , th[] [text "Lastname "]
                ]
            ,tr [] 
                [
                    td[] [text (String.fromInt user.id)]
                    ,td[] [text user.firstName]
                    ,td[] [text user.lastName]
                ]
            ]
        ]
    
    SuccessAll list ->
      table []
        ([ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Firstname" ]
            , th [] [ text "Lastname"]
            ]
         ]
            ++ List.map showUser list
        )

showUser: Member -> Html Msg
showUser member =
    tr [] 
        [
            td[] [text (String.fromInt member.id)]
            ,td[] [text member.firstName]
            ,td[] [text member.lastName]
        ]

-- HTTP

getSingleMember : String -> Cmd Msg
getSingleMember id = 
    Http.get
        { url = "http://localhost:5000/getmember/"++id
        , expect = Http.expectJson GotMember memberDecoder
        }

getAllMembers : Cmd Msg
getAllMembers =
    Http.get
        { url = "http://localhost:5000/getmembers"
        , expect = Http.expectJson GotAllMembersList memberListDecoder
        }

-- JSON DECODERS

memberDecoder: Decoder Member
memberDecoder = map3 Member (field "id" int) (field "firstName" string) (field "lastName" string)

memberListDecoder: Decoder ( List Member )
memberListDecoder = JD.list memberDecoder
  

msgDecoder : Decoder String
msgDecoder = 
  field "name" string