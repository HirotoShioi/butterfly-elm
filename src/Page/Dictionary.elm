module Page.Dictionary exposing (..)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)


type Msg
    = Click


type SearchTerm
    = Name String
    | Color String
    | Region Region
    | Category String


type Region
    = OldNorth
    | NewNorth
    | NewTropical
    | TropicalAfrica
    | IndiaAustralia


type alias Model =
    { session : Session
    , butterflies : List Butterfly
    , searchName : Maybe String
    , searchColor : Maybe String
    , searchRegion : Maybe Region
    , searchCategory : Maybe String
    }


type alias Butterfly =
    { jp_name : String
    , eng_name : String
    , category : String
    }


init : Session -> Model
init session =
    Model session [] Nothing Nothing Nothing Nothing


getKey : Model -> Nav.Key
getKey model =
    model.session.key


getSession : Model -> Session
getSession model =
    model.session


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "蝶の図鑑"
    , content =
        div []
            [ h1 [] [ text "This is Dictionary view" ]
            , ul [] <|
                List.map showButterflies dummyButterflies
            ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( model, Cmd.none )


dummyButterflies : List Butterfly
dummyButterflies =
    [ Butterfly "a" "a" "a"
    , Butterfly "b" "b" "b"
    , Butterfly "c" "c" "c"
    ]


showButterflies : Butterfly -> Html Msg
showButterflies butterfly =
    li [] [ text butterfly.jp_name ]
