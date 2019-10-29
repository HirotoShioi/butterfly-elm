module Page.Dictionary exposing (..)

import Browser.Navigation as Nav
import Butterfly.Api as Api exposing (Msg(..))
import Butterfly.Type exposing (Butterfly)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Session exposing (Session)


type Msg
    = Click
    | GotButterflyResponse Api.Msg


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


init : Session -> Model
init session =
    Model session [] Nothing Nothing Nothing Nothing


getKey : Model -> Nav.Key
getKey model =
    model.session.key


getSession : Model -> Session
getSession model =
    model.session


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "This is Dictionary view" ]
        , ul [] <|
            List.map showButterflies model.butterflies
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( model, Cmd.none )

        GotButterflyResponse apimsg ->
            case apimsg of
                Api.GotButterflies (Ok butterflies) ->
                    ( { model | butterflies = List.take 100 butterflies }, Cmd.none )

                Api.GotButterflies (Err err) ->
                    ( model, Cmd.none )


showButterflies : Butterfly -> Html Msg
showButterflies butterfly =
    li [] [ text butterfly.jp_name ]


title : String
title =
    Page.dictionaryTitle
