module Page.Dictionary exposing (..)

import Browser.Navigation as Nav
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements as E exposing (..)
import Bulma.Modifiers as Mod
import Bulma.Modifiers.Typography exposing (..)
import Butterfly.Api as Api exposing (Msg(..))
import Butterfly.Type exposing (Butterfly)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick)
import Page
import Route
import Session exposing (Session)


type Msg
    = Click
    | GotButterflyResponse Api.Msg
    | ButterflyClicked Butterfly


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


type Model
    = Loading Session
    | Result ResultModel


type alias ResultModel =
    { session : Session
    , butterflies : List Butterfly
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Loading session, Cmd.map GotButterflyResponse Api.getButterflies )


getKey : Model -> Nav.Key
getKey model =
    case model of
        Loading s ->
            s.key

        Result m ->
            m.session.key


getSession : Model -> Session
getSession model =
    case model of
        Loading s ->
            s

        Result m ->
            m.session


updateSession : Model -> Session -> Model
updateSession model session =
    case model of
        Loading _ ->
            Loading session

        Result m ->
            Result { m | session = session }


view : Model -> Html Msg
view model =
    case model of
        Loading s ->
            loadingView

        Result m ->
            resultView m


loadingView : Html Msg
loadingView =
    div []
        [ text "Loading..."
        , E.progress loadingProgressModifiers [ A.max "100", class "loading" ] [ text "50%" ]
        ]


loadingProgressModifiers : ProgressModifiers
loadingProgressModifiers =
    ProgressModifiers Mod.Medium Mod.Info


resultView : ResultModel -> Html Msg
resultView model =
    div [ class "dictionary-view" ]
        [ columns myColumnsModifiers [] <| List.map showButterflies model.butterflies
        ]


myColumnsModifiers : ColumnsModifiers
myColumnsModifiers =
    { multiline = True
    , gap = Gap3
    , display = TabletAndBeyond
    , centered = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( model, Cmd.none )

        GotButterflyResponse apiMsg ->
            case apiMsg of
                Api.GotButterflies (Ok butterflies) ->
                    let
                        session =
                            getSession model

                        bs =
                            List.take 100 butterflies
                    in
                    ( Result <| ResultModel session bs, Cmd.none )

                Api.GotButterflies (Err err) ->
                    let
                        key =
                            getKey model

                        route =
                            Route.routeToString Route.Error
                    in
                    ( model, Nav.pushUrl key route )

        ButterflyClicked butterfly ->
            let
                session =
                    getSession model

                newSession =
                    Session.update (Session.EnableModal butterfly) session
            in
            ( updateSession model newSession, Cmd.none )


showButterflies : Butterfly -> Html Msg
showButterflies butterfly =
    div [ class "column is-one-third-tablet is-one-fifth-desktop" ]
        [ card [ class "butterfly-card", onClick (ButterflyClicked butterfly) ]
            [ cardImage [] [ butterflyImage butterfly.img_src ]
            , cardContent [ textCentered, textSize Small ]
                [ div []
                    [ text butterfly.jp_name
                    , div [ class "content", textColor Grey ] [ text butterfly.eng_name ]
                    ]
                ]
            ]
        ]


butterflyImage : String -> Html msg
butterflyImage img_src =
    image SixteenByNine
        []
        [ img [ src <| String.concat [ "http://biokite.com/worldbutterfly/", img_src ] ] []
        ]


title : String
title =
    Page.dictionaryTitle
