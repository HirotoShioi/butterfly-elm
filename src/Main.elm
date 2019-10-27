module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Bulma.CDN exposing (stylesheet)
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Html exposing (Html, main_, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import NavBar
import Page exposing (Page)
import Page.Area as Area
import Page.Category as Category
import Page.Description as Desc
import Page.Dictionary as Dic
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Reference as Ref
import Route exposing (Route)
import Session as S exposing (Session)
import Url



---- MODEL ----


type Model
    = Home Session
    | NotFound Session
    | Reference Session
    | Area Session
    | Category Session
    | Description Session
    | Dictionary Dic.Model


getKey : Model -> Nav.Key
getKey model =
    case model of
        Home s ->
            S.getKey s

        NotFound s ->
            S.getKey s

        Reference s ->
            S.getKey s

        Area s ->
            S.getKey s

        Category s ->
            S.getKey s

        Description s ->
            S.getKey s

        Dictionary m ->
            Dic.getKey m


getSession : Model -> Session
getSession model =
    case model of
        Home s ->
            s

        NotFound s ->
            s

        Reference s ->
            s

        Area s ->
            s

        Category s ->
            s

        Description s ->
            s

        Dictionary m ->
            m.session


updateSession : Model -> Session -> Model
updateSession model s =
    case model of
        Home _ ->
            Home s

        NotFound _ ->
            NotFound s

        Reference _ ->
            Reference s

        Area _ ->
            Area s

        Category _ ->
            Category s

        Description _ ->
            Description s

        Dictionary m ->
            Dictionary { m | session = s }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        initSession =
            S.init key NavBar.init
    in
    changeRouteTo (Route.parseUrl url) (Home initSession)


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            S.disableMenu <| getSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            ( Home session, Cmd.none )

        Just Route.Reference ->
            ( Reference session, Cmd.none )

        Just Route.Description ->
            ( Description session, Cmd.none )

        Just Route.Category ->
            ( Category session, Cmd.none )

        Just Route.Area ->
            ( Area session, Cmd.none )

        Just Route.Dictionary ->
            ( Dictionary <| Dic.init session, Cmd.none )

        Just Route.ToggleMenu ->
            NavBar.update NavBar.ToggleMenu session.navModel
                |> updateWith (\n -> S.updateNavModel session n |> updateSession model) GotNavBarMessage


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | GotDictionaryMessage Dic.Msg
    | GotNavBarMessage NavBar.Msg
    | NoOp
    | MainClicked



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (getKey model) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.parseUrl url) model

        ( GotDictionaryMessage submsg, Dictionary submodel ) ->
            Dic.update submsg submodel
                |> updateWith Dictionary GotDictionaryMessage

        ( MainClicked, somemodel ) ->
            let
                session =
                    getSession somemodel
            in
            NavBar.update NavBar.DisableMenu session.navModel
                |> updateWith (\n -> S.updateNavModel session n |> updateSession model) GotNavBarMessage

        ( _, _ ) ->
            ( model, Cmd.none )



-- Map over submodel as well as submsg to main


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model of
        NotFound s ->
            toViewNoOp s Page.NotFound NotFound.view

        Home s ->
            toViewNoOp s Page.Home Home.view

        Reference s ->
            toViewNoOp s Page.Reference Ref.view

        Category s ->
            toViewNoOp s Page.Category Category.view

        Description s ->
            toViewNoOp s Page.Description Desc.view

        Area s ->
            toViewNoOp s Page.Area Area.view

        Dictionary submodel ->
            toView (Dic.getSession submodel) Page.Dictionary GotDictionaryMessage (Dic.view submodel)


toViewNoOp : Session -> Page -> { title : String, content : Html msg } -> Browser.Document Msg
toViewNoOp session page { title, content } =
    let
        body =
            [ main_ []
                [ stylesheet
                , Html.map GotNavBarMessage <| NavBar.view page session.navModel
                , heroView (\_ -> NoOp) title content
                ]
            ]
    in
    Browser.Document title body


toView : Session -> Page -> (msg -> Msg) -> { title : String, content : Html msg } -> Browser.Document Msg
toView session page toMsg { title, content } =
    let
        body =
            [ main_ []
                [ stylesheet
                , Html.map GotNavBarMessage <| NavBar.view page session.navModel
                , sectionView toMsg content
                ]
            ]
    in
    Browser.Document title body


sectionView : (msg -> Msg) -> Html msg -> Html Msg
sectionView toMsg content =
    section NotSpaced
        [ onClick MainClicked ]
        [ container []
            [ Html.map toMsg content ]
        ]


heroView : (msg -> Msg) -> String -> Html msg -> Html Msg
heroView toMsg t content =
    hero myHeroModifiers
        []
        [ hero myHeroModifiers
            [ onClick MainClicked ]
            [ heroBody []
                [ container []
                    [ title H2
                        []
                        [ text t ]
                    ]
                ]
            , section NotSpaced
                []
                [ container [] [ Html.map toMsg content ]
                ]
            ]
        ]


myHeroModifiers : HeroModifiers
myHeroModifiers =
    { bold = False
    , size = Small
    , color = Default
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
