module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Butterfly.Api as Api
import Html exposing (Html, div, main_, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Modal
import NavBar
import Page exposing (Page)
import Page.Area as Area
import Page.Category as Category
import Page.Description as Desc
import Page.Dictionary as Dic
import Page.Error as Error
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
    | Error Session


getKey : Model -> Nav.Key
getKey model =
    case model of
        Error s ->
            S.getKey s

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
        Error s ->
            s

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
            Dic.getSession m


updateSession : Model -> Session -> Model
updateSession model s =
    case model of
        Error _ ->
            Error s

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
            Dictionary (Dic.updateSession m s)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        initSession =
            S.init key NavBar.init Modal.init
    in
    changeRouteTo (Route.parseUrl url) (Home initSession)


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            S.update S.DisableMenu <| getSession model
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
            updateWith Dictionary GotDictionaryMessage (Dic.init session)

        Just Route.Error ->
            ( Error session, Cmd.none )

        Just Route.ToggleMenu ->
            NavBar.update NavBar.ToggleMenu session.navModel
                |> updateWith (\navmodel -> S.update (S.UpdateNavbar navmodel) session |> updateSession model) GotNavBarMessage



-- Map over submodel as well as submsg to main


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | GotDictionaryMessage Dic.Msg
    | GotNavBarMessage NavBar.Msg
    | NoOp
    | MainClicked
    | GotModalMessage Modal.Msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (getKey model) (Url.toString url) )

                Browser.External "" ->
                    ( model, Cmd.none )

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
                |> updateWith (\navmodel -> S.update (S.UpdateNavbar navmodel) session |> updateSession model) GotNavBarMessage

        ( GotModalMessage modalMsg, someModel ) ->
            let
                session =
                    getSession someModel

                ( subMsg, subCmd ) =
                    Modal.update modalMsg session.modalModel
            in
            updateWith
                (\modalModel -> S.update (S.UpdateModal modalModel) session |> updateSession someModel)
                GotModalMessage
                ( subMsg, subCmd )

        ( GotNavBarMessage navMsg, someModel ) ->
            let
                session =
                    getSession someModel

                ( subMsg, subCmd ) =
                    NavBar.update navMsg session.navModel
            in
            updateWith
                (\navbarModel -> S.update (S.UpdateNavbar navbarModel) session |> updateSession someModel)
                GotNavBarMessage
                ( subMsg, subCmd )

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model of
        Error s ->
            toViewNoOp s Page.Error Error.title Error.view

        NotFound s ->
            toViewNoOp s Page.NotFound NotFound.title NotFound.view

        Home s ->
            toViewNoOp s Page.Home Home.title Home.view

        Reference s ->
            toViewNoOp s Page.Reference Ref.title Ref.view

        Category s ->
            toViewNoOp s Page.Category Category.title Category.view

        Description s ->
            toViewNoOp s Page.Description Desc.title Desc.view

        Area s ->
            toViewNoOp s Page.Area Area.title Area.view

        Dictionary submodel ->
            toView (Dic.getSession submodel) Page.Dictionary GotDictionaryMessage Dic.title (Dic.view submodel)


toViewNoOp : Session -> Page -> String -> Html msg -> Browser.Document Msg
toViewNoOp session page title content =
    let
        body =
            mainView session page (heroView title (\_ -> NoOp) content)
    in
    Browser.Document title body


toView : Session -> Page -> (msg -> Msg) -> String -> Html msg -> Browser.Document Msg
toView session page toMsg title content =
    let
        body =
            mainView session page (sectionView toMsg content)
    in
    Browser.Document title body


mainView : Session -> Page -> Html Msg -> List (Html Msg)
mainView session page content =
    [ main_ []
        [ Html.map GotModalMessage <| Modal.view session.modalModel
        , Html.map GotNavBarMessage <| NavBar.view page session.navModel
        , columns myColumnsModifiers
            [ onClick MainClicked ]
            [ content ]
        ]
    ]


sectionView : (msg -> Msg) -> Html msg -> Html Msg
sectionView toMsg content =
    section NotSpaced
        [ class "content section-view" ]
        [ Html.map toMsg content ]


heroView : String -> (msg -> Msg) -> Html msg -> Html Msg
heroView t toMsg content =
    div [ class "column is-8 is-offset-2" ]
        [ hero myHeroModifiers
            []
            [ heroBody []
                [ title H2
                    []
                    [ text t ]
                ]
            , section NotSpaced
                [ class "content" ]
                [ Html.map toMsg content
                ]
            ]
        ]


myHeroModifiers : HeroModifiers
myHeroModifiers =
    { bold = False
    , size = Small
    , color = Default
    }


myColumnsModifiers : ColumnsModifiers
myColumnsModifiers =
    { multiline = False
    , gap = Gap0
    , display = TabletAndBeyond
    , centered = True
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
