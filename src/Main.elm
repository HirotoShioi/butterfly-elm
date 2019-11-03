module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Bulma.Columns exposing (ColumnsModifiers, Display(..), Gap(..), columns)
import Bulma.Elements exposing (TitleSize(..), title)
import Bulma.Layout exposing (HeroModifiers, SectionSpacing(..), hero, heroBody, section)
import Bulma.Modifiers exposing (Color(..), Size(..))
import Butterfly.Api as Api
import Html exposing (Html, div, main_, text)
import Html.Attributes exposing (class)
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
import Page.Loading as Loading
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
    | Loading Session Url.Url -- Page that will load after session initiation has been completed


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

        Loading s _ ->
            S.getKey s


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

        Loading s _ ->
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

        Loading _ url ->
            Loading s url

        Dictionary m ->
            Dictionary (Dic.updateSession m s)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( initSession, sessionCmd ) =
            S.init key NavBar.init Nothing

        liftedSessionCmd =
            Cmd.map GotSessionMsg sessionCmd

        ( model, cmd ) =
            changeRouteTo (Route.parseUrl url) (Loading initSession url)

        -- Go Loading page
    in
    ( model, Cmd.batch [ cmd, liftedSessionCmd ] )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        ( session, _ ) =
            S.update S.DisableMenu <| getSession model
    in
    case model of
        Loading s url ->
            ( Loading s url, Cmd.none )

        _ ->
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
    | GotSessionMsg S.Msg



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

        ( MainClicked, someModel ) ->
            let
                session =
                    getSession someModel

                ( navModel, navCmd ) =
                    NavBar.update NavBar.DisableMenu session.navModel

                ( updatedSession, sessionCmd ) =
                    S.update (S.UpdateNavbar navModel) session
            in
            ( updateSession someModel updatedSession
            , Cmd.batch
                [ Cmd.map GotNavBarMessage navCmd
                , Cmd.map GotSessionMsg sessionCmd
                ]
            )

        ( GotModalMessage modalMsg, someModel ) ->
            let
                session =
                    getSession someModel

                ( updatedSession, cmd ) =
                    Modal.update modalMsg session
            in
            ( updateSession someModel updatedSession
            , Cmd.map GotModalMessage cmd
            )

        ( GotNavBarMessage navMsg, someModel ) ->
            let
                session =
                    getSession someModel

                ( navbarModel, navCmd ) =
                    NavBar.update navMsg session.navModel

                ( updatedSession, sessionCmd ) =
                    S.update (S.UpdateNavbar navbarModel) session
            in
            ( updateSession someModel updatedSession
            , Cmd.batch
                [ Cmd.map GotNavBarMessage navCmd
                , Cmd.map GotSessionMsg sessionCmd
                ]
            )

        ( GotSessionMsg sessionMsg, Loading s url ) ->
            case sessionMsg of
                -- Intercept the Api response in order to redirect pages
                S.GotButterflyResponse (Api.GotButterflies res) ->
                    case res of
                        Ok _ ->
                            let
                                ( session, cmd ) =
                                    S.update sessionMsg s

                                ( someModel, routeCmd ) =
                                    changeRouteTo (Route.parseUrl url) (Home session)
                            in
                            ( someModel, Cmd.batch [ routeCmd, Cmd.map GotSessionMsg cmd ] )

                        Err _ ->
                            ( Error s, Cmd.none )

                _ ->
                    S.update sessionMsg s
                        |> updateWith (updateSession (Error s)) GotSessionMsg

        ( GotSessionMsg sessionMsg, someModel ) ->
            S.update sessionMsg (getSession someModel)
                |> updateWith (updateSession someModel) GotSessionMsg

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model of
        Error s ->
            toViewNoOp s Page.Error Error.view

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

        Loading s _ ->
            -- Loading
            Browser.Document (Page.toTitle Page.Loading) (mainView s Page.Loading Loading.view)

        Dictionary submodel ->
            toView (Dic.getSession submodel) Page.Dictionary GotDictionaryMessage (Dic.view submodel)


toViewNoOp : Session -> Page -> Html msg -> Browser.Document Msg
toViewNoOp session page content =
    let
        title =
            Page.toTitle page

        body =
            mainView session page (heroView title (\_ -> NoOp) content)
    in
    Browser.Document title body


toView : Session -> Page -> (msg -> Msg) -> Html msg -> Browser.Document Msg
toView session page toMsg content =
    let
        title =
            Page.toTitle page

        body =
            mainView session page (sectionView toMsg content)
    in
    Browser.Document title body


mainView : Session -> Page -> Html Msg -> List (Html Msg)
mainView session page content =
    [ main_ []
        [ Html.map GotModalMessage <| Modal.view session
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
