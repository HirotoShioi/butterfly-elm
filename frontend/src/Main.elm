module Main exposing (Model(..), Msg(..), changeRouteTo, getNav, getSession, init, main, update, updateSession, view)

import Browser
import Browser.Navigation as Nav
import Bulma.Columns exposing (ColumnsModifiers, Display(..), Gap(..), columns)
import Bulma.Elements exposing (TitleSize(..), title)
import Bulma.Layout exposing (HeroModifiers, SectionSpacing(..), hero, heroBody, section)
import Bulma.Modifiers exposing (Color(..), Size(..))
import Butterfly.Api as Api
import Butterfly.Type exposing (getImageName)
import Html exposing (Html, div, main_, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import NavBar
import Navigation as Nav exposing (Nav)
import Page exposing (Page)
import Page.Area as Area
import Page.Category as Category
import Page.Description as Desc
import Page.Detail as Detail
import Page.Dictionary as Dic
import Page.Error as Error
import Page.Home as Home
import Page.Loading as Loading
import Page.NotFound as NotFound
import Page.Reference as Ref
import Route exposing (Route)
import Session as S exposing (Session)
import Url
import Util exposing (updateWith)



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
    | Detail Detail.Model


getNav : Model -> Nav Msg
getNav model =
    let
        mapWith nav =
            Nav.map GotSessionMsg nav
    in
    case model of
        Error s ->
            mapWith <| S.getNav s

        Home s ->
            mapWith <| S.getNav s

        NotFound s ->
            mapWith <| S.getNav s

        Reference s ->
            mapWith <| S.getNav s

        Area s ->
            mapWith <| S.getNav s

        Category s ->
            mapWith <| S.getNav s

        Description s ->
            mapWith <| S.getNav s

        Dictionary m ->
            Nav.map GotDictionaryMsg <| Dic.getNav m

        Loading s _ ->
            mapWith <| S.getNav s

        Detail m ->
            Nav.map GotDetailMsg <| Detail.getNav m


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

        Detail m ->
            Detail.getSession m


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

        Detail m ->
            Detail (Detail.updateSession m s)


initWithKey : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
initWithKey _ url key =
    let
        nav =
            Nav.initWithKey key
    in
    init url nav


init : Url.Url -> Nav S.Msg -> ( Model, Cmd Msg )
init url nav =
    let
        ( initSession, sessionCmd ) =
            S.init nav NavBar.init

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

                Just (Route.Dictionary query) ->
                    updateWith Dictionary GotDictionaryMsg (Dic.init session query)

                Just Route.Error ->
                    ( Error session, Cmd.none )

                Just (Route.Detail image_name) ->
                    case session.butterflies of
                        Ok butterflies ->
                            let
                                filterByName name =
                                    List.filter (\butterfly -> getImageName butterfly == Just name) butterflies

                                mButterfly =
                                    Maybe.andThen
                                        (\name -> filterByName name |> List.head)
                                        (Url.percentDecode image_name)
                            in
                            case mButterfly of
                                Nothing ->
                                    ( NotFound session, Cmd.none )

                                Just butterfly ->
                                    updateWith Detail GotDetailMsg (Detail.init session butterfly)

                        Err _ ->
                            ( Error session, Cmd.none )



-- Map over submodel as well as submsg to main


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | GotDictionaryMsg Dic.Msg
    | GotNavBarMsg NavBar.Msg
    | NoOp
    | MainClicked
    | GotSessionMsg S.Msg
    | GotDetailMsg Detail.Msg
    | GotNotFoundMsg NotFound.Msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        nav =
                            getNav model
                    in
                    ( model, nav.pushUrl (Url.toString url) )

                Browser.External "" ->
                    ( model, Cmd.none )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.parseUrl url) model

        ( GotDictionaryMsg submsg, Dictionary submodel ) ->
            Dic.update submsg submodel
                |> updateWith Dictionary GotDictionaryMsg

        ( GotDetailMsg submsg, Detail submodel ) ->
            Detail.update submsg submodel
                |> updateWith Detail GotDetailMsg

        ( MainClicked, someModel ) ->
            S.update (S.GotNavMessage NavBar.DisableMenu) (getSession someModel)
                |> updateWith (updateSession someModel) GotSessionMsg

        ( GotNavBarMsg navMsg, someModel ) ->
            S.update (S.GotNavMessage navMsg) (getSession someModel)
                |> updateWith (updateSession someModel) GotSessionMsg

        ( GotSessionMsg sessionMsg, Loading s url ) ->
            case sessionMsg of
                -- Intercept the Api response in order to redirect pages
                S.GotButterflyResponse (Api.GotButterflies _) ->
                    let
                        ( session, cmd ) =
                            S.update sessionMsg s

                        ( someModel, routeCmd ) =
                            changeRouteTo (Route.parseUrl url) (Home session)
                    in
                    ( someModel, Cmd.batch [ routeCmd, Cmd.map GotSessionMsg cmd ] )

                -- If any other message was being triggered while the model is at
                -- Loading state, ignore it
                _ ->
                    ( model, Cmd.none )

        ( GotSessionMsg sessionMsg, someModel ) ->
            S.update sessionMsg (getSession someModel)
                |> updateWith (updateSession someModel) GotSessionMsg

        ( GotNotFoundMsg notFoundMsg, NotFound session ) ->
            NotFound.update notFoundMsg session
                |> updateWith NotFound GotNotFoundMsg

        ( _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model of
        Error s ->
            toHeroView s Page.Error Error.view (\_ -> NoOp)

        NotFound s ->
            toHeroView s Page.NotFound (NotFound.view s) GotNotFoundMsg

        Home s ->
            toHeroView s Page.Home Home.view (\_ -> NoOp)

        Reference s ->
            toHeroView s Page.Reference Ref.view (\_ -> NoOp)

        Category s ->
            toHeroView s Page.Category Category.view (\_ -> NoOp)

        Description s ->
            toHeroView s Page.Description Desc.view (\_ -> NoOp)

        Area s ->
            toHeroView s Page.Area Area.view (\_ -> NoOp)

        Loading s _ ->
            -- Loading
            Browser.Document (Page.toTitle Page.Loading) (mainView s Page.Loading Loading.view)

        Detail m ->
            Browser.Document
                m.butterfly.jpName
                (detailView m.session Page.Detail (Html.map GotDetailMsg <| Detail.view m))

        Dictionary submodel ->
            toView (Dic.getSession submodel) Page.Dictionary GotDictionaryMsg (Dic.view submodel)


toHeroView : Session -> Page -> Html msg -> (msg -> Msg) -> Browser.Document Msg
toHeroView session page content liftMsg =
    let
        title =
            Page.toTitle page

        body =
            mainView session page (heroView title liftMsg content)
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
        [ Html.map GotNavBarMsg <| NavBar.view page session.navModel
        , columns myColumnsModifiers
            [ onClick MainClicked ]
            [ content ]
        ]
    ]


detailView : Session -> Page -> Html Msg -> List (Html Msg)
detailView session page content =
    [ main_ []
        [ Html.map GotNavBarMsg <| NavBar.view page session.navModel
        , columns detailColumnsModifiers
            [ onClick MainClicked ]
            [ div [ class "column is-two-thirds" ] [ content ]
            ]
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


detailColumnsModifiers : ColumnsModifiers
detailColumnsModifiers =
    { multiline = True
    , gap = Gap0
    , display = TabletAndBeyond
    , centered = True
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
        { init = initWithKey
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
