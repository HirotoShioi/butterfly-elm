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
import NavBar
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

        Detail m ->
            Detail.getKey m


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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( initSession, sessionCmd ) =
            S.init key NavBar.init

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

                Just (Route.Detail butterfly_name) ->
                    case session.butterflies of
                        Ok butterflies ->
                            let
                                filterByName name =
                                    List.filter (\b -> b.engName == name) butterflies

                                mButterfly =
                                    Maybe.andThen
                                        (\name -> filterByName name |> List.head)
                                        (Url.percentDecode butterfly_name)
                            in
                            case mButterfly of
                                Nothing ->
                                    ( NotFound session, Cmd.none )

                                Just butterfly ->
                                    updateWith Detail GotDetailMessage (Detail.init session butterfly)

                        Err _ ->
                            ( Error session, Cmd.none )



-- Map over submodel as well as submsg to main


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | GotDictionaryMessage Dic.Msg
    | GotNavBarMessage NavBar.Msg
    | NoOp
    | MainClicked
    | GotSessionMsg S.Msg
    | GotDetailMessage Detail.Msg
    | GotNotFoundMsg NotFound.Msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
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

        ( GotDetailMessage submsg, Detail submodel ) ->
            Detail.update submsg submodel
                |> updateWith Detail GotDetailMessage

        ( MainClicked, someModel ) ->
            S.update (S.GotNavMessage NavBar.DisableMenu) (getSession someModel)
                |> updateWith (updateSession someModel) GotSessionMsg

        ( GotNavBarMessage navMsg, someModel ) ->
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

                -- If any other message was being triggered while the model is at Loading
                -- , ignore it
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
                (detailView m.session Page.Detail (Html.map GotDetailMessage <| Detail.view m))

        Dictionary submodel ->
            toView (Dic.getSession submodel) Page.Dictionary GotDictionaryMessage (Dic.view submodel)


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
        [ Html.map GotNavBarMessage <| NavBar.view page session.navModel
        , columns myColumnsModifiers
            [ onClick MainClicked ]
            [ content ]
        ]
    ]


detailView : Session -> Page -> Html Msg -> List (Html Msg)
detailView session page content =
    [ main_ []
        [ Html.map GotNavBarMessage <| NavBar.view page session.navModel
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
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }