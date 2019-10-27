module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Browser.Navigation as Nav
import Url

import Page exposing (Page)
import Route exposing (Route)
import Session as S exposing (Session)

import Page.Area as Area
import Page.Dictionary as Dic
import Page.Category as Category
import Page.Description as Desc
import Page.Home as Home
import Page.Reference as Ref
import Page.NotFound as NotFound

---- MODEL ----

type Model =
    Home Session
  | NotFound Session
  | Reference Session
  | Area Session
  | Category Session
  | Description Session
  | Dictionary Dic.Model

getKey : Model -> Nav.Key
getKey model
  = case model of
    Home s -> S.getKey s
    NotFound s -> S.getKey s
    Reference s -> S.getKey s
    Area s -> S.getKey s
    Category s -> S.getKey s
    Description s -> S.getKey s
    Dictionary m -> Dic.getKey m

getSession : Model -> Session
getSession model
  = case model of
    Home s -> s
    NotFound s -> s
    Reference s -> s
    Area s -> s
    Category s -> s
    Description s -> s
    Dictionary m -> m.session

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key = 
 let initPage = Page.init Page.Home
     initSession = S.init key initPage
 in changeRouteTo (Route.parseUrl url) (Home initSession)

changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            getSession model
    in
    case maybeRoute of
        Nothing ->
          ( NotFound <| S.setPage Page.NotFound session, Cmd.none )
        Just Route.Home ->
          ( Home <| S.setPage Page.Home session, Cmd.none )
        Just Route.Reference ->
          ( Reference <| S.setPage Page.Reference session, Cmd.none )
        Just Route.Description ->
          ( Description <| S.setPage Page.Description session, Cmd.none )
        Just Route.Category ->
          ( Category <| S.setPage Page.Category session, Cmd.none )
        Just Route.Area ->
          ( Area <| S.setPage Page.Area session, Cmd.none )
        Just Route.Dictionary ->
          case model of
            Dictionary dmodel -> (Dictionary dmodel, Cmd.none)
            _ -> (Dictionary <| Dic.init <| S.setPage Page.Dictionary session, Cmd.none)

type Msg
  = UrlChanged Url.Url
  | UrlRequested Browser.UrlRequest
  -- Msg type that would enable lifting of page to main
  | GotDictionaryMessage Dic.Msg

---- UPDATE ----
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        (UrlRequested urlRequest, _)->
          case urlRequest of
            Browser.Internal url ->
              (model, Nav.pushUrl (getKey model) (Url.toString url))
            Browser.External href ->
              (model, Nav.load href)

        (UrlChanged url, _)->
          changeRouteTo (Route.parseUrl url) model

        (GotDictionaryMessage submsg, Dictionary submodel) ->
          Dic.update submsg submodel
            |> updateWith Dictionary GotDictionaryMessage
        (_, _) ->
          (model, Cmd.none)

-- Map over submodel as well as submsg to main
updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )

---- VIEW ----

view : Model -> Browser.Document Msg
view model = case model of
    NotFound s -> toView s Page.NotFound NotFound.view 
    Home s -> toView s Page.Home Home.view
    Reference s -> toView s Page.Reference Ref.view
    Category s -> toView s Page.Category Category.view
    Description s -> toView s Page.Description Desc.view
    Area s -> toView s Page.Area Area.view
    Dictionary submodel -> toViewWithModel (Dic.getSession submodel) GotDictionaryMessage (Dic.view submodel)

toView : Session -> Page -> { title : String, content : Html msg} -> Browser.Document msg
toView session page { title, content } = Page.view session.pageModel title content

toViewWithModel : Session -> (msg -> Msg) -> { title : String, content : Html msg} -> Browser.Document Msg
toViewWithModel session toMsg { title , content } = 
  let document = Page.view session.pageModel title content
  in
    { title = document.title
    , body = List.map (Html.map toMsg) document.body
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
