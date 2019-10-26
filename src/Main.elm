module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Browser.Navigation as Nav
import Url

import Page
import Route exposing (Route)

import Page.Area as Area
import Page.Dictionary as Dic
import Page.Category as Category
import Page.Description as Desc
import Page.Home as Home
import Page.Reference as Ref
import Page.NotFound as NotFound

---- MODEL ----

type Model =
    Home Nav.Key
  | NotFound Nav.Key
  | Reference Nav.Key
  | Area Nav.Key
  | Category Nav.Key
  | Description Nav.Key
  | Dictionary Dic.Model

getKey : Model -> Nav.Key
getKey model
  = case model of
    Home k -> k
    NotFound k -> k
    Reference k -> k
    Area k -> k
    Category k -> k
    Description k -> k
    Dictionary m -> m.key

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key = changeRouteTo (Route.parseUrl url) (Home key)

changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        key =
            getKey model
    in
    case maybeRoute of
        Nothing ->
          ( NotFound key, Cmd.none )
        Just Route.Home ->
          ( Home key, Cmd.none )
        Just Route.Reference ->
          ( Reference key, Cmd.none )
        Just Route.Description ->
          ( Description key, Cmd.none )
        Just Route.Category ->
          ( Category key, Cmd.none )
        Just Route.Area ->
          ( Area key, Cmd.none )
        Just Route.Dictionary ->
          case model of
            Dictionary dmodel -> (Dictionary dmodel, Cmd.none)
            _ -> (Dictionary (Dic.init key), Cmd.none)

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
view model =
  let 
    viewPage toMsg v = 
      let { title, body } = Page.view v
      in  { title = title
          , body = List.map (Html.map toMsg) body
          }
  in case model of
    NotFound _ -> Page.view NotFound.view
    Home _ -> Page.view Home.view
    Reference _ -> Page.view Ref.view
    Category _ -> Page.view Category.view
    Description _ -> Page.view Desc.view
    Area _ -> Page.view Area.view
    Dictionary submodel -> viewPage GotDictionaryMessage (Dic.view submodel)

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
