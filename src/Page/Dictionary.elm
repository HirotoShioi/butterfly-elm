module Page.Dictionary exposing (..)

import Browser.Navigation as Nav
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements as B exposing (..)
import Bulma.Modifiers as Mod
import Bulma.Modifiers.Typography exposing (..)
import Butterfly.Api as Api exposing (Msg(..))
import Butterfly.Type exposing (Butterfly, Query, Region(..), filterButterflies, fromRegion, toRegion)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus, preventDefaultOn)
import Html.Keyed as Keyed
import Json.Decode as Json
import Maybe.Extra exposing (unwrap)
import Page
import Route
import Session exposing (Session)
import Set exposing (Set)


type Msg
    = GotButterflyResponse Api.Msg
    | ButterflyClicked Butterfly
    | ToggleRegionMenu
    | RegionClicked String
    | ToggleCategoryMenu
    | CategoryClicked String
    | ToggleColorMenu
    | ColorClicked String
    | DeleteTag
    | ResetColor


type Model
    = Loading Session
    | Result ResultModel


type alias ResultModel =
    { session : Session
    , butterflies : List Butterfly
    , isRegionMenuOpen : Bool
    , isCategoryMenuOpen : Bool
    , isColorMenuOpen : Bool
    }


disableMenus : ResultModel -> ResultModel
disableMenus resultModel =
    ResultModel resultModel.session resultModel.butterflies False False False


init : Session -> ( Model, Cmd Msg )
init session =
    ( Loading session, Cmd.map GotButterflyResponse Api.getButterflies )


initResult : Session -> List Butterfly -> ( Model, Cmd Msg )
initResult session butterflies =
    ( Result <| ResultModel session butterflies False False False, Cmd.none )


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
        , B.progress loadingProgressModifiers [ A.max "100", class "loading" ] [ text "50%" ]
        ]


loadingProgressModifiers : ProgressModifiers
loadingProgressModifiers =
    ProgressModifiers Mod.Medium Mod.Info


resultView : ResultModel -> Html Msg
resultView model =
    let
        filteredButterflies =
            filterButterflies model.butterflies model.session.query

        categoryDropdown =
            mkCategoryDropdown model.butterflies model

        regionDropdown =
            mkRegionDropdown model

        colorDropdown =
            mkColorDropdown model
    in
    div [ class "dictionary-view" ]
        [ div [ class "dictionary-search-content" ]
            [ div [ class "field is-grouped is-grouped-multiline" ]
                [ div [ class "control" ] [ regionDropdown ]
                , div [ class "control" ] [ categoryDropdown ]
                , div [ class "control" ] [ colorDropdown ]
                , case model.session.query.hexColor of
                    Nothing ->
                        div [] []

                    Just color ->
                        div [ class "control" ] [ colorTag color ResetColor ]
                ]
            ]
        , if List.isEmpty filteredButterflies then
            emptyView

          else
            Keyed.node "div" [ class "columns  is-multiline" ] <|
                List.map (\butterfly -> ( butterfly.jpName, showButterflies butterfly )) filteredButterflies
        ]


colorTag : String -> msg -> Html msg
colorTag hexString deleteMsg =
    tagWithDelete colorTagModifiers
        [ class "color-tag" ]
        deleteMsg
        [ div [ style "background-color" hexString, class "color-box" ] []
        ]


colorTagModifiers : TagModifiers
colorTagModifiers =
    TagModifiers Mod.Large Mod.Default False


emptyView : Html Msg
emptyView =
    div [] [ text "該当する蝶はみつかりませでした。" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotButterflyResponse apiMsg, _ ) ->
            case apiMsg of
                Api.GotButterflies (Ok butterflies) ->
                    let
                        session =
                            getSession model

                        bs =
                            butterflies
                    in
                    initResult session bs

                Api.GotButterflies (Err err) ->
                    let
                        key =
                            getKey model

                        route =
                            Route.routeToString Route.Error
                    in
                    ( model, Nav.pushUrl key route )

        ( ButterflyClicked butterfly, Result m ) ->
            let
                newSession =
                    Session.update (Session.EnableModal butterfly) m.session
            in
            ( updateSession model newSession, Cmd.none )

        ( CategoryClicked category, Result m ) ->
            let
                newSession =
                    if String.contains "分類" category then
                        Session.update Session.ResetCategory m.session

                    else
                        Session.update (Session.AddCategory category) m.session

                updatedModel =
                    disableMenus m
            in
            ( updateSession (Result updatedModel) newSession, Cmd.none )

        ( RegionClicked regionStr, Result m ) ->
            if String.contains "生息地" regionStr then
                let
                    newSession =
                        Session.update Session.ResetRegion m.session

                    updatedModel =
                        disableMenus m
                in
                ( updateSession (Result updatedModel) newSession, Cmd.none )

            else
                case toRegion regionStr of
                    Err _ ->
                        ( model, Cmd.none )

                    Ok region ->
                        let
                            newSession =
                                Session.update (Session.UpdateRegion region) m.session

                            updatedModel =
                                disableMenus m
                        in
                        ( updateSession (Result updatedModel) newSession, Cmd.none )

        ( ColorClicked hexString, Result m ) ->
            let
                newSession =
                    Session.update (Session.UpdateColor hexString) m.session

                updatedModel =
                    disableMenus m
            in
            ( updateSession (Result updatedModel) newSession, Cmd.none )

        ( ToggleRegionMenu, Result m ) ->
            let
                updatedModel =
                    { m | isRegionMenuOpen = not m.isRegionMenuOpen, isCategoryMenuOpen = False, isColorMenuOpen = False }
            in
            ( Result updatedModel, Cmd.none )

        ( ToggleCategoryMenu, Result m ) ->
            let
                updatedModel =
                    { m | isCategoryMenuOpen = not m.isCategoryMenuOpen, isRegionMenuOpen = False, isColorMenuOpen = False }
            in
            ( Result updatedModel, Cmd.none )

        ( ToggleColorMenu, Result m ) ->
            let
                updatedModel =
                    { m | isColorMenuOpen = not m.isColorMenuOpen, isRegionMenuOpen = False, isCategoryMenuOpen = False }
            in
            ( Result updatedModel, Cmd.none )

        ( ResetColor, Result m ) ->
            let
                newSession =
                    Session.update Session.ResetColor m.session
            in
            ( updateSession (Result m) newSession, Cmd.none )

        _ ->
            ( model, Cmd.none )


showButterflies : Butterfly -> Html Msg
showButterflies butterfly =
    div [ class "column is-one-third-tablet is-one-fifth-desktop" ]
        [ card [ class "butterfly-card", onClick (ButterflyClicked butterfly) ]
            [ cardImage [] [ butterflyImage butterfly.imgSrc ]
            , cardContent [ textCentered, textSize Small ]
                [ div []
                    [ text butterfly.jpName
                    , div [ class "content", textColor Grey ] [ text butterfly.engName ]
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



--------------------------------------------------------------------------------
-- Dropdown
--------------------------------------------------------------------------------


mkRegionDropdown : ResultModel -> Html Msg
mkRegionDropdown model =
    let
        activeRegion =
            Maybe.withDefault "生息地" (Maybe.map fromRegion model.session.query.region)

        regionList =
            List.map fromRegion [ OldNorth, NewNorth, NewTropical, TropicalAfrica, IndiaAustralia ]

        regionListWithReset =
            case model.session.query.region of
                Just _ ->
                    "生息地" :: regionList

                Nothing ->
                    regionList
    in
    searchDropdown activeRegion regionListWithReset ToggleRegionMenu model.isRegionMenuOpen False RegionClicked


mkCategoryDropdown : List Butterfly -> ResultModel -> Html Msg
mkCategoryDropdown butterflies model =
    let
        activeCategory =
            Maybe.withDefault "分類" model.session.query.category

        categoryList =
            Set.toList <| mkCategorySet model.session.query butterflies

        categoryListWithReset =
            case model.session.query.category of
                Nothing ->
                    categoryList

                Just _ ->
                    "分類" :: categoryList
    in
    searchDropdown
        activeCategory
        categoryListWithReset
        ToggleCategoryMenu
        model.isCategoryMenuOpen
        (List.isEmpty categoryList)
        CategoryClicked



-- div [ class "field is-grouped is-grouped-multiline" ]
--                 [ div [ class "control" ] [ regionDropdown ]
--                 , div [ class "control" ] [ categoryDropdown ]
--                 , div [ class "control" ] [ colorDropdown ]
--                 ]


mkColorDropdown : ResultModel -> Html Msg
mkColorDropdown resultModel =
    let
        colorList =
            [ "#e60012"
            , "#f39800"
            , "#fff100"
            , "#8fc31f"
            , "#009944"
            , "#009e96"
            , "#00a0e9"
            , "#0068b7"
            , "#1d2088"
            , "#920783"
            , "#e4007f"
            , "#e5004f"
            ]
    in
    dropdown resultModel.isColorMenuOpen
        dropdownModifiers
        []
        [ searchDropdownTrigger ToggleColorMenu "色" False
        , dropdownMenu []
            [ class "color-palette-wrapper" ]
            [ dropdownItem False
                []
                [ div [ class "color-palette field is-grouped is-grouped-multiline" ] <|
                    List.map
                        (\hexColor ->
                            div [ class "control color-palette-item", style "background-color" hexColor, onClick (ColorClicked hexColor) ] []
                        )
                        colorList
                ]
            ]
        ]


searchDropdown :
    String -- Active link
    -> List String -- List of items
    -> Msg -- Toggle Msg
    -> Bool -- Bool flag for menu status
    -> Bool -- Bool flag for trigger disabled attribute
    -> (String -> Msg) -- Msg used to send clicked item
    -> Html Msg
searchDropdown activeLink list toggleMsg isMenuOpen isDisabled clickMsg =
    dropdown isMenuOpen
        dropdownModifiers
        []
        [ searchDropdownTrigger toggleMsg activeLink isDisabled
        , searchDropdownMenu activeLink list clickMsg
        ]


searchDropdownTrigger : Msg -> String -> Bool -> Html Msg
searchDropdownTrigger toggleMsg buttonName isDisabled =
    div [ class "dropdown-trigger" ]
        [ Html.button
            [ onClick toggleMsg
            , attribute "aria-haspopup" "true"
            , attribute "aria-controls" "dropdown-menu"
            , disabled isDisabled
            , class "button"
            ]
            [ span [] [ text buttonName ]
            , span [ class "icon is-small" ]
                [ i [ class "fas fa-angle-down", attribute "aria-hidden" "true" ] []
                ]
            ]
        ]


searchDropdownMenu : String -> List String -> (String -> Msg) -> Html Msg
searchDropdownMenu active list clickedMsg =
    let
        isActive str =
            active == str
    in
    dropdownMenu [] [] <|
        List.map
            (\element ->
                dropdownItemLink (isActive element) [ onClickAnchor (clickedMsg element) ] [ text element ]
            )
            list


mkCategorySet : Query -> List Butterfly -> Set String
mkCategorySet query butterflies =
    let
        cQuery =
            { query | category = Nothing }

        filteredButterflies =
            filterButterflies butterflies cQuery
    in
    List.foldr (\butterfly set -> Set.insert butterfly.category set) Set.empty filteredButterflies


onClickAnchor : msg -> Attribute msg
onClickAnchor msg =
    preventDefaultOn "click" <| Json.succeed ( msg, True )
