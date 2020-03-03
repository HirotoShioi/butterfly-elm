module Page.Dictionary.View exposing (colorTag, emptyView, errorView, nameSearchView, searchDropdown, searchDropdownTrigger, searchTag, showButterflies)

import Bulma.Components as B
import Bulma.Elements as B
import Bulma.Modifiers as B
import Bulma.Modifiers.Typography exposing (Color(..), Size(..), textCentered, textColor, textSize)
import Butterfly.Type exposing (Butterfly, getImageName)
import Html exposing (Attribute, Html, a, div, i, img, input, p, span, text)
import Html.Attributes exposing (attribute, class, disabled, href, placeholder, src, style, type_)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Json.Decode as Json
import Route as Route
import Url.Builder exposing (relative)


searchDropdown :
    String -- Link name
    -> List String -- List of items
    -> msg -- Toggle Msg
    -> Bool -- Bool flag for menu status
    -> Bool -- Bool flag for trigger disabled attribute
    -> (String -> msg) -- Msg used to send clicked item
    -> Html msg
searchDropdown activeLink list toggleMsg isMenuOpen isDisabled clickMsg =
    B.dropdown isMenuOpen
        B.dropdownModifiers
        []
        [ searchDropdownTrigger toggleMsg activeLink isDisabled
        , searchDropdownMenu list clickMsg
        ]


searchDropdownTrigger : msg -> String -> Bool -> Html msg
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


searchDropdownMenu : List String -> (String -> msg) -> Html msg
searchDropdownMenu list clickedMsg =
    B.dropdownMenu [] [] <|
        List.map
            (\element ->
                B.dropdownItemLink False [ onClickAnchor (clickedMsg element) ] [ text element ]
            )
            list


onClickAnchor : msg -> Attribute msg
onClickAnchor msg =
    preventDefaultOn "click" <| Json.succeed ( msg, True )


emptyView : Html msg
emptyView =
    div [] [ text "該当する蝶はみつかりませでした。" ]


butterflyImage : Maybe String -> Html msg
butterflyImage mImgSource =
    let
        imgPath =
            Maybe.withDefault "Todo" mImgSource
    in
    B.image B.SixteenByNine
        []
        [ img [ src imgPath ] []
        ]


colorTag : msg -> String -> Html msg
colorTag deleteMsg hexColor =
    B.tagWithDelete searchTagModifiers
        [ class "color-tag" ]
        deleteMsg
        [ div [ style "background-color" hexColor, class "color-box" ] []
        ]


searchTag : msg -> String -> Html msg
searchTag deleteMsg name =
    B.tagWithDelete searchTagModifiers
        []
        deleteMsg
        [ text name ]


searchTagModifiers : B.TagModifiers
searchTagModifiers =
    B.TagModifiers B.Medium B.Default False


showButterflies : Butterfly -> Html msg
showButterflies butterfly =
    a [ href (mkLink butterfly), class "column is-one-third-tablet is-one-fifth-desktop" ]
        [ B.card [ class "butterfly-card" ]
            [ B.cardImage [] [ butterflyImage butterfly.imgPath ]
            , B.cardContent [ textCentered, textSize Small ]
                [ div []
                    [ text butterfly.jpName
                    , div [ class "content", textColor Grey ]
                        [ text butterfly.engName
                        ]
                    ]
                ]
            ]
        ]


mkLink : Butterfly -> String
mkLink butterfly =
    case getImageName butterfly of
        Nothing ->
            ""

        Just imageName ->
            relative [ "detail", imageName ] []


errorView : Html msg
errorView =
    div [ class "columns" ]
        [ div [ class "column is-8 is-offset-2" ]
            [ B.title B.H4 [] [ text "データの読み込みに失敗しました。" ]
            , p [] [ text "インターネットの接続等を確認の後再度読み込んでください。" ]
            , a [ Route.href Route.Home ] [ text "ホームへ戻る" ]
            ]
        ]


nameSearchView : (String -> msg) -> msg -> Html msg
nameSearchView onInputMsg onSubmitMsg =
    div [ class "field has-addons" ]
        [ div [ class "control" ]
            [ input [ class "input", type_ "text", placeholder "名前で検索", onInput onInputMsg ] [] ]
        , div [ class "control" ]
            [ a [ class "button is-default", onClick onSubmitMsg ]
                [ text "検索" ]
            ]
        ]
