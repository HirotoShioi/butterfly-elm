module Page.Dictionary.View exposing (..)

import Bulma.Components as B
import Bulma.Elements as B
import Bulma.Modifiers as B
import Bulma.Modifiers.Typography as Typo
import Butterfly.Type exposing (Butterfly, Query, filterButterflies)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Json
import Set exposing (Set)


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


butterflyImage : String -> Html msg
butterflyImage img_src =
    B.image B.SixteenByNine
        []
        [ img [ src <| String.concat [ "http://biokite.com/worldbutterfly/", img_src ] ] []
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


loadingView : Html msg
loadingView =
    div []
        [ text "Loading..."
        , B.progress loadingProgressModifiers [ A.max "100", class "loading" ] [ text "50%" ]
        ]


loadingProgressModifiers : B.ProgressModifiers
loadingProgressModifiers =
    B.ProgressModifiers B.Medium B.Info


showButterflies : Butterfly -> (Butterfly -> msg) -> Html msg
showButterflies butterfly clickedMsg =
    div [ class "column is-one-third-tablet is-one-fifth-desktop" ]
        [ B.card [ class "butterfly-card", onClick (clickedMsg butterfly) ]
            [ B.cardImage [] [ butterflyImage butterfly.imgSrc ]
            , B.cardContent [ Typo.textCentered, Typo.textSize Typo.Small ]
                [ div []
                    [ text butterfly.jpName
                    , div [ class "content", Typo.textColor Typo.Grey ] [ text butterfly.engName ]
                    ]
                ]
            ]
        ]
