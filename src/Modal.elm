module Modal exposing (Msg(..), update, view)

import Bulma.Components exposing (modal, modalBackground, modalContent)
import Bulma.Elements exposing (ImageShape(..), TitleSize(..), box, image, title)
import Bulma.Modifiers.Typography exposing (Color(..), Weight(..), textColor, textWeight)
import Butterfly.Type exposing (Butterfly, Color)
import Html exposing (Html, a, div, h6, img, p, span, text)
import Html.Attributes exposing (class, href, src, style)
import Html.Events exposing (onClick)
import Session exposing (Session)


type Msg
    = ModalBackgroundClicked
    | ModalEnabled Butterfly
    | ColorClicked String


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        ModalBackgroundClicked ->
            ( Session.update Session.DisableModal session, Cmd.none )

        ModalEnabled butterfly ->
            ( Session.update (Session.EnableModal butterfly) session, Cmd.none )

        ColorClicked hexColor ->
            ( Session.update (Session.UpdateColorFromModal hexColor) session, Cmd.none )


view : Session -> Html Msg
view model =
    modal (isJust model.modalContent)
        []
        [ modalBackground [ onClick ModalBackgroundClicked ] []
        , modalContent [ class "butterfly-modal" ]
            [ butterflyView model.modalContent
            ]
        ]


butterflyView : Maybe Butterfly -> Html Msg
butterflyView mButterfly =
    case mButterfly of
        Nothing ->
            div [] [ text "This should never happen" ]

        Just butterfly ->
            box [ class "butterfly-modal-box" ]
                [ butterflyImage butterfly.imgSrc
                , colorBar butterfly.dominantColors
                , div [ class "content butterfly-modal-content" ]
                    [ title H5 [] [ text butterfly.jpName ]
                    , h6 [ class "subtitle", textColor Grey ] [ text butterfly.engName ]
                    , fieldValueView "分類" butterfly.category
                    , fieldValueView "生息地" butterfly.region
                    ]
                ]


fieldValueView : String -> String -> Html msg
fieldValueView field value =
    p []
        [ span [ textWeight Bold ] [ text <| String.concat [ field, ": " ] ]
        , span [] [ text value ]
        ]


butterflyImage : String -> Html msg
butterflyImage img_src =
    image SixteenByNine
        []
        [ img [ src <| String.concat [ "http://biokite.com/worldbutterfly/", img_src ] ] []
        ]


isJust : Maybe a -> Bool
isJust mValue =
    case mValue of
        Nothing ->
            False

        Just _ ->
            True


colorBar : List Color -> Html Msg
colorBar colors =
    div [ class "color-wrapper" ]
        [ div [ class "color-container" ] <|
            List.map colorBlockView <|
                List.reverse <|
                    List.sortBy (\color -> color.pixelFraction) colors
        ]


colorBlockView : Color -> Html Msg
colorBlockView color =
    let
        percentage =
            color.pixelFraction * 100

        percentageText =
            String.concat [ String.fromFloat percentage, "%" ]
    in
    a
        [ class "color"
        , style "width" percentageText
        , style "background-color" color.hexColor
        , onClick <| ColorClicked color.hexColor
        , href "#"
        ]
        []



-- pixelFraction * 100 / totalPixelFraction
