module Modal exposing (Model, Msg(..), init, update, view)

import Bulma.Components as Components
import Bulma.Elements as Elements
import Bulma.Modifiers.Typography as Typography
import Butterfly.Type exposing (Butterfly, Color)
import Html exposing (Html, div, h6, img, p, span, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)


type alias Model =
    Maybe Butterfly


type Msg
    = ModalBackgroundClicked
    | ModalEnabled Butterfly


init : Model
init =
    Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        ModalBackgroundClicked ->
            ( Nothing, Cmd.none )

        ModalEnabled butterfly ->
            ( Just butterfly, Cmd.none )


view : Model -> Html Msg
view model =
    Components.modal (isJust model)
        []
        [ Components.modalBackground [ onClick ModalBackgroundClicked ] []
        , Components.modalContent [ class "butterfly-modal" ]
            [ butterflyView model
            ]
        ]


butterflyView : Maybe Butterfly -> Html Msg
butterflyView mButterfly =
    case mButterfly of
        Nothing ->
            div [] [ text "This should never happen" ]

        Just butterfly ->
            Elements.box [ class "butterfly-modal-box" ]
                [ butterflyImage butterfly.imgSrc
                , colorBar butterfly.dominantColors
                , div [ class "content butterfly-modal-content" ]
                    [ Elements.title Elements.H5 [] [ text butterfly.jpName ]
                    , h6 [ class "subtitle", Typography.textColor Typography.Grey ] [ text butterfly.engName ]
                    , fieldValueView "分類" butterfly.category
                    , fieldValueView "生息地" butterfly.region
                    ]
                ]


fieldValueView : String -> String -> Html msg
fieldValueView field value =
    p []
        [ span [ Typography.textWeight Typography.Bold ] [ text <| String.concat [ field, ": " ] ]
        , span [] [ text value ]
        ]


butterflyImage : String -> Html msg
butterflyImage img_src =
    Elements.image Elements.SixteenByNine
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
    let
        colorBlocks =
            List.sortBy (\color -> color.pixelFraction) colors
                |> List.reverse
                |> List.map coloBlockView
    in
    div [ class "color-wrapper" ]
        [ div [ class "color-container" ]
            colorBlocks
        ]


coloBlockView : Color -> Html Msg
coloBlockView color =
    let
        percentage =
            color.pixelFraction * 100

        percentageText =
            String.concat [ String.fromFloat percentage, "%" ]
    in
    div [ class "color", style "width" percentageText, style "background-color" color.hexColor ]
        []



-- pixelFraction * 100 / totalPixelFraction
