module Page.Detail exposing (Model, Msg(..), getNav, getSession, init, update, updateSession, view)

import Browser.Navigation as Nav
import Bulma.Columns exposing (ColumnsModifiers, Display(..), Gap(..), columns)
import Bulma.Elements exposing (ImageShape(..), ImageSize(..), TitleSize(..), image, title)
import Bulma.Layout exposing (SectionSpacing(..), section)
import Bulma.Modifiers.Typography exposing (Color(..), textColor)
import Butterfly.Query as Query
import Butterfly.Type exposing (Butterfly, Color, toRegion)
import Html exposing (Html, a, div, h6, img, p, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Navigation as Nav exposing (Nav)
import Route
import Session exposing (Session)
import Util exposing (updateWith)


type alias Model =
    { session : Session
    , butterfly : Butterfly
    }


init : Session -> Butterfly -> ( Model, Cmd Msg )
init session butterfly =
    ( Model session butterfly, Cmd.none )


getSession : Model -> Session
getSession model =
    model.session


getNav : Model -> Nav Msg
getNav model =
    Nav.map GotSessionMsg model.session.nav


updateSession : Model -> Session -> Model
updateSession model session =
    { model | session = session }


type Msg
    = ColorClicked String
    | GotSessionMsg Session.Msg
    | RegionClicked String
    | CategoryClicked String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ColorClicked hexColor ->
            Session.update (Session.FromDetail <| Query.UpdateColor hexColor) model.session
                |> updateWith (updateSession model) GotSessionMsg

        RegionClicked regionStr ->
            case toRegion regionStr of
                Err _ ->
                    ( model, Cmd.none )

                Ok region ->
                    Session.update (Session.FromDetail <| Query.UpdateRegion region) model.session
                        |> updateWith (updateSession model) GotSessionMsg

        CategoryClicked category ->
            Session.update (Session.FromDetail <| Query.UpdateCategory category) model.session
                |> updateWith (updateSession model) GotSessionMsg

        GotSessionMsg sessionMsg ->
            Session.update sessionMsg model.session
                |> updateWith (updateSession model) GotSessionMsg


view : Model -> Html Msg
view model =
    section NotSpaced
        []
        [ columns myColumnsModifiers
            []
            [ div [ class "column is-half is-full-tablet" ]
                [ butterflyImage model.butterfly.imgPath
                , colorBar model.butterfly.dominantColors ColorClicked
                ]
            , div [ class "column is-half is-full-tablet" ]
                [ div [ class "content" ]
                    [ butterflyDescription model.butterfly ]
                ]
            ]
        , case model.butterfly.remarks of
            Nothing ->
                div [] []

            Just remark ->
                div [ class "content" ] [ p [] [ text remark ] ]
        , div [ class "has-text-centered" ] [ a [ class "button", Route.href Route.Dictionary ] [ text "戻る" ] ]
        ]


myColumnsModifiers : ColumnsModifiers
myColumnsModifiers =
    { multiline = True
    , gap = Gap3
    , display = TabletAndBeyond
    , centered = False
    }


butterflyImage : Maybe String -> Html msg
butterflyImage mImgSrc =
    let
        imgPath =
            Maybe.withDefault "Todo" mImgSrc
    in
    image SixteenByNine
        [ class "detail-image" ]
        [ img [ src <| imgPath ] []
        ]


butterflyDescription : Butterfly -> Html Msg
butterflyDescription butterfly =
    div []
        ([ title H4 [] [ text butterfly.jpName ]
         , h6 [ class "subtitle", textColor Grey ] [ text butterfly.engName ]
         , fieldValueView "生息地域" butterfly.region (Just RegionClicked)
         , fieldValueView "属" butterfly.category (Just CategoryClicked)
         , fieldValueView "開長" (String.concat [ String.fromInt butterfly.openLength, "mm" ]) Nothing
         ]
            ++ (case butterfly.diet of
                    Nothing ->
                        []

                    Just diet ->
                        [ h6 [ class "subtitle field-name" ] [ text "食草" ]
                        , p [] [ text diet ]
                        ]
               )
        )


fieldValueView : String -> String -> Maybe (String -> Msg) -> Html Msg
fieldValueView field value mClickMsg =
    div [ class "field-value-wrapper" ]
        [ h6 [ class "subtitle field-name" ] [ text field ]
        , case mClickMsg of
            Nothing ->
                p [] [ text value ]

            Just clickMsg ->
                a [ onClick <| clickMsg value ] [ text value ]
        ]


colorBar : List Color -> (String -> Msg) -> Html Msg
colorBar colors toMsg =
    div [ class "color-wrapper" ]
        [ div [ class "color-container" ] <|
            List.map (colorBlockView toMsg) <|
                List.reverse <|
                    List.sortBy (\color -> color.pixelFraction) colors
        ]


colorBlockView : (String -> Msg) -> Color -> Html Msg
colorBlockView toMsg color =
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
        , onClick <| toMsg color.hexColor
        ]
        []
