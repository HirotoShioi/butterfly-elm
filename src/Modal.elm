module Modal exposing (..)

import Bulma.Components as B
import Bulma.Elements as B
import Bulma.Layout as B
import Bulma.Modifiers as B
import Bulma.Modifiers.Typography as B
import Butterfly.Type as Butterfly exposing (Butterfly)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { mButterfly : Maybe Butterfly
    }


type Msg
    = ModalBackgroundClicked
    | ModalEnabled Butterfly


init : Model
init =
    Model Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModalBackgroundClicked ->
            ( { model | mButterfly = Nothing }, Cmd.none )

        ModalEnabled butterfly ->
            ( { model | mButterfly = Just butterfly }, Cmd.none )


view : Model -> Html Msg
view model =
    B.modal (isJust model.mButterfly)
        []
        [ B.modalBackground [ onClick ModalBackgroundClicked ] []
        , B.modalContent [ class "butterfly-modal" ]
            [ butterflyView model.mButterfly
            ]
        ]


butterflyView : Maybe Butterfly -> Html Msg
butterflyView mButterfly =
    case mButterfly of
        Nothing ->
            div [] [ text "This should never happen" ]

        Just butterfly ->
            B.box [ class "butterfly-modal-box" ]
                [ butterflyImage butterfly.img_src
                , div [ class "content butterfly-modal-content" ]
                    [ B.title B.H5 [] [ text butterfly.jp_name ]
                    , h6 [ class "subtitle", B.textColor B.Grey ] [ text butterfly.eng_name ]
                    , fieldValueView "分類" butterfly.category
                    , fieldValueView "生息地" butterfly.region
                    ]
                ]


fieldValueView : String -> String -> Html msg
fieldValueView field value =
    p []
        [ span [ B.textWeight B.Bold ] [ text <| String.concat [ field, ": " ] ]
        , span [] [ text value ]
        ]


butterflyImage : String -> Html msg
butterflyImage img_src =
    B.image B.SixteenByNine
        []
        [ img [ src <| String.concat [ "http://biokite.com/worldbutterfly/", img_src ] ] []
        ]


isJust : Maybe a -> Bool
isJust mValue =
    case mValue of
        Nothing ->
            False

        Just value ->
            True
