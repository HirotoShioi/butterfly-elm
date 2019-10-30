module Modal exposing (..)

import Bulma.Components as B
import Bulma.Modifiers as B
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
        , B.modalContent []
            [ B.modalCard []
                [ B.modalCardBody [ class "content" ] [ butterflyView model.mButterfly ]
                ]
            ]
        ]


butterflyView : Maybe Butterfly -> Html Msg
butterflyView mButterfly =
    case mButterfly of
        Nothing ->
            div [] [ text "This should never happen" ]

        Just butterfly ->
            div [] [ text butterfly.jp_name ]


isJust : Maybe a -> Bool
isJust mValue =
    case mValue of
        Nothing ->
            False

        Just value ->
            True
