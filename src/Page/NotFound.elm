module Page.NotFound exposing (Msg, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, a, div, h3, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Session exposing (Session)


type Msg
    = BackHome


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        BackHome ->
            ( session, Nav.back session.key 1 )


view : Session -> Html Msg
view session =
    div []
        [ h3 [] [ text "お探しのページは見つかりませんでした。" ]
        , div []
            [ a [ class "button", onClick BackHome ] [ text "戻る" ]
            ]
        ]
