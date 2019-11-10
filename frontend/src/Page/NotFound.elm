module Page.NotFound exposing (Msg, update, view)

import Html exposing (Html, a, div, h3, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Session exposing (Session)
import Util exposing (updateWith)


type Msg
    = BackHome
    | GotSessionMsg Session.Msg


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        BackHome ->
            ( session, Cmd.map GotSessionMsg <| session.nav.back 1 )

        GotSessionMsg sessionMsg ->
            Session.update sessionMsg session
                |> updateWith (\s -> s) GotSessionMsg


view : Session -> Html Msg
view _ =
    div []
        [ h3 [] [ text "お探しのページは見つかりませんでした。" ]
        , div []
            [ a [ class "button", onClick BackHome ] [ text "戻る" ]
            ]
        ]
