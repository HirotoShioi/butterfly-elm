module Page.Error exposing (view)

import Html exposing (Html, a, div, p, text)
import Html.Attributes exposing (href)


view : Html msg
view =
    div []
        [ p [] [ text "ページを読み込み中に問題が発生しました。インターネットの接続を確認してください。" ]
        , a [ href "#/" ] [ text "ホームへ戻る" ]
        ]
