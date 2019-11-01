module Page.Error exposing (view)

import Html exposing (Html, div, text)


view : Html msg
view =
    div [] [ text "ページを読み込み中に問題が発生しました。インターネットの接続を確認してください。" ]
