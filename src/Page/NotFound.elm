module Page.NotFound exposing (view)

import Html exposing (Html, a, div, h3, text)
import Html.Attributes exposing (href)


view : Html msg
view =
    div []
        [ h3 [] [ text "お探しのページは見つかりませんでした。" ]
        , div []
            [ a [ href "/" ] [ text "ホームへ戻る" ]
            ]
        ]
