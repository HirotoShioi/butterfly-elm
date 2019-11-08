module Page.Loading exposing (view)

import Bulma.Components as B
import Bulma.Elements as B
import Bulma.Modifiers as B
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)


view : Html msg
view =
    div [ class "loading-container" ]
        [ B.progress loadingProgressModifiers [ Attr.max "100", class "loading" ] [ text "50%" ]
        ]


loadingProgressModifiers : B.ProgressModifiers
loadingProgressModifiers =
    B.ProgressModifiers B.Medium B.Info
