module Page.Components exposing (her, sectionHeader)

import Bulma.Elements exposing (TitleSize(..), title)
import Html exposing (Html, hr, text)
import Html.Attributes exposing (class)


sectionHeader : String -> Html msg
sectionHeader t =
    title H4 [] [ text t ]


her : Html msg
her =
    hr [ class "hr" ] []
