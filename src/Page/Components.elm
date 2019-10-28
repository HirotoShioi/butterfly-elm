module Page.Components exposing (..)

import Bulma.Elements exposing (..)
import Bulma.Modifiers.Typography exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, src)


sectionHeader : String -> Html msg
sectionHeader t =
    title H4 [] [ text t ]


her : Html msg
her =
    hr [ class "hr" ] []
