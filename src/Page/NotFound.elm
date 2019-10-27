module Page.NotFound exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "NotFound"
    , content =
        div []
            [ h1 [] [ text "Page not found!" ] ]
    }
