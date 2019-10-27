module Page.Area exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "Area"
    , content =
        div []
            [ h1 [] [ text "This is area view" ] ]
    }
