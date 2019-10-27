module Page.Description exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "Description"
    , content =
        div []
            [ h1 [] [ text "This is description view" ] ]
    }
