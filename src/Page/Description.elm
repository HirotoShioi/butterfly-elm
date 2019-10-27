module Page.Description exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "蝶の解説"
    , content =
        div []
            [ h1 [] [ text "This is description view" ] ]
    }
