module Page.Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "Home"
    , content =
        div []
            [ h1 [] [ text "This is home view" ] ]
    }
