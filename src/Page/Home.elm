module Page.Home exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "ホーム"
    , content =
        div []
            [ h1 [] [ text "This is home view" ] ]
    }
