module Page.Area exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "生物区・蝶分類の地理"
    , content =
        div []
            [ h1 [] [ text "This is area view" ] ]
    }
