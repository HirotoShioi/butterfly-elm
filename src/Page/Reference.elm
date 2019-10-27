module Page.Reference exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "参考文献"
    , content =
        div []
            [ h1 [] [ text "This is reference view" ] ]
    }
