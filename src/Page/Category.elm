module Page.Category exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

view : { title : String, content : Html msg }
view =
  { title = "Category"
  , content = 
    div []
    [ h1 [] [text "This is category view"] ]
  }