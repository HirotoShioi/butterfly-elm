module Page.NotFound exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Page


view : Html msg
view =
    div []
        [ h1 [] [ text "Page not found!" ] ]


title : String
title =
    Page.notFoundTitle
