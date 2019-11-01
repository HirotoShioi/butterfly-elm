module Page.NotFound exposing (title, view)

import Html exposing (Html, div, h1, text)
import Page


view : Html msg
view =
    div []
        [ h1 [] [ text "Page not found!" ] ]


title : String
title =
    Page.notFoundTitle
