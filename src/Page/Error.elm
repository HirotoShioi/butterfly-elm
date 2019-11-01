module Page.Error exposing (title, view)

import Html exposing (Html, div, text)
import Page


title : String
title =
    Page.errorTitle


view : Html msg
view =
    div [] [ text "Someting occured while loading" ]
