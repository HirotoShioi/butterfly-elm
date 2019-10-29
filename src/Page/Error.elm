module Page.Error exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Page


title : String
title =
    Page.errorTitle


view : Html msg
view =
    div [] [ text "Someting occured while loading" ]
