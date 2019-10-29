module Route exposing (Route(..), href, parseUrl, routeToString)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser exposing (..)
import Url.Parser.Query as Q


type Route
    = Home
    | Reference
    | Category
    | Description
    | Area
    | Dictionary
    | ToggleMenu
    | Error


parseUrl : Url -> Maybe Route
parseUrl url =
    Url.Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Reference (s "reference")
        , map Category (s "category")
        , map Description (s "description")
        , map Area (s "area")
        , map Dictionary (s "dictionary")
        , map ToggleMenu (s "toggle-menu")
        , map Error (s "error")
        ]


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Reference ->
                    [ "reference" ]

                Category ->
                    [ "category" ]

                Description ->
                    [ "description" ]

                Area ->
                    [ "area" ]

                Dictionary ->
                    [ "dictionary" ]

                ToggleMenu ->
                    [ "toggle-menu" ]

                Error ->
                    [ "error" ]
    in
    String.join "/" pieces
