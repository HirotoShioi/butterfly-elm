module Route exposing (Route(..), href, parseUrl, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


type Route
    = Home
    | Reference
    | Category
    | Description
    | Area
    | Dictionary
    | Error
    | Detail String


parseUrl : Url -> Maybe Route
parseUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> parse parser


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Reference (s "reference")
        , map Category (s "category")
        , map Description (s "description")
        , map Area (s "area")
        , map Dictionary (s "dictionary")
        , map Error (s "error")
        , map Detail (s "detail" </> string)
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

                Error ->
                    [ "error" ]

                _ ->
                    []
    in
    "#/" ++ String.join "/" pieces
