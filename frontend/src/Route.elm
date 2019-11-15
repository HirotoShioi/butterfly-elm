module Route exposing (Route(..), href, parseUrl, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Butterfly.Query as Query exposing (Query)
import Butterfly.Type exposing (Region, toRegion)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
    | Reference
    | Category
    | Description
    | Area
    | Dictionary Query
    | Error
    | Detail String


isNothing : Maybe a -> Bool
isNothing mValue =
    case mValue of
        Nothing ->
            True

        Just _ ->
            False


allNothing : List (Maybe a) -> Bool
allNothing maybes =
    List.all isNothing maybes



-- name region category color


mkDictionary : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Route
mkDictionary mName mRegionStr mCategory mColor =
    let
        query =
            Query.initWithArgs mName mRegionStr mCategory mColor
    in
    Dictionary query


parseUrl : Url -> Maybe Route
parseUrl url =
    parse parser url


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
        , map mkDictionary (s "dictionary" <?> Query.string "name" <?> Query.string "region" <?> Query.string "category" <?> Query.string "hexColor")
        , map Error (s "error")
        , map Detail (s "detail" </> string)
        ]


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)



-- Fix this


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    ""

                Reference ->
                    "reference"

                Category ->
                    "category"

                Description ->
                    "description"

                Area ->
                    "area"

                Dictionary query ->
                    Builder.relative [ "dictionary" ] (Query.intoQueryParameter query)

                Error ->
                    "error"

                _ ->
                    ""
    in
    "/" ++ pieces
