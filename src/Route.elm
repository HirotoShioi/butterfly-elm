module Route exposing (Route(..), parseUrl, href, routeToString)

import Url exposing (Url)
import Url.Parser exposing (..)
import Url.Parser.Query as Q
import Html exposing (Attribute)
import Html.Attributes as Attr

type Route
  = Home
  | Reference
  | Category
  | Description
  | Area
  | Dictionary

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
    in
      String.join "/" pieces