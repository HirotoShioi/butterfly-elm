module Tests exposing (testParse, urlRoundTrip, routeTest)

import Butterfly.Query as Query
import Expect
import Fuzzer as Fuzzer
import Route exposing (Route)
import Test exposing (Test, test, describe, fuzz)
import Url as Url



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


testParse : String -> String -> Maybe Route -> Test
testParse name path expectedRoute =
    test name <|
        \_ ->
            Url.fromString ("http://example.com/" ++ path)
                |> Maybe.andThen Route.parseUrl
                |> Expect.equal expectedRoute


routeTest : Test
routeTest =
    describe "Route"
        [ testParse "should parse Area" "area" (Just Route.Area)
        , testParse "should parse Category" "category" (Just Route.Category)
        , testParse "should parse Description" "description" (Just Route.Description)
        , testParse "should parse Detail" "detail/test" (Just <| Route.Detail "test")
        , testParse "should parse Dictionary" "dictionary" (Just <| Route.Dictionary Query.init)
        , testParse "should parse Error" "error" (Just Route.Error)
        , testParse "should parse Home" "" (Just Route.Home)
        , testParse "should parse Reference" "reference" (Just Route.Reference)
        ]



-- Roundtrip test for query


urlRoundTrip : Test
urlRoundTrip =
    describe "Url round trip"
        [ fuzz Fuzzer.fuzzRoute "Should be able to perform round trip test" <|
            \route ->
                let
                    url =
                        "http://example.com"
                            ++ Route.routeToString route
                            |> Url.fromString
                            |> Maybe.andThen Route.parseUrl

                    expectedRoute =
                        case route of
                            Route.Dictionary query ->
                                Route.Dictionary { query | maxShowCount = Query.maxShowCount }

                            others ->
                                others
                in
                Expect.equal url (Just expectedRoute)
        ]
