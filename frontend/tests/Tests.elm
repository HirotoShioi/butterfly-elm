module Tests exposing (..)

import Expect
import Route exposing (Route)
import Test exposing (..)
import Url exposing (Url)
import Butterfly.Query as Query
import Fuzzer as Fuzzer

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