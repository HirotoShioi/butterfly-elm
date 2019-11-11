module Generator exposing
    ( genButterfly
    , genDetailModel
    , genDetailMsg
    , genDictionaryMsg
    , genMainModel
    , genNavBarMsg
    , genQuery
    , genQueryMsg
    , genRoute
    , genSession
    , genSessionMsg
    )

import Butterfly.Api as Api
import Butterfly.Query as Query exposing (Query)
import Butterfly.Type exposing (Butterfly, Color, Region(..), fromRegion, regionList, toRegion)
import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer)
import Main
import NavBar
import Navigation as Nav exposing (Nav)
import Page.Detail as Detail
import Page.Dictionary as Dictionary
import Random exposing (Generator)
import Random.Char as Random
import Random.Extra as Random
import Random.Float as Random
import Random.Int as Random
import Random.List as Random
import Random.String as Random
import Route exposing (Route)
import Session exposing (Session)
import Shrink
import Test exposing (..)
import Url as Url exposing (Url)



--------------------------------------------------------------------------------
-- Fuzzer
--------------------------------------------------------------------------------


genDictionaryMsg : Fuzzer Dictionary.Msg
genDictionaryMsg =
    Fuzz.oneOf
        [ Fuzz.constant Dictionary.ToggleRegionMenu
        , Fuzz.constant Dictionary.ToggleCategoryMenu
        , Fuzz.constant Dictionary.ToggleColorMenu
        , Fuzz.constant Dictionary.ResetColor
        , Fuzz.constant Dictionary.ResetCategory
        , Fuzz.constant Dictionary.ResetRegion
        , Fuzz.map Dictionary.RegionClicked
            (Fuzz.oneOf [ genRegionStr, Fuzz.string ])
        , Fuzz.map Dictionary.ColorClicked Fuzz.string
        , Fuzz.map Dictionary.CategoryClicked Fuzz.string
        ]


genQueryMsg : Fuzzer Query.Msg
genQueryMsg =
    Fuzz.oneOf
        [ Fuzz.constant Query.ResetRegion
        , Fuzz.constant Query.ResetCategory
        , Fuzz.constant Query.ResetColor
        , Fuzz.map Query.UpdateCategory Fuzz.string
        , Fuzz.map Query.UpdateColor Fuzz.string
        , Fuzz.map Query.UpdateRegion genRegion
        ]


genNavBarMsg : Fuzzer NavBar.Msg
genNavBarMsg =
    Fuzz.oneOf
        [ Fuzz.constant NavBar.ToggleMenu
        , Fuzz.constant NavBar.DisableMenu
        ]


genSessionMsg : Fuzzer Session.Msg
genSessionMsg =
    Fuzz.oneOf
        [ Fuzz.constant Session.DisableMenu
        , Fuzz.map Session.FromDictionary genQueryMsg
        , Fuzz.map Session.FromDetail genQueryMsg
        , Fuzz.map Session.GotButterflyResponse
            (Fuzz.list genButterfly
                |> Fuzz.map
                    (\bs ->
                        Ok bs
                            |> Api.GotButterflies
                    )
            )
        , Fuzz.map Session.GotNavMessage genNavBarMsg
        ]


genRoute : Fuzzer Route
genRoute =
    Fuzz.oneOf
        [ Fuzz.constant Route.Home
        , Fuzz.constant Route.Reference
        , Fuzz.constant Route.Category
        , Fuzz.constant Route.Description
        , Fuzz.constant Route.Area
        , Fuzz.constant Route.Dictionary
        , Fuzz.constant Route.Error

        --  , Fuzz.map Route.Detail (Fuzz.string)
        ]


genMainModel : Fuzzer Main.Model
genMainModel =
    let
        dictionaryModel session =
            Dictionary.init session |> Tuple.first
    in
    Fuzz.oneOf
        [ Fuzz.map Main.Home genSession
        , Fuzz.map Main.NotFound genSession
        , Fuzz.map Main.Reference genSession
        , Fuzz.map Main.Area genSession
        , Fuzz.map Main.Category genSession
        , Fuzz.map Main.Error genSession
        , Fuzz.map Main.Description genSession
        , Fuzz.map Main.Dictionary (Fuzz.map dictionaryModel genSession)

        -- | Loading Session Url.Url -- Generate random url
        -- | Detail Detail.Model -- Init requires butterfly data
        ]


genSession : Fuzzer Session
genSession =
    let
        session =
            Session.init Nav.initWithStub NavBar.init |> Tuple.first
    in
    Fuzz.map3
        (\isNavOpen butterflies query ->
            { session
                | butterflies =
                    if List.isEmpty butterflies then
                        Err "No butterflies found"

                    else
                        Ok butterflies
                , navModel = isNavOpen
                , query = query
            }
        )
        Fuzz.bool
        (Fuzz.list genButterfly)
        genQuery


genDetailModel : Fuzzer Detail.Model
genDetailModel =
    Fuzz.map2
        Detail.Model
        genSession
        genButterfly


genDetailMsg : Fuzzer Detail.Msg
genDetailMsg =
    Fuzz.oneOf
        [ Fuzz.map Detail.ColorClicked genHexString
        , Fuzz.map Detail.RegionClicked
            (Fuzz.oneOf [ genRegionStr, Fuzz.string ])
        , Fuzz.map Detail.CategoryClicked Fuzz.string
        , Fuzz.map Detail.GotSessionMsg genSessionMsg
        ]


genButterfly : Fuzzer Butterfly
genButterfly =
    Fuzz.custom generateButterfly Shrink.noShrink


genQuery : Fuzzer Query
genQuery =
    Fuzz.custom generateQuery Shrink.noShrink


genRegion : Fuzzer Region
genRegion =
    Fuzz.custom generateRegion Shrink.noShrink


genRegionStr : Fuzzer String
genRegionStr =
    Fuzz.custom generateRegionStr Shrink.noShrink


genHexString : Fuzzer String
genHexString =
    Fuzz.custom generateHexString Shrink.noShrink



--------------------------------------------------------------------------------
-- Generator
--------------------------------------------------------------------------------


generateButterfly : Generator Butterfly
generateButterfly =
    Random.map Butterfly generateRegionStr
        |> Random.andMap genKatakana
        |> Random.andMap genString
        |> Random.andMap genString
        |> Random.andMap genKatakana
        |> Random.andMap genString
        |> Random.andMap genString
        |> Random.andMap genString
        |> Random.andMap (Random.int 10 300)
        |> Random.andMap (Random.maybe (Random.oneIn 5) genString)
        |> Random.andMap (Random.maybe (Random.oneIn 5) genString)
        |> Random.andMap (Random.list 10 genColor)


genColor : Generator Color
genColor =
    Random.map Color (Random.float 0 1)
        |> Random.andMap (Random.float 0 1)
        |> Random.andMap generateHexString


generateHexString : Generator String
generateHexString =
    let
        genHexChar =
            Random.sample
                [ "0"
                , "1"
                , "2"
                , "3"
                , "4"
                , "5"
                , "6"
                , "7"
                , "8"
                , "9"
                , "a"
                , "b"
                , "c"
                , "d"
                , "e"
                , "f"
                ]
                |> Random.map (Maybe.withDefault "0")
    in
    Random.list 6 genHexChar
        |> Random.map (\ls -> "#" :: ls |> String.concat)


genString : Generator String
genString =
    Random.string 10 Random.english


genKatakana : Generator String
genKatakana =
    Random.string 10 Random.katakana


genHiragana : Generator String
genHiragana =
    Random.string 10 Random.hiragana


generateRegionStr : Generator String
generateRegionStr =
    Random.choose regionList
        |> Random.map
            (\pair ->
                Tuple.first pair
                    |> Maybe.withDefault OldNorth
                    |> fromRegion
            )


generateRegion : Generator Region
generateRegion =
    Random.choose regionList
        |> Random.map Tuple.first
        |> Random.map (Maybe.withDefault OldNorth)


generateQuery : Generator Query
generateQuery =
    let
        toMaybe gen =
            Random.maybe (Random.oneIn 5) gen
    in
    Random.map Query (toMaybe generateRegion)
        |> Random.andMap (toMaybe genKatakana)
        |> Random.andMap (toMaybe genHiragana)
        |> Random.andMap (toMaybe generateHexString)
        |> Random.andMap (Random.constant 70)
