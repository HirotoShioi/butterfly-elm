module Fuzzer exposing
    ( fuzzButterfly
    , fuzzDetailModel
    , fuzzDetailMsg
    , fuzzDictionaryModel
    , fuzzDictionaryMsg
    , fuzzMainModel
    , fuzzNavBarMsg
    , fuzzQuery
    , fuzzQueryMsg
    , fuzzRoute
    , fuzzSession
    , fuzzSessionMsg
    )

import Butterfly.Api as Api
import Butterfly.Query as Query exposing (Query)
import Butterfly.Type exposing (Butterfly, Color, Region(..), fromRegion, regionList)
import Fuzz as Fuzz exposing (Fuzzer)
import Main
import NavBar
import Navigation as Nav
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



--------------------------------------------------------------------------------
-- Fuzzer
--------------------------------------------------------------------------------


fuzzDictionaryMsg : Fuzzer Dictionary.Msg
fuzzDictionaryMsg =
    Fuzz.oneOf
        [ Fuzz.constant Dictionary.ToggleRegionMenu
        , Fuzz.constant Dictionary.ToggleCategoryMenu
        , Fuzz.constant Dictionary.ToggleColorMenu
        , Fuzz.constant Dictionary.ResetColor
        , Fuzz.constant Dictionary.ResetCategory
        , Fuzz.constant Dictionary.ResetRegion
        , Fuzz.constant Dictionary.LoadButterflies
        , Fuzz.map Dictionary.RegionClicked
            (Fuzz.oneOf [ fuzzRegionStr, Fuzz.string ])
        , Fuzz.map Dictionary.ColorClicked Fuzz.string
        , Fuzz.map Dictionary.CategoryClicked Fuzz.string
        , Fuzz.constant Dictionary.NameSearch
        , Fuzz.map Dictionary.SearchInput Fuzz.string
        ]


fuzzDictionaryModel : Fuzzer Dictionary.Model
fuzzDictionaryModel =
    Fuzz.map
        Dictionary.Model
        fuzzSession
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap fuzzQuery
        |> Fuzz.andMap Fuzz.string


fuzzQueryMsg : Fuzzer Query.Msg
fuzzQueryMsg =
    Fuzz.oneOf
        [ Fuzz.constant Query.ResetRegion
        , Fuzz.constant Query.ResetCategory
        , Fuzz.constant Query.ResetColor
        , Fuzz.map Query.UpdateCategory Fuzz.string
        , Fuzz.map Query.UpdateColor Fuzz.string
        , Fuzz.map Query.UpdateRegion fuzzRegion
        , Fuzz.constant Query.ResetAll
        , Fuzz.constant Query.LoadMore
        ]


fuzzNavBarMsg : Fuzzer NavBar.Msg
fuzzNavBarMsg =
    Fuzz.oneOf
        [ Fuzz.constant NavBar.ToggleMenu
        , Fuzz.constant NavBar.DisableMenu
        ]


fuzzSessionMsg : Fuzzer Session.Msg
fuzzSessionMsg =
    Fuzz.oneOf
        [ Fuzz.constant Session.DisableMenu
        , Fuzz.map Session.GotButterflyResponse
            (Fuzz.list fuzzButterfly
                |> Fuzz.map
                    (\bs ->
                        Ok bs
                            |> Api.GotButterflies
                    )
            )
        , Fuzz.map Session.GotNavMessage fuzzNavBarMsg
        ]


fuzzRoute : Fuzzer Route
fuzzRoute =
    Fuzz.oneOf
        [ Fuzz.constant Route.Home
        , Fuzz.constant Route.Reference
        , Fuzz.constant Route.Category
        , Fuzz.constant Route.Description
        , Fuzz.constant Route.Area
        , Fuzz.map Route.Dictionary fuzzQuery
        , Fuzz.constant Route.Error

        --  , Fuzz.map Route.Detail (Fuzz.string)
        ]


fuzzMainModel : Fuzzer Main.Model
fuzzMainModel =
    let
        dictionaryModel session query =
            Dictionary.init session query |> Tuple.first
    in
    Fuzz.oneOf
        [ Fuzz.map Main.Home fuzzSession
        , Fuzz.map Main.NotFound fuzzSession
        , Fuzz.map Main.Reference fuzzSession
        , Fuzz.map Main.Area fuzzSession
        , Fuzz.map Main.Category fuzzSession
        , Fuzz.map Main.Error fuzzSession
        , Fuzz.map Main.Description fuzzSession
        , Fuzz.map Main.Dictionary (Fuzz.map2 dictionaryModel fuzzSession fuzzQuery)

        -- | Loading Session Url.Url -- Generate random url
        -- | Detail Detail.Model -- Init requires butterfly data
        ]


fuzzSession : Fuzzer Session
fuzzSession =
    let
        session =
            Session.init Nav.initWithStub NavBar.init |> Tuple.first
    in
    Fuzz.map2
        (\isNavOpen butterflies ->
            { session
                | butterflies =
                    if List.isEmpty butterflies then
                        Err "No butterflies found"

                    else
                        Ok butterflies
                , navModel = isNavOpen
            }
        )
        Fuzz.bool
        (Fuzz.list fuzzButterfly)


fuzzDetailModel : Fuzzer Detail.Model
fuzzDetailModel =
    Fuzz.map2
        Detail.Model
        fuzzSession
        fuzzButterfly


fuzzDetailMsg : Fuzzer Detail.Msg
fuzzDetailMsg =
    Fuzz.oneOf
        [ Fuzz.map Detail.ColorClicked fuzzHexString
        , Fuzz.map Detail.RegionClicked
            (Fuzz.oneOf [ fuzzRegionStr, Fuzz.string ])
        , Fuzz.map Detail.CategoryClicked Fuzz.string
        , Fuzz.map Detail.GotSessionMsg fuzzSessionMsg
        ]


fuzzButterfly : Fuzzer Butterfly
fuzzButterfly =
    Fuzz.custom genButterfly Shrink.noShrink


fuzzQuery : Fuzzer Query
fuzzQuery =
    Fuzz.custom genQuery Shrink.noShrink


fuzzRegion : Fuzzer Region
fuzzRegion =
    Fuzz.custom genRegion Shrink.noShrink


fuzzRegionStr : Fuzzer String
fuzzRegionStr =
    Fuzz.custom genRegionStr Shrink.noShrink


fuzzHexString : Fuzzer String
fuzzHexString =
    Fuzz.custom genHexString Shrink.noShrink



--------------------------------------------------------------------------------
-- Generator
--------------------------------------------------------------------------------


genButterfly : Generator Butterfly
genButterfly =
    Random.map Butterfly genRegionStr
        |> Random.andMap genKatakana
        |> Random.andMap genString
        |> Random.andMap (Random.maybe (Random.oneIn 5) genString)
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
        |> Random.andMap genHexString


genHexString : Generator String
genHexString =
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


genRegionStr : Generator String
genRegionStr =
    Random.choose regionList
        |> Random.map
            (\pair ->
                Tuple.first pair
                    |> Maybe.withDefault OldNorth
                    |> fromRegion
            )


genRegion : Generator Region
genRegion =
    Random.choose regionList
        |> Random.map Tuple.first
        |> Random.map (Maybe.withDefault OldNorth)


genQuery : Generator Query
genQuery =
    let
        toMaybe gen =
            Random.maybe (Random.oneIn 5) gen
    in
    Random.map Query (toMaybe genRegion)
        |> Random.andMap (toMaybe genKatakana)
        |> Random.andMap (toMaybe genHiragana)
        |> Random.andMap (toMaybe genHexString)
        |> Random.andMap (Random.constant 70)
        |> Random.andMap (Random.int 10 100)
