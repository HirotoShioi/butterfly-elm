module Generator exposing (genDictionaryMsg, genMainModel, genNavBarMsg, genQueryMsg, genRoute, genSessionMsg)

import Butterfly.Api as Api
import Butterfly.Query as Query exposing (Query)
import Butterfly.Type exposing (toRegion)
import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer)
import Main
import NavBar
import Navigation as Nav exposing (Nav)
import Page.Dictionary as Dictionary
import Route exposing (Route)
import Session exposing (Session)
import Test exposing (..)
import Url as Url exposing (Url)



--------------------------------------------------------------------------------
-- Generators
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
        , Fuzz.map Dictionary.RegionClicked Fuzz.string
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
        , Fuzz.map Session.GotButterflyResponse (Fuzz.constant <| Api.GotButterflies <| Ok [])
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
        session =
            Tuple.first <| Session.init Nav.initWithStub NavBar.init

        dictionaryModel =
            Tuple.first <| Dictionary.init session
    in
    Fuzz.oneOf
        [ Fuzz.constant (Main.Home session)
        , Fuzz.constant (Main.NotFound session)
        , Fuzz.constant (Main.Reference session)
        , Fuzz.constant (Main.Area session)
        , Fuzz.constant (Main.Category session)
        , Fuzz.constant (Main.Error session)
        , Fuzz.constant (Main.Description session)
        , Fuzz.constant (Main.Dictionary dictionaryModel)

        -- | Loading Session Url.Url -- Generate random url
        -- | Detail Detail.Model -- Init requires butterfly data
        ]
