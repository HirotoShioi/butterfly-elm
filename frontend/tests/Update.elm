module Update exposing (..)

import Butterfly.Api as Api
import Butterfly.Query as Query exposing (Query)
import Butterfly.Type exposing (toRegion)
import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer)
import NavBar
import Navigation as Nav exposing (Nav)
import Page.Dictionary as Dictionary
import Session exposing (Session)
import Test exposing (..)


navBarTest : Test
navBarTest =
    describe "NavBar"
        [ test "Init" <| \_ -> Expect.equal False NavBar.init
        , fuzz randomNavBarMsg "should properly update" <|
            \navMsg -> validateNavBar navMsg NavBar.init
        ]


validateNavBar : NavBar.Msg -> NavBar.Model -> Expectation
validateNavBar msg model =
    let
        ( updatedModel, cmd ) =
            NavBar.update msg model
    in
    case msg of
        NavBar.ToggleMenu ->
            Expect.equal updatedModel (not model)

        NavBar.DisableMenu ->
            Expect.equal updatedModel False


sessionUpdateTest : Test
sessionUpdateTest =
    describe "Session"
        [ test "Init" <|
            \_ ->
                initSession
                    |> (\( session, _ ) -> Expect.equal session expectedInitSession)
        , fuzz randomSessionMsg "Should properly update its model with update" <|
            \sessionMsg ->
                initSession
                    |> (\( session, _ ) -> validateSession sessionMsg session)
        ]


validateSession : Session.Msg -> Session -> Expectation
validateSession msg session =
    let
        ( updatedSession, cmd ) =
            Session.update msg session
    in
    case msg of
        Session.DisableMenu ->
            Expect.equal updatedSession.navModel False

        Session.FromDictionary queryMsg ->
            Expect.equal updatedSession.query (Query.update Query.init queryMsg)

        Session.FromDetail queryMsg ->
            Expect.equal updatedSession.query (Query.update Query.init queryMsg)

        Session.GotButterflyResponse (Api.GotButterflies res) ->
            Expect.equal updatedSession.butterflies (Ok [])

        Session.GotNavMessage navMsg ->
            Expect.equal updatedSession.navModel
                (Tuple.first <| NavBar.update navMsg NavBar.init)


dictionaryUpdateTest : Test
dictionaryUpdateTest =
    describe "Dictionary"
        [ fuzz randomDictionaryMsg "Query" <|
            \dictionaryMsg ->
                initSession
                    |> Tuple.first
                    |> Dictionary.init
                    |> validateDictionary dictionaryMsg
        ]


validateDictionary :
    Dictionary.Msg
    -> ( Dictionary.Model, Cmd Dictionary.Msg )
    -> Expectation
validateDictionary msg ( fromModel, fromCmd ) =
    let
        ( toModel, cmd ) =
            Dictionary.update msg fromModel
    in
    case msg of
        Dictionary.ToggleRegionMenu ->
            let
                expectedModel =
                    { fromModel | isRegionMenuOpen = True }
            in
            Expect.equal expectedModel toModel

        Dictionary.ToggleColorMenu ->
            let
                expectedModel =
                    { fromModel | isColorMenuOpen = True }
            in
            Expect.equal expectedModel toModel

        Dictionary.ToggleCategoryMenu ->
            let
                expectedModel =
                    { fromModel | isCategoryMenuOpen = True }
            in
            Expect.equal expectedModel toModel

        Dictionary.ColorClicked color ->
            Expect.equal toModel.session.query.hexColor (Just color)

        Dictionary.RegionClicked regionStr ->
            case toRegion regionStr of
                Err _ ->
                    Expect.equal toModel.session.query.region Nothing

                Ok region ->
                    Expect.equal toModel.session.query.region (Just region)

        Dictionary.CategoryClicked category ->
            Expect.equal toModel.session.query.category (Just category)

        Dictionary.ResetCategory ->
            Expect.equal toModel.session.query.category Nothing

        Dictionary.ResetColor ->
            Expect.equal toModel.session.query.hexColor Nothing

        Dictionary.ResetRegion ->
            Expect.equal toModel.session.query.region Nothing

        _ ->
            Expect.true "Not implemented" True


expectedInitSession : Session
expectedInitSession =
    let
        nav =
            Nav.initWithStub

        navBar =
            NavBar.init

        query =
            Query.init

        response =
            Ok []
    in
    Session nav navBar query response


initSession : ( Session, Cmd Session.Msg )
initSession =
    Session.init Nav.initWithStub NavBar.init



--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------


randomDictionaryMsg : Fuzzer Dictionary.Msg
randomDictionaryMsg =
    Fuzz.oneOf
        [ Fuzz.constant Dictionary.ToggleRegionMenu
        , Fuzz.map Dictionary.RegionClicked Fuzz.string
        , Fuzz.constant Dictionary.ToggleCategoryMenu
        , Fuzz.constant Dictionary.ToggleColorMenu
        , Fuzz.constant Dictionary.ResetColor
        , Fuzz.constant Dictionary.ResetCategory
        , Fuzz.constant Dictionary.ResetRegion
        , Fuzz.map Dictionary.ColorClicked Fuzz.string
        , Fuzz.map Dictionary.CategoryClicked Fuzz.string
        ]


randomQueryMsg : Fuzzer Query.Msg
randomQueryMsg =
    Fuzz.oneOf
        [ Fuzz.constant Query.ResetRegion
        , Fuzz.constant Query.ResetCategory
        , Fuzz.constant Query.ResetColor
        , Fuzz.map Query.UpdateCategory Fuzz.string
        , Fuzz.map Query.UpdateColor Fuzz.string
        ]


randomNavBarMsg : Fuzzer NavBar.Msg
randomNavBarMsg =
    Fuzz.oneOf
        [ Fuzz.constant NavBar.ToggleMenu
        , Fuzz.constant NavBar.DisableMenu
        ]


randomSessionMsg : Fuzzer Session.Msg
randomSessionMsg =
    Fuzz.oneOf
        [ Fuzz.constant Session.DisableMenu
        , Fuzz.map Session.FromDictionary randomQueryMsg
        , Fuzz.map Session.FromDetail randomQueryMsg
        , Fuzz.map Session.GotButterflyResponse (Fuzz.constant <| Api.GotButterflies <| Ok [])
        , Fuzz.map Session.GotNavMessage randomNavBarMsg
        ]
