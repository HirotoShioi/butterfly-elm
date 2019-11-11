module UpdateTest exposing (dictionaryUpdateTest, navBarTest, sessionUpdateTest)

import Butterfly.Api as Api
import Butterfly.Query as Query exposing (Query)
import Butterfly.Type exposing (toRegion)
import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer)
import Generator as Gen
import Main
import NavBar
import Navigation as Nav exposing (Nav)
import Page.Dictionary as Dictionary
import Session exposing (Session)
import Test exposing (..)
import Url as Url exposing (Url)



--------------------------------------------------------------------------------
-- NavBar
--------------------------------------------------------------------------------


navBarTest : Test
navBarTest =
    describe "NavBar"
        [ test "Init" <| \_ -> Expect.equal False NavBar.init
        , fuzz Gen.genNavBarMsg "should properly update" <|
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



--------------------------------------------------------------------------------
-- Session
--------------------------------------------------------------------------------


sessionUpdateTest : Test
sessionUpdateTest =
    describe "Session"
        [ test "Init" <|
            \_ ->
                initSession
                    |> (\( session, _ ) -> Expect.equal session expectedInitSession)
        , fuzz (Fuzz.tuple ( Gen.genSessionMsg, Gen.genSession )) "Should properly update its model with update" <|
            \( sessionMsg, session ) -> validateSession sessionMsg session
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
            case res of
                Ok butterflies ->
                    Expect.equal updatedSession.butterflies (Ok butterflies)

                Err _ ->
                    Expect.true "Not yet implemented" True

        Session.GotNavMessage navMsg ->
            Expect.equal updatedSession.navModel
                (Tuple.first <| NavBar.update navMsg session.navModel)



--------------------------------------------------------------------------------
-- Dictionary
--------------------------------------------------------------------------------


dictionaryUpdateTest : Test
dictionaryUpdateTest =
    describe "Dictionary"
        [ fuzz (Fuzz.tuple ( Gen.genDictionaryMsg, Gen.genSession )) "Should handle update as expected" <|
            \( dictionaryMsg, session ) ->
                Dictionary.init session
                    |> validateDictionary dictionaryMsg
        ]


validateDictionary :
    Dictionary.Msg
    -> ( Dictionary.Model, Cmd Dictionary.Msg )
    -> Expectation
validateDictionary msg ( before, fromCmd ) =
    let
        ( after, cmd ) =
            Dictionary.update msg before
    in
    case msg of
        Dictionary.ToggleRegionMenu ->
            let
                expectedModel =
                    { before | isRegionMenuOpen = True }
            in
            Expect.equal expectedModel after

        Dictionary.ToggleColorMenu ->
            let
                expectedModel =
                    { before | isColorMenuOpen = True }
            in
            Expect.equal expectedModel after

        Dictionary.ToggleCategoryMenu ->
            let
                expectedModel =
                    { before | isCategoryMenuOpen = True }
            in
            Expect.equal expectedModel after

        Dictionary.ColorClicked color ->
            Expect.equal after.session.query.hexColor (Just color)

        Dictionary.RegionClicked regionStr ->
            case toRegion regionStr of
                Err _ ->
                    Expect.equal after.session.query.region Nothing

                Ok region ->
                    Expect.equal after.session.query.region (Just region)

        Dictionary.CategoryClicked category ->
            Expect.equal after.session.query.category (Just category)

        Dictionary.ResetCategory ->
            Expect.equal after.session.query.category Nothing

        Dictionary.ResetColor ->
            Expect.equal after.session.query.hexColor Nothing

        Dictionary.ResetRegion ->
            Expect.equal after.session.query.region Nothing

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
