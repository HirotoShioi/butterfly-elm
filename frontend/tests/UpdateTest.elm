module UpdateTest exposing (detailUpdateTest, dictionaryUpdateTest, navBarTest, queryUpdateTest, sessionUpdateTest)

import Butterfly.Api as Api
import Butterfly.Query as Query exposing (Query)
import Butterfly.Type exposing (toRegion)
import Expect exposing (Expectation)
import Fuzz as Fuzz
import Fuzzer as Fuzz
import NavBar
import Navigation as Nav
import Page.Detail as Detail
import Page.Dictionary as Dictionary
import Session exposing (Session)
import Test exposing (Test, describe, fuzz, test)



--------------------------------------------------------------------------------
-- NavBar
--------------------------------------------------------------------------------


navBarTest : Test
navBarTest =
    describe "NavBar"
        [ test "Init" <| \_ -> Expect.equal False NavBar.init
        , fuzz Fuzz.fuzzNavBarMsg "should properly update" <|
            \navMsg -> validateNavBar navMsg NavBar.init
        ]


validateNavBar : NavBar.Msg -> NavBar.Model -> Expectation
validateNavBar msg model =
    let
        ( updatedModel, _ ) =
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
        , fuzz (Fuzz.tuple ( Fuzz.fuzzSessionMsg, Fuzz.fuzzSession ))
            "Should properly update its model with update"
          <|
            \( sessionMsg, session ) -> validateSession sessionMsg session
        ]


expectedInitSession : Session
expectedInitSession =
    let
        nav =
            Nav.initWithStub

        navBar =
            NavBar.init

        response =
            Ok []
    in
    Session nav navBar response


validateSession : Session.Msg -> Session -> Expectation
validateSession msg session =
    let
        ( updatedSession, _ ) =
            Session.update msg session
    in
    case msg of
        Session.DisableMenu ->
            Expect.equal updatedSession.navModel False

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
        [ fuzz (Fuzz.tuple ( Fuzz.fuzzDictionaryMsg, Fuzz.fuzzDictionaryModel ))
            "Should handle update as expected"
          <|
            \( dictionaryMsg, model ) -> validateDictionary dictionaryMsg model
        ]


validateDictionary :
    Dictionary.Msg
    -> Dictionary.Model
    -> Expectation
validateDictionary msg before =
    let
        ( after, _ ) =
            Dictionary.update msg before
    in
    case msg of
        Dictionary.ToggleRegionMenu ->
            let
                expectedModel =
                    { before
                        | isRegionMenuOpen = not before.isRegionMenuOpen
                        , isCategoryMenuOpen = False
                        , isColorMenuOpen = False
                    }
            in
            Expect.equal expectedModel after

        Dictionary.ToggleColorMenu ->
            let
                expectedModel =
                    { before
                        | isColorMenuOpen = not before.isColorMenuOpen
                        , isRegionMenuOpen = False
                        , isCategoryMenuOpen = False
                    }
            in
            Expect.equal expectedModel after

        Dictionary.ToggleCategoryMenu ->
            let
                expectedModel =
                    { before
                        | isCategoryMenuOpen = not before.isCategoryMenuOpen
                        , isColorMenuOpen = False
                        , isRegionMenuOpen = False
                    }
            in
            Expect.equal expectedModel after

        Dictionary.ColorClicked color ->
            Expect.equal after.query.hexColor (Just color)

        Dictionary.RegionClicked regionStr ->
            case toRegion regionStr of
                Err _ ->
                    Expect.equal after.query.region before.query.region

                Ok region ->
                    Expect.equal after.query.region (Just region)

        Dictionary.CategoryClicked category ->
            Expect.equal after.query.category (Just category)

        Dictionary.ResetCategory ->
            Expect.equal after.query.category Nothing

        Dictionary.ResetColor ->
            Expect.equal after.query.hexColor Nothing

        Dictionary.ResetRegion ->
            Expect.equal after.query.region Nothing

        Dictionary.LoadButterflies ->
            Expect.equal after.query.maxShowCount (before.query.maxShowCount + Query.maxShowCount)

        _ ->
            Expect.true "Not implemented" True



--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------


queryUpdateTest : Test
queryUpdateTest =
    describe "Query"
        [ fuzz (Fuzz.tuple ( Fuzz.fuzzQueryMsg, Fuzz.fuzzQuery )) "Should update Query as expected" <|
            \( queryMsg, query ) -> validateQueryUpdate queryMsg query
        , test "Init should generate appopriate Query" <|
            \_ ->
                Expect.all
                    [ \q -> Expect.equal Nothing q.hexColor
                    , \q -> Expect.equal Nothing q.region
                    , \q -> Expect.equal Nothing q.category
                    ]
                    Query.init
        ]


validateQueryUpdate : Query.Msg -> Query -> Expectation
validateQueryUpdate msg query =
    let
        updatedQuery =
            Query.update query msg
    in
    case msg of
        Query.ResetCategory ->
            let
                expected =
                    { query
                        | category = Nothing
                        , maxShowCount = Query.maxShowCount
                    }
            in
            Expect.equal updatedQuery expected

        Query.ResetColor ->
            let
                expected =
                    { query
                        | hexColor = Nothing
                        , maxShowCount = Query.maxShowCount
                    }
            in
            Expect.equal updatedQuery expected

        Query.ResetRegion ->
            let
                expected =
                    { query
                        | region = Nothing
                        , maxShowCount = Query.maxShowCount
                    }
            in
            Expect.equal updatedQuery expected

        Query.ResetName ->
            let
                expected =
                    { query
                        | name = Nothing
                        , maxShowCount = Query.maxShowCount
                    }
            in
            Expect.equal updatedQuery expected

        Query.UpdateCategory category ->
            let
                expected =
                    { query
                        | category = Just category
                        , maxShowCount = Query.maxShowCount
                    }
            in
            Expect.equal updatedQuery expected

        Query.UpdateColor hexColor ->
            let
                expected =
                    { query
                        | hexColor = Just hexColor
                        , maxShowCount = Query.maxShowCount
                    }
            in
            Expect.equal updatedQuery expected

        Query.UpdateRegion region ->
            let
                expected =
                    { query
                        | region = Just region
                        , maxShowCount = Query.maxShowCount
                    }
            in
            Expect.equal updatedQuery expected

        Query.UpdateName name ->
            let
                expected =
                    { query | name = Just name, maxShowCount = Query.maxShowCount }
            in
            Expect.equal updatedQuery expected

        Query.ResetAll ->
            Expect.all
                [ \q -> Expect.equal q.hexColor Nothing
                , \q -> Expect.equal q.region Nothing
                , \q -> Expect.equal q.category Nothing
                , \q -> Expect.equal q.maxShowCount Query.maxShowCount
                ]
                updatedQuery

        Query.LoadMore ->
            Expect.equal updatedQuery.maxShowCount (query.maxShowCount + Query.maxShowCount)



--------------------------------------------------------------------------------
-- Detail
--------------------------------------------------------------------------------


detailUpdateTest : Test
detailUpdateTest =
    describe "Detail"
        [ fuzz (Fuzz.tuple ( Fuzz.fuzzSession, Fuzz.fuzzButterfly )) "Should initiate as expected" <|
            \( session, butterfly ) ->
                let
                    detailModel =
                        Detail.init session butterfly |> Tuple.first
                in
                Expect.all
                    [ \m -> Expect.equal m.session session
                    , \m -> Expect.equal m.butterfly butterfly
                    ]
                    detailModel
        , fuzz (Fuzz.tuple ( Fuzz.fuzzDetailMsg, Fuzz.fuzzDetailModel )) "Should update appopriately" <|
            \( msg, model ) -> validateDetailUpdate msg model
        ]


validateDetailUpdate : Detail.Msg -> Detail.Model -> Expectation
validateDetailUpdate msg model =
    let
        updatedModel =
            Detail.update msg model |> Tuple.first
    in
    case msg of
        Detail.ColorClicked _ ->
            Expect.equal updatedModel model

        Detail.RegionClicked regionStr ->
            case toRegion regionStr of
                Err _ ->
                    Expect.equal updatedModel model

                Ok _ ->
                    Expect.equal updatedModel model

        Detail.CategoryClicked _ ->
            Expect.equal updatedModel model

        Detail.GotSessionMsg sessionMsg ->
            let
                updatedSession =
                    Session.update sessionMsg model.session |> Tuple.first

                expectedModel =
                    { model | session = updatedSession }
            in
            Expect.equal updatedModel expectedModel

        Detail.GoBack ->
            Expect.equal updatedModel model


initSession : ( Session, Cmd Session.Msg )
initSession =
    Session.init Nav.initWithStub NavBar.init
