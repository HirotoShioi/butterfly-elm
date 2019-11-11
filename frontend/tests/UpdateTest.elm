module UpdateTest exposing (detailUpdateTest, dictionaryUpdateTest, navBarTest, queryUpdateTest, sessionUpdateTest)

import Butterfly.Api as Api
import Butterfly.Query as Query exposing (Query)
import Butterfly.Type exposing (toRegion)
import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer)
import Generator as Gen
import Main
import NavBar
import Navigation as Nav exposing (Nav)
import Page.Detail as Detail
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
        , fuzz (Fuzz.tuple ( Gen.genSessionMsg, Gen.genSession ))
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

        query =
            Query.init

        response =
            Ok []
    in
    Session nav navBar query response


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
            Expect.equal updatedSession.query (Query.update session.query queryMsg)

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
        [ fuzz (Fuzz.tuple ( Gen.genDictionaryMsg, Gen.genSession ))
            "Should handle update as expected"
          <|
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
                    Expect.equal after.session.query.region before.session.query.region

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



--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------


queryUpdateTest : Test
queryUpdateTest =
    describe "Query"
        [ fuzz (Fuzz.tuple ( Gen.genQueryMsg, Gen.genQuery )) "Should update Query as expected" <|
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
                    { query | category = Nothing }
            in
            Expect.equal updatedQuery expected

        Query.ResetColor ->
            let
                expected =
                    { query | hexColor = Nothing }
            in
            Expect.equal updatedQuery expected

        Query.ResetRegion ->
            let
                expected =
                    { query | region = Nothing }
            in
            Expect.equal updatedQuery expected

        Query.UpdateCategory category ->
            let
                expected =
                    { query | category = Just category }
            in
            Expect.equal updatedQuery expected

        Query.UpdateColor hexColor ->
            let
                expected =
                    { query | hexColor = Just hexColor }
            in
            Expect.equal updatedQuery expected

        Query.UpdateRegion region ->
            let
                expected =
                    { query | region = Just region }
            in
            Expect.equal updatedQuery expected

        Query.ResetAll ->
            Expect.all
                [ \q -> Expect.equal q.hexColor Nothing
                , \q -> Expect.equal q.region Nothing
                , \q -> Expect.equal q.category Nothing
                ]
                updatedQuery



--------------------------------------------------------------------------------
-- Detail
--------------------------------------------------------------------------------


detailUpdateTest : Test
detailUpdateTest =
    describe "Detail"
        [ fuzz (Fuzz.tuple ( Gen.genSession, Gen.genButterfly )) "Should initiate as expected" <|
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
        , fuzz (Fuzz.tuple ( Gen.genDetailMsg, Gen.genDetailModel )) "Should update appopriately" <|
            \( msg, model ) -> validateDetailUpdate msg model
        ]


validateDetailUpdate : Detail.Msg -> Detail.Model -> Expectation
validateDetailUpdate msg model =
    let
        updatedModel =
            Detail.update msg model |> Tuple.first
    in
    case msg of
        Detail.ColorClicked hexColor ->
            Expect.equal updatedModel.session.query.hexColor (Just hexColor)

        Detail.RegionClicked regionStr ->
            case toRegion regionStr of
                Err _ ->
                    Expect.equal updatedModel.session.query.region model.session.query.region

                Ok region ->
                    Expect.equal updatedModel.session.query.region (Just region)

        Detail.CategoryClicked category ->
            Expect.equal updatedModel.session.query.category (Just category)

        Detail.GotSessionMsg sessionMsg ->
            let
                updatedSession =
                    Session.update sessionMsg model.session |> Tuple.first

                expectedModel =
                    { model | session = updatedSession }
            in
            Expect.equal updatedModel expectedModel


initSession : ( Session, Cmd Session.Msg )
initSession =
    Session.init Nav.initWithStub NavBar.init
