module MainTest exposing (testMain)

import Butterfly.Query as Query
import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer)
import Fuzzer as Fuzz
import Main
import NavBar
import Navigation as Nav exposing (Nav)
import Page.Detail as Detail
import Page.Dictionary as Dictionary
import Route exposing (Route)
import Session
import Test exposing (..)
import Url exposing (Url)


homeUrl : Url
homeUrl =
    Url Url.Https "example.com" (Just 3000) "/" Nothing Nothing


initMain : ( Main.Model, Cmd Main.Msg )
initMain =
    Main.init homeUrl Nav.initWithStub


testMain : Test
testMain =
    describe "Main"
        [ test "Init should lead to loading state" <|
            \_ ->
                initMain
                    |> Tuple.first
                    |> isLoadingState
        , test "MainClick should disable navbar" <|
            \_ ->
                initMain
                    |> Tuple.first
                    |> enableNavBar
                    |> Main.update Main.MainClicked
                    |> Tuple.first
                    |> Main.getSession
                    |> (\session -> Expect.equal False session.navModel)
        , fuzz Fuzz.fuzzRoute "changeRouteTo should not change model if given model is in Loading" <|
            \route ->
                initMain
                    |> Tuple.first
                    |> Main.changeRouteTo (Just route)
                    |> Tuple.first
                    |> isLoadingState
        , fuzz (Fuzz.tuple ( Fuzz.fuzzRoute, Fuzz.fuzzMainModel )) "changeRouteTo should switch to appopriate model" <|
            \( route, model ) ->
                Main.changeRouteTo (Just route) model
                    |> Tuple.first
                    |> validatePage route
        , fuzz (Fuzz.tuple ( Fuzz.fuzzButterfly, Fuzz.fuzzMainModel )) "changeRouteTo should transition to detail view" <|
            \( butterfly, mainModel ) ->
                let
                    session =
                        Main.getSession mainModel

                    updatedSession =
                        { session
                            | butterflies = Result.map (\bs -> butterfly :: bs) session.butterflies
                            , navModel = False
                        }

                    route =
                        Just (Route.Detail butterfly.engName)

                    updatedModel =
                        Main.updateSession mainModel updatedSession
                            |> Main.changeRouteTo route
                            |> Tuple.first

                    detailModel =
                        Detail.init (Main.getSession updatedModel) butterfly |> Tuple.first
                in
                case session.butterflies of
                    Ok _ ->
                        Expect.equal updatedModel (Main.Detail detailModel)

                    Err _ ->
                        Expect.equal updatedModel (Main.Error updatedSession)
        , fuzz (Fuzz.tuple ( Fuzz.fuzzSessionMsg, Fuzz.fuzzMainModel )) "Should handle GotSessionMsg as expected" <|
            \( sessionMsg, before ) ->
                Main.update (Main.GotSessionMsg sessionMsg) before
                    |> Tuple.first
                    |> (\after -> validateSessionUpdate sessionMsg before after)
        , fuzz Fuzz.fuzzDictionaryMsg "Should handle GotDictionaryMsg as expected" <|
            \dictionaryMsg ->
                initMain
                    |> Tuple.first
                    |> goDictionary
                    |> (\before ->
                            Main.update (Main.GotDictionaryMsg dictionaryMsg) before
                                |> Tuple.first
                                |> validateDictionaryUpdate dictionaryMsg before
                       )
        , fuzz (Fuzz.tuple ( Fuzz.fuzzNavBarMsg, Fuzz.fuzzMainModel )) "Should handle GotNavBarMsg as expected" <|
            \( navBarMsg, before ) ->
                Main.update (Main.GotNavBarMsg navBarMsg) before
                    |> Tuple.first
                    |> validateNavBarUpdate navBarMsg before
        ]


validateNavBarUpdate : NavBar.Msg -> Main.Model -> Main.Model -> Expectation
validateNavBarUpdate msg before after =
    let
        session =
            Main.getSession before

        updatedNavModel =
            Tuple.first <| NavBar.update msg session.navModel

        updatedSession =
            { session | navModel = updatedNavModel }

        updatedMainModel =
            Main.updateSession before updatedSession
    in
    Expect.equal updatedMainModel after


validateSessionUpdate : Session.Msg -> Main.Model -> Main.Model -> Expectation
validateSessionUpdate msg before after =
    let
        session =
            Main.getSession before

        updatedSession =
            Session.update msg session |> Tuple.first

        updatedModel =
            Main.updateSession before updatedSession
    in
    Expect.equal updatedModel after


validateDictionaryUpdate : Dictionary.Msg -> Main.Model -> Main.Model -> Expectation
validateDictionaryUpdate msg before after =
    case before of
        Main.Dictionary dicModel ->
            let
                updatedDicModel =
                    Tuple.first <| Dictionary.update msg dicModel
            in
            Expect.equal (Main.Dictionary updatedDicModel) after

        _ ->
            Expect.fail "Update failed"


validatePage : Route -> Main.Model -> Expectation
validatePage route model =
    let
        session =
            Main.getSession model

        updatedSession =
            { session | navModel = False }

        expectEqual s =
            Expect.equal s updatedSession
    in
    case ( route, model ) of
        ( Route.Home, Main.Home s ) ->
            expectEqual s

        ( Route.Reference, Main.Reference s ) ->
            expectEqual s

        ( Route.Category, Main.Category s ) ->
            expectEqual s

        ( Route.Description, Main.Description s ) ->
            expectEqual s

        ( Route.Area, Main.Area s ) ->
            expectEqual s

        ( Route.Dictionary query, Main.Dictionary m ) ->
            expectEqual m.session

        ( Route.Error, Main.Error s ) ->
            expectEqual s

        _ ->
            Expect.fail "No match"


isLoadingState : Main.Model -> Expectation
isLoadingState model =
    case model of
        Main.Loading _ _ ->
            Expect.pass

        _ ->
            Expect.fail "Invalid state"


enableNavBar : Main.Model -> Main.Model
enableNavBar someModel =
    let
        session =
            Main.getSession someModel

        updatedSession =
            { session | navModel = True }
    in
    Main.updateSession someModel updatedSession


goHome : Main.Model -> Main.Model
goHome someModel =
    Main.Home (Main.getSession someModel)


goDictionary : Main.Model -> Main.Model
goDictionary someModel =
    let
        dictionaryModel =
            Tuple.first <| Dictionary.init (Main.getSession someModel) Query.init
    in
    Main.Dictionary dictionaryModel
