module MainTest exposing (testMain)

import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer)
import Generator as Gen
import Main
import NavBar
import Navigation as Nav exposing (Nav)
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
                    |> Expect.true "Should be loading state"
        , test "MainClick should disable navbar" <|
            \_ ->
                initMain
                    |> Tuple.first
                    |> enableNavBar
                    |> Main.update Main.MainClicked
                    |> Tuple.first
                    |> Main.getSession
                    |> (\session -> Expect.equal False session.navModel)
        , fuzz Gen.genRoute "changeRouteTo should not change model if given model is in Loading" <|
            \route ->
                initMain
                    |> Tuple.first
                    |> Main.changeRouteTo (Just route)
                    |> Tuple.first
                    |> isLoadingState
                    |> Expect.true "Should be loading state"
        , fuzz Gen.genRoute "changeRouteTo should switch to appopriate model" <|
            \route ->
                initMain
                    |> Tuple.first
                    |> goHome
                    |> Main.changeRouteTo (Just route)
                    |> Tuple.first
                    |> validatePage route
                    |> Expect.true "Should be in appopriate model"
        , fuzz (Fuzz.tuple ( Gen.genSessionMsg, Gen.genMainModel )) "Should handle GotSessionMsg as expected" <|
            \( sessionMsg, before ) ->
                Main.update (Main.GotSessionMsg sessionMsg) before
                    |> Tuple.first
                    |> (\after -> validateSessionUpdate sessionMsg before after)
        , fuzz Gen.genDictionaryMsg "Should handle GotDictionaryMsg as expected" <|
            \dictionaryMsg ->
                initMain
                    |> Tuple.first
                    |> goDictionary
                    |> (\before ->
                            Main.update (Main.GotDictionaryMsg dictionaryMsg) before
                                |> Tuple.first
                                |> (\after -> validateDictionaryUpdate dictionaryMsg before after)
                       )
        , fuzz (Fuzz.tuple ( Gen.genNavBarMsg, Gen.genMainModel )) "Should handle GotNavBarMsg as expected" <|
            \( navBarMsg, before ) ->
                Main.update (Main.GotNavBarMsg navBarMsg) before
                    |> Tuple.first
                    |> (\after -> validateNavBarUpdate navBarMsg before after)
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


isLoadingState : Main.Model -> Bool
isLoadingState model =
    case model of
        Main.Loading _ _ ->
            True

        _ ->
            False


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
            Tuple.first <| Dictionary.init (Main.getSession someModel)
    in
    Main.Dictionary dictionaryModel


validatePage : Route -> Main.Model -> Bool
validatePage route model =
    case ( route, model ) of
        ( Route.Home, Main.Home _ ) ->
            True

        ( Route.Reference, Main.Reference _ ) ->
            True

        ( Route.Category, Main.Category _ ) ->
            True

        ( Route.Description, Main.Description _ ) ->
            True

        ( Route.Area, Main.Area _ ) ->
            True

        ( Route.Dictionary, Main.Dictionary _ ) ->
            True

        ( Route.Error, Main.Error _ ) ->
            True

        _ ->
            False
