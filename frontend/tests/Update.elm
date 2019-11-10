module Update exposing (..)

import Test exposing (..)
import NavBar as Nav
import Expect
import Session exposing (Session)

navBarTest : Test
navBarTest = describe "NavBar"
    [ test "Init" <| \_ -> Expect.equal False Nav.init
    , test "Toggle menu" <| 
        \_ -> Nav.init |> Nav.update Nav.ToggleMenu |> Expect.equal (True, Cmd.none)
    , test "Toggle menu twice" <|
        \_ -> Nav.init 
            |> Nav.update Nav.ToggleMenu
            |> chain Nav.update Nav.ToggleMenu
            |> Expect.equal (False, Cmd.batch [Cmd.none, Cmd.none])
    , test "Toggle, then disable" <|
        \_ -> Nav.init
            |> Nav.update Nav.ToggleMenu
            |> chain Nav.update Nav.DisableMenu
            |> Expect.equal (False, Cmd.batch [Cmd.none, Cmd.none])
    ]

chain : (msg -> preModel ->  (nextModel, Cmd msg)) -> msg -> (preModel, Cmd msg) -> (nextModel, Cmd msg)
chain update nextMsg (preModel, preCmd) = 
    let (nextModel, nextCmd) = update nextMsg preModel
    in (nextModel, Cmd.batch [nextCmd, preCmd])
