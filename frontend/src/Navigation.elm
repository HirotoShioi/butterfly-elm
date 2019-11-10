module Navigation exposing (Nav, initWithKey, initWithStub, map)

import Browser.Navigation as Navigation


type alias Nav msg =
    { pushUrl : String -> Cmd msg
    , replaceUrl : String -> Cmd msg
    , back : Int -> Cmd msg
    , forward : Int -> Cmd msg
    }


initWithKey : Navigation.Key -> Nav msg
initWithKey key =
    Nav
        (Navigation.pushUrl key)
        (Navigation.replaceUrl key)
        (Navigation.back key)
        (Navigation.forward key)


initWithStub : Nav msg
initWithStub =
    Nav
        (\_ -> Cmd.none)
        (\_ -> Cmd.none)
        (\_ -> Cmd.none)
        (\_ -> Cmd.none)


map : (subMsg -> msg) -> Nav subMsg -> Nav msg
map toMsg nav =
    Nav
        (\str -> Cmd.map toMsg <| nav.pushUrl str)
        (\str -> Cmd.map toMsg <| nav.replaceUrl str)
        (\num -> Cmd.map toMsg <| nav.back num)
        (\num -> Cmd.map toMsg <| nav.forward num)
