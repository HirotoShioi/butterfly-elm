module Butterfly.Api exposing (Msg(..), getButterflies)

import Butterfly.Type exposing (Butterfly, butterfliesDecoder)
import Http


type Msg
    = GotButterflies (Result Http.Error (List Butterfly))


getButterflies : Cmd Msg
getButterflies =
    Http.get
        { url = gistUrl
        , expect = Http.expectJson GotButterflies butterfliesDecoder
        }


gistUrl : String
gistUrl =
    "https://raw.githubusercontent.com/HirotoShioi/butterfly-elm/master/frontend/butterfly.json"



-- Add more request like getByRegion etc
