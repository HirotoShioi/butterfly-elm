module Butterfly.Api exposing (..)

import Butterfly.Type as B exposing (Butterfly, Color)
import Http


type Msg
    = GotButterflies (Result Http.Error (List Butterfly))


getButterflies : Cmd Msg
getButterflies =
    Http.get
        { url = "https://gist.githubusercontent.com/HirotoShioi/99c70f4141961b8a0ed5c0f6ab24dd0a/raw/3ee114ca1d0f5be98007942a01cb1cbcfe8622d8/butterfly.json"
        , expect = Http.expectJson GotButterflies B.butterfliesDecoder
        }

-- Add more request like getByRegion etc