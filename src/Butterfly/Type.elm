module Butterfly.Type exposing (..)

import Json.Decode as Decode exposing (Decoder, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


butterfliesDecoder : Decoder (List Butterfly)
butterfliesDecoder =
    list butterflyDecoder


type alias Butterfly =
    { region : String
    , category : String
    , img_src : String
    , pdf_src : String
    , jp_name : String
    , eng_name : String
    , bgcolor : String
    , dominant_colors : List Color
    }


type alias Color =
    { pixel_fraction : Float
    , score : Float
    , hex_color : String
    }


butterflyDecoder : Decoder Butterfly
butterflyDecoder =
    Decode.succeed Butterfly
        |> required "region" string
        |> required "category" string
        |> required "img_src" string
        |> required "pdf_src" string
        |> required "jp_name" string
        |> required "eng_name" string
        |> required "bgcolor" string
        |> required "dominant_colors" (list colorDecoder)


colorDecoder : Decoder Color
colorDecoder =
    Decode.succeed Color
        |> required "pixel_fraction" float
        |> required "score" float
        |> required "hex_color" string



-- /// Buttterfly struct
-- #[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize)]
-- pub struct Butterfly {
--     /// Region
--     pub region: String,
--     /// Category
--     pub category: String,
--     /// Url of an image
--     pub img_src: String,
--     /// Url to pdf
--     pub pdf_src: String,
--     /// Path to image
--     pub img_path: Option<String>,
--     /// Path to pdf file
--     pub pdf_path: String,
--     /// Japanese name
--     pub jp_name: String,
--     /// English name
--     pub eng_name: String,
--     /// Background color in 6 digit Hex
--     pub bgcolor: String,
--     /// List of dominant colors
--     pub dominant_colors: Vec<Color>,
-- }
-- #[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize)]
-- pub struct Color {
--     /// Pixel fraction
--     pub pixel_fraction: f32,
--     /// Score
--     pub score: f32,
--     /// Color in hex string
--     pub hex_color: String,
-- }
--
