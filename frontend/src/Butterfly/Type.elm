module Butterfly.Type exposing (Butterfly, Color, Region(..), butterfliesDecoder, fromRegion, getImageName, regionList, toRegion)

import Json.Decode as Decode exposing (Decoder, field, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (required)


butterfliesDecoder : Decoder (List Butterfly)
butterfliesDecoder =
    field "butterflies" <| list butterflyDecoder


type alias Butterfly =
    { region : String
    , category : String
    , imgSrc : String
    , imgPath : Maybe String
    , pdfSrc : String
    , jpName : String
    , engName : String
    , bgColor : String
    , distribution : String
    , openLength : Int
    , diet : Maybe String
    , remarks : Maybe String
    , dominantColors : List Color
    }



-- ./assets/india_australia/images/mesuakamonki.jpg


getImageName : Butterfly -> Maybe String
getImageName butterfly =
    Maybe.andThen
        (\path ->
            String.split "/" path
                |> List.reverse
                |> List.head
                |> Maybe.andThen
                    (\str -> String.split "." str |> List.head)
        )
        butterfly.imgPath


type alias Color =
    { pixelFraction : Float
    , score : Float
    , hexColor : String
    }


butterflyDecoder : Decoder Butterfly
butterflyDecoder =
    Decode.succeed Butterfly
        |> required "region" string
        |> required "category" string
        |> required "img_src" string
        |> required "img_path" (nullable string)
        |> required "pdf_src" string
        |> required "jp_name" string
        |> required "eng_name" string
        |> required "bgcolor" string
        |> required "distribution" string
        |> required "open_length" int
        |> required "diet" (nullable string)
        |> required "remarks" (nullable string)
        |> required "dominant_colors"
            (list colorDecoder |> Decode.map computePercentage)


computePercentage : List Color -> List Color
computePercentage colors =
    let
        filteredColors =
            List.filter (\c -> c.score > 0.05) colors

        fractionSum =
            List.foldl (\color acc -> color.pixelFraction + acc) 0 filteredColors

        updateFraction color =
            { color | pixelFraction = color.pixelFraction / fractionSum }
    in
    if
        fractionSum == 0
        -- If sum is 0, then return an empty list
    then
        []

    else
        List.map updateFraction filteredColors


colorDecoder : Decoder Color
colorDecoder =
    Decode.succeed Color
        |> required "pixel_fraction" float
        |> required "score" float
        |> required "hex_color" string


type Region
    = OldNorth
    | NewNorth
    | NewTropical
    | TropicalAfrica
    | IndiaAustralia


regionList : List Region
regionList =
    [ OldNorth
    , NewNorth
    , NewTropical
    , TropicalAfrica
    , IndiaAustralia
    ]


fromRegion : Region -> String
fromRegion region =
    case region of
        OldNorth ->
            "旧北区"

        NewNorth ->
            "新北区"

        NewTropical ->
            "新熱帯区"

        IndiaAustralia ->
            "インド・オーストラリア区"

        TropicalAfrica ->
            "熱帯アフリカ区"


toRegion : String -> Result String Region
toRegion str =
    case str of
        "旧北区" ->
            Ok OldNorth

        "新北区" ->
            Ok NewNorth

        "新熱帯区" ->
            Ok NewTropical

        "インド・オーストラリア区" ->
            Ok IndiaAustralia

        "熱帯アフリカ区" ->
            Ok TropicalAfrica

        _ ->
            Err <| String.concat [ "Unknown region: ", str ]



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
