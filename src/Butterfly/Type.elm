module Butterfly.Type exposing (Butterfly, Color, Query, Region(..), butterfliesDecoder, filterButterflies, fromRegion, initQuery, toRegion)

import Json.Decode as Decode exposing (Decoder, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Maybe.Extra exposing (unwrap)


butterfliesDecoder : Decoder (List Butterfly)
butterfliesDecoder =
    list butterflyDecoder


type alias Butterfly =
    { region : String
    , category : String
    , imgSrc : String
    , pdfSrc : String
    , jpName : String
    , engName : String
    , bgColor : String
    , dominantColors : List Color
    }


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


type Region
    = OldNorth
    | NewNorth
    | NewTropical
    | TropicalAfrica
    | IndiaAustralia


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


type alias Query =
    { region : Maybe Region
    , name : Maybe String
    , category : Maybe String
    }


initQuery : Query
initQuery =
    Query (Just OldNorth) Nothing Nothing


toSearchTerms : Query -> List SearchTerm
toSearchTerms query =
    let
        regionTerm =
            unwrap [] (\r -> [ Region r ]) query.region

        nameTerm =
            unwrap [] (\n -> [ Name n ]) query.name

        categoryTerm =
            unwrap [] (\c -> [ Category c ]) query.category
    in
    regionTerm ++ nameTerm ++ categoryTerm


type SearchTerm
    = Name String
    | Region Region
    | Category String


filterByRegion : Region -> List Butterfly -> List Butterfly
filterByRegion region butterflies =
    List.filter (\butterfly -> fromRegion region == butterfly.region) butterflies


filterButterflies : List Butterfly -> Query -> List Butterfly
filterButterflies butterflies query =
    let
        terms =
            toSearchTerms query
    in
    List.foldl filterByTerm butterflies terms


filterByTerm : SearchTerm -> List Butterfly -> List Butterfly
filterByTerm term butterflies =
    case term of
        Name butterflyName ->
            List.filter
                (\butterfly ->
                    String.contains butterflyName butterfly.jpName
                        || String.contains butterflyName butterfly.engName
                )
                butterflies

        Region region ->
            filterByRegion region butterflies

        Category category ->
            List.filter (\butterfly -> String.contains category butterfly.category) butterflies



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
