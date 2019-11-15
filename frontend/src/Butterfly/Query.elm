module Butterfly.Query exposing (Msg(..), Query, filterButterflies, init, initWithArgs, intoQueryParameter, maxShowCount, update)

import Butterfly.Type exposing (Butterfly, Region, fromRegion, toRegion)
import Chroma.Chroma as Chroma
import Chroma.Types exposing (ExtColor)
import Maybe.Extra exposing (unwrap)
import Url.Builder as Builder exposing (QueryParameter)


maxShowCount : Int
maxShowCount =
    100


type alias Query =
    { region : Maybe Region
    , name : Maybe String
    , category : Maybe String
    , hexColor : Maybe String
    , colorDistance : Float
    , maxShowCount : Int
    }


init : Query
init =
    Query Nothing Nothing Nothing Nothing 70 maxShowCount


initWithArgs : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Query
initWithArgs mName mRegionStr mCategory mColor =
    let
        mRegion =
            Maybe.andThen (\str -> toRegion str |> Result.toMaybe) mRegionStr
    in
    Query mRegion mName mCategory mColor 70 maxShowCount


type Msg
    = ResetCategory
    | ResetColor
    | ResetRegion
    | UpdateCategory String
    | UpdateRegion Region
    | UpdateColor String
    | ResetAll
    | LoadMore


update : Query -> Msg -> Query
update query msg =
    case msg of
        ResetCategory ->
            { query
                | category = Nothing
                , maxShowCount = maxShowCount
            }

        ResetColor ->
            { query
                | hexColor = Nothing
                , maxShowCount = maxShowCount
            }

        ResetRegion ->
            { query
                | region = Nothing
                , maxShowCount = maxShowCount
            }

        UpdateCategory category ->
            { query
                | category = Just category
                , maxShowCount = maxShowCount
            }

        UpdateRegion region ->
            { query
                | region = Just region
                , maxShowCount = maxShowCount
            }

        UpdateColor color ->
            { query
                | hexColor = Just color
                , maxShowCount = maxShowCount
            }

        LoadMore ->
            { query | maxShowCount = query.maxShowCount + maxShowCount }

        ResetAll ->
            init


toSearchTerms : Query -> List SearchTerm
toSearchTerms query =
    let
        regionTerm =
            unwrap [] (\r -> [ Region r ]) query.region

        nameTerm =
            unwrap [] (\n -> [ Name n ]) query.name

        categoryTerm =
            unwrap [] (\c -> [ Category c ]) query.category

        hexStringTerm =
            unwrap [] (\h -> [ HexColor h query.colorDistance ]) query.hexColor
    in
    regionTerm ++ nameTerm ++ categoryTerm ++ hexStringTerm


intoQueryParameter : Query -> List QueryParameter
intoQueryParameter query =
    let
        toQuery term =
            case term of
                Name str ->
                    Builder.string "name" str

                Region region ->
                    Builder.string "region" <| fromRegion region

                Category category ->
                    Builder.string "category" category

                HexColor hexColor _ ->
                    Builder.string "hexColor" hexColor
    in
    toSearchTerms query |> List.map toQuery


type SearchTerm
    = Name String
    | Region Region
    | Category String
    | HexColor String Float


filterButterflies : List Butterfly -> Query -> List Butterfly
filterButterflies butterflies query =
    toSearchTerms query |> List.foldl runFilter butterflies


runFilter : SearchTerm -> List Butterfly -> List Butterfly
runFilter term butterflies =
    case term of
        Name butterflyName ->
            List.filter
                (\butterfly ->
                    String.contains butterflyName butterfly.jpName
                        || String.contains butterflyName butterfly.engName
                )
                butterflies

        Region region ->
            List.filter (\butterfly -> fromRegion region == butterfly.region) butterflies

        Category category ->
            List.filter (\butterfly -> String.contains category butterfly.category) butterflies

        HexColor hexString distance ->
            filterBySimilarColor distance hexString butterflies


filterBySimilarColor : Float -> String -> List Butterfly -> List Butterfly
filterBySimilarColor distance hexColor butterflies =
    Chroma.chroma hexColor
        |> Result.map (\c -> List.filter (isSimilarColor c distance) butterflies)
        |> Result.withDefault []


isSimilarColor : ExtColor -> Float -> Butterfly -> Bool
isSimilarColor color distance butterfly =
    let
        isWithinRange hexString =
            Chroma.chroma hexString
                |> Result.map (\someColor -> Chroma.distance255 color someColor <= distance)
                |> Result.withDefault False
    in
    List.filter (\colors -> colors.pixelFraction >= 0.1) butterfly.dominantColors
        |> List.any (\c -> isWithinRange c.hexColor)
