module Page exposing (Page(..), toTitle)


type Page
    = Home
      -- 世界の蝶
    | Reference
      -- 参考文献
    | Category
      -- 蝶の分類
    | Description
      -- 蝶の解説
    | Area
      -- 生物区・蝶分類の地理
    | Dictionary
      -- 570種の図鑑と解説
    | NotFound
    | Error
    | Loading


toTitle : Page -> String
toTitle page =
    case page of
        Home ->
            "世界の蝶"

        Reference ->
            "参考文献"

        Category ->
            "蝶の分類"

        Description ->
            "蝶の解説"

        Area ->
            "生物区・蝶分類の地理"

        Dictionary ->
            "570種の図鑑と解説"

        Error ->
            "エラー"

        NotFound ->
            "404 Not Found"

        Loading ->
            "読み込み中"
