module Page exposing (Page(..), toTitle)


type
    Page
    -- 世界の蝶
    = Home
      -- 参考文献
    | Reference
      -- 蝶の分類
    | Category
      -- 蝶の解説
    | Description
      -- 生物区・蝶分類の地理
    | Area
      -- 570種の図鑑と解説
    | Dictionary
      -- ページが見つかりませんでした
    | NotFound
      -- エラー
    | Error
      -- 読み込み中
    | Loading
      -- 詳細
    | Detail


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

        Detail ->
            "蝶の図鑑"
