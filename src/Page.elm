module Page exposing (..)


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


homeTitle : String
homeTitle =
    "世界の蝶"


referenceTitle : String
referenceTitle =
    "参考文献"


categoryTitle : String
categoryTitle =
    "蝶の分類"


descriptionTitle : String
descriptionTitle =
    "蝶の解説"


areaTitle : String
areaTitle =
    "生物区・蝶分類の地理"


dictionaryTitle : String
dictionaryTitle =
    "570種の図鑑と解説"


notFoundTitle : String
notFoundTitle =
    "Page not found"


errorTitle : String
errorTitle =
    "Error"
