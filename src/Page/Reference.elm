module Page.Reference exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Components as C


view : { title : String, content : Html msg }
view =
    { title = "参考文献"
    , content =
        div []
            [ C.sectionHeader "世界の蝶"
            , ul [] <|
                List.map refList
                    [ "坂口浩平：世界の昆虫 1～5，保育社，1979～ 1980"
                    , "H.L.Lewis著、坂口浩平訳：原色世界蝶類図鑑，保育社，1973"
                    , "PaulSmart：Butterfly World，Crescent Books New York"
                    , "今森光男・海野和男・松香宏隆・山口進：世界のチョウ 小学館学習百科図鑑"
                    , "海野和男：世界珍重図鑑 熱帯雨林編，人類文化社，2000"
                    , "五十嵐邁ほか：蝶の世界，朝日新聞社"
                    , "村田泰隆：チョウのいる風景，保育社"
                    , "村田泰隆：夢蝶美,保育者"
                    , "藤岡知夫編・築山洋・千葉秀行：日本産蝶類及び世界近縁種大図鑑Ⅰ，出版芸術社， 1997"
                    ]
            , hr [ class "hr" ] []
            , C.sectionHeader "アジアの蝶"
            , ul [] <|
                List.map refList
                    [ "五十嵐邁・福田：アジア産蝶類生活史図鑑Ⅰ/Ⅱ，東海大学出版会，1997/2000"
                    , "白水・川副・若林；原色日本蝶類図鑑，保育社，1976"
                    , "塚田悦造編：図鑑 東南アジア島嶼の蝶類 1～4巻 1979～1985"
                    , "白水隆・浜野栄次：台湾産蝶類生態大図鑑，講談社，1986"
                    , "藤岡知夫・大屋厚夫：野外ハンドブック・２ 蝶，山と渓谷社，1975"
                    , "A.S.Corbert・H.M.Pendlebury：The Butterflies of the Malay Peninsula，Doubleday＆Company New York，1978"
                    , "W.A.Fleming：Butterflies of West Malaysia and Singapore，Classy Publications，1975"
                    , "B.D'Abrera：Butterflies of Austrarian Regions，1990"
                    ]
            , hr [ class "hr" ] []
            , C.sectionHeader "ヨーロッパの蝶"
            , ul [] <|
                List.map refList
                    [ "L.G.Higgins・N.D.Riley：A Field Guide to the Butterflies of Britain and Europe，1970"
                    , "G?nter Ebert：Die Schmetteringe Baden-W?rttembergs Band1 Tagfalter，1991"
                    , "G?nter Ebert：Die Schmetteringe Baden-W?rttembergs Band2 Tagfalter，1991"
                    ]
            , hr [ class "hr" ] []
            , C.sectionHeader "アフリカの蝶"
            , ul [] <|
                List.map refList
                    [ "R.H.Carcasson：Collins Handguide to the Butterflies of Africa，Collins London，1980"
                    , "B.D'Abrera：Butterflies of Afrotropical Region，1990"
                    ]
            , hr [ class "hr" ] []
            , C.sectionHeader "北米の蝶"
            , ul [] <|
                List.map refList
                    [ "W.H.Howe：The Butterflies of North America，1975"
                    , "J.A.Scott：The Butterflies of North America，1986"
                    , "R.M.Pyle：The Audubon Society A Field Guide to the Butterflies of North America，A.A.Knopf，1970"
                    , "Marcus Schneck：North Amercan Butterflies Wall Chart"
                    ]
            , hr [ class "hr" ] []
            , C.sectionHeader "中南米の蝶"
            , ul [] <|
                List.map refList
                    [ "Bernard D'Abrera：Butterflie of South America，Hill House，1984"
                    , "B.D'Abrera：Butterflies of the Neotropical Region Ⅰ～Ⅳ，1987"
                    ]
            , hr [ class "hr" ] []
            , C.sectionHeader "飛翔理論"
            , ul [] <|
                List.map refList
                    [ "T.Itoh ,K.Ootsuta &SAkishita:A new approach toAtomspheric Mechanics Education, AIAA-99-4264"
                    ]
            ]
    }


refList : String -> Html msg
refList name =
    li [] [ text name ]
