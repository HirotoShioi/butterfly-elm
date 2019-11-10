module Page.Area exposing (view)

import Html exposing (Html, div, img, p, text)
import Html.Attributes exposing (class, src)
import Page.Components exposing (her, sectionHeader)


view : Html msg
view =
    div []
        [ areaView "5つの生物区" "\u{3000}共通種や特徴のある科の存在を考慮すれば、蝶を含む生物の地理的世界は、図2に示すように旧北区、インド・オーストラリア区、熱帯アフリカ区、新熱帯区、新北区、五つの生物区に分割される。"
        , her
        , areaView "旧北区" "\u{3000}ヒマラヤより北のユーラシア大陸を中心として、西はサハラ砂漠より北の北アフリカ、東は日本を含む広大な地域である。沖縄と奄美大島との間が日本におけるこの区の境界である。ここで種が多いのは中国南部で、各種タテハの発生の一つの原点と考えられている。またアゲハではウスバシロチョウ属が山々で種を異にし多様な展開を誇っている。ギフチョウなどの多様な異型アゲハを有することもこの区の特徴である。なお、いわゆるアフリカ北部やバルカンン半島を含むヨーロッパは、面積の割には種の数が少なく約370種しか生息していない。また、旧北区とインド・オーストラリア区との境界に当たる中国南部や台湾では、高山では旧北区に属する種が低地ではインド・オ-ストラリア区に属する種が生息している。"
        , her
        , areaView "インド・オーストラリア区" "\u{3000}インド・オーストラリア区は、生物学的にインド～ベトナム、スンダランド（比較的最近まで陸続きであったマレー半島南部、スマトラ、ボルネオ、ジャワを含む地区）、フィリピン群島、ニューギニアとオーストラリア北部とその周辺を含む大洋州、大洋州とスンダランドの中間に位置するセレベスの5つの地区に分けられ、それぞれで特徴のある種が生息している。そのおかげで、種の数も数千に上るものと思われる。このうち、スンダランドは、キシタアゲハ属やマダラチョウ群の多様な展開が見られるほかイナズマチョウ群やムラサキシジミ属の多様な展開が見られる。フィリピン群島は、セジロアケボノやアカネアゲハのような共通種を持っていることも興味あるところである。\u{3000}大洋州は世界一の大きさをほこるアレキサンドラアゲハを容するトリバネアゲハ属が展開されている。またカザリシロチョウやワモンチョウの種の数が多いのも特徴の一つである。セレベス地区は、どの属のチョウも独自の進化を遂げて異種大型化しているのが特徴である。"
        , her
        , areaView "熱帯アフリカ区" "\u{3000}サハラ砂漠以南の熱帯アフリカとマダガスカル島からなる区で、種の数が3100種程度でうち240種がマダガスカルの固有種である。一見地味なチョウが多いといわれているが、多くの種を仔細にみると美麗で派手なものもかなり多い。ホソチョウの発生の中心であり、ツマアカシロチョウやフタオチョウの群にきわめて多様な展開が見られるのもこの区の特徴である。"
        , her
        , areaView "新熱帯区" "\u{3000}種の数が万になんなんとするほど多いだけあって、きわめて美麗な種を擁している。なかでも、ミイロタテハ属、モルフォチョウ属、カラスシジミとして一括されるシジミチョウ群は世界一の美麗さを誇っている。また極彩色のドクチョウや万華鏡から抜け出したような美麗種を含むシジミタテハが他の区よりもはるかに多様な展開を見せている。さらに、他の区では地味なセセリ科やジャノメ科が妖艶な種を擁しているのもこの地区の特徴である。"
        , her
        , areaView "新北区" "\u{3000}約700種を擁してはいるが、オオカバマダラ以外は独自を誇る種はなく、北中部はユーラシヤの出店、南部は南米の出店と言える。ただし最近店を出した南米の出店が種に変化がみられないのに対して、ユーラシヤから店が出たのは、かなり古いようで、いまではすっかり種が似て非なるものになっている。"
        , div [ class "has-text-centered bottom-img" ]
            [ img [ src "./world_map.gif" ] [] ]
        ]


areaView : String -> String -> Html msg
areaView t content =
    div []
        [ sectionHeader t
        , p [] [ text content ]
        ]