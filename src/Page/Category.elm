module Page.Category exposing (title, view)

import Bulma.Elements as B
import Html exposing (Html, div, p, text)
import Page
import Page.Components as C


view : Html msg
view =
    div []
        [ categoryView "Papilionidae（アゲハチョウ科）" "最多の種を擁するPapilio属をはじめ大型種が多い。しかし開長40mmのLamploptera（アオオビスカシアゲハ）属のような小さいものもある。ニューギニアを中心として美しいトリバネアゲハ属が、一方ヒマラヤや中央アジアや寒冷地にウスバシロチョウ属が分布している。ウスバシロチョウ属は山々で種ないし亜種を異にするといわれる。Papilio属の幼虫の食草はミカン科を食べ、幼虫は刺激をうけると頭と前胸部の間から臭角を出す。トリバネアゲハの仲間やジャコウアゲハの仲間の幼虫はウマノスズクサ科を食べるのでウマノスズクサアゲハの仲間といわれる。ウスバシロチョウの仲間はケシ科などをおもに食べる。アゲハ全体で約600種が知られている。"
        , C.her
        , categoryView "Pieridae（シロチョウ科）" "中型種が大半を占め翅の色は白色や黄色が多いがアフリカのColotis属（ツマアカシロチョウの仲間）やインドオーストラリア区のDelias（カザリシロチョウ）属や北区のAnthocaris（ツマキチョウ）属のようにオレンジ色や赤い斑紋のある種もある。幼虫は緑色で，いわゆる青虫とよべるタイプが多く，アブラナ科，マメ科などの植物を好む傾向がある。またDelias属の食草はヤドリギ。全世界には約1000種が知られている。"
        , C.her
        , categoryView "Libytheidae（テングチョウ科）" "口吻を左右から包む下唇のひげが長くのびているところから日本ではテングと命名され，世界に約10種が知られている。北米で第三紀の化石が2種発見されていている年代から推察して、系統的には古いチョウである。幼虫がニレ科のエノキ類を食べることは共通している。"
        , C.her
        , categoryView "Danaidae（マダラチョウ科）" "中型から大型の種が多く細長い体に対して翅の面積が大きく飛び方はゆるやかである。幼虫はアルカロイドのある有毒植物を食べる種類が多いために成虫ともども捕食者にとってはいやなにおいや味のようで彼らにきらわれ、他の科のチョウの擬態（ベーツ型）のモデルになっているものが多い。また同じ科の中で互いに擬態（ミューラー型）しあっている。インド・オーストラリア区に種が多く、全世界から約450種が知られている。生命力が強く、一度腹部を押して熱帯から持ち帰った後、三角紙をあけると飛び出すことがある。またオオカバマダラを筆頭として遠距離を移動する種が多い。"
        , C.her
        , categoryView "Ithomiidae （トンボマダラ科）" "新熱帯区固有の科。マダラチョウ科の亜科として取り扱う人もあるがここでは一つの科として取り扱う。マダラチョウ科との違いは触覚に鱗粉がないこと、♂の腹部にヘアーペンシルがないこと、翅には鱗粉が少なく胴部が長いことなどトンボに似たものが多いのでこの名が付いた。マダラチョウと同じく鳥に有毒で、これに擬態したアゲハやシイロチョウがあるのは、有名。"
        , C.her
        , categoryView "Satyridae（ジャノメ科）" "褐色を基調とする地味な色の翅には眼状紋(目玉模様)のある種類が多いのでジャノメ科といわれる。中型種が大半を占める。温帯から寒帯に分布するものは明るくひらけた場所を好み、温帯から熱帯に分布する種類には暗い林の中などを好むものが多い。南米には翅が透明な美麗種のスカシジャノメア亜科がいる。幼虫は単子葉植物(主としてイネ科)を食べる。全世界に約2500種が知られている。"
        , C.her
        , categoryView "Brassolidae（フクロウチョウ科）" "中央および南アメリカに分布するチョウで、後翅裏面にある大きな眼状紋がフクロウの目を連想させるところからこう命名されたチョウとその近縁種約80種のグループ。ジャノメチョウ科と、したがって下記のフクロウチョウ科と近縁で、幼虫は単子葉植物(たとえばバナナなど)を食べる。"
        , C.her
        , categoryView "Amathusiidae（ワモンチョウ科）" "インド・オーストラリア区に分布する大型または中型のチョウで、成虫は夜明けと夕方に活動し主として森林内にとどまる。トラップに集まる。系統的には上記のフクロウチョウ科と同様にジャノメチョウ科に近く幼虫が単子葉植物を食べるのも共通している。全世界に約100種が知られている。"
        , C.her
        , categoryView "Morphidae （モルフォチョウ科）" "大型が大半。金属光沢に輝く翅をもつ種類が多いが、なかにはまったく光沢のないものもある。系統的にはタテハチョウ科やワモンチョウ科に近く、むしろそれらの祖先型と考えられる。中南米から南米の熱帯地域にだけ分布し約80種が知られている。金属光沢のある♂は同じ色の光沢の紙によってくるといわれる。"
        , C.her
        , categoryView "Nymphalidae （タテハチョウ科）" "小型から大型まで多様なグループで熱帯に分布するものは南米のミイロタテハのように色彩がはでなものが多い。科の特徴としては、成虫の前脚が退化変形して歩行には用いず感覚器官になっていることから最も進化したチョウとされている。幼虫は体じゅうにとげのはえている型とナメクジ型に二分される。全世界に約3500種が知られている。"
        , C.her
        , categoryView "Heliconiidae(ドクチョウ科)" "中南米に分布する。タテハチョウ科の亜科とされることがある。ドクチョウという名が示すように幼虫は鳥に有毒なトケイソウ科を食草とするので、一度食べたことのある鳥からは嫌われるという。極彩色に彩られた翅を有し、かつゆっくりと飛ぶするものが多いのは、このためである。"
        , C.her
        , categoryView "Acraeidae （ホソチョウ科）" "中サイズの蝶で、生物学的にはタテハとよく似ているのでドクチョウ科と同じくタテハ科の亜科とされることがある。アフリカ区に175と大半の種がいるが、インド－オーストラリア区に6種が、13種が新熱帯区にいる。ほとんどの種は真っ赤に近い目立った色をしていてゆっくりと飛ぶ。これは鳥など捕食者にとってまずいからだと考えられている。だが、目立った色は死ぬとすぐにあせるとされている。本ホームページでは、熱帯アフリカのものだけを記載した。"
        , C.her
        , categoryView "Riodinidae （シジミタテハ科）" "小型種が多く名まえのようにシジミチョウとタテハチョウの両方に似た形質をもっているが、よりシジミチョウに近いと考えられる。幼虫が蟻と共生するものもいるのも似ている。数千万年前の化石が見つかっていて、チョウの中でも早く出現したものと思われる。。翅の形には変化が多く色彩も多様で美しいものが多い。中央および南アメリカの熱帯に分布が集中しているが、一部がインド・オーストラリア区にもいる。全世界で約1000種が知られている。"
        , C.her
        , categoryView "Lycaenidae（シジミチョウ科）" "小型種の大きなグループで世界中に分布し，一部は極地で採集された例も報告されている。熱帯には美麗種が多数いる。幼虫の食性は植物に依存するものが大部分であるが，アリと共生する種類やアブラムシを食べる肉食性のものもあって変化に富んでいる。全世界に約5500種が知られていてチョウの中では最大の科である。"
        , C.her
        , categoryView "Hesperiidae(セセリチョウ上科)" "アゲハチョウ上科のものとは形態的に\u{3000}(1)触角の先端がかぎ状かとがることと（解説シート図1参照）、)体が翅の大きさに比べて太いことで異なる。ラッフルズセセリのように後翅基部に翅棘(しきよく)があり前翅と連動できる種がある。ついでながらラッフルズセセリが発見されるまでは、チョウには翅棘がなくガにあることがチョウがガと異なる点だとされていたことがあった。チョウとガをはっきりと区別するためにセセリ上科をガに入れるべきだとする主張もあるが、必ずしも系統的にはガに近いというわけではなく独特な方向に分化したグループと考えるべきである。幼虫は双子葉植物を食べるものもあるが単子葉植物に依存するものが多い。全世界に約3000種が知られているが約2000種は南アメリカに分布する。"
        ]


title : String
title =
    Page.categoryTitle


categoryView : String -> String -> Html msg
categoryView t content =
    div []
        [ B.title B.H5 [] [ text t ]
        , p [] [ text content ]
        ]
