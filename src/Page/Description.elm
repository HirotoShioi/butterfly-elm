module Page.Description exposing (..)

import Bulma.Elements exposing (..)
import Bulma.Modifiers.Typography exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Page.Components as C


view : { title : String, content : Html msg }
view =
    { title = "蝶の解説"
    , content =
        div []
            [ C.sectionHeader "生物の名前"
            , p [] [ text "\u{3000}蝶に限らず生物は、個体それぞれが単位となってそれに名前が付けられていいるのではなく、種すなわち交配して子孫を残す集団を基本単位として名前が付けられてる。すなわち、われわれは、この種にモンシロチョウとかアゲハチョウとかいった名前をつけて蝶を含む生物を識別している。この名前のことを種名という。" ]
            , p [] [ text "\u{3000}名前には、まずそれぞれの国の言語による名前がある。上に述べたモンシロチョウとかアゲハチョウは日本語における名前であり、日常会話では、通常この名前が使われる。しかしこの名前は国ごとに異なり、かつ生物学的な意味を持っていないので学者が蝶について専門家が国際的な対話をするときには不便をきたすことになる。" ]
            , p [] [ text "\u{3000}この問題を解決するためにつけられたのが学名である。学名を説明する前に、蝶の多くの種が生物学的にどう定義され分類されるかについて理解する必要があるので以下にこれを説明する。" ]
            , C.her
            , C.sectionHeader "蝶と蛾"
            , p [] [ text "\u{3000}蝶と蛾の区別はというのはよく受ける質問である、学問的にいえば、どちらも鱗翅目に属し、科学的に区別を定義するのは困難である。すなわち、蝶は、体は密に鱗粉と毛で覆われ、羽は膜質で一面に規則正しく配列された鱗粉で覆われているとうい点でいわゆる蛾と親戚で、両者をあわせて鱗翅目として纏められている。蝶と蛾は一見明瞭な差があるようであるが、実ははっきりとは区別できないのである。強いて言えば蝶は触角が末端部で膨大して突起がないことぐらいであろう。しかしセセリ蝶は末端部が尖っていてこの点からは蛾に入る。実際、ヨーロッパで蝶と蛾をButterflyとMothとして区別するのは英語ぐらいでドイツ語、フランス語では、鱗翅目をそれぞれSchmetterlingとかpapillonと呼んで蝶と蛾を区別していない。" ]
            , p [] [ text "\u{3000}英語の蝶と蛾の区別を知るためには、鱗翅目の分類に立ち入らなければならないのである。" ]
            , C.her
            , C.sectionHeader "鱗翅目の分類"
            , p [] [ text "\u{3000}鱗翅目はその生物的な特性から21上科に分けられ，このうちの「ゴルフクラブ状の触角を持つアゲハチョウ上科 Papilionoidea」と「触角の端部は膨大しているがその先端がかぎ状にとがるセセリチョウ上科 Hesperioidea」 の2上科が蝶に当たる。（図1参照）現在知られている鱗翅目は約14万種であるが、このうちチョウ類は約1万8000種に過ぎない。" ]
            , p [] [ text "\u{3000}これはチョウに限ったことではないが、植物に依存する昆虫はその植物の生育する環境や気象条件に適応した結果独自の特徴をもち、上述の上科にシート2に示す科の分化が、さらに細かく後述の属のレベルの分化が起こったと考えらている。" ]
            , C.her
            , C.sectionHeader "属と学名"
            , p [] [ text "\u{3000}蝶といわれている上科の下には、後述するように多くの科があり、さらにこの科の下には非常に多数の種が存在する。そこでスウェーデンの生物学者リンネは、ギリシャのアリストテレスの考え方を継承して、よく似た種の集りを属とし、個々の種について記載するときには、属の部分で共通の特徴を、種では個別の特徴を述べるようにした。この記述方法が現在まで続いているのである。本ホームページもこの方法で蝶の特徴を記述している。" ]
            , p [] [ text "\u{3000}これに基づいて、種の学名は、その種が属している属名(名詞で冒頭の字は大文字)に、種の性質を示す種小名(形容詞で冒頭の字は小文字)をつけて、2語で表現している。なお、属名も種小名もすべてラテン語ないしラテン語の形式を使用している。" ]
            , div [ class "has-text-centered bottom-img" ]
                [ img [ src "./butterfly-antenna.jpg" ] [] ]
            ]
    }
