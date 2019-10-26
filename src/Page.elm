module Page exposing (Page(..), view, navBar)

import Html exposing (..)
import Html.Attributes exposing (..)
import Browser exposing (Document)


import Route exposing (Route(..))

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

-- Navigation bar
navBar : Html msg
navBar = ul []
  <| (\ls -> [ li [] [a [href "/"] [text "ホーム"]]] ++ ls)
  <| List.map navlink 
    [ (Route.Reference, "参考文献")
    , (Route.Category, "蝶の分類")
    , (Route.Description, "蝶の解説")
    , (Route.Area, "生物区・蝶分類の地理")
    , (Route.Dictionary, "蝶の図鑑")
    ]

navlink : (Route, String) -> Html msg
navlink (route, linkName) =
  li [] [a [Route.href route] [text linkName]]

headerView : Html msg
headerView = header [] [ text "This is header" ]

footerView : Html msg
footerView = footer [] [ text "This is footer" ]

view : { title : String, content : Html msg } -> Document msg
view { title, content } = 
  { title = title
  , body =
    [ div []
      [ navBar
      , headerView
      , content
      , footerView
      ]
    ]
  }