module HDML.Block exposing (Block(..), Attr(..))

import Dict exposing (Dict)


type Block a =
    Named String (AttrChild a) (Blocks a)
    | Anonymous (AttrChild a) (Blocks a)
    | Text (AttrChild a) String

type alias Blocks a =
    List (Block a)


-- TODO 在实际解析时通过 Dict 对同名属性去重
type Attr a =
    Attr a (AttrChild a)

type alias AttrChild a =
    List (String, (Attr a))
