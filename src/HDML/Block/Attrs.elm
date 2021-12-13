module HDML.Block.Attrs exposing
    ( Attr(..)
    , AttrValue(..)
    , AttrList
    , BlockAttrs
    , fromList
    , attrsToBlockAttrs
    )

import Dict exposing (Dict)


type AttrValue =
    WithInt Int
    | WithFloat Float
    | WithBool Bool
    | WithString String
    | WithNone


{-|
    Attr (<name>, <value>, [])
-}
type Attr a =
    Attr String AttrValue (AttrList a)

type alias AttrList a =
    List (Attr a)


type Attrs a =
    Attrs AttrValue (AttrsDict a)

type alias AttrsDict a =
    Dict String (Attrs a)


-- 块属性
type alias BlockAttrs a =
    {
    -- 在文档内声明的属性
    declared: AttrsDict a
    -- 保留属性，用于记录文档原始内容、原始标记等信息
    , reserved: AttrsDict a
    }


fromList : AttrList a -> AttrsDict a
fromList attrs =
    fromListHelper attrs Dict.empty

-- 使用尾递归
fromListHelper : AttrList a -> AttrsDict a -> AttrsDict a
fromListHelper attrs dict =
    case attrs of
        [] ->
            dict
        (Attr name value subs) :: tail ->
            let
                subDict = fromListHelper subs Dict.empty
                topDict =
                    Dict.insert name (Attrs value subDict) dict
            in
            fromListHelper tail topDict


attrsToBlockAttrs : AttrList a -> BlockAttrs a
attrsToBlockAttrs attrs =
    { declared = fromList attrs
    , reserved = fromList []
    }


-- get : Attrs a -> List String -> Attr a
-- get attrs paths =


-- add =


-- sub =
