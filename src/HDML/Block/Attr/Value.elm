module HDML.Block.Attr.Value exposing
    ( get, set
    )

import HDML.Block.Attr as Attr


{-| 获取指定属性的值
-}
get : List String -> Attr.AttrTree a -> Attr.AttrValue
get paths topTree =
    case (Attr.get paths topTree) of
        Nothing ->
            Attr.None
        Just (Attr.Attr name value) ->
            value


{-| 设置指定属性的值
-}
set : List String -> Attr.AttrValue -> Attr.AttrTree a -> Attr.AttrTree a
set paths value topTree =
    topTree
