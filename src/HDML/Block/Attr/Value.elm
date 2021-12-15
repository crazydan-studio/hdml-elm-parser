module HDML.Block.Attr.Value exposing
    ( get, set
    )

import HDML.Block.Attr as Attr

import Dict exposing (Dict)


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
set : Attr.AttrValue -> List String -> Attr.AttrTree a -> Attr.AttrTree a
set value paths topTree =
    case paths of
        [] ->
            topTree
        name :: subPaths ->
            let
                topTreeNode =
                    Dict.get name topTree
            in
                case topTreeNode of
                    Nothing ->
                        if List.isEmpty subPaths then
                            Dict.insert name (Attr.AttrTreeNode value Dict.empty) topTree
                        else
                            let
                                subTree = set value subPaths Dict.empty
                            in
                                Dict.insert name (Attr.AttrTreeNode Attr.None subTree) topTree
                    Just (Attr.AttrTreeNode nodeValue subTree) ->
                        if List.isEmpty subPaths then
                            Dict.insert name (Attr.AttrTreeNode value subTree) topTree
                        else
                            let
                                newSubTree = set value subPaths subTree
                            in
                                Dict.insert name (Attr.AttrTreeNode nodeValue newSubTree) topTree
