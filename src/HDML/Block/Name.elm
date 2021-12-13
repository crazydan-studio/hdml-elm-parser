module HDML.Block.Name exposing
    ( Name(..)
    , from
    )


type Name =
    As String
    -- 匿名块
    | Block
    | Text
    | Paragraph
    | Section
    | Source
    | List
    | Table
    | Head
    | Row


from : String -> Name
from name =
    case name of
        "Block" ->
            Block
        "Text" ->
            Text
        "Paragraph" ->
            Paragraph
        "Section" ->
            Section
        "Source" ->
            Source
        "List" ->
            List
        "Table" ->
            Table
        "Head" ->
            Head
        "Row" ->
            Row
        _ ->
            As name
