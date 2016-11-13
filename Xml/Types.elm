module Xml.Types exposing (..)
{-| Xml Types
###
-}

type alias Name =
  String

type alias Key =
  String

type alias Value =
  String

type alias Attribute =
  ( Key, Value )

{-| The XML AST representation -}
type XmlAst
  = Element Name (List Attribute) (List XmlAst)
  | Body String
  | Comment String
  | CDATA String
