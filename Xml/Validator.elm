module Xml.Validator exposing (parseSchemaXsd)

{-| This module contains a validator that can check an XML document
### for valid format by using a schema document. This implementation
### currently uses the XSD v1.1 specification for defining schema
### documents.

# parse and validate a XSD schema document
@docs parseSchemaXsd

-}

import String
import Maybe
import Result
import List
import Xml.Types exposing (..)
import Xml.Parser exposing (parseXml)

type XsdAst
    = Header
    | Content
    | XsdComment String

headerKeys = ["xs:include", "xs:import", "xs:redefine", "xs:annotation"]
contentKeys = ["xs:element", "xs:attribute", "xs:notation", "xs:simpleType", "xs:complexType", "xs:group", "xs:attributegroup"]

parseInnerXsd : XmlAst -> Result String XsdAst
parseInnerXsd data =
    case data of
        (Element n attrs elems) ->
            if ( (List.any (\x -> (x == n)) headerKeys) )
            then
                (Result.Ok Header)
            else if ( (List.any (\x -> (x==n)) contentKeys) )
            then
                (Result.Ok Content)
            else
                (Result.Err ("Unexpected Key" ++ n))
        (Body str) -> (Result.Err "Unexpected Body in XSD")
        (Comment str) -> (Result.Ok (XsdComment "Comment Str"))
        (CDATA str) -> (Result.Err "Unexpected CDATA in XSD")

isError : Result String XsdAst -> Bool
isError res =
    case res of
        (Result.Ok a) -> False
        (Result.Err msg) -> True

getErrMsg : Result String XsdAst -> Maybe String
getErrMsg res =
    case res of
        (Result.Ok a) -> Maybe.Nothing
        (Result.Err msg) -> msg

getAstList : Result String XsdAst -> Maybe XsdAst
getAstList res =
    case res of
        (Result.Ok a) -> a
        (Result.Err msg) -> Maybe.Nothing

parseSchemaXsd : (List XmlAst) -> Result (List String) (List XsdAst)
parseSchemaXsd data =
    let
        schema = (List.map parseInnerXsd data)
    in
        if ( (List.any isError schema ) )
        then
            (Result.Err (List.filterMap getErrMsg schema))
        else
            (Result.Ok (List.filterMap getAstList schema))



parseXSD : String -> (Result.Result (List String) String)
parseXSD str =
    (\x -> (Result.Ok "Yay")) <| (parseXml str)
    {- (parseXml str) `Result.andThen` (\x -> (Result.Ok "Yay")) -}
