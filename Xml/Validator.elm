module Xml.Validator exposing (parseXsd)

{-| This module contains a validator that can check an XML document
### for valid format by using a schema document. This implementation
### currently uses the XSD v1.1 specification for defining schema
### documents.

# parse and validate a XSD schema document
@docs parseXsd

-}

import String
import Maybe
import Result
import List
import Xml.Types exposing (..)
import Xml.Parser exposing (parseXml)
import Xml.XsdTypes exposing (..)


isError : Result String t -> Bool
isError res =
    case res of
        (Result.Ok a) -> False
        (Result.Err msg) -> True

getErrMsg : Result String t -> Maybe String
getErrMsg res =
    case res of
        (Result.Ok a) -> Maybe.Nothing
        (Result.Err msg) -> Just msg

{-
parseHeader : String -> (List Attribute) -> (List XmlAst) -> Result String XsdSchemaAst
parseHeader n attrs elems =
    Result.Ok (Header [Redefine])

parseContent : String -> (List Attribute) -> (List XmlAst) -> Result String XsdSchemaAst
parseContent n attrs elems =
    Result.Ok (Content [(Definition ElementDef)])
-}

namespace = "xs"
rootKey = "schema"

numericKeys =
    [ "byte"                -- A signed 8-bit integer
    , "decimal"             -- A decimal value
    , "int"                 -- A signed 32-bit integer
    , "integer"             -- An integer value
    , "long"                -- A signed 64-bit integer
    , "negativeInteger"     -- An integer containing only negative values
    , "nonNegativeInteger"  -- An integer containing only non-negative vals
    , "nonPositiveInteger"  -- An integer containing only non-positive vals
    , "positiveInteger"     -- An integer containing only positive values
    , "short"               -- A signed 16-bit integer
    , "unsignedLong"        -- An unsigned 64-bit integer
    , "unsignedInt"         -- An unsigned 32-bit integer
    , "unsignedShort"       -- An unsigned 16-bit integer
    , "unsignedByte"        -- An unsigned 8-bit integer
    , "double"
    , "float"
    ]

dateTimeKeys =
    [ "date"       -- YYYY-MM-DD
    , "time"       -- hh:mm:ss
    , "datetime"   -- YYYY-MM-DDThh:mm:ss
    , "duration"   -- P5Y2M10DT15H
    , "gDay"       --
    , "gMonth"     --
    , "gMonthDay"  --
    , "gYear"      --
    , "gYearMonth" --
    ]

stringKeys =
    [ "string"
    , "normalizedString" -- Whitespace Replaced String
    , "token"      -- String whitespace replaced and collapsed
      -- A string that represents the ID attribute in XML (only
      -- used with schema attributes)
    , "ENTITIES"
    , "ENTITY"
    , "ID"         -- Similar to NCName
      -- A string that represents the IDREF attribute in XML (only
      -- used with schema attributes)
    , "IDREF"
    , "IDREFS"
    , "language"   -- A string that contains a valid language id
    , "Name"       -- A string that contains a valid XML name
    , "NCName"     -- Non-Qualified Name
    , "NMTOKEN"    -- similar to token except spaces and commas are not allowed
    , "NMTOKENS"
    , "QName"
    ]

miscKeys =
    [ "boolean"
    , "hexBinary"
    , "anyURI"
    , "base64Binary"
    , "NOTATION"
    ]

typeKeys = stringKeys ++ numericKeys ++ dateTimeKeys
headerKeys = ["include", "import", "redefine", "annotation"]
contentKeys = ["element", "attribute", "notation", "simpleType", "complexType", "group", "attributegroup"]

{-
parseSchemaElem : XmlAst -> Result String XsdSchemaAst
parseSchemaElem data =
    case data of
        (Element n attrs elems) ->
            if ( (List.any (\x -> (x == n)) headerKeys) )
            then
                (parseHeader n attrs elems)
            else if ( (List.any (\x -> (x==n)) contentKeys) )
            then
                (parseContent n attrs elems)
            else
                (Result.Err ("Unexpected Key " ++ n))
        (Body str) -> (Result.Err "Unexpected Body in XSD Schema")
        (Comment str) -> (Result.Ok (SchemaComment str))
        (CDATA str) -> (Result.Err "Unexpected CDATA in XSD")

getSchemaData : Result String XsdSchemaAst -> Maybe XsdSchemaAst
getSchemaData res =
    case res of
        (Result.Ok a) -> Just a
        (Result.Err msg) -> Maybe.Nothing


parseRootXsd : String -> (List Attribute) -> (List XmlAst) -> Result (List String) XsdAst
parseRootXsd n attrs elems =
    let
        xsdElems = (List.map parseSchemaElem elems)
    in
        if ( (List.any isError xsdElems) )
        then
            (Result.Err (List.filterMap getErrMsg xsdElems))
        else
            (Result.Ok  (XsdRoot attrs (List.filterMap getSchemaData xsdElems)))


parseInnerXsd : XmlAst -> Result String XsdAst
parseInnerXsd data =
    case data of
        (Element n attrs elems) ->
            if ( n == rootKey )
            then
                (parseRootXsd n attrs elems)
            else
                (Result.Err ("Unexpected Root Element " ++ n))
        (Body str) -> (Result.Err "Unexpected Body in XSD")
        (Comment str) -> (Result.Ok (XsdComment str))
        (CDATA str) -> (Result.Err "Unexpected CDATA in XSD")


getXsdData : Result String XsdAst -> Maybe XsdAst
getXsdData res =
    case res of
        (Result.Ok a) -> Just a
        (Result.Err msg) -> Maybe.Nothing


{-| Parse and Validate an XSD Object passed as an
### XML object.
-}
parseSchemaXsd : (List XmlAst) -> Result (List String) (List XsdAst)
parseSchemaXsd data =
    let
        schema = (List.map parseInnerXsd data)
    in
        if ( (List.any isError schema ) )
        then
            (Result.Err (List.filterMap getErrMsg schema))
        else
            (Result.Ok (List.filterMap getXsdData schema))

-}
{-| Parse and Validate an XSD Object passed as an
### String.
-}
parseXsd : String -> (Result.Result (List String) (List XsdAst))
parseXsd str =
    {-
    (parseXml str) `Result.andThen` parseSchemaXsd
     -}
    Result.Err ["Not Implemented Err"]
