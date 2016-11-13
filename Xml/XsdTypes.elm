module Xml.XsdTypes exposing (..)

{-| This module contains type definitions for the XSD v1.1
### specification.

-}

import String
import Maybe
import Maybe.Extra as MaybeE
import Result.Extra as ResultE
import List
import Regex
import Dict
import Xml.Types exposing (..)

type alias AnyURI = String
type alias TOKEN = String
type alias ID = String
type alias QName = String
type alias NCName = String
type alias NonNegativeInt = Int
type alias XPath = String

{- Root Node Attribute Definitions -}
type XsdForm = Qualified | Unqualified

toXsdForm : Maybe String -> Maybe XsdForm
toXsdForm input =
  case input of
    Nothing -> Nothing
    Just str ->
      case str of
        "qualified" -> Just Qualified
        "unqualified" -> Just Unqualified
        _ -> Nothing

type FinalElementControl
  = FinalExtension
  | FinalRestriction
  | List
  | Union
  | FinalAll

type BlockElementControl
  = BlockExtension
  | BlockRestriction
  | Substituion
  | BlockAll

{- General Attribute Mixins -}

type alias IdMixin rec = { rec | id: Maybe ID }
type alias AnyAttributeMixin rec = { rec | attr: (List Attribute) }
type alias NameMixin rec = { rec | name: Maybe NCName }
type alias RefMixin rec = { rec | ref: Maybe QName }

elementNameRegex = Regex.regex "^(([^:\\s]*):)?([^:\\s]*)$"

getElementRootName: String -> Maybe String
getElementRootName str =
  let
    mList = Regex.find (Regex.AtMost 1) elementNameRegex str
  in
    case (List.head mList) of
      Nothing -> Nothing
      Just m ->
        let
          g3 = (List.drop 2 m.submatches)
          nameMatch = (MaybeE.join (List.head g3))
        in
          nameMatch

checkElementName: String -> String -> Bool
checkElementName str name =
  let
    nameMatch = getElementRootName str
  in
    case nameMatch of
      Nothing -> False
      Just val -> (val == name)

{- Error Message Utility Methods -}

errMsgs =
  { invalidIdKey = "Invalid ID Attribute"
  , invalidRefKey = "Invalid Ref Attribute"
  , invalidMaxOccurs = "Invalid maxOccurs Value"
  , invalidMinOccurs = "Invalid minOccurs Value"
  }

invalidErrMsg : String -> String -> Result (List String) t
invalidErrMsg errType elemName =
  let
    mainMsg = "Invalid " ++ errType ++ " Encountered in "
  in
    Result.Err [
       (String.join "'" [mainMsg,elemName,""])
      ]

invalidBodyMsg : String -> Result (List String) t
invalidBodyMsg elemName = invalidErrMsg "Body Text" elemName
invalidCDATAMsg : String -> Result (List String) t
invalidCDATAMsg elemName = invalidErrMsg "CDATA" elemName
invalidElementMsg : String -> Result (List String) t
invalidElementMsg elemName = invalidErrMsg "Element" elemName
invalidMiscAttrMsg : String -> Result (List String) t
invalidMiscAttrMsg elemName = invalidErrMsg "Misc Attr" elemName

{- Standard Attribute Key Names for many elements -}
keys =
  { id = "id"
  , ref = "ref"
  , name = "name"
  , minOccurs = "minOccurs"
  , maxOccurs = "maxOccurs"
  , namespace = "namespace"
  , processContents = "processContents"
  , source = "source"
  , xmllang="xml:lang"
  , schemaLoc = "schemaLoc"
  , type_ = "type"
  , default = "default"
  , fixed = "fixed"
  , form = "form"
  , use = "use"
  , xpath="xpath"
  , refer = "refer"
  }

{- Standard Element Names -}
elements =
  { all = "all"
  , annotation = "annotation"
  , any = "any"
  , anyAttribute = "anyAttribute"
  , appinfo = "appinfo"
  , attribute = "attribute"
  , attributeGroup = "attributeGroup"
  , choice = "choice"
  , complexContent = "complexContent"
  , complexType = "complexType"
  , documentation = "documentation"
  , element = "element"
  , extension = "extension"
  , field = "field"
  , group = "group"
  , import_ = "import"
  , include = "include"
  , key = "key"
  , keyref = "keyref"
  , list = "list"
  , notation = "notation"
  , redefine = "redefine"
  , restriction = "restriction"
  , schema = "schema"
  , selector = "selector"
  , sequence = "sequence"
  , simpleContent = "simpleContent"
  , simpleType = "simpleType"
  , union = "union"
  , unique = "unique"
  }

{- General Attribute Utility Methods -}

getAttrCount: (List Attribute) -> Name -> Int
getAttrCount attr name =
  (List.length (List.filter (\x -> ((fst x) == name)) attr))

getAttr: Name -> (List Attribute) -> Maybe String
getAttr name attr =
  (List.head (List.filter (\x -> ((fst x) == name)) attr))
  `Maybe.andThen` (\x -> Just (snd x))

{- Many Elements have the option that the user can include miscellaneous
   attributes that are not part of the schema spec. This function
   filters out any known attributes from the total list to create this
   misc set of attributes
-}
getMiscAttrs: (List Attribute) -> (List String) -> (List Attribute)
getMiscAttrs attr keys =
  (List.filter (\x -> (not (List.any (\y -> (fst x) == y) keys))) attr)

{- Attribute Validator Functions
   These are helper methods for validating a set of attributes
   associated with a particular element.
-}

type alias ValidateFunc = (Maybe String) -> (Maybe String)

idRegex = Regex.regex "^[a-zA-Z_][\\w.-]*$"
maxOccursRegex = Regex.regex "^([\\d]+|unbounded)$"
nonNegIntRegex = Regex.regex "^[\\d]+$"
{- URI Regex per URI Specification
-- https://tools.ietf.org/html/rfc3986#appendix-B
-- Note I made a slight modification to the final # fragment
-- because the original "(#(.*))?" did not work correctly in
-- javascript.
-}
anyURIRegex = Regex.regex "^(([^:/?#\\s]+):)(\\/\\/([^/?#\\s]*))?([^?\\s#]*)(\\?([^\\s#]*))?(#([^\\s#]*))?$"

qNameRegex = idRegex -- Temp
ncNameRegex = idRegex -- Temp
xsdFormRegex = Regex.regex "^(qualified|unqualified)$"
useAttrRegex = Regex.regex "^(optional|prohibited|required)$"
xpathRegex = Regex.regex "^([^\\s]*)$"

isValidString : (Maybe String) -> Maybe String
isValidString str = Nothing

isValidInput: Regex.Regex -> Bool -> (Maybe String) -> Maybe String
isValidInput r required input =
  case input of
    Just str ->
      if not (Regex.contains r str ) then
        Just "Failed to Match Regex for Value"
      else
        Nothing
    Nothing ->
      if ( required ) then
        Just "Missing Required Value for Attribute"
      else
        Nothing

isValidId : (Maybe String) -> Maybe String
isValidId input = (isValidInput idRegex False input)

isValidQName : (Maybe String) -> Maybe String
isValidQName input = (isValidInput qNameRegex False input)

isValidNCName : (Maybe String) -> Maybe String
isValidNCName input = (isValidInput ncNameRegex False input)

isValidXsdForm : (Maybe String) -> Maybe String
isValidXsdForm input = (isValidInput xsdFormRegex False input)

isValidUseAttr : (Maybe String) -> Maybe String
isValidUseAttr input = (isValidInput useAttrRegex False input)

isValidMaxOccurs: (Maybe String) -> Maybe String
isValidMaxOccurs input = (isValidInput maxOccursRegex False input)

isValidMinOccurs: (Maybe String) -> Maybe String
isValidMinOccurs input = (isValidInput nonNegIntRegex False input)

isValidURI : (Maybe String) -> Maybe String
isValidURI input = (isValidInput anyURIRegex False input)

isRequiredURI : (Maybe String) -> Maybe String
isRequiredURI input = (isValidInput anyURIRegex True input)

isValidXPath : (Maybe String) -> Maybe String
isValidXPath input = (isValidInput xpathRegex False input)

{-
   This function is intended to help extract and validate known
   attributes for a particular element.
   @param keyFuncs List of tuples that provide a validator for each type of
      attribute.
   @param attr List of Attributes - this is the input that we are going
      to parse.
   @result either a list of strings indicating any error messages
      associated with the attributes we found or a dict of the
      strings associated with each attribute. In this case the
      attribute keys passed in keyFuncs are the keys to the dict.
-}
resultDict: (List (String, ValidateFunc)) -> (List Attribute) -> Result (List String) (Dict.Dict String (Maybe String))
resultDict keyFuncs attr =
  let
    {- Create a (key,val) list that we will convert to Dict-}
    res = (List.map (\x -> ((fst x), getAttr (fst x) attr)) keyFuncs)
    valDict = (Dict.fromList res)
    {- Validate each string in the dict if it exists -}
    errInfo = (List.map
                 (\x ->
                    (snd x) (MaybeE.join (Dict.get (fst x) valDict)))
                 keyFuncs)
    errList = (List.filterMap (\x -> x) errInfo)
  in
    if not (List.isEmpty errList) then
      Result.Err errList
    else
      Result.Ok valDict

{- Parsing Tools -}

maybeToInt: (Maybe String) -> (Maybe Int)
maybeToInt input =
  case input of
    Just str -> Result.toMaybe (String.toInt str)
    Nothing -> Nothing

maybeToMaxOccurs: (Maybe String) -> (Maybe MaxOccursType)
maybeToMaxOccurs input =
  case input of
    Just str ->
      case (String.toInt str ) of
        Result.Ok val ->
          if (val >= 0 ) then
            Just (NonNegativeIntOccurs val)
          else
            Nothing
        Result.Err _ ->
          if ( str == "unbounded" ) then
            Just UnboundedOccurs
          else
            Nothing
    Nothing -> Nothing


{- Header Node Element Definitions -}

type alias AppInfoElement =
  {  source : Maybe AnyURI
  ,  content : (List XmlAst)
  }

appInfoElementAttrs = [keys.source]

parseAppInfoElement: (List Attribute) -> (List XmlAst) -> Result (List String) AnnotationContentAst
parseAppInfoElement attr elems =
  let
    validKeys = [
     (keys.source, isValidURI)
    ]
    results = resultDict validKeys attr
    miscAttrs = (getMiscAttrs attr appInfoElementAttrs)
  in
    if (List.isEmpty miscAttrs) then
      case results of
        Result.Err errList -> Result.Err errList
        Result.Ok valDict ->
          Result.Ok (AppInfo
            { source = (MaybeE.join (Dict.get keys.source valDict))
            , content = elems
            })
    else
      {- @todo - we should check the spec and confirm that
        w3c is correct on this -}
      invalidMiscAttrMsg elements.appinfo


type alias DocumentationElement =
  {  source : Maybe AnyURI
  ,  xmllang : Maybe String
  ,  content : (List XmlAst)
  }

docElementAttrs = [keys.source, keys.xmllang]

parseDocElement: (List Attribute) -> (List XmlAst) -> Result (List String) AnnotationContentAst
parseDocElement attr elems =
  let
    validKeys = [
     (keys.source, isValidURI)
    ]
    results = resultDict validKeys attr
    miscAttrs = (getMiscAttrs attr docElementAttrs)
  in
    if (List.isEmpty miscAttrs) then
      case results of
        Result.Err errList -> Result.Err errList
        Result.Ok valDict ->
          Result.Ok (Documentation
            { source = (MaybeE.join (Dict.get keys.source valDict))
            , xmllang = (getAttr keys.xmllang attr)
            , content = elems
            })
    else
      {- @todo - we should check the spec and confirm that
      w3c is correct on this -}
      invalidMiscAttrMsg elements.documentation

type AnnotationContentAst
  = AppInfo AppInfoElement
  | Documentation DocumentationElement
  | AnnotComment String

type alias AnnotationElementData = { content : (List AnnotationContentAst) }
type alias AnnotationElement =
    AnyAttributeMixin (IdMixin AnnotationElementData)

annotationElementAttrs = [keys.id]

getErrList : Result (List String) a -> (List String)
getErrList input =
  case input of
    Result.Ok _ -> []
    Result.Err errList -> errList

fullCombine: (List (Result (List String) t)) -> Result (List String) (List t)
fullCombine data =
  let
    errLists = (List.map getErrList data)
    totalErrs = (List.foldr (List.append) [] errLists)
  in
    if (List.isEmpty totalErrs) then
      Result.Ok (List.filterMap (\x -> (Result.toMaybe x) ) data)
    else
      Result.Err totalErrs

type alias ParseAnnotAstFunc = (List Attribute) -> (List XmlAst) -> Result (List String) AnnotationContentAst

parseAnnotAst: (Dict.Dict String ParseAnnotAstFunc) -> XmlAst -> Maybe (Result (List String) AnnotationContentAst)
parseAnnotAst funcs elem =
  case elem of
    Element n attr elems ->

      let
        name = getElementRootName n
      in
        case name of
          Nothing ->
            Just (Result.Err ["Invalid Element Name: " ++ n])
          Just str ->
            let
              fRes = (Dict.get str funcs)
            in
              case fRes of
                Nothing -> Nothing
                Just f -> Just (f attr elems)
    Comment str -> Just (Result.Ok (AnnotComment str))
    _ -> Nothing

parseAnnotationElement: (List Attribute) -> (List XmlAst) -> Result (List String) AnnotationElement
parseAnnotationElement attr elems =
  let
    -- Parse Elements
    funcs = (Dict.fromList
               [ (elements.documentation, parseDocElement)
               , (elements.appinfo, parseAppInfoElement)
               ])
    contentList = (List.map (\x -> (parseAnnotAst funcs x)) elems)
    contentRes = fullCombine (List.filterMap (\x -> x) contentList)
    -- Parse Attributes
    validKeys = [ (keys.id, isValidId) ]
    attrRes = resultDict validKeys attr
  in
    case attrRes of
      Result.Err attrErrList ->
        case contentRes of
          Result.Err contentErrList -> Result.Err (attrErrList ++ contentErrList)
          Result.Ok _ -> Result.Err attrErrList

      Result.Ok valDict ->
        case contentRes of
          Result.Err contentErrList -> Result.Err contentErrList
          Result.Ok content ->
            Result.Ok
              { id = (MaybeE.join (Dict.get keys.id valDict))
              , attr = getMiscAttrs attr annotationElementAttrs
              , content = content
              }


type alias AnnotationMixin rec =
  { rec | annotation: Maybe AnnotationElement }

getAnnotationElem : (List XmlAst) -> (Result (List String) (Maybe AnnotationElement))
getAnnotationElem data =
  let
    annotFilter =
      (\x ->
         case x of
           Element n attr elems ->
             if ( checkElementName n elements.annotation) then
               Just (parseAnnotationElement attr elems)
             else
               Nothing
           _ -> Nothing)

    results = (List.filterMap annotFilter data)
  in
    -- Note - this just pulls the first result and ignores any
    --  subsequent annotation elements
    case (List.head results) of
      Nothing -> Result.Ok Nothing
      Just res ->
        case res of
          Result.Err msgs -> Result.Err msgs
          Result.Ok annot -> Result.Ok (Just annot)

getUnexpectedElements : (List String) -> (List XmlAst) -> (List XmlAst)
getUnexpectedElements expected elems =
  (List.filter
     (\x -> case x of
              Element n attr elems ->
                not (List.any (\x -> checkElementName n x) expected)
              Comment str -> False
              _ -> True
     ) elems)

type alias IncludeElementData = { schemaLoc: AnyURI }
type alias IncludeElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin IncludeElementData))

parseIncludeElement: (List Attribute) -> (List XmlAst) -> Result (List String) HeaderAst
parseIncludeElement attr elems =
  let
    annotResult = (getAnnotationElem elems)

    -- Parse Attributes
    validKeys =
      [ (keys.id, isValidId)
      , (keys.schemaLoc, isValidURI)
      ]
    includeElementAttrs = (List.map (\x -> (fst x)) validKeys)
    attrRes = resultDict validKeys attr
  in
    case attrRes of
      Result.Err attrErrList ->
        case annotResult of
          Result.Err msgs -> Result.Err (attrErrList ++ msgs)
          Result.Ok _ -> Result.Err attrErrList
      Result.Ok valDict ->
        case annotResult of
          Result.Err msgs -> Result.Err msgs
          Result.Ok maybeAnnot ->
            case (MaybeE.join (Dict.get keys.schemaLoc valDict)) of
              Nothing -> Result.Err ["schemaLoc attribute is required"]
              Just str ->
                Result.Ok
                  (Include { schemaLoc = str
                           , annotation = maybeAnnot
                           , id = (MaybeE.join (Dict.get keys.id valDict))
                           , attr = getMiscAttrs attr includeElementAttrs
                           })

type alias ImportElementData =
  { namespace: Maybe AnyURI
  , schemaLoc: Maybe AnyURI
  }
type alias ImportElement =
    AnnotationMixin (AnyAttributeMixin (IdMixin ImportElementData))

parseImportElement : (List Attribute) -> (List XmlAst) -> Result (List String) HeaderAst
parseImportElement attr elems =
  let
    annotResult = (getAnnotationElem elems)

    validKeys =
      [ (keys.id, isValidId)
      , (keys.schemaLoc, isValidURI)
      , (keys.namespace, isValidURI)
      ]

    importElementAttrs = (List.map (\x -> (fst x)) validKeys)
    attrRes = resultDict validKeys attr
  in
    case attrRes of
      Result.Err attrErrList ->
        case annotResult of
          Result.Err msgs -> Result.Err (attrErrList ++ msgs)
          Result.Ok _ -> Result.Err attrErrList
      Result.Ok valDict ->
        case annotResult of
          Result.Err msgs -> Result.Err msgs
          Result.Ok maybeAnnot ->
            Result.Ok
              (Import
                 { id = (MaybeE.join (Dict.get keys.id valDict))
                 , schemaLoc =
                     (MaybeE.join (Dict.get keys.schemaLoc valDict))
                 , namespace =
                     (MaybeE.join (Dict.get keys.namespace valDict))
                 , attr = getMiscAttrs attr importElementAttrs
                 , annotation = maybeAnnot
                 })

type RedefineElementAst
  = REAnnotation AnnotationElement
  | REType TypeAst

type alias RedefineElementData =
  { schemaLoc: AnyURI
  , content: (List RedefineElementAst)
  }
type alias RedefineElement =
  AnyAttributeMixin (IdMixin RedefineElementData)

parseRedefineElement : (List Attribute) -> (List XmlAst) -> Result (List String) HeaderAst
parseRedefineElement attr elems =
  let
    -- @todo - fix me
    annotResult = (getAnnotationElem elems)

    validKeys =
      [ (keys.id, isValidId)
      , (keys.schemaLoc, isValidURI)
      ]

    redefineElementAttrs = (List.map (\x -> (fst x)) validKeys)
    attrRes = resultDict validKeys attr
  in
    case attrRes of
      Result.Err attrErrList ->
        case annotResult of
          Result.Err msgs -> Result.Err (attrErrList ++ msgs)
          Result.Ok _ -> Result.Err attrErrList
      Result.Ok valDict ->
        case (MaybeE.join (Dict.get keys.schemaLoc valDict) ) of
          Nothing -> Result.Err ["schemaLoc is required!"]
          Just schemaLocStr ->
            Result.Ok
              (Redefine
                 { id = (MaybeE.join (Dict.get keys.id valDict))
                 , schemaLoc = schemaLocStr
                 , attr = getMiscAttrs attr redefineElementAttrs
                 , content = []
                 })



type HeaderAst
  = Include IncludeElement
  | Import ImportElement
  | Redefine RedefineElement
  | HeaderAnnotation AnnotationElement

{- Attribute Definition Ast -}
type alias AttributeElementData =
  { default: Maybe String
  , fixed: Maybe String
  , form: Maybe XsdForm
  , attrtype: Maybe QName
  , use: Maybe String
  , simpleType: Maybe SimpleTypeElement
  }
type alias AttributeElement =
  RefMixin (NameMixin (AnnotationMixin (AnyAttributeMixin (IdMixin AttributeElementData))))

parseAttributeElement : (List Attribute) -> (List XmlAst) -> Result (List String) AttributeElement
parseAttributeElement attr elems =
  let
    annotResult = (getAnnotationElem elems)
    simpleTResult = Result.Ok Nothing --(getSimpleTypeElem elems)
    hasSimpleTResult = (case simpleTResult of
                      Result.Ok stMaybe ->
                        case stMaybe of
                          Just st -> True
                          Nothing -> False
                      Result.Err msgs -> False)
    validKeys =
      [ (keys.id, isValidId)
      , (keys.ref, isValidQName)
      , (keys.name, isValidNCName)
      , (keys.form, isValidXsdForm)
      , (keys.type_, isValidQName)
      , (keys.use, isValidUseAttr)
      , (keys.default, isValidString)
      , (keys.fixed, isValidString)
      ]
    attrElementAttrs = (List.map (\x -> (fst x)) validKeys)
    attrRes = resultDict validKeys attr
    -- These func return true is there is an error
    checkAttrFuncs =
      [ (\x -> (Dict.member keys.default x) && (Dict.member keys.fixed x))
      , (\x -> if (Dict.member keys.ref x) then
                 if ( (Dict.member keys.form x) ||
                      (Dict.member keys.type_ x) ||
                      (Dict.member keys.name x) ) then
                   True
                 else
                   False
               else
                 False
        )
      ]
  in
    case attrRes of
      Result.Err attrErrList ->
        case annotResult of
          Result.Err msgs -> Result.Err (attrErrList ++ msgs)
          Result.Ok _ -> Result.Err attrErrList
      Result.Ok valDict ->
        if (List.any (\x -> x valDict) checkAttrFuncs) then
          Result.Err ["Failed Check on Attributes"]
        else
          if ( (Dict.member keys.ref valDict) && hasSimpleTResult ) then
            Result.Err ["Ref Attribute and SimpleType Element can not be defined as the same time"]
          else if ( (Dict.member keys.type_ valDict) && hasSimpleTResult) then
            Result.Err ["Type Attribute and SimpleType Element can not be defined as the same time"]
          else
            case annotResult of
              Result.Err msgs -> Result.Err msgs
              Result.Ok maybeAnnot ->
                Result.Ok
                  { id = (MaybeE.join (Dict.get keys.id valDict))
                  , name = (MaybeE.join (Dict.get keys.name valDict))
                  , ref = (MaybeE.join (Dict.get keys.ref valDict))
                  , fixed = (MaybeE.join (Dict.get keys.fixed valDict))
                  , default = (MaybeE.join (Dict.get keys.default valDict))
                  , attrtype = (MaybeE.join (Dict.get keys.default valDict))
                  , use = (MaybeE.join (Dict.get keys.use valDict))
                  , form = (toXsdForm (MaybeE.join (Dict.get keys.form valDict)))
                  , attr = getMiscAttrs attr attrElementAttrs
                  , annotation = maybeAnnot
                  , simpleType = Nothing
                }


type alias AnyAttributeElementData =
  { namespace: Maybe String
  , processContents: Maybe String
  }
type alias AnyAttributeElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin AnyAttributeElementData))

parseAnyAttributeElement : (List Attribute) -> (List XmlAst) -> Result (List String) AnyAttributeElement
parseAnyAttributeElement attr elems =
  Result.Err ["Not Implemented Error"]

type alias AttributeGroupElementData =
  { attrdefs : (List AttributeAst)
  , anyattr : Maybe AnyAttributeElement
  }
type alias AttributeGroupElement =
  RefMixin (NameMixin (AnnotationMixin (AnyAttributeMixin (IdMixin AttributeGroupElementData))))

parseAttributeGroupElement : (List Attribute) -> (List XmlAst) -> Result (List String) AttributeAst
parseAttributeGroupElement attr elems =
  Result.Err ["Not Implemented Error"]

type AttributeAst
  = AttributeSingle AttributeElement
  | AttributeGroup AttributeGroupElement

{- Element Definition Ast -}

type ElementTypeAst
  = ElementSimpleType
  | ElementComplexType

type alias SelectorElementData = { xpath: XPath }
type alias SelectorElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin SelectorElementData))

parseSelectorElement : (List Attribute) -> (List XmlAst) -> Result (List String) SelectorElement
parseSelectorElement attr elems =
  let
    annotResult = (getAnnotationElem elems)

    validKeys =
      [ (keys.id, isValidId)
      , (keys.xpath, isValidXPath)
      ]

    selectorElementAttrs = (List.map (\x -> (fst x)) validKeys)
    attrRes = resultDict validKeys attr
  in
    case attrRes of
      Result.Err attrErrList ->
        case annotResult of
          Result.Err msgs -> Result.Err (attrErrList ++ msgs)
          Result.Ok _ -> Result.Err attrErrList
      Result.Ok valDict ->
        case (MaybeE.join (Dict.get keys.xpath valDict)) of
          Nothing -> Result.Err ["xpath attribute is required"]
          Just xpathStr ->
            case annotResult of
              Result.Err msgs -> Result.Err msgs
              Result.Ok maybeAnnot ->
                Result.Ok
                  { id = (MaybeE.join (Dict.get keys.id valDict))
                  , xpath = xpathStr
                  , attr = getMiscAttrs attr selectorElementAttrs
                  , annotation = maybeAnnot
                  }

type alias FieldElementData = { xpath: XPath }
type alias FieldElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin FieldElementData))

parseFieldElement : (List Attribute) -> (List XmlAst) -> Result (List String) FieldElement
parseFieldElement attr elems =
  let
    annotResult = (getAnnotationElem elems)

    validKeys =
      [ (keys.id, isValidId)
      , (keys.xpath, isValidXPath)
      ]

    fieldElementAttrs = (List.map (\x -> (fst x)) validKeys)
    attrRes = resultDict validKeys attr
  in
    case attrRes of
      Result.Err attrErrList ->
        case annotResult of
          Result.Err msgs -> Result.Err (attrErrList ++ msgs)
          Result.Ok _ -> Result.Err attrErrList
      Result.Ok valDict ->
        case (MaybeE.join (Dict.get keys.xpath valDict)) of
          Nothing -> Result.Err ["xpath attribute is required"]
          Just xpathStr ->
            case annotResult of
              Result.Err msgs -> Result.Err msgs
              Result.Ok maybeAnnot ->
                Result.Ok
                  { id = (MaybeE.join (Dict.get keys.id valDict))
                  , xpath = xpathStr
                  , attr = getMiscAttrs attr fieldElementAttrs
                  , annotation = maybeAnnot
                  }

type alias UniqueElementData =
  { name: NCName  -- Name required - hence no Maybe
  , selector: SelectorElement
  , fields: (List FieldElement)
  }
type alias UniqueElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin UniqueElementData))

parseUniqueElement : (List Attribute) -> (List XmlAst) -> Result (List String) ElementQualifierAst
parseUniqueElement attr elems =
  let
    annotResult = (getAnnotationElem elems)

    validKeys =
      [ (keys.id, isValidId)
      , (keys.name, isValidNCName)
      ]

    uniqueElementAttrs = (List.map (\x -> (fst x)) validKeys)
    attrRes = resultDict validKeys attr
  in
    case attrRes of
      Result.Err attrErrList ->
        case annotResult of
          Result.Err msgs -> Result.Err (attrErrList ++ msgs)
          Result.Ok _ -> Result.Err attrErrList
      Result.Ok valDict ->
        case (MaybeE.join (Dict.get keys.name valDict)) of
          Nothing -> Result.Err ["name attribute is required"]
          Just nameStr ->
            case annotResult of
              Result.Err msgs -> Result.Err msgs
              Result.Ok maybeAnnot ->
                Result.Ok (ElementUniqueQual
                  { id = (MaybeE.join (Dict.get keys.id valDict))
                  , name = nameStr
                  , attr = getMiscAttrs attr uniqueElementAttrs
                  , annotation = maybeAnnot
                  , selector = { id = Nothing -- @todo
                               , xpath = ""
                               , attr = []
                               , annotation = Nothing
                               }
                  , fields = []
                  })

type alias KeyElementData =
  { name: NCName
  , selector: SelectorElement
  , fields: (List FieldElement)
  }
type alias KeyElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin KeyElementData))

parseKeyElement : (List Attribute) -> (List XmlAst) -> Result (List String) ElementQualifierAst
parseKeyElement attr elems =
  let
    annotResult = (getAnnotationElem elems)

    validKeys =
      [ (keys.id, isValidId)
      , (keys.name, isValidNCName)
      ]

    keyElementAttrs = (List.map (\x -> (fst x)) validKeys)
    attrRes = resultDict validKeys attr
  in
    case attrRes of
      Result.Err attrErrList ->
        case annotResult of
          Result.Err msgs -> Result.Err (attrErrList ++ msgs)
          Result.Ok _ -> Result.Err attrErrList
      Result.Ok valDict ->
        case (MaybeE.join (Dict.get keys.name valDict)) of
          Nothing -> Result.Err ["name attribute is required"]
          Just nameStr ->
            case annotResult of
              Result.Err msgs -> Result.Err msgs
              Result.Ok maybeAnnot ->
                Result.Ok (ElementKeyQual
                  { id = (MaybeE.join (Dict.get keys.id valDict))
                  , name = nameStr
                  , attr = getMiscAttrs attr keyElementAttrs
                  , annotation = maybeAnnot
                  , selector = { id = Nothing -- @todo
                               , xpath = ""
                               , attr = []
                               , annotation = Nothing
                               }
                  , fields = [] -- @todo
                  })

type alias KeyRefElementData =
  { name: NCName
  , refer: QName
  , selector: SelectorElement
  , fields: (List FieldElement)
  }
type alias KeyRefElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin KeyRefElementData))

parseKeyRefElement : (List Attribute) -> (List XmlAst) -> Result (List String) ElementQualifierAst
parseKeyRefElement attr elems =
  let
    annotResult = (getAnnotationElem elems)

    validKeys =
      [ (keys.id, isValidId)
      , (keys.name, isValidNCName)
      , (keys.refer, isValidQName)
      ]

    keyRefElementAttrs = (List.map (\x -> (fst x)) validKeys)
    attrRes = resultDict validKeys attr
  in
    case attrRes of
      Result.Err attrErrList ->
        case annotResult of
          Result.Err msgs -> Result.Err (attrErrList ++ msgs)
          Result.Ok _ -> Result.Err attrErrList
      Result.Ok valDict ->
        case (MaybeE.join (Dict.get keys.name valDict)) of
          Nothing -> Result.Err ["name attribute is required"]
          Just nameStr ->
            case (MaybeE.join (Dict.get keys.refer valDict)) of
              Nothing -> Result.Err ["refer attribute is required"]
              Just referStr ->
                case annotResult of
                  Result.Err msgs -> Result.Err msgs
                  Result.Ok maybeAnnot ->
                    Result.Ok (ElementKeyRefQual
                      { id = (MaybeE.join (Dict.get keys.id valDict))
                      , name = nameStr
                      , refer = referStr
                      , attr = getMiscAttrs attr keyRefElementAttrs
                      , annotation = maybeAnnot
                      , selector = { id = Nothing -- @todo
                                   , xpath = ""
                                   , attr = []
                                   , annotation = Nothing
                                   }
                      , fields = [] -- @todo
                      })

type ElementQualifierAst
  = ElementUniqueQual UniqueElement
  | ElementKeyQual KeyElement
  | ElementKeyRefQual KeyRefElement

type alias ElemDefElementData =
  { deftype: Maybe QName
  , subGroup: Maybe QName
  , default: Maybe String
  , fixed: Maybe String
  , form: Maybe XsdForm
  , nillable: Maybe Bool
  , abstract: Maybe Bool
  , block: Maybe String
  , final: Maybe String
  , elemtype: Maybe ElementTypeAst
  , qualifiers: (List ElementQualifierAst)
  }
type alias ElemDefElement =
  OccurenceMixin (RefMixin (NameMixin (AnnotationMixin (AnyAttributeMixin (IdMixin ElemDefElementData)))))

parseElemDefElement : (List Attribute) -> (List XmlAst) -> Result (List String) ElemDefElement
parseElemDefElement attr elems =
  Result.Err ["Not Implemented Error"]

{- Type Definition Asts (Simple and Complex -}
type ContentChildAst
  = CCExtension ExtensionElement
  | CCRestriction RestrictionElement

type alias ComplexContentElementData =
  { mixed: Maybe Bool
  , content: ContentChildAst
  }
type alias ComplexContentElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin ComplexContentElementData))

parseComplexContentElement : (List Attribute) -> (List XmlAst) -> Result (List String) ComplexContentElement
parseComplexContentElement attr elems =
  Result.Err ["Not Implemented Error"]

type alias SimpleContentElementData =
  { simpleType: Maybe SimpleTypeElement
  , content: ContentChildAst
  }

type alias SimpleContentElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin SimpleContentElementData))

parseSimpleContentElement : (List Attribute) -> (List XmlAst) -> Result (List String) SimpleContentElement
parseSimpleContentElement attr elems =
  Result.Err ["Not Implemented Error"]


{- Collection Ast -}

type MaxOccursType
  = NonNegativeIntOccurs Int
  | UnboundedOccurs

type alias OccurenceMixin rec =
  { rec | maxOccurs: Maybe MaxOccursType
  , minOccurs: Maybe NonNegativeInt
  }

type alias AnyElementData =
  { namespace: Maybe String
  , processContents: Maybe String
  }

type alias AnyElement =
  OccurenceMixin (AnnotationMixin (AnyAttributeMixin (IdMixin AnyElementData)))

anyElementAttrs =
  [ keys.id
  , keys.maxOccurs
  , keys.minOccurs
  , keys.namespace
  , keys.processContents
  ]

parseAnyElement: XmlAst -> Result (List String) AnyElement
parseAnyElement data =
  case data of
    Element n attr elems ->
      let
        validKeys =
          [ (keys.id, isValidId)
          , (keys.maxOccurs, isValidMaxOccurs)
          , (keys.minOccurs, isValidMinOccurs)
          ]
        results = resultDict validKeys attr

      in
        case results of
          Result.Err errList -> Result.Err errList
          Result.Ok valDict ->
            Result.Ok
              { id = (MaybeE.join (Dict.get keys.id valDict))
              , maxOccurs = (maybeToMaxOccurs (MaybeE.join (Dict.get keys.maxOccurs valDict)))
              , minOccurs = (maybeToInt (MaybeE.join (Dict.get keys.minOccurs valDict)))
              , namespace = getAttr keys.namespace attr
              , processContents = getAttr keys.processContents attr
              , attr = getMiscAttrs attr anyElementAttrs
              , annotation = Maybe.Nothing
              }

    _ -> Result.Err ["Invalid Element"]


{- AllElement does not use the OccurenceMixin because it
-- is subtly different in behavior
-}
type alias AllElementData =
  { maxOccurs: Maybe NonNegativeInt
  , minOccurs: Maybe NonNegativeInt
  , content: (List ElemDefElement)
  }
type alias AllElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin AllElementData))

parseAllElement : (List Attribute) -> (List XmlAst) -> Result (List String) AllElement
parseAllElement attr elems =
  Result.Err ["Not Implemented Error"]

type CollectionChildType
  = ColChildElement ElemDefElement
  | ColChildGroup GroupElement
  | ColChildChoice ChoiceElement
  | ColChildSequence SequenceElement
  | ColChildAny AnyElement

type alias SequenceElementData =
  { content: (List CollectionChildType)
  }
type alias SequenceElement =
  OccurenceMixin (AnnotationMixin (AnyAttributeMixin (IdMixin SequenceElementData)))

parseSequenceElement : (List Attribute) -> (List XmlAst) -> Result (List String) CollectionAst
parseSequenceElement attr elems =
  Result.Err ["Not Implemented Error"]

type alias ChoiceElementData =
  { content: (List CollectionChildType)
  }

type alias ChoiceElement =
  OccurenceMixin (AnnotationMixin (AnyAttributeMixin (IdMixin ChoiceElementData)))

parseChoiceElement : (List Attribute) -> (List XmlAst) -> Result (List String) CollectionAst
parseChoiceElement attr elems =
  Result.Err ["Not Implemented Error"]

type SimpleCollectionAst
  = AllSimpleCollection AllElement
  | ChoiceSimpleCollection ChoiceElement
  | SequenceSimpleCollection SequenceElement

type alias GroupElementData =
  { content: Maybe SimpleCollectionAst
  }
type alias GroupElement =
  OccurenceMixin (RefMixin ( NameMixin (AnnotationMixin (AnyAttributeMixin (IdMixin GroupElementData)))))

parseGroupElement : (List Attribute) -> (List XmlAst) -> Result (List String) CollectionAst
parseGroupElement attr elems =
  Result.Err ["Not Implemented Error"]

type CollectionAst
  = GroupCollection GroupElement
  | AllCollection AllElement
  | ChoiceCollection ChoiceElement
  | SequenceCollection SequenceElement

{- Extension Element Definition -}

type alias ExtensionElementData =
  { base: QName
  , collection: Maybe CollectionAst
  , attrdef: List AttributeAst
  , anyattr: Maybe AnyAttributeElement
  }

type alias ExtensionElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin ExtensionElementData))


{- Restriction Element Definitions -}

type WhiteSpaceType
  = WhiteSpacePreserve
  | WhiteSpaceReplace
  | WhiteSpaceCollapse

type RestrictionValType
  = ValFloat Float
  | ValInt Int

type RestrictionTypeAst
  = MinExclusive RestrictionValType
  | MinInclusive RestrictionValType
  | MaxExclusive RestrictionValType
  | MaxInclusive RestrictionValType
  | TotalDigits NonNegativeInt
  | FractionDigits NonNegativeInt
  | TotalLength NonNegativeInt
  | MinLength NonNegativeInt
  | MaxLength NonNegativeInt
  | Emueration String
  | WhiteSpace WhiteSpaceType
  | CharPattern Regex.Regex

type alias SimpleTypeRestrictionElementData =
  { simpleType: Maybe SimpleTypeElement
  , restrictions: (List RestrictionTypeAst)
  }
type alias SimpleTypeRestrictionElement =
  AnnotationMixin SimpleTypeRestrictionElementData

type alias SimpleContentRestrictionElementData =
  { simpleType: Maybe SimpleTypeElement
  , restrictions: (List RestrictionTypeAst)
  , attrdefs: (List AttributeAst)
  , anyattr: Maybe AnyAttributeElement
  }

type alias SimpleContentRestrictionElement =
  AnnotationMixin SimpleContentRestrictionElementData

type alias ComplexContentRestrictionElementData =
  { collection: Maybe CollectionAst
  , attrdefs: (List AttributeAst)
  , anyattr: Maybe AnyAttributeElement
  }

type alias ComplexContentRestrictionElement =
  AnnotationMixin ComplexContentRestrictionElementData

type RestrictionAst
  = SimpleTypeRestriction SimpleTypeRestrictionElement
  | SimpleContentRestriction SimpleContentRestrictionElement
  | ComplexContentRestriction ComplexContentRestrictionElement

type alias RestrictionElementData =
  { base: Maybe QName
  , restriction: RestrictionAst
  }

type alias RestrictionElement =
  AnyAttributeMixin (IdMixin RestrictionElementData)

{- Simple and Complex Type Element Definitions -}

type ListItemType
  = ListTypeAttr QName
  | ListTypeElem SimpleTypeElement

type alias ListElementData = { itemType : ListItemType }
type alias ListElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin ListElementData))

parseListElement : (List Attribute) -> (List XmlAst) -> Result (List String) SimpleTypeAst
parseListElement attr elems =
  Result.Err ["Not Implemented Error"]

type UnionItemType
  = UnionTypeAttr (List QName)
  | UnionTypeElem (List SimpleTypeElement)

type alias UnionElementData = { memberTypes : UnionItemType }
type alias UnionElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin UnionElementData))

parseUnionElement : (List Attribute) -> (List XmlAst) -> Result (List String) SimpleTypeAst
parseUnionElement attr elems =
  Result.Err ["Not Implemented Error"]

type SimpleTypeAst
  = STRestriction RestrictionElement
  | STList ListElement
  | STUnion UnionElement

type alias SimpleTypeElementData =
  { content: SimpleTypeAst
  }
type alias SimpleTypeElement =
  NameMixin (AnnotationMixin (AnyAttributeMixin (IdMixin SimpleTypeElementData)))

parseSimpleTypeElement : (List Attribute) -> (List XmlAst) -> Result (List String) SimpleTypeElement
parseSimpleTypeElement attr elems =
  Result.Err ["Not Implemented Error"]

{-
(annotation?,
     (simpleContent|complexContent|(
           (group|all|choice|sequence)?,(
                  (attribute|attributeGroup)*,anyAttribute?
                  )
           )
     )
)
-}

type alias ComplexTypeCustomData =
  { collection: Maybe CollectionAst
  , attrdefs: (List AttributeAst)
  , anyattr: Maybe AnyAttributeElement
  }

type ComplexTypeContentType
  = CTSimpleContent SimpleContentElement
  | CTComplexContent ComplexContentElement
  | CTCustomContent ComplexTypeCustomData

type alias ComplexTypeElementData =
  { abstract: Maybe Bool
  , mixed: Maybe Bool
  , block: Maybe String
  , final: Maybe String
  , content: ComplexTypeContentType
  }

type alias ComplexTypeElement =
  NameMixin (AnnotationMixin (AnyAttributeMixin (IdMixin ComplexTypeElementData)))

parseComplexTypeElement : (List Attribute) -> (List XmlAst) -> Result (List String) ComplexTypeElement
parseComplexTypeElement attr elems =
  Result.Err ["Not Implemented Error"]

type TypeAst
  = SimpleType SimpleTypeElement
  | ComplexType ComplexTypeElement
  | GroupType GroupElement
  | AttributeGroupType AttributeGroupElement

type alias NotationElementData =
  { name: NCName
  , public: AnyURI
  , system: Maybe AnyURI
  }
type alias NotationElement =
  AnnotationMixin (AnyAttributeMixin (IdMixin NotationElementData))

parseNotationElement : (List Attribute) -> (List XmlAst) -> Result (List String) NotationElement
parseNotationElement attr elems =
  Result.Err ["Not Implemented Error"]

type DefAst
  = TypeDec TypeAst
  | ElementDef ElemDefElement
  | AttributeDef AttributeAst
  | Notation NotationElement

type ContentAst
  = Definition DefAst
  | ContentAnnotation AnnotationElement

type XsdSchemaAst
  = Header (List HeaderAst)
  | Content (List ContentAst)
  | SchemaComment String

type XsdAst
  = XsdRoot (List Attribute) (List XsdSchemaAst)
  | XsdComment String
