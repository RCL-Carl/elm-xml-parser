module XsdTypes exposing (..)

import Test exposing (..)
import Expect
import String
import Result
import Maybe
import Xml.XsdTypes exposing (..)
import Xml.Types exposing (..)

allTests : Test
allTests =
  describe "Set of Test Suites for the Xsd Types"
    [ annotElemTest
    , anyElemTest
    , docElemTest
    , appinfoElemTest
    , includeElemTest
    , importElemTest
    , redefineElemTest
    , fieldElementTest
    , selectorElementTest
    ]

expectError : Result a b -> Expect.Expectation
expectError res =
  case res of
    Result.Ok t -> Expect.fail "Expected an Error Respone - Received OK"
    Result.Err s -> Expect.pass

annotElemTest : Test
annotElemTest =
  describe "Test Suite for the AnnotationElement Parsing"
    [
     test "Minimal Annot Element" <|
       \() ->
         let
           annotElem = parseAnnotationElement [] []
           result = Result.Ok { id = Maybe.Nothing
                              , attr = []
                              , content = []
                              }
         in
           Expect.equal annotElem result
    , test "Attributed Annot Element" <|
       \() ->
         let
           annotElem = parseAnnotationElement [("id", "asdf"), ("misc", "qwer")] []
           result = Result.Ok { id = Just "asdf"
                              , attr = [("misc", "qwer")]
                              , content = []
                              }
         in
           Expect.equal annotElem result
    , test "Annot Element with Content" <|
      \() ->
        let
          url = "http://www.google.com"
          msg = "Some Content Message"
          content = [
           (Comment "This is a comment"),
           (Element "appinfo" [("source", url)] []),
           (Element "documentation" [] [(Body msg)])
             ]
          elem = parseAnnotationElement [("id", "asdf")] content
          result = Result.Ok { id = Just "asdf"
                              , attr = []
                              , content = [
                                  (AnnotComment "This is a comment"),
                                  (AppInfo { source = Just url, content=[] }),
                                  (Documentation {source = Nothing, xmllang= Nothing, content = [(Body msg)] })
                                    ]
                              }
        in
          Expect.equal elem result
    ]

anyElemTest : Test
anyElemTest =
  describe "Test Suite for the AnyElement parsing"
    [
     test "Minimal Any Element" <|
       \() ->
         let
           eut = Element "any" [] []
           anyElem = parseAnyElement eut
           result = Result.Ok { id = Maybe.Nothing
                              , attr = []
                              , maxOccurs = Maybe.Nothing
                              , minOccurs = Maybe.Nothing
                              , namespace = Maybe.Nothing
                              , processContents = Maybe.Nothing
                              , annotation = Maybe.Nothing
                              }
         in
           Expect.equal anyElem result
    , test "Empty with Attributes" <|
      \() ->
        let
          eut = Element "any" [("id", "asdf"), ("minOccurs", "1")] []
          anyElem = parseAnyElement eut
          result = Result.Ok { id = Just "asdf"
                             , attr = []
                             , maxOccurs = Maybe.Nothing
                             , minOccurs = Just 1
                             , namespace = Maybe.Nothing
                             , processContents = Maybe.Nothing
                             , annotation = Maybe.Nothing
                             }
        in
          Expect.equal anyElem result
    ]


docElemTest : Test
docElemTest =
  describe "Test Suite for the DocumentationElement Parsing"
    [
     test "Minimal Doc Element" <|
       \() ->
         let
           docElem = parseDocElement [] []
           result = Result.Ok (Documentation { source = Nothing
                              , xmllang = Nothing
                              , content = []
                              })
         in
           Expect.equal docElem result
    , test "Attributed Doc Element" <|
      \() ->
        let
          url = "http://werwer.com"
          elem = parseDocElement [("source", url), ("xml:lang", "en")] []
          result = Result.Ok (Documentation { source = Just url
                             , xmllang = Just "en"
                             , content = []
                             })
        in
          Expect.equal elem result
    , test "Full Doc Element" <|
      \() ->
        let
          url = "https://www.slashdot.com/news"
          elems = [
           (Element "div" [] []),
           (Comment "asdf")
             ]
          elem = parseDocElement [("source",url), ("xml:lang", "de")] elems
          result = Result.Ok (Documentation { source = Just url
                             , xmllang = Just "de"
                             , content = elems
                             })
        in
          Expect.equal elem result
    , test "Invalid Misc Attributes Doc Element" <|
      \() ->
        let
          url = "https://www.dsdfe.doc/erwd/were/sdfwe/#ser"
          elem = parseDocElement [("source",url), ("something", "else")] []
        in
          expectError elem
    , test "Bad URL Attribute Doc Element" <|
      \() ->
        let
          url = "https//asdfwer"
          elem = parseDocElement [("source",url)] []
        in
          expectError elem
    ]

appinfoElemTest : Test
appinfoElemTest =
  describe "Test Suite for the AppInfo Element Parsing"
    [
     test "Minimal AppInfo Element" <|
       \() ->
         let
           elem = parseAppInfoElement [] []
           result = Result.Ok (AppInfo { source = Nothing
                              , content = []
                              })
         in
           Expect.equal elem result
    , test "Attributed Source 1 AppInfo Element" <|
       \() ->
         let
           elem = parseAppInfoElement [("source", "http://www.google.com")] []
           result = Result.Ok (AppInfo { source = Just "http://www.google.com"
                              , content = []
                              })
         in
           Expect.equal elem result
    , test "Attributed Source 2 AppInfo Element" <|
       \() ->
         let
           url = "http://www.example.com/asdf#iewie"
           elem = parseAppInfoElement [("source", url)] []
           result = Result.Ok (AppInfo { source = Just url
                              , content = []
                              })
         in
           Expect.equal elem result
    , test "AppInfo with Content" <|
      \() ->
        let
           url = "http://www.example.com/asdf"
           elems = [
            (Element "p" [] []),
            (Element "a" [("href","http://www.google.com")] [])
              ]
           elem = parseAppInfoElement [("source", url)] elems
           result = Result.Ok (AppInfo { source = Just url
                              , content =elems
                              })
         in
           Expect.equal elem result
    , test "Bad URL Source AppInfo Element" <|
      \() ->
        let
          elem = parseAppInfoElement [("source", "234-sd3234-fee")] []
        in
          expectError elem
    , test "Invalid Misc Attr AppInfo Element" <|
      \() ->
        let
          elem = parseAppInfoElement [("asdf", "werwe")] []
        in
          expectError elem
    ]


includeElemTest : Test
includeElemTest =
  describe "Test Suite for Include Element Parsing"
    [ test "Minimal Include Test" <|
       \() ->
         let
           url = "http://www.reddit.com/r/news"
           elem = parseIncludeElement [("schemaLoc", url)] []
           result =
             Result.Ok
               (Include { id = Nothing
                        , attr = []
                        , annotation = Nothing
                        , schemaLoc = url })
         in
           Expect.equal elem result
        , test "Attributed Include Element" <|
          \() ->
            let
              url = "http://www.reddit.com/r/news"
              attr = [ ("schemaLoc", url), ("id", "kgkjfk"), ("sim", "som")]
              elems = [ (Element "annotation" [] []) ]
              elem = parseIncludeElement attr elems
              result = Result.Ok
                       (Include { id = Just "kgkjfk"
                                , schemaLoc = url
                                , annotation = Just { id = Maybe.Nothing
                                                    , attr = []
                                                    , content = []
                                                    }
                                , attr = [("sim", "som")]
                                })
            in
              Expect.equal elem result
        , test "Invalid SchemaLoc Attr Test" <|
          \() ->
            let
              elem = parseIncludeElement [] []
            in
              expectError elem
    ]

importElemTest : Test
importElemTest =
  describe "Test Suite for the Import Element"
    [ test "Minimal Import Element" <|
        \() ->
          let
            elem = parseImportElement [] []
            result = Result.Ok (Import { id = Nothing
                               , schemaLoc = Nothing
                               , namespace = Nothing
                               , attr = []
                               , annotation = Nothing
                               })
          in
            Expect.equal elem result
    , test "Attributed Import Element" <|
      \() ->
        let
          url1 = "http://sdfsdf.ecom/were"
          url2 = "https://erwerd.dsd/we#sdf"
          attr = [ ("id", "ire3")
                 , ("namespace", url1)
                 , ("schemaLoc", url2)
                 , ("garbage", "value")
                 , ("Laji", "dian nao")
                 ]
          elems = [
           (Element "annotation" [] [])
          ]
          elem = parseImportElement attr elems
          result = Result.Ok
                   (Import { id = Just "ire3"
                           , namespace = Just url1
                           , schemaLoc = Just url2
                           , attr = [ ("garbage", "value")
                                    , ("Laji", "dian nao")
                                    ]
                           , annotation = Just { id = Maybe.Nothing
                                               , attr = []
                                               , content = []
                                               }
                           })
        in
          Expect.equal elem result
    ]

redefineElemTest : Test
redefineElemTest =
  describe "Test Suite for the Redefine Element"
    [  test "asdf" <| \() -> Expect.equal 1 1
    , test "Minimal Redefine Element" <|
       \() ->
         let
           url = "https://qqwer.qwer/werer"
           elem = parseRedefineElement [("schemaLoc", url)] []
           result = Result.Ok (Redefine { id = Nothing
                              , schemaLoc = url
                              , content = []
                              , attr = []
                              })
         in
           Expect.equal elem result
    , test "Attributed Redefine Element" <|
      \() ->
        let
          url = "ftp://werwesdf.wer/erwererwer#11"
          attr = [ ("schemaLoc", url)
                 , ("id", "qwer")
                 , ("asdf", "rewq")
                 ]
          elem = parseRedefineElement attr []
          result = Result.Ok
                   (Redefine
                      { id = Just "qwer"
                      , attr = [("asdf", "rewq")]
                      , schemaLoc = url
                      , content = []
                      })
        in
          Expect.equal elem result
    , test "Invalid SchemaLoc Attr Test" <|
      \() ->
        let
          elem = parseRedefineElement [] []
        in
          expectError elem

    ]

fieldElementTest : Test
fieldElementTest =
  describe "Test Suite for the Field Element"
    [ test "Minimal Field Element" <|
      \() ->
        let
          xpath = "/bookstore"
          elem = parseFieldElement [("xpath", xpath)] []
          result = Result.Ok { id = Nothing
                             , attr = []
                             , xpath = xpath
                             , annotation = Nothing
                             }
        in
          Expect.equal elem result
    , test "Missing XPath Test" <|
      \() ->
        let
          elem = parseFieldElement [] []
        in
          expectError elem
    ]

selectorElementTest : Test
selectorElementTest =
  describe "Test Suite for the Selector Element"
    [ test "Minimal Selector Element" <|
      \() ->
        let
          xpath = "/vinyl"
          elem = parseSelectorElement [("xpath", xpath)] []
          result = Result.Ok { id = Nothing
                             , xpath = xpath
                             , attr = []
                             , annotation = Nothing
                             }
        in
          Expect.equal elem result
    , test "Missing XPath Test" <|
      \() ->
        let
          elem = parseSelectorElement [] []
        in
          expectError elem
    ]
