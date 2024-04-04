{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Language.Thrift.ParserSpec (spec) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Control.Monad
import Data.Text       (pack)
import Test.Hspec
import Text.Megaparsec (SourcePos, eof, skipMany)
import Text.Megaparsec.Char (spaceChar)

import TestUtils

import qualified Language.Thrift.AST    as T
import qualified Language.Thrift.Parser as P

spec :: Spec
spec = describe "Parser" $ do

    it "can parse empty documents" $
        parseSuccessCases P.program $ map (, T.Program [] [])
            [ ""
            , "// line comment at EOF"
            , "# line comment at EOF\n"
            , "/** docstring at EOF */"
            , "/** docstring with space\nand newline at EOF\n\t\t*/ "
            , "/* comment\n\tat eof\n\t */"
            ]

    it "can parse docstrings" $ forM_
        [ ( return "/** foo */", "foo" )
        , ( return "/** foo\n */", "foo" )
        , ( readFile "test/data/docstring-1.txt"
          , "Hello. This is the first\n" ++
            "paragraph.\n\n" ++
            "    This is some text indented 4 spaces,\n" ++
            "    spread across multiple lines.\n\n" ++
            "Back."
          )
        , ( readFile "test/data/docstring-2.txt"
          , "The docstring itself is indented 4 spaces.\n\n" ++
            "And has a missing * in between."
          )
        ] $ \(i, o) -> do
            input <- i
            assertParses (skipMany spaceChar *> P.docstring) (pack o) input

    it "can parse basic constants" $ parseSuccessCases P.constantValue
        [ ("42", T.ConstInt 42 ())
        , ("0x2a", T.ConstInt 42 ())
        , ("1.2", T.ConstFloat 1.2 ())
        , ("foo", T.ConstIdentifier "foo" ())
        , ("\"foo\"", T.ConstLiteral "foo" ())
        ]

    testParseFailure

testParseFailure :: Spec
testParseFailure = do

    it "cannot parse invalid documents" $
      parseFailureCases P.program
        [ "namespace foo"
        , "const i32 100 = \"foo\""
        , "include \"foo"
        , "enum { }"
        , "struct { }"
        , "service A extends B"
        , "service A extends {}"
        ]

    it "cannot parse invalid type references" $
      parseFailureCases (P.typeReference <* eof)
        [ "list<i32"
        , "set<string, string>"
        , "map<i64>"
        , "something_else<foo>"
        ]

    it "cannot parse reserved keywords in names" $
        parseFailureCases P.program
            [ "enum struct {}"
            , "strut Foo { 1: required string service }"
            ]


parseFailureCases :: Show a => Parser a -> [String] -> Expectation
parseFailureCases p = mapM_ (p `shouldNotParse`)

parseSuccessCases
    :: (Show (a ()), Eq (a ()), Functor a)
    => Parser (a SourcePos) -> [(String, a ())] -> Expectation
parseSuccessCases parser = mapM_ $ \(input, value) ->
    assertParses (void `fmap` parser) value input
