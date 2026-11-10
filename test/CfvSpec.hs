{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : CfvSpec
Description : Spec for Cfv
Copyright   : (c) Harley Eades, 2026
Maintainer  : harley.eades@gmail.com

-}
module CfvSpec (spec) where

import Test.Hspec ( Spec, runIO, shouldBe, it, SpecWith, describe )

import Configuration.Cfv.AST ( Var(..), VarAttr(..) )
import Configuration.Cfv.Parser
    ( parseCfvFile,
      parseEvalCfv,
      CfvError(FileSyntaxError),
      SyntaxError(..),
      Position(..),
      ErrorKind(..),
      Item(..), renderCfvError )

import System.IO (stderr)
import Data.Text.IO (hPutStrLn)

spec :: Spec
spec = do 
    describe "Parser Tests:" $ do
        testParseBasicCfv
        testParseNoSemiColon
        testParseNoSemiColonMultiline
        testParseSecret
    describe "Secret Tests:" $ do
        testShowSecret

testParseBasicCfv :: SpecWith ()
testParseBasicCfv = do
    r <- runIO $ parseCfvFile "test/files/basic.cfv"    
    either 
        (\err -> do runIO . hPutStrLn stderr . renderCfvError $ err
                    it "parseBasicCfv" False) 
        (\_ -> it "parseBasicCfv" True) 
        r    

testParseNoSemiColon :: SpecWith ()
testParseNoSemiColon = do
    let file = "test/files/noSemiColon.cfv" 
    let testConfig = "exampleVar = 'example value'"
    r <- runIO $ parseEvalCfv file testConfig
    it "parseNoSemiColon" $ r `shouldBe` (Left . FileSyntaxError $ SyntaxError 
        { syntaxErrorPosition = (Position 
                                { posFile = file
                                , posLine = 1
                                , posColumn = 29
                                , posOffset = 28 })
        , syntaxErrorKind = UnexpectedInput (Just EndOfFile) [Literal ";"]
        , syntaxErrorSourceLine = Just testConfig
        })

testParseNoSemiColonMultiline :: SpecWith ()
testParseNoSemiColonMultiline = do
    let file = "test/files/noSemiColonMultiLine.cfv" 
    let testConfig = "exampleVar1 = 'example value 1';\nexampleVar2 = 'example value 2'\nexampleVar3 = 'example value 3';"
    r <- runIO $ parseEvalCfv file testConfig    
    it "parseNoSemiColonMultiLine" $ r `shouldBe` (Left . FileSyntaxError $ SyntaxError 
        { syntaxErrorPosition = (Position 
                                { posFile = file
                                , posLine = 3
                                , posColumn = 1
                                , posOffset = 65 })
        , syntaxErrorKind = UnexpectedInput (Just (Literal "e")) [Literal ";"]
        , syntaxErrorSourceLine = Just "exampleVar3 = 'example value 3';"
        })

testParseSecret :: SpecWith ()
testParseSecret = do
    let file = "test/files/secret.cfv" 
    let testConfig = "secret exampleVar = \"secret value\";"
    r <- runIO $ parseEvalCfv file testConfig    
    it "testParseSecret" $ r `shouldBe` Right [("exampleVar", Var Secret "secret value")]

testShowSecret :: SpecWith ()
testShowSecret = do
    let file = "test/files/secret.cfv" 
    let testConfig = "secret exampleVar = \"secret value\";"
    r <- runIO $ parseEvalCfv file testConfig    
    it "testParseSecret" $ do 
        r `shouldBe` Right [("exampleVar", Var Secret "secret value")]
        let (Right r') = r
        (show r') `shouldBe` ("[(\"exampleVar\",Secret ())]")
