{-|
Module      : CfvSpec
Description : Spec for Cfv
Copyright   : (c) Harley Eades, 2026
Maintainer  : harley.eades@gmail.com

-}
module CfvSpec (spec) where

import Test.Hspec ( Spec, runIO, shouldBe, it, SpecWith, describe )
import Configuration.Cfv (parseCfvFile)
import Data.Either (isRight)

spec :: Spec
spec = do 
    describe "Parser Tests:" $ do
        testParseBasicCfv

testParseBasicCfv :: SpecWith ()
testParseBasicCfv = do
    r <- runIO $ parseCfvFile "test/files/basic.cfv"
    runIO $ putStrLn . show $ r
    it "" $ (isRight r) `shouldBe` True
