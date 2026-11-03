{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : AST
Description : Abstract syntax
Copyright   : (c) Harley Eades, 2026
Maintainer  : harley.eades@gmail.com

-}
module Configuration.Cfv.AST  
    (Binding
    ,Var(..)
    ,VarAttr(..)
    ,varAttr
    ,revealVar) where
import MAC.Labeled (Labeled, Id (unId), labelT)
import MAC.Lattice (L, H, Less)
import MAC.MAC (MACT, Res)
import MAC.Effects (readdownT, createT, writeupT)
import Control.Monad.State (State)

-- | Whether a variable's value is sensitive.
data VarAttr
  = Public  -- ^ The default; has no syntax.
  | Secret  -- ^ Declared with a @secret@ prefix.
  deriving (Eq, Ord, Show)

-- | A variable's final value (after any command has been run) together with
-- its attribute.
--
-- The 'Show' instance never prints a 'Secret' value, so a list of bindings can
-- be logged safely:
--
-- > [("name",Public "x"),("key",Secret <redacted>)]
--
-- There is deliberately no 'Read' instance. Use 'revealVar' to get at the
-- value.

type VarValue = String

data Var 
  = Var VarAttr VarValue  
  deriving (Eq)

instance Show Var where
  showsPrec :: Int -> Var -> ShowS
  showsPrec d (Var Public v) =
    showParen (d > 10) (showString "Public " . showsPrec 11 v)
  showsPrec d (Var Secret _) = showString "Secret ()"

-- | The variable's attribute.
varAttr :: Var -> VarAttr
varAttr (Var a _) = a

-- | The variable's value, secret or not.
revealVar :: Var -> String
revealVar (Var _ v) = v

type VarName = String
type Binding = (VarName, VarValue)

type CfgVars = (Res H [Binding],Res L [Binding])

publicCfgVars :: CfgVars -> Res L [Binding]
publicCfgVars = snd

secretCfvVars :: CfgVars -> Res H [Binding]
secretCfvVars = fst

readVar :: Less l l 
        => VarName 
        -> Res l [(VarName,VarValue)]
        -> MACT l Maybe (Labeled l VarValue)
readVar name store 
  = readdownT (lookup name . fmap (\v' -> (name,v'))) store
      >>= labelT . snd

printVar :: VarName -> Res L [Binding] -> String -> MACT L IO ()
printVar name store errMsg = writeupT (putStrLn . msg) store
  where
    msg s = 
      let value_ = lookup name s
       in case value_ of
            Nothing -> errMsg
            Just v -> name <> " = " <> v <> ";"
