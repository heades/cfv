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
    (VarName
    ,VarValue
    ,Binding
    ,Var(..)
    ,VarAttr(..)
    ,varAttr
    ,revealVar) where

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
type Binding = (VarName, Var)

