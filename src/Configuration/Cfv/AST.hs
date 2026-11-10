{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : AST
Description : Abstract syntax
Copyright   : (c) Harley Eades, 2026
Maintainer  : harley.eades@gmail.com

The abstract syntax for CFV files.
-}
module Configuration.Cfv.AST  
    (VarName
    ,VarValue
    ,Binding
    ,Var(..)
    ,VarAttr(..)
    ,varAttr
    ,revealVar) where
import Data.Text (Text)

-- | Security level for variables.
data VarAttr
  = Public  -- ^ The default; has no syntax.
  | Secret  -- ^ Declared with a @secret@ prefix.
  deriving (Eq, Ord, Show)

-- | A variables value.
type VarValue = Text

-- | A variable's final value (after any command has been run) together with its
-- attribute.
--
-- The 'Show' instance never prints a secret value, so a list of bindings can
-- be logged safely:
--
-- > [("name",Public "x"),("key",Secret ())]
--
data Var 
  = Var VarAttr VarValue  
  deriving (Eq)

instance Show Var where
  showsPrec :: Int -> Var -> ShowS
  showsPrec d (Var Public v) =
    showParen (d > 10) (showString "Public " . showsPrec 11 v)
  showsPrec _ (Var Secret _) = showString "Secret ()"

-- | The variable's attribute.
varAttr :: Var -> VarAttr
varAttr (Var a _) = a

-- | The variable's value, secret or not.
revealVar :: Var -> Text
revealVar (Var _ v) = v

-- | A variables name.
type VarName = Text

-- | A variable binding: a name, attribute, and a value.
type Binding = (VarName, Var)

