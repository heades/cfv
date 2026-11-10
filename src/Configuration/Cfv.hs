{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Safe #-}
{-|
Module      : Cfv
Description : The CFV API
Copyright   : (c) Harley Eades, 2026
Maintainer  : harley.eades@gmail.com

A CFV file is a sequence of configuration variable declarations. 

A valid variable declarations consist of one of the following:

* Strings: @name = \'...\';@ or @name = "...";@ declares a string. The escapes
  @\\\\, \\', \\", \\n, \\t, \\r, \\0@ are recognized; raw newlines are allowed.

* Commands: @name = \`cmd\`@ runs @cmd@ in the user's shell and uses its
  stdout as the value. Like shell command substitution, trailing
  newlines are stripped. @cmd@ is everything up to the next backtick (there is
  no escape for a backtick; use @$(...)@ for nested substitution). The command
  runs in the current working directory with the calling process's environment
  and a closed standard input. 

* Secrets: A declaration may be prefixed with @secret@ (@secret name = ...@) to
  mark its value as a secret. When no @secret@ attribute labels a declaration it
  is assumed to be non-sensitive (public). Note that @secret@ is only a keyword
  when a variable name follows it, so @secret = \'x\';@ still declares a public
  variable called @secret@.

Parsing a CFV file returns a list of t'Binding' which is simply a pair
@('VarName', t'Var')@ of a variable name ( t'String' ) and a variable ( t'Var' ). The latter
labels a variable's value with an attribute which currently consist of 'Public'
and 'Secret'. After parsing you are free to use these variable declarations in
any way you wish. 

This API does place one restriction on the declarations attributed
with @sercret@. The default t'Show' instance does not convert a secret variable's
value to a string, but simply translates it to @Secret ()@. This makes printing
the contents of a CFV file to a log or the screen safe.
-}
module Configuration.Cfv
  ( -- * Parsing and evaluating
    Binding
  , Var (..)
  , varAttr
  , revealVar
  , VarAttr (..)
  , parseCfvFile
  , parseCfvFileWith
  , parseEvalCfv
  , parseEvalCfvWith
    -- * Shell configuration
  , ShellConfig (..)
  , defaultShellConfig
    -- * Errors
  , CfvError (..)
  , SyntaxError (..)
  , ErrorKind (..)
  , Item (..)
  , Position (..)
  , CommandError (..)
  , CommandProblem (..)
  , renderCfvError
  ) where

import Configuration.Cfv.AST
import Configuration.Cfv.Shell
import Configuration.Cfv.Parser

