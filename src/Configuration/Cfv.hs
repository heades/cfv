{-|
Module      : Parser
Description : The CFV Parser
Copyright   : (c) Harley Eades, 2026
Maintainer  : harley.eades@gmail.com

Parser and evaluator for @.cfv@ ("Configuration Variables") files.

A file is a sequence of declarations. Whitespace (including newlines) and
Haskell-style @--@ line comments are allowed between any two tokens and are
ignored.

> -- a comment
> greeting = 'hello';          -- trailing comment
> empty    = "";
> now      = `date +%s`       -- semicolon optional after a command
> home'    = `echo "$HOME"`;
> secret api_key = `pass show service/api-key`

* @name = '...';@ or @name = "...";@ declares a string. The escapes
  @\\\\ \\' \\" \\n \\t \\r \\0@ are recognised; raw newlines are allowed.

* @name = \`cmd\`@ runs @cmd@ in the user's shell and uses its standard
  output as the value. Like shell command substitution, trailing newlines
  are stripped. @cmd@ is everything up to the next backtick (there is no
  escape for a backtick; use @$(...)@ for nested substitution). The command
  runs in the current working directory with the calling process's
  environment and a closed standard input.

* A declaration may be prefixed with @secret@ (@secret name = ...@) to mark
  its value as a secret. Everything else is 'Public', which is the default
  and has no keyword. @secret@ is only a keyword when another name follows
  it, so @secret = 'x';@ still declares a public variable called @secret@.

__Secrets:__ the result is an association list @[(String, 'Var')]@. The
'Show' instance of 'Var' prints a 'Secret' value as @\<redacted\>@, so
logging or @show@ing the whole list (or a single @lookup@ result) does not
leak it. The value is only available through 'revealVar'. 'Declaration's
are redacted the same way, and so are errors: a failing 'Secret' command
reports neither the command text nor its output, an invalid escape inside a
secret string does not name the character, and syntax errors never quote
source text from a file that mentions @secret@ (see 'syntaxSourceLine').

__Security:__ evaluating a file runs arbitrary shell commands. Use
'readCfvFile' or 'parseCfv' to inspect a file without running anything.

Errors are reported as plain Haskell data ('CfvError'), never as
pre-formatted strings; 'renderCfvError' is there if a string is all you want.
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
  ) where

import Configuration.Cfv.AST
import Configuration.Cfv.Shell
import Configuration.Cfv.Parser