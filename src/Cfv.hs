{-|
Module      : Cfv
Description : Configuration Variables 
Copyright   : (c) Harley Eades, 2026
Maintainer  : harley.eades@gmail.com

-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parser and evaluator for @.cfv@ ("Configuration Variables") files.
--
-- A file is a sequence of declarations. Whitespace (including newlines) and
-- Haskell-style @--@ line comments are allowed between any two tokens and are
-- ignored.
--
-- > -- a comment
-- > greeting = 'hello';          -- trailing comment
-- > empty    = "";
-- > now      = `date +%s`       -- semicolon optional after a command
-- > home'    = `echo "$HOME"`;
--
-- * @name = '...';@ or @name = "...";@ declares a string. The escapes
--   @\\\\ \\' \\" \\n \\t \\r \\0@ are recognised; raw newlines are allowed.
--
-- * @name = \`cmd\`@ runs @cmd@ in the user's shell and uses its standard
--   output as the value. Like shell command substitution, trailing newlines
--   are stripped. @cmd@ is everything up to the next backtick (there is no
--   escape for a backtick; use @$(...)@ for nested substitution). The command
--   runs in the current working directory with the calling process's
--   environment and a closed standard input.
--
-- __Security:__ evaluating a file runs arbitrary shell commands. Use
-- 'readCfvFile' or 'parseCfv' to inspect a file without running anything.
--
-- Errors are reported as plain Haskell data ('CfvError'), never as
-- pre-formatted strings; 'renderCfvError' is there if a string is all you want.
--
-- Dependencies: @base@, @bytestring@, @containers@, @filepath@, @process@,
-- @text@, @megaparsec >= 9@.
module Cfv
  ( -- * Parsing and evaluating
    Binding
  , parseCfvFile
  , parseCfvFileWith

    -- * Individual steps
  , readCfvFile
  , parseCfv
  , runDeclarations

    -- * Syntax tree
  , Declaration (..)
  , Value (..)

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

    -- * Rendering errors
  , renderCfvError
  , renderSyntaxError
  , renderCommandError
  ) where

import Control.Concurrent (forkIO, MVar, takeMVar, newEmptyMVar, putMVar)
import qualified Control.Exception as E
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isLower)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (UnicodeException, lenientDecode)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension)
import System.IO (Handle, hClose)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , waitForProcess
  )
import Text.Megaparsec
    ( ParseErrorBundle(bundlePosState, bundleErrors),
      Parsec,
      SourcePos(sourceColumn, sourceName, sourceLine),
      PosState(pstateSourcePos),
      ParseError(FancyError, TrivialError),
      TraversableStream(reachOffset),
      ErrorItem(..),
      ErrorFancy(..),
      MonadParsec(eof, parseError, try, notFollowedBy, takeWhileP),
      (<|>),
      empty,
      parse,
      unPos,
      errorOffset,
      getOffset,
      getSourcePos,
      satisfy,
      (<?>),
      many,
      optional,
      anySingle )
import Text.Megaparsec.Char ( char, space1, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void, when)

------------------------------------------------------------------------------
-- Public types

-- | A variable and its final value (after any command has been run).
type Binding = (String, String)

-- | One @name = value@ declaration, as written in the file.
data Declaration = Declaration
  { declName     :: String
  , declValue    :: Value
  , declPosition :: Position
    -- ^ Where the declaration starts (its variable name).
  } deriving (Eq, Show)

-- | The right-hand side of a declaration.
data Value
  = StringValue String   -- ^ A quoted string, already unescaped.
  | CommandValue String  -- ^ The text between backticks, unmodified.
  deriving (Eq, Show)

-- | How commands are run.
data ShellConfig = ShellConfig
  { shellProgram :: Maybe FilePath
    -- ^ The shell to use. 'Nothing' means the @SHELL@ environment variable.
  , shellFlags   :: [String]
    -- ^ Arguments placed before the command text, which is passed last.
  } deriving (Eq, Show)

-- | Use @$SHELL@ as a login shell: @$SHELL -l -c cmd@. A login shell picks up
-- the user's profile (@PATH@ additions and so on). Add @"-i"@ to the flags if
-- you also need what the shell's interactive rc file sets up (aliases,
-- functions), at the cost of rc-file chatter possibly ending up in the output.
defaultShellConfig :: ShellConfig
defaultShellConfig = ShellConfig {shellProgram = Nothing, shellFlags = ["-l", "-c"]}

-- | Everything that can go wrong when processing a file.
data CfvError
  = InvalidExtension FilePath
    -- ^ The file name does not end in @.cfv@ (checked before anything is read).
  | FileReadError FilePath E.IOException
    -- ^ The file could not be opened or read.
  | FileDecodeError FilePath UnicodeException
    -- ^ The file's contents are not valid UTF-8.
  | FileSyntaxError SyntaxError
    -- ^ The contents do not parse.
  | FileCommandError CommandError
    -- ^ A backtick command could not be run, or failed. (Only produced by
    -- functions that run commands.)
  deriving (Eq, Show)

-- | A problem with the text itself, with a location.
data SyntaxError = SyntaxError
  { syntaxPosition   :: Position
  , syntaxKind       :: ErrorKind
  , syntaxSourceLine :: Maybe String
    -- ^ The source line containing 'syntaxPosition', when available.
  } deriving (Eq, Show)

-- | The specific kind of syntax error.
data ErrorKind
  = UnexpectedInput (Maybe Item) [Item]
    -- ^ Found this (if known), but expected one of these.
  | ReservedWord String
    -- ^ A Haskell keyword (or a lone @_@) was used as a variable name.
    -- Position: start of the word.
  | InvalidEscape Char
    -- ^ A backslash followed by a character that is not a known escape.
    -- Position: the backslash.
  | UnterminatedString Char
    -- ^ A string literal with no closing quote (carries the quote character).
    -- Position: the opening quote.
  | UnterminatedCommand
    -- ^ A backtick command with no closing backtick.
    -- Position: the opening backtick.
  | OtherFailure String
    -- ^ Anything else; should not occur with the current grammar.
  deriving (Eq, Show)

-- | Something the parser found or expected.
data Item
  = Literal String      -- ^ Specific text, such as @";"@.
  | Description String  -- ^ A named construct, such as @"variable name"@.
  | EndOfFile
  deriving (Eq, Ord, Show)

-- | A location in the source. Line and column are 1-based; the offset is the
-- 0-based character offset from the start of the input.
data Position = Position
  { posFile   :: FilePath
  , posLine   :: Int
  , posColumn :: Int
  , posOffset :: Int
  } deriving (Eq, Show)

-- | A backtick command that could not produce a value.
data CommandError = CommandError
  { cmdVariable :: String
  , cmdCommand  :: String
  , cmdPosition :: Position
  , cmdProblem  :: CommandProblem
  } deriving (Eq, Show)

data CommandProblem
  = NoShell
    -- ^ No shell configured and @SHELL@ is unset or empty.
  | ShellLaunchFailed FilePath E.IOException
    -- ^ The shell could not be started (not found, not executable, ...).
  | NonZeroExit Int String
    -- ^ The command exited with this status (negative: killed by that signal)
    -- and this standard error output, trimmed.
  | OutputNotUtf8 UnicodeException
    -- ^ The command's standard output is not valid UTF-8.
  deriving (Eq, Show)

instance E.Exception CfvError where
  displayException = renderCfvError

instance E.Exception SyntaxError where
  displayException = renderSyntaxError

------------------------------------------------------------------------------
-- Public API

-- | Read, parse and evaluate a @.cfv@ file using 'defaultShellConfig'.
-- Returns the variables in source order; a variable declared more than once
-- appears more than once (so 'lookup' finds the first declaration). Commands
-- run one at a time in file order, and the first failure aborts the whole
-- operation.
--
-- Never throws for extension, read, decode, parse or command problems; they are
-- all returned as 'CfvError'. (To throw instead:
-- @either E.throwIO pure =<< parseCfvFile p@.)
parseCfvFile :: FilePath -> IO (Either CfvError [Binding])
parseCfvFile = parseCfvFileWith defaultShellConfig

-- | Like 'parseCfvFile' with an explicit shell configuration.
parseCfvFileWith :: ShellConfig -> FilePath -> IO (Either CfvError [Binding])
parseCfvFileWith cfg path = do
  parsed <- readCfvFile path
  case parsed of
    Left err    -> pure (Left err)
    Right decls -> first FileCommandError <$> runDeclarations cfg decls

-- | Check the extension, read and parse a file, without running any command.
-- Never returns 'FileCommandError'.
readCfvFile :: FilePath -> IO (Either CfvError [Declaration])
readCfvFile path
  | takeExtension path /= ".cfv" = pure (Left (InvalidExtension path))
  | otherwise = do
      result <- E.try (BS.readFile path)
      pure $ case result of
        Left e -> Left (FileReadError path e)
        Right bytes -> case TE.decodeUtf8' bytes of
          Left e    -> Left (FileDecodeError path e)
          Right txt -> first FileSyntaxError (parseCfv path txt)

-- | Pure parser; runs nothing. The 'FilePath' is only used to label error
-- positions and is not checked for an extension.
parseCfv :: FilePath -> Text -> Either SyntaxError [Declaration]
parseCfv name input = first fromBundle (parse program name input)

-- | Turn declarations into bindings, running each 'CommandValue' in the user's
-- shell. The shell is only looked up if there is at least one command.
runDeclarations :: ShellConfig -> [Declaration] -> IO (Either CommandError [Binding])
runDeclarations cfg = go []
  where
    go acc [] = pure (Right (reverse acc))
    go acc (d : ds) = do
      result <- evalValue cfg d
      case result of
        Left err -> pure (Left err)
        Right v  -> go ((declName d, v) : acc) ds

------------------------------------------------------------------------------
-- Running commands

evalValue :: ShellConfig -> Declaration -> IO (Either CommandError String)
evalValue _ (Declaration _ (StringValue s) _) = pure (Right s)
evalValue cfg (Declaration name (CommandValue cmd) pos) = do
  let failWith = Left . CommandError name cmd pos
  mShell <- resolveShell cfg
  case mShell of
    Nothing -> pure (failWith NoShell)
    Just shell -> do
      result <- E.try (execShell shell (shellFlags cfg) cmd)
      pure $ case result of
        Left e -> failWith (ShellLaunchFailed shell e)
        Right (ExitFailure n, _, err) ->
          failWith (NonZeroExit n (T.unpack (T.strip (TE.decodeUtf8With lenientDecode err))))
        Right (ExitSuccess, out, _) -> case TE.decodeUtf8' out of
          Left e  -> failWith (OutputNotUtf8 e)
          Right t -> Right (T.unpack (T.dropWhileEnd (== '\n') t))

resolveShell :: ShellConfig -> IO (Maybe FilePath)
resolveShell cfg = case shellProgram cfg of
  Just p  -> pure (Just p)
  Nothing -> do
    v <- lookupEnv "SHELL"
    pure $ case v of
      Just s | not (null s) -> Just s
      _                     -> Nothing

-- | Run @shell flags... cmd@ with stdin closed, collecting stdout and stderr
-- as raw bytes (read concurrently, so a chatty command cannot deadlock).
execShell :: FilePath -> [String] -> String -> IO (ExitCode, BS.ByteString, BS.ByteString)
execShell shell flags cmd = do
  (mIn, mOut, mErr, ph) <-
    createProcess
      (proc shell (flags ++ [cmd]))
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  case (mIn, mOut, mErr) of
    (Just hin, Just hout, Just herr) -> do
      hClose hin
      outV <- readAll hout
      errV <- readAll herr
      code <- waitForProcess ph
      (,,) code <$> takeMVar outV <*> takeMVar errV
    _ -> E.throwIO (userError "could not create pipes for the shell process")

readAll :: Handle -> IO (MVar BS.ByteString)
readAll h = do
  v <- newEmptyMVar
  _ <- forkIO $ (BS.hGetContents h `E.catch` ignoreIO) >>= putMVar v
  pure v
  where
    ignoreIO :: E.IOException -> IO BS.ByteString
    ignoreIO _ = pure BS.empty

------------------------------------------------------------------------------
-- Rendering

renderCfvError :: CfvError -> String
renderCfvError (InvalidExtension p) =
  p ++ ": not a .cfv file (the file name must end in \".cfv\")"
renderCfvError (FileReadError p e)   = p ++ ": cannot read file: " ++ show e
renderCfvError (FileDecodeError p e) = p ++ ": file is not valid UTF-8: " ++ show e
renderCfvError (FileSyntaxError e)   = renderSyntaxError e
renderCfvError (FileCommandError e)  = renderCommandError e

-- | Multi-line, compiler-style message with a caret under the error.
renderSyntaxError :: SyntaxError -> String
renderSyntaxError (SyntaxError (Position f l c _) kind srcLine) =
  intercalate "\n" $
    [ f ++ ":" ++ show l ++ ":" ++ show c ++ ": error: " ++ describeKind kind ]
      ++ case srcLine of
           Just ln -> [ "  | " ++ ln, "  | " ++ replicate (c - 1) ' ' ++ "^" ]
           Nothing -> []

renderCommandError :: CommandError -> String
renderCommandError (CommandError name cmd (Position f l c _) problem) =
  f ++ ":" ++ show l ++ ":" ++ show c ++ ": error: variable " ++ show name
    ++ ": " ++ describeProblem problem
    ++ "\n  command: " ++ show cmd

describeKind :: ErrorKind -> String
describeKind (UnexpectedInput found expected) =
  "unexpected " ++ maybe "input" describeItem found
    ++ (if null expected
          then ""
          else "; expected " ++ intercalate ", " (map describeItem expected))
describeKind (ReservedWord w) =
  "reserved word " ++ show w ++ " cannot be used as a variable name"
describeKind (InvalidEscape c) = "invalid escape sequence \\" ++ [c]
describeKind (UnterminatedString q) =
  "unterminated string literal (no closing " ++ [q] ++ " for this one)"
describeKind UnterminatedCommand =
  "unterminated command (no closing backtick for this one)"
describeKind (OtherFailure msg) = msg

describeItem :: Item -> String
describeItem (Literal s)     = show s
describeItem (Description d) = d
describeItem EndOfFile       = "end of input"

describeProblem :: CommandProblem -> String
describeProblem NoShell =
  "cannot run command: no shell configured and $SHELL is not set"
describeProblem (ShellLaunchFailed sh e) =
  "cannot start shell " ++ show sh ++ ": " ++ show e
describeProblem (NonZeroExit n err) =
  "command exited with status " ++ show n ++ (if null err then "" else ": " ++ err)
describeProblem (OutputNotUtf8 e) =
  "command output is not valid UTF-8: " ++ show e

------------------------------------------------------------------------------
-- Converting Megaparsec's errors into ours

-- | Internal custom errors raised by the parser itself.
data Custom
  = CReservedWord String
  | CInvalidEscape Char
  | CUnterminatedString Char
  | CUnterminatedCommand
  deriving (Eq, Ord, Show)

toPosition :: SourcePos -> Int -> Position
toPosition sp = Position (sourceName sp) (unPos (sourceLine sp)) (unPos (sourceColumn sp))

fromBundle :: ParseErrorBundle Text Custom -> SyntaxError
fromBundle bundle =
  SyntaxError
    { syntaxPosition   = toPosition (pstateSourcePos posState) offset
    , syntaxKind       = kind
    , syntaxSourceLine = srcLine
    }
  where
    -- Without error recovery there is exactly one error in the bundle.
    err = NE.head (bundleErrors bundle)
    offset = errorOffset err
    (srcLine, posState) = reachOffset offset (bundlePosState bundle)
    kind = case err of
      TrivialError _ unexpected_ expected_ ->
        UnexpectedInput
          (fromItem <$> unexpected_)
          (map fromItem (Set.toList expected_))
      FancyError _ fancy -> case Set.toList fancy of
        (f : _) -> fromFancy f
        []      -> OtherFailure "unknown error"

fromItem :: ErrorItem Char -> Item
fromItem (Tokens ts) = Literal (NE.toList ts)
fromItem (Label cs)  = Description (NE.toList cs)
fromItem EndOfInput  = EndOfFile

fromFancy :: ErrorFancy Custom -> ErrorKind
fromFancy (ErrorCustom (CReservedWord w))       = ReservedWord w
fromFancy (ErrorCustom (CInvalidEscape c))      = InvalidEscape c
fromFancy (ErrorCustom (CUnterminatedString q)) = UnterminatedString q
fromFancy (ErrorCustom CUnterminatedCommand)    = UnterminatedCommand
fromFancy (ErrorFail msg)                       = OtherFailure msg
fromFancy ErrorIndentation {}                   = OtherFailure "indentation error"

------------------------------------------------------------------------------
-- Parser

type Parser = Parsec Custom Text

-- | Fail with a custom error reported at the given offset.
failAt :: Int -> Custom -> Parser a
failAt o c = parseError (FancyError o (Set.singleton (ErrorCustom c)))

currentPosition :: Parser Position
currentPosition = do
  off <- getOffset
  sp  <- getSourcePos
  pure (toPosition sp off)

-- Whitespace and comments -----------------------------------------------------

-- | Space consumer: whitespace (incl. newlines) and @--@ line comments.
sc :: Parser ()
sc = L.space space1 lineComment empty

-- | A Haskell line comment: two or more dashes NOT followed by a symbol
-- character (so @-->@ or @--|@ is an operator, not a comment, as in Haskell).
lineComment :: Parser ()
lineComment = try $ do
  _ <- string "--"
  _ <- takeWhileP Nothing (== '-')
  notFollowedBy (satisfy isSymbolChar)
  void (takeWhileP (Just "comment text") (/= '\n'))
  where
    isSymbolChar c = c `elem` ("!#$%&*+./<=>?@\\^|~:" :: String)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Tokens ----------------------------------------------------------------------

-- | A Haskell variable identifier: starts with a lowercase letter or @_@,
-- continues with letters, digits, @_@ or @'@, and is not a reserved word.
varName :: Parser String
varName = lexeme $ do
  start <- getOffset
  c  <- satisfy (\x -> isLower x || x == '_') <?> "variable name"
  cs <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x == '\'')
  let name = c : T.unpack cs
  when (name `elem` reserved) $ failAt start (CReservedWord name)
  pure name

reserved :: [String]
reserved = ["_"]

-- | A single- or double-quoted string. Supports the escapes
-- @\\\\ \\' \\" \\n \\t \\r \\0@. Raw newlines are allowed inside strings.
stringLit :: Parser String
stringLit = lexeme (quoted '\'' <|> quoted '"') <?> "string literal"

quoted :: Char -> Parser String
quoted q = do
  start <- getOffset
  _ <- char q
  body <- many (escaped start <|> plain)
  -- 'many' only stops on the closing quote or at end of input.
  closed <- optional (char q)
  case closed of
    Nothing -> failAt start (CUnterminatedString q)
    Just _  -> pure body
  where
    plain = satisfy (\c -> c /= q && c /= '\\')

    escaped start = do
      slash <- getOffset
      _ <- char '\\'
      next <- optional anySingle
      case next of
        Nothing -> failAt start (CUnterminatedString q)
        Just c  -> maybe (failAt slash (CInvalidEscape c)) pure (lookup c escapes)

    escapes =
      [ ('n', '\n'), ('t', '\t'), ('r', '\r'), ('0', '\0')
      , ('\\', '\\'), ('\'', '\''), ('"', '"')
      ]

-- | A backtick-delimited shell command: everything up to the next backtick.
commandLit :: Parser String
commandLit = lexeme body <?> "backtick command"
  where
    body = do
      start <- getOffset
      _ <- char '`'
      cmd <- takeWhileP Nothing (/= '`')
      closed <- optional (char '`')
      case closed of
        Nothing -> failAt start CUnterminatedCommand
        Just _  -> pure (T.unpack cmd)

-- Grammar ---------------------------------------------------------------------

-- | @name = 'text';@, @name = "text";@ or @name = `cmd`@ (the semicolon is
-- optional after a command).
declaration :: Parser Declaration
declaration = do
  pos   <- currentPosition
  name  <- varName
  _     <- symbol "="
  value <- stringValue <|> commandValue
  pure (Declaration name value pos)
  where
    stringValue  = StringValue  <$> stringLit  <* symbol ";"
    commandValue = CommandValue <$> commandLit <* optional (symbol ";")

-- | A whole file: any number of declarations, with comments/whitespace anywhere.
program :: Parser [Declaration]
program = sc *> many declaration <* eof