{-|
Module      : Parser
Description : The CFV Parser
Copyright   : (c) Harley Eades, 2026
Maintainer  : harley.eades@gmail.com

-}
module  Configuration.Cfv.Parser 
    (parseCfvFile
    ,parseCfvFileWith
    ,CfvError (..)
    ,SyntaxError (..)
    ,ErrorKind (..)
    ,Item (..)
    ,Position (..)) where

import qualified GHC.IO.Exception as E
import Data.Text.Encoding.Error (UnicodeException)
import qualified GHC.Exception as E
import Data.Text (Text)
import Text.Megaparsec (SourcePos (..), ParseErrorBundle (..), ParseError (..), ErrorItem (..), ErrorFancy (..), Parsec, unPos, PosState (..), errorOffset, TraversableStream (reachOffset), parseError, getOffset, getSourcePos, empty, try, MonadParsec (takeWhileP, notFollowedBy, observing, eof), satisfy, (<?>), (<|>), many, optional, anySingle, option, parse)
import System.FilePath (takeExtension)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1, string, char)
import Control.Monad (void, when)
import Data.Char (isLower, isAlphaNum)
import qualified Data.Bifunctor as BI

import Configuration.Cfv.AST (VarAttr (..), Binding, Var (..))
import Configuration.Cfv.Shell (CommandError (..), ShellConfig, defaultShellConfig, evalValue, describeProblem)

-------------------------------------------------------------------------------------------------------------------
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
    Right decls -> runDeclarations cfg decls

-------------------------------------------------------------------------------------------------------------------

-- | One @[secret] name = value@ declaration, as written in the file.
data Declaration = Declaration
  { declName     :: String
  , declAttr     :: VarAttr
  , declValue    :: Value
  , declPosition :: Position
    -- ^ Where the declaration starts (its @secret@ prefix if it has one,
    -- otherwise its variable name).
  } deriving (Eq)

-- | Like the derived instance, except that the value of a 'Secret'
-- declaration is shown as @\<redacted\>@.
instance Show Declaration where
  showsPrec :: Int -> Declaration -> ShowS
  showsPrec d (Declaration n a v p) =
    showParen (d > 10) $
      showString "Declaration "
        . showsPrec 11 n . showChar ' '
        . showsPrec 11 a . showChar ' '
        . (case a of
             Public -> showsPrec 11 v
             Secret -> showString "<redacted>")
        . showChar ' '
        . showsPrec 11 p

-- | The right-hand side of a declaration.
data Value
  = StringValue String   -- ^ A quoted string, already unescaped.
  | CommandValue String  -- ^ The text between backticks, unmodified.
  deriving (Eq, Show)

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
  | FileCommandError CommandError String VarAttr Position
    -- ^ A backtick command could not be run, or failed. (Only produced by
    -- functions that run commands.)
  deriving (Eq, Show)

-- | A problem with the text itself, with a location.
data SyntaxError = SyntaxError
  { syntaxPosition   :: Position
  , syntaxKind       :: ErrorKind
  , syntaxSourceLine :: Maybe String
    -- ^ The source line containing 'syntaxPosition'. Always 'Nothing' when the
    -- input contains the text @secret@ anywhere (even in a comment), because
    -- the line could then contain, or be part of, a secret value. Every
    -- secret declaration contains that text, so this cannot miss one.
  } deriving (Eq, Show)

-- | The specific kind of syntax error.
data ErrorKind
  = UnexpectedInput (Maybe Item) [Item]
    -- ^ Found this (if known), but expected one of these.
  | ReservedWord String
    -- ^ A Haskell keyword (or a lone @_@) was used as a variable name.
    -- Position: start of the word.
  | InvalidEscape (Maybe Char)
    -- ^ A backslash followed by a character that is not a known escape.
    -- Position: the backslash. 'Nothing' (character withheld) inside the value
    -- of a 'Secret' declaration.
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

instance E.Exception CfvError where
  displayException :: CfvError -> String
  displayException = renderCfvError

instance E.Exception SyntaxError where
  displayException :: SyntaxError -> String
  displayException = renderSyntaxError

-- | Turn declarations into bindings, running each 'CommandValue' in the user's
-- shell. The shell is only looked up if there is at least one command.
runDeclarations :: ShellConfig -> [Declaration] -> IO (Either CfvError [Binding])
runDeclarations cfg = go []
  where
    go acc [] = pure (Right (reverse acc))
    go acc (d : ds) = do
      case declValue d of
        CommandValue cmd -> do
            result <- evalValue cfg cmd
            case result of
              Left err -> pure (Left $ FileCommandError err (declName d) (declAttr d) (declPosition d))
              Right v  -> go ((declName d, Var (declAttr d) v) : acc) ds
        StringValue v -> go ((declName d, Var (declAttr d) v) : acc) ds

-- | Check the extension, read and parse a file, without running any command.
-- Never returns 'FileCommandError'.
readCfvFile :: FilePath -> IO (Either CfvError [Declaration])
readCfvFile path
  | takeExtension path /= ".cfv" = pure (Left (InvalidExtension path))
  | otherwise = do
      result <- E.try (BS.readFile path)
      pure $ case result of
        Left e -> Left (FileReadError path e)
        Right bytes -> 
          case TE.decodeUtf8' bytes of
            Left e    -> Left (FileDecodeError path e)
            Right txt -> BI.first FileSyntaxError $ parseCfv path txt

-- | Pure parser; runs nothing. The 'FilePath' is only used to label error
-- positions and is not checked for an extension.
parseCfv :: FilePath -> Text -> Either SyntaxError [Declaration]
parseCfv name input = BI.first fromBundle $ parse program name input

renderCommandError :: CommandError -> String -> VarAttr -> Position -> String
renderCommandError (CommandError cmd problem) name varAttr (Position f l c _)
  = f ++ ":" ++ show l ++ ":" ++ show c ++ ": error: variable " ++ show name
      ++ ": " ++ describeProblem problem
      ++ if varAttr == Secret then "" else "\n  command: " ++ cmd 

renderCfvError :: CfvError -> String
renderCfvError (InvalidExtension p)
  = p ++ ": not a .cfv file (the file name must end in \".cfv\")"
renderCfvError (FileReadError p e)            
  = p ++ ": cannot read file: " ++ show e
renderCfvError (FileDecodeError p e)          
  = p ++ ": file is not valid UTF-8: " ++ show e
renderCfvError (FileSyntaxError e)            
  = renderSyntaxError e
renderCfvError (FileCommandError e n vatr p)  
  = renderCommandError e n vatr p

-- | Multi-line, compiler-style message with a caret under the error.
renderSyntaxError :: SyntaxError -> String
renderSyntaxError (SyntaxError (Position f l c _) kind srcLine) =
  intercalate "\n" $
    [ f ++ ":" ++ show l ++ ":" ++ show c ++ ": error: " ++ describeKind kind ]
      ++ case srcLine of
           Just ln -> [ "  | " ++ ln, "  | " ++ replicate (c - 1) ' ' ++ "^" ]
           Nothing -> []

describeKind :: ErrorKind -> String
describeKind (UnexpectedInput found expected) =
  "unexpected " ++ maybe "input" describeItem found
    ++ (if null expected
          then ""
          else "; expected " ++ intercalate ", " (map describeItem expected))
describeKind (ReservedWord w) =
  "reserved word " ++ show w ++ " cannot be used as a variable name"
describeKind (InvalidEscape (Just c)) = "invalid escape sequence \\" ++ [c]
describeKind (InvalidEscape Nothing) =
  "invalid escape sequence (character withheld: secret value)"
describeKind (UnterminatedString q) =
  "unterminated string literal (no closing " ++ [q] ++ " for this one)"
describeKind UnterminatedCommand =
  "unterminated command (no closing backtick for this one)"
describeKind (OtherFailure msg) = msg

describeItem :: Item -> String
describeItem (Literal s)     = show s
describeItem (Description d) = d
describeItem EndOfFile       = "end of input"

-- | Internal custom errors raised by the parser itself.
data Custom
  = CReservedWord String
  | CInvalidEscape (Maybe Char)
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
    , syntaxSourceLine = if "secret" `T.isInfixOf` pstateInput (bundlePosState bundle)
                           then Nothing
                           else srcLine
    }
  where
    -- Without error recovery there is exactly one error in the bundle.
    err = NE.head (bundleErrors bundle)
    offset = errorOffset err
    (srcLine, posState) = reachOffset offset (bundlePosState bundle)
    kind = case err of
      TrivialError _ found expected ->
        UnexpectedInput
          (fromItem <$> found)
          (map fromItem (Set.toList expected))
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

type Parser = Parsec Custom Text

-- | Fail with a custom error reported at the given offset.
failAt :: Int -> Custom -> Parser a
failAt o c = parseError (FancyError o (Set.singleton (ErrorCustom c)))

currentPosition :: Parser Position
currentPosition = do
  off <- getOffset
  sp  <- getSourcePos
  pure (toPosition sp off)

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
reserved =
  [ "_" ]

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
        Just c  -> maybe (failAt slash (CInvalidEscape (Just c))) pure (lookup c escapes)

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


-- | The optional @secret@ prefix. It is only a keyword when something other
-- than @=@ follows it, so a variable may still be called @secret@. Absent
-- means 'Public'.
attribute :: Parser VarAttr
attribute = option Public (Secret <$ try (keyword <* notFollowedBy (char '=')))
  where
    keyword = lexeme (string "secret" <* notFollowedBy (satisfy isNameChar))
    isNameChar x = isAlphaNum x || x == '_' || x == '\''

-- | @[secret] name = 'text';@, @[secret] name = "text";@ or
-- @[secret] name = `cmd`@ (the semicolon is optional after a command).
declaration :: Parser Declaration
declaration = do
  pos   <- currentPosition
  attr  <- attribute
  name  <- varName
  _     <- symbol "="
  value <- withholdIfSecret attr (StringValue <$> stringLit <|> CommandValue <$> commandLit)
  case value of
    StringValue _  -> void (symbol ";")
    CommandValue _ -> void (optional (symbol ";"))
  pure (Declaration name attr value pos)

-- | Run a parser for the value of a declaration. If it fails inside a
-- 'Secret' value, strip from the error any input that was quoted in it: the
-- unexpected character (which could be the start of the secret) and the
-- character after a bad backslash.
withholdIfSecret :: VarAttr -> Parser a -> Parser a
withholdIfSecret Public p = p
withholdIfSecret Secret p = observing p >>= either (parseError . withhold) pure
  where
    withhold (TrivialError o _ expected) = TrivialError o Nothing expected
    withhold (FancyError o fancy)        = FancyError o (Set.map hide fancy)
    hide (ErrorCustom (CInvalidEscape _)) = ErrorCustom (CInvalidEscape Nothing)
    hide f                                = f

-- | A whole file: any number of declarations, with comments/whitespace anywhere.
program :: Parser [Declaration]
program = sc *> many declaration <* eof