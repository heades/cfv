{-|
Module      : Shell
Description : Command evocation on the shell
Copyright   : (c) Harley Eades, 2026
Maintainer  : harley.eades@gmail.com

This is the API for accessing the users shell. All commands are ran in the login
shell with access to the @PATH@ environment variable, but the shell's rc-file is
not executed by default, and so aliases are not useable. If you need access to
your rc-file, then define a custom t`ShellConfig` for your project.
-}
module Configuration.Cfv.Shell 
  (CommandError(..)
  ,CommandProblem(..)
  ,ShellConfig(..)
  ,evalValue
  ,defaultShellConfig
  ,describeProblem) where

import qualified GHC.IO.Exception as E
import Data.Text.Encoding.Error (UnicodeException, lenientDecode)
import GHC.IO.Exception (ExitCode(..))
import qualified Data.ByteString as BS
import System.Process (CreateProcess(..), waitForProcess, proc, createProcess, StdStream (..))
import GHC.IO.Handle (Handle, hClose)
import Control.Concurrent (MVar, newEmptyMVar, forkIO, putMVar, takeMVar)
import qualified Control.Exception as E
import System.Environment (lookupEnv)
import qualified Data.Text.Encoding as TE
import Control.Exception (try)
import Data.Text (Text)
import qualified Data.Text as DT

-- | A backtick command that could not produce a value.
--
-- For a @Configuration.Cfv.AST.Secret@ variable anything that could contain or reveal the secret is
-- withheld: the command text (which may embed it) and the details of the
-- command's output (see 'CommandProblem').
data CommandError = CommandError
  { cmdCommand  :: Text    
  , cmdProblem  :: CommandProblem
  } deriving (Eq, Show)

-- | Errors produced by running a command on the shell.
data CommandProblem
  = NoShell
    -- ^ No shell configured and @SHELL@ is unset or empty.
  | ShellLaunchFailed FilePath E.IOException
    -- ^ The shell could not be started (not found, not executable, ...).
  | NonZeroExit Int Text
    -- ^ The command exited with this status (negative: killed by that signal)
    -- and this standard error output, trimmed. The output is 'Nothing' for a
    -- '@Configuration.Cfv.AST.Secret' variable.
  | OutputNotUtf8 UnicodeException
    -- ^ The command's standard output is not valid UTF-8. The exception names
    -- the offending byte, so it is 'Nothing' for a '@Configuration.Cfv.AST.Secret' variable.
  deriving (Eq, Show)

-- | Shell configuration.
data ShellConfig = ShellConfig
  { shellProgram :: Maybe FilePath
    -- ^ The shell to use. 'Nothing' means the @SHELL@ environment variable.
  , shellFlags   :: [String]
    -- ^ Arguments placed before the command text, which is passed last.
  } deriving (Eq, Show)

-- | Use @$SHELL@ as a login shell: @$SHELL -l -c cmd@. A login shell picks up
-- the user's profile (@PATH@ additions and so on). Add @"-i"@ to the flags if
-- you also need what the shell's interactive rc file sets up (aliases,
-- functions).
defaultShellConfig :: ShellConfig
defaultShellConfig = ShellConfig {shellProgram = Nothing, shellFlags = ["-l", "-c"]}

-- | Evaluate a shell command reading in its @System.IO.stdout@. 
evalValue :: ShellConfig -- ^ Shell configuration
          -> Text        -- ^ Command to execute
          -> IO (Either CommandError [Char])
evalValue shellCfg cmd 
  = do let failWith = Left . CommandError cmd
       mShell <- resolveShell shellCfg
       case mShell of
         Nothing -> pure (failWith NoShell)
         Just shell -> do
          result <- try $ execShell shell (shellFlags shellCfg) $ DT.unpack cmd
          pure $ case result of
                    Left e -> failWith (ShellLaunchFailed shell e)
                    Right (ExitFailure n, _, err) ->
                      failWith . NonZeroExit n . DT.strip $ TE.decodeUtf8With lenientDecode err
                    Right (ExitSuccess, out, _) 
                      -> case TE.decodeUtf8' out of
                            Left e  -> failWith (OutputNotUtf8 e)
                            Right t -> Right (DT.unpack (DT.dropWhileEnd (== '\n') t))

-- | Figure out the shell's path.
resolveShell :: ShellConfig -> IO (Maybe FilePath)
resolveShell cfg = case shellProgram cfg of
  Just p  -> pure (Just p)
  Nothing -> do
    v <- lookupEnv "SHELL"
    pure $ case v of
      Just s | not (null s) -> Just s
      _                     -> Nothing          

-- | Run @shell flags... cmd@ with stdin closed, collecting stdout and stderr as
-- raw bytes; to prevent deadlock we read both in concurrently.
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

-- | Read a handle until there is no more output to consume.
readAll :: Handle -> IO (MVar BS.ByteString)
readAll h = do
  v <- newEmptyMVar
  _ <- forkIO $ (BS.hGetContents h `E.catch` ignoreIO) >>= putMVar v
  pure v
  where
    ignoreIO :: E.IOException -> IO BS.ByteString
    ignoreIO _ = pure BS.empty

-- | Converts a command problem into a 'Text'.
describeProblem :: CommandProblem -> Text
describeProblem NoShell =
  "cannot run command: no shell configured and $SHELL is not set"
describeProblem (ShellLaunchFailed sh e) =
  "cannot start shell " <> DT.show sh <> ": " <> DT.show e
describeProblem (NonZeroExit n err) =
  "command exited with status " <> DT.show n <> (if DT.null err then "" else ": " <> err)
describeProblem (OutputNotUtf8 e) =
  "command output is not valid UTF-8: " <> DT.show e
