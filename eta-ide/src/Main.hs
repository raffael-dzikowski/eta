module Main where

import Eta.BasicTypes.Module (emptyModuleEnv)
import Eta.Main.DynFlags (getDynFlags)
import Eta.Main.GHC (runGhc, setProgramDynFlags)
import Eta.Main.SysTools (findTopDir)
import Eta.REPL.UI
import Eta.REPL.UI.Monad

import qualified Data.Map as M
import System.Console.Haskeline

main :: IO ()
main = do
  libdir <- findTopDir Nothing
  print libdir
  runGhc (Just libdir) $ do
    dflags <- getDynFlags
    setProgramDynFlags dflags
    startGHCi
      (runInputT defaultSettings loop)
      defaultGHCiState
  where
  loop :: InputT GHCi ()
  loop = do
    mInput <- getInputLine "> "
    case mInput of
      Nothing  -> return ()
      Just "q" -> return ()
      Just _ -> do
        browseCmd False "Control.Monad"
        loop

  -- Ported from Eta.REPL.UI.interactiveUI
  defaultGHCiState =
    GHCiState{
               -- progname           = default_progname,
               -- args               = default_args,
               -- evalWrapper        = eval_wrapper,
               -- prompt             = default_prompt,
               -- prompt_cont        = default_prompt_cont,
               -- stop               = default_stop,
               -- editor             = default_editor,
               -- options            = [ShowType],
               -- -- We initialize line number as 0, not 1, because we use
               -- -- current line number while reporting errors which is
               -- -- incremented after reading a line.
               line_number        = 0,
               break_ctr          = 0,
               breaks             = [],
               -- tickarrays         = emptyModuleEnv,
               -- ghci_commands      = availableCommands config,
               -- ghci_macros        = [],
               -- last_command       = Nothing,
               -- cmdqueue           = [],
               -- remembered_ctx     = [],
               -- transient_ctx      = [],
               -- extra_imports      = [],
               -- prelude_imports    = [prelude_import],
               -- ghc_e              = False,
               -- short_help         = shortHelpText config,
               -- long_help          = fullHelpText config,
               -- lastErrorLocations = lastErrLocationsRef,
               mod_infos          = M.empty
               -- flushStdHandles    = flush,
               -- noBuffering        = nobuffering
             }
