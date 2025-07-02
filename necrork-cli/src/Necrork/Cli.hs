module Necrork.Cli (runNecrorkCli) where

import Necrork.Cli.Command.Delete (runNecrorkDelete)
import Necrork.Cli.OptParse

runNecrorkCli :: IO ()
runNecrorkCli = do
  Instructions settings dispatch <- getInstructions
  case dispatch of
    DispatchDelete name -> runNecrorkDelete settings name
