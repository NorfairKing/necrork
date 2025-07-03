module Necrork.Cli (runNecrorkCli) where

import Necrork.Cli.Command.Delete (runNecrorkDelete)
import Necrork.Cli.Command.Notify (runNecrorkNotify)
import Necrork.Cli.OptParse

runNecrorkCli :: IO ()
runNecrorkCli = do
  Instructions settings dispatch <- getInstructions
  case dispatch of
    DispatchNotify name -> runNecrorkNotify settings name
    DispatchDelete name -> runNecrorkDelete settings name
