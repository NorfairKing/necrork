module Necrork.Cli (runNecrorkCli) where

import Necrork.Cli.Command.Delete (runNecrorkDelete)
import Necrork.Cli.Command.Notify (runNecrorkNotify)
import Necrork.Cli.Env
import Necrork.Cli.OptParse

runNecrorkCli :: IO ()
runNecrorkCli = do
  Instructions settings dispatch <- getInstructions
  runCliM settings $
    case dispatch of
      DispatchNotify name -> runNecrorkNotify name
      DispatchDelete name -> runNecrorkDelete name
