module Necrork.Client
  ( module Necrork.Client,
    module Necrork.API,
    module X,
  )
where

import Necrork.API
import Servant.API as X
import Servant.Client as X
import Servant.Client.Generic

necrorkClient :: NecrorkRoutes (AsClientT ClientM)
necrorkClient = genericClient
