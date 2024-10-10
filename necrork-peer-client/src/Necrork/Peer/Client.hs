module Necrork.Peer.Client
  ( module Necrork.Peer.Client,
    module Necrork.Peer.API,
    module X,
  )
where

import Necrork.Peer.API
import Servant.API as X
import Servant.Client as X
import Servant.Client.Generic

necrorkClient :: FooBarRoutes (AsClientT ClientM)
necrorkClient = genericClient
