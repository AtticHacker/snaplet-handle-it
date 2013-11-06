module Snap.Snaplet.HandleIt where

import Snap.Snaplet(SnapletInit, makeSnaplet)
import Snap.Snaplet.HandleIt.Header(HandleIt(..), Routing)
import Snap.Snaplet.HandleIt.Router(manageRouting)
import Snap.Snaplet.Heist(HasHeist)

import Paths_snaplet_handle_it

-- | Initiates Handle-it
handleItInit :: HasHeist a => Routing -> SnapletInit a (HandleIt Routing)
handleItInit routes = makeSnaplet "handleIt" "" Nothing $ do
    manageRouting routes >>= return . HandleIt