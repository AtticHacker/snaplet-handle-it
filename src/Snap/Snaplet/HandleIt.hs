module Snap.Snaplet.HandleIt( module Snap.Snaplet.HandleIt.Header
                            , module Snap.Snaplet.HandleIt.Router
                            ) where

--import Snap.Snaplet(SnapletInit, makeSnaplet)
import Snap.Snaplet.HandleIt.Header
import Snap.Snaplet.HandleIt.Router
--import Snap.Snaplet.HandleIt.Internal.Router
--import Snap.Snaplet.Heist(HasHeist)

-- | Initiates Handle-it
-- handleItInit :: HasHeist b => Router () b -> SnapletInit b (HandleIt (Routing () b) b)
-- handleItInit routes = makeSnaplet "handleIt" "" Nothing $ do return $ HandleIt []
--     manageRouting (routing routes) >>= return . HandleIt
