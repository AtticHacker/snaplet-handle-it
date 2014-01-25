module Snap.Snaplet.HandleIt.Router where

import Snap(Handler, Initializer, addRoutes)
import Snap.Snaplet.HandleIt.Header
import Snap.Snaplet.HandleIt.Internal.Router
import Snap.Snaplet.Heist(HasHeist(..))
import qualified Data.ByteString.Char8 as BS
import Control.Monad.State(put, get)

-- | resources adds all restful actions to State
resources :: Handling a b => a -> b -> Router () b
resources a b = mapM (setSingle a b)
              [ IndexR   , ShowR
              , NewR     , EditR
              , CreateR  , UpdateR
              , DestroyR ] >> return ()

-- | setSingle adds a single
setSingle  :: Handling a b => a -> b -> Restful -> Router () b
setSingle a b r = get >>= put . ((r, HDL a b):)

-- | This function takes the state result and adds the paths to the Snap app
-- manageRouting
--   :: Handling a b => [(Restful, HDL a b)] ->
--                  Initializer b b [(BS.ByteString, Handler b b ())]
-- manageRouting routes = do
--     let newRoutes = map routePath routes
--     addRoutes newRoutes
--     return newRoutes

handleRoutes  :: Handling () b => Router () b ->
                 Initializer b b [(BS.ByteString, Handler b b ())]
handleRoutes routes = do
    let newRoutes = map routePath $ routing routes
    addRoutes newRoutes
    return newRoutes
