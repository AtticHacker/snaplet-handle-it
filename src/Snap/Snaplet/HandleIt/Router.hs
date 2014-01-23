module Snap.Snaplet.HandleIt.Router where

import Snap(Handler, Initializer, addRoutes)
import Snap.Snaplet.HandleIt.Header
import Snap.Snaplet.HandleIt.Internal.Router
import Snap.Snaplet.Heist(HasHeist(..))
import qualified Data.ByteString.Char8 as BS
import Control.Monad.State(put, get)

-- | resources adds all restful actions to State
resources :: Handling s => s -> Router ()
resources a = mapM (setSingle a)
              [ IndexR   , ShowR
              , NewR     , EditR
              , CreateR  , UpdateR
              , DestroyR ] >> return ()

-- | setSingle adds a single
setSingle  :: Handling s => s -> Restful -> Router ()
setSingle a r = get >>= put . ((r, HDL a):)

-- | This function takes the state result and adds the paths to the Snap app
manageRouting :: HasHeist b => Routing ->
                 Initializer b c [(BS.ByteString, Handler b c ())]
manageRouting routes = do
    let newRoutes = map routePath routes
    addRoutes newRoutes
    return newRoutes

handleRoutes :: HasHeist b => Router () ->
                 Initializer b c [(BS.ByteString, Handler b c ())]
handleRoutes routes = do
    let newRoutes = map routePath $ routing routes
    addRoutes newRoutes
    return newRoutes
