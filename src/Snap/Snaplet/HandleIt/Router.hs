module Snap.Snaplet.HandleIt.Router where

import Snap.Snaplet.HandleIt.Header
import Control.Monad.State(runState, put, get)
import Snap.Snaplet
import Snap.Snaplet.Heist(HasHeist(..))

import qualified Data.ByteString.Char8 as BS

routing :: Router () -> Routing
routing a = snd $ runState a []

handleIt :: (Restful, HDL) -> Router ()
handleIt a = get >>= put . (a:)

resources :: Handling s => s -> Router ()
resources a = mapM (setSingle a)
              [ RIndex  , RShow
              , RNew    , REdit
              , RCreate , RUpdate
              , RDestroy ] >> return ()

setSingle  :: Handling s => s -> Restful -> Router ()
setSingle a r = handleIt (r,  HDL a)

prepend :: BS.ByteString -> BS.ByteString -> BS.ByteString
prepend = flip BS.append

restfulToFunction :: (HasHeist b, Handling a) => Restful -> a -> Handler b c ()
restfulToFunction RIndex   = hIndex
restfulToFunction RShow    = hShow
restfulToFunction RNew     = hNew
restfulToFunction REdit    = hEdit
restfulToFunction RCreate  = hCreate
restfulToFunction RUpdate  = hUpdate
restfulToFunction RDestroy = hDestroy

restfulToURL :: Handling a => Restful -> a -> BS.ByteString
restfulToURL RIndex   = (BS.append "/")      . handleName
restfulToURL RShow    = (prepend "/:id")     . (BS.append "/") . handleName
restfulToURL RNew     = (prepend "/new")     . (BS.append "/") . handleName
restfulToURL REdit    = (prepend "/edit")    . (BS.append "/") . handleName
restfulToURL RCreate  = (prepend "/create")  . (BS.append "/") . handleName
restfulToURL RUpdate  = (prepend "/update")  . (BS.append "/") . handleName
restfulToURL RDestroy = (prepend "/destroy") . (BS.append "/") . handleName

routePath :: HasHeist b => (Restful, HDL) -> (BS.ByteString, Handler b c ())
routePath (rest, HDL h) = (restfulToURL rest h, restfulToFunction rest h)

manageRouting :: HasHeist b => Routing -> Initializer b c [(BS.ByteString, Handler b c ())]
manageRouting routes = do
    let newRoutes = map routePath routes
    addRoutes newRoutes
    return newRoutes
