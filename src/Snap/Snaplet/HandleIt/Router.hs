module Snap.Snaplet.HandleIt.Router where

import Snap.Snaplet.HandleIt.Header
import Control.Monad.State(runState, put, get)
import Snap.Snaplet
import Snap.Snaplet.Heist(HasHeist(..), render)
import qualified Data.ByteString.Char8 as BS

routing :: Router () -> Routing
routing a = snd $ runState a []

handleRoute :: (Restful, HDL) -> Router ()
handleRoute a = get >>= put . (a:)

resources :: Handling s => s -> Router ()
resources a = mapM (setSingle a)
              [ IndexR   , ShowR
              , NewR     , EditR
              , CreateR  , UpdateR
              , DestroyR ] >> return ()

setSingle  :: Handling s => s -> Restful -> Router ()
setSingle a r = handleRoute (r,  HDL a)

prepend :: BS.ByteString -> BS.ByteString -> BS.ByteString
prepend = flip BS.append

restfulToFunction :: (HasHeist b, Handling a) => Restful -> a -> Handler b c ()
restfulToFunction IndexR   = indexH
restfulToFunction ShowR    = showH
restfulToFunction NewR     = newH
restfulToFunction EditR    = editH
restfulToFunction CreateR  = createH
restfulToFunction UpdateR  = updateH
restfulToFunction DestroyR = destroyH

restfulToURL :: Handling a => Restful -> a -> BS.ByteString
restfulToURL IndexR   = (BS.append "/")      . handleName
restfulToURL ShowR    = (prepend "/:id")     . (BS.append "/") . handleName
restfulToURL NewR     = (prepend "/new")     . (BS.append "/") . handleName
restfulToURL EditR    = (prepend "/edit")    . (BS.append "/") . handleName
restfulToURL CreateR  = (prepend "/create")  . (BS.append "/") . handleName
restfulToURL UpdateR  = (prepend "/update")  . (BS.append "/") . handleName
restfulToURL DestroyR = (prepend "/destroy") . (BS.append "/") . handleName

routePath :: HasHeist b => (Restful, HDL) -> (BS.ByteString, Handler b c ())
routePath (rest, HDL h) =
    let url = restfulToURL rest h
    in (url, restfulToFunction rest h >> renderPath rest h url)

renderPath :: (Handling a, HasHeist b) => Restful -> a ->
              BS.ByteString -> Handler b c ()
renderPath IndexR _ url = render $ url `BS.append` "/index"
renderPath ShowR    a _ = render $ handleName a `BS.append` "/show"
renderPath CreateR  _ _ = return ()
renderPath DestroyR _ _ = return ()
renderPath UpdateR  _ _ = return ()
renderPath      _ _ url = render url

manageRouting :: HasHeist b => Routing ->
                 Initializer b c [(BS.ByteString, Handler b c ())]
manageRouting routes = do
    let newRoutes = map routePath routes
    addRoutes newRoutes
    return newRoutes
