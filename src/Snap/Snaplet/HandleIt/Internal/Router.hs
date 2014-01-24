module Snap.Snaplet.HandleIt.Internal.Router where

import Snap(Handler, method, Method(..))
import Snap.Snaplet.HandleIt.Header
import Snap.Snaplet.HandleIt.Util(prepend)
import Snap.Snaplet.Heist(HasHeist(..), cRender)
import Data.ByteString.Char8
import Control.Monad.State(runState)
import Data.Monoid((<>))

-- | restfulToFunction takes a Restful data constructor and
-- returns it's associated function
restfulToFunction :: (HasHeist b, Handling a) => Restful -> a -> Handler b c ()
restfulToFunction IndexR   = indexH
restfulToFunction ShowR    = showH
restfulToFunction NewR     = newH
restfulToFunction EditR    = editH
restfulToFunction CreateR  = createH
restfulToFunction UpdateR  = updateH
restfulToFunction DestroyR = destroyH

-- | restfulToUrl takes a Restful data constructor and
-- returns it's associated path
restfulToURL :: Handling a => Restful -> a -> ByteString
restfulToURL IndexR   = ((<>) "/")               . handleName
restfulToURL ShowR    = (prepend "/:id")         . ((<>) "/") . handleName
restfulToURL NewR     = (prepend "/new")         . ((<>) "/") . handleName
restfulToURL EditR    = (prepend "/:id/edit")    . ((<>) "/") . handleName
restfulToURL CreateR  = (prepend "/create")      . ((<>) "/") . handleName
restfulToURL UpdateR  = (prepend "/:id/update")  . ((<>) "/") . handleName
restfulToURL DestroyR = (prepend "/:id/destroy") . ((<>) "/") . handleName

-- | routePath takes the Handler and Restful action and creates the route
routePath :: HasHeist b => (Restful, HDL) -> (ByteString, Handler b c ())
routePath (rest, HDL h) = let url = restfulToURL rest h in
    (url, renderPath rest h url)

-- | renderPath builds the pathname and renders it if necessary
renderPath :: (Handling a, HasHeist b) => Restful -> a -> ByteString -> Handler b c ()
renderPath IndexR a url = do
    restfulToFunction IndexR a
    cRender $ url <> "/index"

renderPath ShowR    a _ = do
    restfulToFunction ShowR a
    cRender $ handleName a <> "/show"

renderPath NewR    a _ = do
    restfulToFunction NewR a
    cRender $ handleName a <> "/new"

renderPath EditR    a _ = do
    restfulToFunction EditR a
    cRender $ handleName a <> "/edit"

renderPath CreateR  a _   = restfulToFunction CreateR  a
renderPath DestroyR a _   = restfulToFunction DestroyR a
renderPath UpdateR  a _   = restfulToFunction UpdateR  a
renderPath restful  a url = restfulToFunction restful  a >> cRender url

-- | Routing takes the routes that need to be added out of state
routing :: Router () -> Routing
routing a = snd $ runState a []
