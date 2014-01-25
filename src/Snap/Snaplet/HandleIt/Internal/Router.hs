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
restfulToFunction :: (HasHeist b, Handling a b) => Restful -> a -> Handler b b ()
restfulToFunction IndexR   = indexH
restfulToFunction ShowR    = showH
restfulToFunction NewR     = newH
restfulToFunction EditR    = editH
restfulToFunction CreateR  = createH
restfulToFunction UpdateR  = updateH
restfulToFunction DestroyR = destroyH

-- | restfulToUrl takes a Restful data constructor and
-- returns it's associated path
restfulToURL :: (HasHeist b, Handling a b) => Restful -> a -> b -> ByteString
restfulToURL IndexR   a b = ((<>) "/")                            $ handleName a b
restfulToURL ShowR    a b = (prepend "/:id")         . ((<>) "/") $ handleName a b
restfulToURL NewR     a b = (prepend "/new")         . ((<>) "/") $ handleName a b
restfulToURL EditR    a b = (prepend "/:id/edit")    . ((<>) "/") $ handleName a b
restfulToURL CreateR  a b = (prepend "/create")      . ((<>) "/") $ handleName a b
restfulToURL UpdateR  a b = (prepend "/:id/update")  . ((<>) "/") $ handleName a b
restfulToURL DestroyR a b = (prepend "/:id/destroy") . ((<>) "/") $ handleName a b

-- | routePath takes the Handler and Restful action and creates the route
routePath :: (Handling a b, HasHeist b) => (Restful, HDL a b) -> (ByteString, Handler b b ())
routePath (rest, HDL a b) = let url = restfulToURL rest a b in
    (url, renderPath rest a b url)

-- | renderPath builds the pathname and renders it if necessary
renderPath :: (Handling a b, HasHeist b) => Restful -> a -> b -> ByteString -> Handler b b ()
renderPath IndexR a b url = do
    restfulToFunction IndexR a
    cRender $ url <> "/index"

renderPath ShowR    a b _ = do
    restfulToFunction ShowR a
    cRender $ handleName a b <> "/show"

renderPath NewR    a b _ = do
    restfulToFunction NewR a
    cRender $ handleName a b <> "/new"

renderPath EditR    a b _ = do
    restfulToFunction EditR a
    cRender $ handleName a b <> "/edit"

renderPath CreateR  a b _   = restfulToFunction CreateR  a
renderPath DestroyR a b _   = restfulToFunction DestroyR a
renderPath UpdateR  a b _   = restfulToFunction UpdateR  a
renderPath restful  a b url = restfulToFunction restful  a >> cRender url

-- | Routing takes the routes that need to be added out of state
routing :: Router () b -> Routing () b
routing a = snd $ runState a []
