module Snap.Snaplet.HandleIt.Internal.Router where

import Snap(Handler, method, Method(..))
import Snap.Snaplet.HandleIt.Header
import Snap.Snaplet.HandleIt.Util(prepend)
import Heist.Interpreted(bindSplices)
import Snap.Snaplet.Heist(HasHeist(..), render, heistLocal)
import qualified Data.ByteString.Char8 as BS

restfulToFunction :: (HasHeist b, Handling a) => Restful -> a -> Handler b c ()
restfulToFunction IndexR   = indexH
restfulToFunction ShowR    = showH
restfulToFunction NewR     = newH
restfulToFunction EditR    = editH
restfulToFunction CreateR  = createH
restfulToFunction UpdateR  = updateH
restfulToFunction DestroyR = destroyH

restfulToURL :: Handling a => Restful -> a -> BS.ByteString
restfulToURL IndexR   = (BS.append "/")          . handleName
restfulToURL ShowR    = (prepend "/:id")         . (BS.append "/") . handleName
restfulToURL NewR     = (prepend "/new")         . (BS.append "/") . handleName
restfulToURL EditR    = (prepend "/:id/edit")    . (BS.append "/") . handleName
restfulToURL CreateR  = (prepend "/create")      . (BS.append "/") . handleName
restfulToURL UpdateR  = (prepend "/:id/update")  . (BS.append "/") . handleName
restfulToURL DestroyR = (prepend "/:id/destroy") . (BS.append "/") . handleName

routePath :: HasHeist b => (Restful, HDL) -> (BS.ByteString, Handler b c ())
routePath (rest, HDL h) = let url = restfulToURL rest h in
    (url, wrapSplice h $ renderPath rest h url)

wrapSplice :: (HasHeist b, Handling a) => a -> Handler b c () -> Handler b c ()
wrapSplice h a = method GET $ heistLocal (bindSplices $ handleSplices h) $ a

renderPath :: (Handling a, HasHeist b) => Restful
              -> a -> BS.ByteString -> Handler b c ()

renderPath IndexR a url = do
    restfulToFunction IndexR a
    render $ url `BS.append` "/index"

renderPath ShowR    a _ = do
    restfulToFunction ShowR a
    render $ handleName a `BS.append` "/show"

renderPath CreateR  a _   = restfulToFunction CreateR  a
renderPath DestroyR a _   = restfulToFunction DestroyR a
renderPath UpdateR  a _   = restfulToFunction UpdateR  a
renderPath restful  a url = restfulToFunction restful  a >> render url
