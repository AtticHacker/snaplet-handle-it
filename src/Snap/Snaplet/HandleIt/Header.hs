module Snap.Snaplet.HandleIt.Header where

import Snap.Snaplet(Handler)
import qualified Data.ByteString.Char8 as BS
import Snap.Snaplet.Heist(HasHeist, render)

-- | HandleIt Data type for your app
data HandleIt a where
    HandleIt :: a -> HandleIt [(BS.ByteString, HDL)]

-- | Wrapping for Handlings
data HDL = forall s. Handling s => HDL s

-- | Restful actions Data Type
data Restful = RIndex  | RShow   | RNew    | REdit
             | RCreate | RUpdate | RDelete | RDefault

-- | Class for Handler types
class Show a => Handling a where
    handleName :: a -> BS.ByteString
    handleName = BS.takeWhile (/=' ') . BS.pack . show

    indexH   :: HasHeist b => a -> Handler b c ()
    showH    :: HasHeist b => a -> Handler b c ()
    newH     :: HasHeist b => a -> Handler b c ()
    editH    :: HasHeist b => a -> Handler b c ()
    saveH    :: HasHeist b => a -> Handler b c ()
    updateH  :: HasHeist b => a -> Handler b c ()
    destroyH :: HasHeist b => a -> Handler b c ()
    customH  :: HasHeist b => a -> [(BS.ByteString, Handler b c ())]

    indexH   _ = render "404"
    showH    _ = render "404"
    newH     _ = render "404"
    editH    _ = render "404"
    saveH    _ = render "404"
    updateH  _ = render "404"
    destroyH _ = render "404"
    customH  _ = []
