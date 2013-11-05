module Snap.Snaplet.HandleIt where

import Snap.Snaplet(Handler, SnapletInit)
import qualified Data.ByteString.Char8 as BS
import Snap.Snaplet.Heist(HasHeist, render)
import Paths_snaplet_handle_it

-- | HandleIt Data type for your app
data HandleIt = HandleIt

-- | Data type with all the routes
data Routes a where
    Routes :: Handling a => Routes [a]

-- | Class for Handler types
class Show a => Handling a where
    handleName :: a -> String
    handleName = takeWhile (/=' ') . show

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

-- | Initiates Handle-it
initHandleIt :: Int -> SnapletInit a HandleIt
initHandleIt = undefined
