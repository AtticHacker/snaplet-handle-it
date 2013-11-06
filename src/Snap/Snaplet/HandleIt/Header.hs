module Snap.Snaplet.HandleIt.Header where

import Snap.Snaplet(Handler)
import qualified Data.ByteString.Char8 as BS
import Snap.Snaplet.Heist(HasHeist, render)
import Control.Monad.State(State)
import Data.Char

-- | Types for routing
type Routing  = [(Restful, HDL)]
type Router a = State Routing a

-- | HandleIt Data type for your app
data HandleIt a where
    HandleIt :: a -> HandleIt [(Restful, HDL)]

-- | Wrapping for Handlings
data HDL = forall s. Handling s => HDL s

-- | Restful actions Data Type
data Restful = RIndex  | RShow   | RNew     | REdit
             | RCreate | RUpdate | RDestroy | RDefault

-- | Class for Handler types
class Show a => Handling a where
    handleName :: a -> BS.ByteString
    handleName = BS.takeWhile (/=' ') . BS.pack . map toLower . show

    hIndex   :: HasHeist b => a -> Handler b c ()
    hShow    :: HasHeist b => a -> Handler b c ()
    hNew     :: HasHeist b => a -> Handler b c ()
    hEdit    :: HasHeist b => a -> Handler b c ()
    hCreate  :: HasHeist b => a -> Handler b c ()
    hUpdate  :: HasHeist b => a -> Handler b c ()
    hDestroy :: HasHeist b => a -> Handler b c ()

    hIndex   _ = render "404"
    hShow    _ = render "404"
    hNew     _ = render "404"
    hEdit    _ = render "404"
    hCreate  _ = render "404"
    hUpdate  _ = render "404"
    hDestroy _ = render "404"
