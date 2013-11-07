module Snap.Snaplet.HandleIt.Header where

import Snap.Snaplet(Handler)
import Snap.Snaplet.Heist(HasHeist)
import Heist(HeistT)

import Control.Monad.State(State)

import Data.Char(toLower)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Text.XmlHtml(Node)

-- | Types for routing
type Routing  = [(Restful, HDL)]
type Router a = State Routing a

-- | HandleIt Data type for your app
data HandleIt a where
    HandleIt :: a -> HandleIt [(Restful, HDL)]

-- | Wrapping for Handlings
data HDL = forall s. Handling s => HDL s

-- | Restful actions Data Type
data Restful = IndexR  | ShowR   | NewR     | EditR
             | CreateR | UpdateR | DestroyR | DefaultR
             deriving (Show, Eq)

-- | Class for Handler types
class Show a => Handling a where
    handleName :: a -> BS.ByteString
    handleName = BS.takeWhile (/=' ') . BS.pack . map toLower . show

    handleSplices :: a -> [(T.Text, HeistT n (Handler b c) [Node])]
    handleSplices _ = []

    indexH   :: HasHeist b => a -> Handler b c ()
    showH    :: HasHeist b => a -> Handler b c ()
    newH     :: HasHeist b => a -> Handler b c ()
    editH    :: HasHeist b => a -> Handler b c ()
    createH  :: HasHeist b => a -> Handler b c ()
    updateH  :: HasHeist b => a -> Handler b c ()
    destroyH :: HasHeist b => a -> Handler b c ()
    indexH   _ = return ()
    showH    _ = return ()
    newH     _ = return ()
    editH    _ = return ()
    createH  _ = return ()
    updateH  _ = return ()
    destroyH _ = return ()
