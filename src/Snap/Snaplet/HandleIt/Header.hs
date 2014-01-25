{-# LANGUAGE MultiParamTypeClasses #-}
module Snap.Snaplet.HandleIt.Header where

import Snap.Snaplet(Handler)
import Snap.Snaplet.Heist(HasHeist)
import Heist(HeistT)

import Control.Monad.State(State)

import Data.Char(toLower)
import qualified Data.ByteString.Char8 as BS
import Text.XmlHtml(Node)
import Data.Monoid (mempty)
-- | Types for routing
type Routing a b = [(Restful, HDL a b)]
type Router a b = State (Routing a b) a

-- | HandleIt Data type for your app
data HandleIt a b = HandleIt [(Restful, HDL a b)]

-- | Wrapping for Handlings
data HDL s b = forall s. Handling s b => HDL s b

-- | Restful actions Data Type
data Restful = IndexR  | ShowR   | NewR     | EditR
             | CreateR | UpdateR | DestroyR | DefaultR
             deriving (Show, Eq)

-- | Class for Handler types
class (HasHeist b, Show a) => Handling a b where
    handleName :: a -> b -> BS.ByteString
    handleName a _ = BS.takeWhile (/=' ') . BS.pack . map toLower . show $ a

    indexH   :: a -> Handler b b ()
    showH    :: a -> Handler b b ()
    newH     :: a -> Handler b b ()
    editH    :: a -> Handler b b ()
    createH  :: a -> Handler b b ()
    updateH  :: a -> Handler b b ()
    destroyH :: a -> Handler b b ()
    indexH   _ = return ()
    showH    _ = return ()
    newH     _ = return ()
    editH    _ = return ()
    createH  _ = return ()
    updateH  _ = return ()
    destroyH _ = return ()
