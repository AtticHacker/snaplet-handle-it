module Snap.Snaplet.HandleIt where

import Snap.Snaplet(SnapletInit, makeSnaplet)
import Snap.Snaplet.HandleIt.Header(HandleIt(..), HDL(..))
import qualified Data.ByteString.Char8 as BS
import Paths_snaplet_handle_it


-- | Initiates Handle-it
handleItInit :: [HDL] -> SnapletInit a (HandleIt [(BS.ByteString, HDL)])
handleItInit routes = makeSnaplet "handleIt" "" Nothing $ do
    return $ HandleIt []
