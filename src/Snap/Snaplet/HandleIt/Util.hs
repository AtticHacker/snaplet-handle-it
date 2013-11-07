module Snap.Snaplet.HandleIt.Util(prepend) where
import qualified Data.ByteString.Char8 as BS

prepend :: BS.ByteString -> BS.ByteString -> BS.ByteString
prepend = flip BS.append
