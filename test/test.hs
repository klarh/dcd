
import Data.ByteString.Lazy as B
import Control.Applicative ((<$>))
import Codec.DCD as DCD
import Data.Binary.Get

main = do
  (nFrames, frames) <- DCD.readFile <$> B.readFile "test.dcd"
  print $ nFrames
  print $ frames !! 999
  return ()
