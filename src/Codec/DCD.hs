module Codec.DCD where

import Control.Monad as M (replicateM, liftM, liftM2, sequence)
import Control.Applicative ((<$>), (<*>))
import Data.Binary
import Data.Binary.Get
import Data.Monoid (mconcat)
import Data.Vector.Unboxed as V (Vector(..), zip3, replicateM)
import Data.Binary.IEEE754 (getFloat32le, getFloat64le)
import Data.Word
import Data.ByteString.Lazy (ByteString(..))

data FileHeader = FileHeader {nFrames :: Word32,
                              firstFrame :: Word32,
                              period :: Word32,
                              titles :: [ByteString],
                              nParticles :: Word32} deriving Show

fortranSection'::Get a->Get (Word32, a)
fortranSection' q = do
  begin <- getWord32le
  result <- q
  end <- getWord32le

  let message = mconcat ["Mismatched size boundary in fortran section: left ",
                         show begin ,"; right ", show end]
  if begin /= end then fail message
    else return (begin, result)

fortranSection::Get a->Get a
fortranSection = liftM snd . fortranSection'

readFileHeader::Get FileHeader
readFileHeader = do

  (nFrames, firstFrame, period, timesteps) <- fortranSection $ do
    -- c0, c1, c2, c3 should spell 'CORD'
    --  [c0, c1, c2, c3] <- replicateM 4 getWord8
    skip 4 -- skip 'CORD' magic
    [nFrames, firstFrame, period, timesteps] <- M.replicateM 4 getWord32le
    skip $ 4*16 -- Misc. fields we don't care about
    return (nFrames, firstFrame, period, timesteps)

  titleSectionSize <- (subtract 4) . fromIntegral <$> lookAhead getWord32le
  titles <- fortranSection $ do
    -- Title size in bytes
    numTitles <- fromIntegral <$> getWord32le
    -- Number of title strings
    let titleSize = quot titleSectionSize numTitles
    titles <- M.replicateM numTitles $ getLazyByteString (fromIntegral titleSize)
    return titles

  nParticles <- fortranSection getWord32le

  return $ FileHeader nFrames firstFrame period titles nParticles

data Box = Box {boxA :: Double,
                boxB :: Double,
                boxC :: Double,
                boxAlpha :: Double,
                boxBeta :: Double,
                boxGamma :: Double} deriving Show

-- | swap around members of a box to be in the right order
unpackBox [a, gamma, b, beta, alpha, c] = Box a b c alpha beta gamma

readFrameHeader::Get Box
readFrameHeader = fortranSection $ unpackBox <$> M.replicateM 6 getFloat64le

type Positions = Vector (Float, Float, Float)

readFramePositions::Int->Get Positions
readFramePositions nParticles = do
  [xs, ys, zs] <- M.replicateM 3 $ fortranSection $  V.replicateM nParticles getFloat32le
  return $ V.zip3 xs ys zs

readFrame n = liftM2 (,) readFrameHeader (readFramePositions n)

readFrames n = do
  done <- isEmpty
  if done then return []
    else liftM2 (:) (readFrame n) (readFrames n)

readFile::ByteString->(Word32, [(Box, Positions)])
readFile contents = (nFrames header, positions)
  where
    (header, positions) = runGet readFile' (contents)

readFile' = do
  header <- readFileHeader
  frames <- readFrames (fromIntegral . nParticles $ header)
  return (header, frames)
