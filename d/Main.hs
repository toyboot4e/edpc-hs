#!/usr/bin/env stack
{- stack script --resolver lts-16.31 --package array --package bytestring --package vector -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- {{{ Imports

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

-- }}}

ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

ints2 :: IO (Int, Int)
ints2 = (\[a, b] -> (a, b)) <$> ints

{-# INLINE foldForVG #-}
foldForVG :: (VG.Vector v a) => b -> v a -> (b -> a -> b) -> b
foldForVG !s0 !xs !m = VG.foldl' m s0 xs

main :: IO ()
main = do
  (!nItems, !wLimit) <- ints2
  !input <- VU.replicateM nItems ints2

  let !undef = minBound :: Int
  let !s0 = VU.generate (succ wLimit) (\case 0 -> 0; _ -> undef)

  print . VU.maximum . foldForVG s0 input $ \vec (!w, !v) ->
    let f i v1 =
          let !v2 = maybe undef (+ v) $ vec VU.!? (i - w)
           in max v1 v2
     in VU.imap f vec
