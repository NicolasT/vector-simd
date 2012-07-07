{- vector-simd - Combining the vector API with SIMD intrinsics
 -
 - Copyright (c) 2012 Nicolas Trangez
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU Lesser General Public License as published
 - by the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This package is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU Lesser General Public License for more details.
 -
 - You should have received a copy of the GNU Lesser General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields -O3 #-}

module Main (
    main
) where

import Data.Bits (xor)
import Data.Word (Word8)

import qualified Criterion.Main as CM
import qualified Criterion.Config as CC

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.SIMD as MV

import qualified Data.Vector.SIMD.Algorithms as MVA

main :: IO ()
main = do
    -- Sanity check
    let uv = benchUV uv1024a uv1024b
        sv = benchSV sv1024a sv1024b
        mv = benchMV mv1024a mv1024b
        mva = benchMVA mv1024a mv1024b

    putStr "Checking sanity / correctness... "
    assert "SV" $ SV.toList sv == UV.toList uv
    assert "MV" $ MV.toList mv == UV.toList uv
    assert "MVA" $ MV.toList mva == UV.toList uv
    putStrLn "looks OK!"

    CM.defaultMainWith CC.defaultConfig (return ()) [
        CM.bgroup "XOR Unboxed Vector" [
            CM.bench "benchUV 1024" $ CM.whnf (benchUV uv1024a) uv1024b,
            CM.bench "benchUV 4096" $ CM.whnf (benchUV uv4096a) uv4096b
        ],
        CM.bgroup "XOR Storable Vector" [
            CM.bench "benchSV 1024" $ CM.whnf (benchSV sv1024a) sv1024b,
            CM.bench "benchSV 4096" $ CM.whnf (benchSV sv4096a) sv4096b
        ],
        CM.bgroup "XOR SIMD Vector" [
            CM.bench "benchMV 1024" $ CM.whnf (benchMV mv1024a) mv1024b,
            CM.bench "benchMV 4096" $ CM.whnf (benchMV mv4096a) mv4096b
        ],
        CM.bgroup "XOR SIMD Vector using SSE4.2" [
            CM.bench "benchMVA 1024" $ CM.whnf (benchMVA mv1024a) mv1024b,
            CM.bench "benchMVA 4096" $ CM.whnf (benchMVA mv4096a) mv4096b
        ]
       ]
  where
    uv1024a, uv1024b, uv4096a, uv4096b :: UV.Vector Word8
    !(!uv1024a, !uv1024b) = gen UV.fromListN 1024
    !(!uv4096a, !uv4096b) = gen UV.fromListN 4096

    sv1024a, sv1024b, sv4096a, sv4096b :: SV.Vector Word8
    !(!sv1024a, !sv1024b) = gen SV.fromListN 1024
    !(!sv4096a, !sv4096b) = gen SV.fromListN 4096

    mv1024a, mv1024b, mv4096a, mv4096b :: MV.Vector MV.A32 Word8
    !(!mv1024a, !mv1024b) = gen MV.fromListN 1024
    !(!mv4096a, !mv4096b) = gen MV.fromListN 4096

    gen :: (Int -> [Word8] -> v Word8) -> Int -> (v Word8, v Word8)
    gen f n = (a, b)
      where
        !a = f n $ cycle [0 :: Word8 .. 255]
        !b = f n $ cycle [255 :: Word8, 254 .. 0]
    {-# INLINE gen #-}

    assert :: String -> Bool -> IO ()
    assert m False = error $ "assertion failed: " ++ m
    assert _ True = return ()

benchUV :: UV.Vector Word8 -> UV.Vector Word8 -> UV.Vector Word8
benchUV !a !b = r
  where
    r :: UV.Vector Word8
    !r = UV.zipWith xor a b
    {-# INLINE r #-}

benchSV :: SV.Vector Word8 -> SV.Vector Word8 -> SV.Vector Word8
benchSV !a !b = r
  where
    r :: SV.Vector Word8
    !r = SV.zipWith xor a b
    {-# INLINE r #-}

benchMV :: MV.Vector MV.A32 Word8 -> MV.Vector MV.A32 Word8 -> MV.Vector MV.A32 Word8
benchMV !a !b = r
  where
    r :: MV.Vector MV.A32 Word8
    !r = MV.zipWith xor a b
    {-# INLINE r #-}

-- unsafeXorSSE42 is typed
-- unsafeXorSSE42 :: (Storable a,
--  MSV.AlignedToAtLeast MSV.A16 o1, MSV.Alignment o1,
--  MSV.AlignedToAtLeast MSV.A16 o2, MSV.Alignment o2,
--  MSV.AlignedToAtLeast MSV.A16 o3, MSV.Alignment o3) =>
--  SV.Vector o1 a -> SV.Vector o2 a -> SV.Vector o3 a
-- We restrict the type of benchMVA just a little more, to show how A32 is
-- compatible with the requested A16 (as intended).
-- The result is of a different type though, imagining we only need a 16-byte
-- aligned vector further on for some random reason
benchMVA :: MV.Vector MV.A32 Word8 -> MV.Vector MV.A32 Word8 -> MV.Vector MV.A16 Word8
benchMVA !a !b = r
  where
    r :: MV.Vector MV.A16 Word8
    !r = MVA.unsafeXorSSE42 a b
    {-# INLINE r #-}
