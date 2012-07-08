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

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Vector.SIMD.Algorithms (
    unsafeXorSSE42
) where

import Data.Word (Word8)

import Foreign (unsafePerformIO)
import Foreign.Storable (Storable, sizeOf)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr)

import qualified Data.Vector.SIMD as SV
import qualified Data.Vector.SIMD.Mutable as MSV

foreign import ccall unsafe "_vector_simd_xor_sse42" _c_xor_sse42
    :: Ptr a -> Ptr a -> Ptr a -> CSize -> IO ()

unsafeXorSSE42 :: (Storable a,
    SV.AlignedToAtLeast SV.A16 (o1, o2, o3),
    SV.Alignment o1, SV.Alignment o2, SV.Alignment o3) =>
    SV.Vector o1 a -> SV.Vector o2 a -> SV.Vector o3 a
unsafeXorSSE42 !v1 !v2 = unsafePerformIO $ helper (undefined :: a) v1 v2
  where
    helper :: (Storable b,
        SV.AlignedToAtLeast SV.A16 (o4, o5, o6),
        SV.Alignment o4, SV.Alignment o5, SV.Alignment o6) =>
        b -> SV.Vector o4 b -> SV.Vector o5 b -> IO (SV.Vector o6 b)
    helper o !a !b = do
        let !l = SV.length a
            !bl = SV.length a * sizeOf o

        SV.unsafeWith a $ \(!pa) ->
            SV.unsafeWith b $ \(!pb) -> do
                !n <- MSV.new l

                MSV.unsafeWith n $ \(!pn) -> do
                    _c_xor_sse42 pa pb pn (fromIntegral bl)

                    SV.unsafeFreeze n
    {-# INLINE helper #-}
    {-# SPECIALIZE helper ::
            Word8 -> SV.Vector SV.A16 Word8 -> SV.Vector SV.A16 Word8 -> IO (SV.Vector SV.A16 Word8) #-}
{-# INLINE unsafeXorSSE42 #-}
{-# SPECIALIZE unsafeXorSSE42 ::
        SV.Vector SV.A16 Word8 -> SV.Vector SV.A16 Word8 -> SV.Vector SV.A16 Word8 #-}
