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

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, ScopedTypeVariables #-}

module Data.Vector.SIMD.Algorithms (
    unsafeXorSSE42
) where

import Data.Word (Word8)

import Foreign (unsafePerformIO)
import Foreign.Storable (sizeOf)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

import Control.Monad.ST (runST)
import Control.Monad.Primitive

import qualified Data.Vector.SIMD as SV
import qualified Data.Vector.SIMD.Mutable as MSV


foreign import ccall unsafe "_vector_simd_xor_sse42" _c_xor_sse42
    :: Ptr a -> Ptr a -> Ptr a -> CSize -> IO ()

unsafeXorSSE42 :: Storable a => SV.Vector SV.A16 a -> SV.Vector SV.A16 a -> SV.Vector SV.A16 a
unsafeXorSSE42 !a !b = unsafePerformIO $ do
    let l = SV.length a
        --bl = l * (sizeOf (undefined :: a))
        bl = l -- TODO HACK!! Assumes a == Word8

    SV.unsafeWith a $ \pa ->
        SV.unsafeWith b $ \pb -> do
            n <- MSV.new l

            MSV.unsafeWith n $ \pn -> do
                _c_xor_sse42 pa pb pn (fromIntegral bl)

                SV.unsafeFreeze n
{-# INLINE unsafeXorSSE42 #-}
