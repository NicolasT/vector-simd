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

{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Data.Vector.SIMD (
    Vector, A8, A16,
    fromListN,
    length,
    unsafeWith,
    unsafeFreeze,
    zipWith,
    toList
) where

import qualified Data.Vector.Generic as G
import Data.Vector.Storable.Internal (updPtr)

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (Storable, peekElemOff)
import Foreign.Marshal.Array (advancePtr, copyArray)

import Data.Typeable (Typeable)

import Control.Monad.Primitive

import Data.Vector.SIMD.Mutable (MVector(..), Alignment, A8, A16)

import Prelude hiding (length, zipWith)

data Vector o a = Vector {-# UNPACK #-} !Int
                         {-# UNPACK #-} !(ForeignPtr a)
  deriving (Typeable)

type instance G.Mutable (Vector o) = MVector o

instance (Storable a, Alignment o) => G.Vector (Vector o) a where
    basicUnsafeFreeze (MVector n fp) = return $ Vector n fp
    {-# INLINE basicUnsafeFreeze #-}

    basicUnsafeThaw (Vector n fp) = return $ MVector n fp
    {-# INLINE basicUnsafeThaw #-}

    basicLength (Vector n _) = n
    {-# INLINE basicLength #-}

    basicUnsafeSlice i n (Vector _ fp) = Vector n (updPtr (`advancePtr` i) fp)
    {-# INLINE basicUnsafeSlice #-}

    basicUnsafeIndexM (Vector _ fp) i = return
                                      . unsafeInlineIO
                                      $ withForeignPtr fp $ \p ->
                                        peekElemOff p i
    {-# INLINE basicUnsafeIndexM #-}

    basicUnsafeCopy (MVector n fp) (Vector _ fq) =
        unsafePrimToPrim $ withForeignPtr fp $ \p ->
        withForeignPtr fq $ \q ->
        copyArray p q n
    {-# INLINE basicUnsafeCopy #-}

    elemseq _ = seq
    {-# INLINE elemseq #-}

fromListN :: (Storable a, Alignment o) => Int -> [a] -> Vector o a
fromListN = G.fromListN
{-# INLINE fromListN #-}

zipWith :: (Storable a, Storable b, Storable c, Alignment o)
        => (a -> b -> c) -> Vector o a -> Vector o b -> Vector o c
zipWith = G.zipWith
{-# INLINE zipWith #-}

length :: (Storable a, Alignment o) => Vector o a -> Int
length = G.length
{-# INLINE length #-}

unsafeWith :: (Storable a, Alignment o) => Vector o a -> (Ptr a -> IO b) -> IO b
unsafeWith (Vector n fp) = withForeignPtr fp
{-# INLINE unsafeWith #-}

unsafeFreeze :: (Storable a, PrimMonad m, Alignment o) => MVector o (PrimState m) a -> m (Vector o a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = G.unsafeFreeze

toList :: (Storable a, Alignment o) => Vector o a -> [a]
toList = G.toList
{-# INLINE toList #-}
