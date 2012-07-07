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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface, EmptyDataDecls #-}

module Data.Vector.SIMD.Mutable (
    MVector(..), IOVector, STVector,
    Alignment, A8, A16,
    new,
    unsafeWith
) where

import qualified Data.Vector.Generic.Mutable as G

import Foreign.Storable hiding (alignment)
import Foreign.ForeignPtr
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Array (advancePtr, copyArray, moveArray)
import Foreign.C.Types (CSize)

import GHC.IO.Exception

import Control.Monad.Primitive

import Data.Typeable (Typeable)

import Control.Monad.ST

import Data.Vector.Storable.Internal (getPtr, updPtr)

data MVector o s a = MVector {-# UNPACK #-} !Int
                             {-# UNPACK #-} !(ForeignPtr a)
  deriving (Typeable)

type IOVector o = MVector o RealWorld
type STVector o s = MVector o s

class Alignment o where
    alignment :: o -> Int

data A8
instance Alignment A8 where
    alignment _ = 8

data A16
instance Alignment A16 where
    alignment _ = 16

instance (Storable a, Alignment o) => G.MVector (MVector o) a where
    basicLength (MVector n _) = n
    {-# INLINE basicLength #-}

    -- TODO Validate alignment is retained
    basicUnsafeSlice j m (MVector n fp) = MVector m (updPtr (`advancePtr` j) fp)
    {-# INLINE basicUnsafeSlice #-}

    -- FIXME: this relies on non-portable pointer comparisons
    basicOverlaps (MVector m fp) (MVector n fq) =
        between p q (q `advancePtr` n) || between q p (p `advancePtr` m)
      where
        between x y z = x >= y && x < z
        p = getPtr fp
        q = getPtr fq
    {-# INLINE basicOverlaps #-}

    basicUnsafeNew n = unsafePrimToPrim $ do
        fp <- mallocVector n (undefined :: o)
        return $ MVector n fp
    {-# INLINE basicUnsafeNew #-}

    basicUnsafeRead (MVector _ fp) i =
        unsafePrimToPrim $ withForeignPtr fp (`peekElemOff` i)
    {-# INLINE basicUnsafeRead #-}

    basicUnsafeWrite (MVector _ fp) i x =
        unsafePrimToPrim $ withForeignPtr fp $ \p -> pokeElemOff p i x
    {-# INLINE basicUnsafeWrite #-}

    basicUnsafeCopy (MVector n fp) (MVector _ fq) =
        unsafePrimToPrim $ withForeignPtr fp $ \p ->
        withForeignPtr fq $ \q ->
        copyArray p q n
    {-# INLINE basicUnsafeCopy #-}

    basicUnsafeMove (MVector n fp) (MVector _ fq) =
        unsafePrimToPrim $ withForeignPtr fp $ \p ->
        withForeignPtr fq $ \q ->
        moveArray p q n
    {-# INLINE basicUnsafeMove #-}

foreign import ccall unsafe "_mm_malloc_stub" _mm_malloc :: CSize -> CSize -> IO (Ptr a)
foreign import ccall unsafe "&_mm_free_stub" finalizer_mm_free :: FinalizerPtr a

mallocVector :: (Storable a, Alignment o) => Int -> o -> IO (ForeignPtr a)
mallocVector = doMalloc undefined
  where
    doMalloc :: (Storable a', Alignment o') => a' -> Int -> o' -> IO (ForeignPtr a')
    doMalloc dummyA size dummyO = do
        ptr <- _mm_malloc (fromIntegral (size * sizeOf dummyA))
                (fromIntegral $ alignment dummyO)
        if ptr == nullPtr
            then ioError (IOError Nothing ResourceExhausted "_mm_malloc" "out of memory"
                            Nothing Nothing)
            else newForeignPtr finalizer_mm_free ptr
    {-# INLINE doMalloc #-}
{-# INLINE mallocVector #-}

new :: (PrimMonad m, Storable a, Alignment o) => Int -> m (MVector o (PrimState m) a)
new = G.new
{-# INLINE new #-}

unsafeWith :: (Storable a, Alignment o) => IOVector o a -> (Ptr a -> IO b) -> IO b
unsafeWith (MVector n fp) = withForeignPtr fp
{-# INLINE unsafeWith #-}
