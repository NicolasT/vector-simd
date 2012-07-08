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
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts #-}
{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.SIMD.Mutable (
    MVector(..), IOVector, STVector,
    AlignedToAtLeast, AlignedToAtLeast2, AlignedToAtLeast3, AlignedToAtLeast4,
    Alignment, A1, A2, A4, A8, A16, A32,
    new,
    unsafeWith
) where

import qualified Data.Vector.Generic.Mutable as G

import qualified Data.Word as W

import Foreign.Storable hiding (alignment)
import Foreign.ForeignPtr
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (advancePtr, copyArray, moveArray)

import Control.Monad.Primitive

import Data.Typeable (Typeable)

import GHC.Ptr (Ptr(..))

import qualified Data.Primitive.ByteArray as BA
import qualified Data.Primitive.Types as PT

import Data.Vector.Storable.Internal (getPtr, updPtr)

data MVector o s a = MVector {-# UNPACK #-} !Int
                             {-# UNPACK #-} !(ForeignPtr a)
  deriving (Typeable)

type IOVector o = MVector o RealWorld
type STVector o s = MVector o s

class Alignment o where
    alignment :: o -> Int

data One
data Twice n

class Alignment a => AlignedToAtLeast n a
instance AlignedToAtLeast One One
instance Alignment a => AlignedToAtLeast One (Twice a)
instance AlignedToAtLeast n a => AlignedToAtLeast (Twice n) (Twice a)

class (AlignedToAtLeast n a, AlignedToAtLeast n b) => AlignedToAtLeast2 n a b
instance (AlignedToAtLeast n a, AlignedToAtLeast n b) => AlignedToAtLeast2 n a b
class (AlignedToAtLeast n a, AlignedToAtLeast n b, AlignedToAtLeast n c) => AlignedToAtLeast3 n a b c
instance (AlignedToAtLeast n a, AlignedToAtLeast n b, AlignedToAtLeast n c) => AlignedToAtLeast3 n a b c
class (AlignedToAtLeast n a, AlignedToAtLeast n b, AlignedToAtLeast n c, AlignedToAtLeast n d) => AlignedToAtLeast4 n a b c d
instance (AlignedToAtLeast n a, AlignedToAtLeast n b, AlignedToAtLeast n c, AlignedToAtLeast n d) => AlignedToAtLeast4 n a b c d

type A1 = One
type A2 = Twice A1
type A4 = Twice A2
type A8 = Twice A4
type A16 = Twice A8
type A32 = Twice A16

instance Alignment A1 where
    alignment _ = 1
instance Alignment a => Alignment (Twice a) where
    alignment _ = 2 * alignment (undefined :: a)


instance (Storable a, Alignment o) => G.MVector (MVector o) a where
    basicLength (MVector n _) = n
    {-# INLINE basicLength #-}

    -- TODO Validate alignment is retained
    basicUnsafeSlice j m (MVector _ fp) = MVector m (updPtr (`advancePtr` j) fp)
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
    {-# SPECIALIZE basicUnsafeNew ::
            (Storable a, PrimMonad m) => Int -> m (MVector A16 (PrimState m) a) #-}

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

mallocVector :: (Storable a, Alignment o) => Int -> o -> IO (ForeignPtr a)
mallocVector = doMalloc undefined
  where
    doMalloc :: (Storable b, Alignment p) => b -> Int -> p -> IO (ForeignPtr b)
    doMalloc b !l a = do
        if bytes `rem` align /= 0
            then error "Data.Vector.SIMD.Mutable.mallocVector: Unaligned length"
            else do
                !ba <- BA.newAlignedPinnedByteArray bytes align
                let !(PT.Addr addr) = BA.mutableByteArrayContents ba
                    !ptr = Ptr addr

                newForeignPtr_ ptr
      where
        bytes :: Int
        !bytes = sizeOf b * l
        {-# INLINE bytes #-}
        align :: Int
        !align = alignment a
        {-# INLINE align #-}
    {-# INLINE doMalloc #-}
    {-# SPECIALIZE doMalloc :: W.Word8 -> Int -> A16 -> IO (ForeignPtr W.Word8) #-}
{-# INLINE mallocVector #-}
{-# SPECIALIZE mallocVector :: Int -> A16 -> IO (ForeignPtr W.Word8) #-}

new :: (PrimMonad m, Storable a, Alignment o) => Int -> m (MVector o (PrimState m) a)
new = G.new
{-# INLINE new #-}

unsafeWith :: (Storable a, Alignment o) => IOVector o a -> (Ptr a -> IO b) -> IO b
unsafeWith (MVector _ fp) = withForeignPtr fp
{-# INLINE unsafeWith #-}
