module Main (main) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.SIMD as MV
import Data.Word (Word8)

main = loop 0
  where
    loop n | n > 8192 = putStrLn "OK!"
           | otherwise = do
               print n

               let uv = gen UV.fromListN n
                   sv = gen SV.fromListN n
                   mv = gen MV.fromListN n :: MV.Vector MV.A16 Word8

               putStrLn $ "uv: " ++ show (UV.toList uv)
               putStrLn $ "mv: " ++ show (MV.toList mv)

               if UV.toList uv /= (take n $ cycle [0 :: Word8 .. 255])
                   then print "Oops"
                   else return ()

               if SV.toList sv /= (take n $ cycle [0 :: Word8 .. 255])
                   then print "Oop 2"
                   else return ()

               if MV.toList mv /= (take n $ cycle [0 :: Word8 .. 255])
                   then do
                       print "Oops3"
                       print $ zip3 [0..] (cycle [0 .. 255]) (MV.toList mv)
                       print $ head $ dropWhile (\(a, b, c) -> b == c) $ zip3 [0..] (cycle [0 .. 255]) (MV.toList mv)
                   else return ()

               if (UV.toList uv /= MV.toList mv)
                   then putStrLn $ "Fail at " ++ show n
                   else loop (n + 16)

gen :: (Int -> [Word8] -> v Word8) -> Int -> v Word8
gen f n = a
  where
    !a = f n $ cycle [0 :: Word8 .. 255]
