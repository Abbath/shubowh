{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
import Data.Bits
import System.Random
import Data.Time.Clock.POSIX
import System.IO.Unsafe
import Math.NumberTheory.Primes.Testing (isPrime)
import Math.NumberTheory.Powers.Modular
import Debug.Trace
import Text.Format

powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

main :: IO ()
main = do 
    x <- getLine
    t <- getPOSIXTime
    let gen = mkStdGen (round t)
    let (p,q,n,e,d) = generate (read x :: Int) gen
    let enc = map (encrypt e n) [65]
    let dec = map (decrypt d n) enc
    putStrLn $ format "p:{}\nq:{}\nn:{}\ne:{}\nd:{}" p q n e d
    print enc
    print dec

inverse :: Integral p => p -> p -> p
inverse a n = go 0 n 1 a
  where 
    go t r newt newr = if newr == 0
      then decide r t 
      else let q = r `div` newr in go newt newr (t-q*newt) (t-q*newr)
    decide r t = if r > 1
      then error "Dupa!"
      else if t < 0 then t + n else t  

-- generate p and q such that λ(n) = lcm(p − 1, q − 1) is coprime with e and |p-q| >= 2^(keysize/2 - 100)
generate :: Int -> StdGen -> (Integer, Integer, Integer, Integer, Integer)
generate keysize gen = 
    if gcd e lambda /= 1 || abs (p - q) `shiftR` (keysize `div` 2 - 100) == 0
        then generate keysize ng2
        else let d = inverse e lambda in (p, q, p * q, e, d) 
    where
        e = 65537
        (p, ng) = randomPrime (keysize `div` 2 - 4) gen
        (q, ng2) = randomPrime (keysize `div` 2 + 4) ng
        lambda = lcm (p-1) (q-1)

randomPrime :: RandomGen t => Int -> t -> (Integer, t)
randomPrime bits gen = let (p, ng) = uniformR (min, max) gen 
                       in if isPrime p 
                           then (p, ng) 
                           else randomPrime bits ng
    where 
        min = 6074001000 `shiftL` (bits - 33) -- min ≈ √2 × 2^(bits - 1)
        max = 1 `shiftL` bits - 1  -- max = 2^(bits) - 1

encrypt :: (Integral a, Integral b) => b -> a -> a -> a
encrypt e n m = powMod m e n

decrypt :: (Integral a, Integral b) => b -> a -> a -> a
decrypt d n c = powMod c d n

-- 200 564598 897894 373221 102549 839740 574051 (39 digits) = 13 664185 055464 474081 × 14 678123 728841 490371

-- 101746 537045 016640 506811 166362 834058 820797 496713 290834 026507 787806 857335 007423 (78 digits) = 306 947996 659193 420977 182415 796834 272123 (39 digits) × 331 478094 505977 688189 298223 887261 801101 (39 digits)