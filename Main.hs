{-# LANGUAGE FlexibleContexts #-}
import Data.Bits
import System.Random
import Data.Time.Clock.POSIX
import System.IO.Unsafe

powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

main = do 
    x <- getLine
    t <- getPOSIXTime
    let gen = mkStdGen (round t)
    print $ generate (read x :: Int) gen

-- dividor b = go b 0
--     where go b n = if b `mod` 2 == 0 then go (b `div` 2) (n + 1) else (b, n)  

-- millerRabinTest n ns
--     | null l = True
--     | 
--     where
--         np = n - 1
--         (b, r) = dividor np
--         l = dropWhile (n<) a  


-- isBasicPrime :: Integer -> Maybe Bool
-- isBasicPrime v  
--     | n == 1 = Just False
--     | n == 2 || n == 3 || n == 5 = Just True
--     | even n || n `mod` 3 == 0 || n `mod` 5 == 0 = Just False
--     | n < 49 = Just True
--     | otherwise = Nothing
--     where n = abs v

-- isPrime v bits = 
--     if bits < 64
--         then millerRabinTest n [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
--         else let a = [i + 2 | i <- [0..t]] in millerRabinTest n a
--     where 
--         n = abs v
--         logN = 2 * bits
--         t = 2 * logN^2

isPrime :: Integer -> Bool
isPrime n = unsafePerformIO (isMillerRabinPrime 100 n)
 
 
isMillerRabinPrime k n
   | even n    = return (n==2)
   | n < 100   = return (n `elem` primesTo100)
   | otherwise = do ws <- witnesses k n
                    return $ and [test n (pred n) evens (head odds) a | a <- ws]
  where
    (evens,odds) = span even (iterate (`div` 2) (pred n))
 
test n n_1 evens d a = x `elem` [1,n_1] || n_1 `elem` powers 
  where
    x = powerMod n a d
    powers = map (powerMod n a) evens
 
witnesses k n 
  | n < 9080191         = return [31,73]
  | n < 4759123141      = return [2,7,61]
  | n < 3474749660383   = return [2,3,5,7,11,13]
  | n < 341550071728321 = return [2,3,5,7,11,13,17]
  | otherwise           = do g <- newStdGen
                             return $ take k (randomRs (2,n-1) g)
 
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

powerMod :: Integer -> Integer -> Integer -> Integer
powerMod m x n  = f (n - 1) x x `rem` m 
  where
  f d a y = if d==0 then y else g d a y 
  g i b y | even i    = g (i `quot` 2) (b*b `rem` m) y
          | otherwise = f (i-1) b (b*y `rem` m)

-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv a m = mkPos i
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x
 
-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

-- generate p and q such that λ(n) = lcm(p − 1, q − 1) is coprime with e and |p-q| >= 2^(keysize/2 - 100)
generate :: Int -> StdGen -> (Integer, Integer, Integer, Integer, Integer)
generate keysize gen = 
    if (gcd e lambda /= 1 || abs (p - q) `shiftR` (keysize `div` 2 - 100) == 0 ) && p /= q && e `modInv` lambda /= 1
        then (p, q, p * q, e , e `modInv` lambda) 
        else generate keysize ng2
    where
        e = 65537
        (p, ng) = randomPrime (keysize `div` 2) gen
        (q, ng2) = randomPrime (keysize `div` 2) ng
        lambda = lcm (p-1) (q-1)

randomPrime bits gen = let (p, ng) = uniformR (min, max) gen 
                       in if isPrime p 
                           then (p, ng) 
                           else randomPrime bits ng
    where 
        min = 6074001000 `shiftL` (bits - 33) -- min ≈ √2 × 2^(bits - 1)
        max = 1 `shiftL` bits - 1  -- max = 2^(bits) - 1

encrypt m n e = powm m e n 1 

decrypt c d n = powm c d n 1

-- 200 564598 897894 373221 102549 839740 574051 (39 digits) = 13 664185 055464 474081 × 14 678123 728841 490371

-- 101746 537045 016640 506811 166362 834058 820797 496713 290834 026507 787806 857335 007423 (78 digits) = 306 947996 659193 420977 182415 796834 272123 (39 digits) × 331 478094 505977 688189 298223 887261 801101 (39 digits)