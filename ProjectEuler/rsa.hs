module RSA where

import Problems1_23 (primes4)
import Data.Char


-------------------------------------------------------------------------------
------------------------------RSA-VERFAHREN------------------------------------
-------------------------------------------------------------------------------

-- E wählt 2 große "unbekannte" Primzahlen p /= q
--
-- E berenchnet n = p * q
--  und phi(n) = (p-1)*(q-1)
--
-- E wählt ein "zufälliges" zu phi(n)
--  teilerfremdes e und berechnet d = e^-1 in Z_phi(n)
--
-- Nachricht (a_1, ..., a_k) ∈ Z_n^k
-- Verschlüsselung: (a_1^e, ..., a_k^e) ∈ Z_n^k
-- Entschlüsselung: (a_1^e^d, ..., a_k^e^d) ∈ Z_n^k
--
-- Satz: a^e^d = a in Z_n
--

-- | PublicKey n, e
data PublicKey a = PublicKey a a
  deriving Eq

-- | PrivateKey phi(n), p, q, d
data PrivateKey a = PrivateKey a a a a
  deriving Eq


instance Show a => Show (PublicKey a) where
  show (PublicKey n e) = "PublicKey n:" ++ show n ++ " e:" ++ show e

instance Show a => Show (PrivateKey a) where
  show (PrivateKey phi p q d) =
    "PrivateKey phi:" ++ show phi ++ " p:" ++ show p ++ " q:" ++ show q ++ " d:" ++ show d


get2Primes :: (Int, Int) -> (Integer, Integer)
get2Primes (a, b) = (primes4 !! a, primes4 !! b)


createKeyPair :: (Int, Int, Int) -> (PrivateKey Integer, PublicKey Integer)
createKeyPair (a, b, c) = (PrivateKey phi p q d, PublicKey n e)
  where
    (p, q) = get2Primes (a, b)
    phi = (p - 1) * (q - 1)
    n = p * q
    d = undefined
    e = undefined



encodeRSA :: Integral a => PublicKey a -> a -> a
encodeRSA (PublicKey n e) a = a ^ e `mod` n

decodeRSA :: (Integral a) => PrivateKey a -> a -> a
decodeRSA (PrivateKey _phi p q d) a = a ^ d `mod` (p * q)



toNumber :: Char -> Int
toNumber c
  | ord c >= 97 = ord c - 96
  | ord c >= 65 = ord c - 64
  | otherwise = ord c

