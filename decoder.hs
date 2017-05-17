
mostly_decode :: [Bool] -> [Bool]
mostly_decode bits = unXor (take 1 bits) ((take 1 bits) ++ )

unXor :: Int -> [Bool] -> [Bool]
unXor offset bits = unXor2 (take offset bits) (
    where leadingBits = take offset bits

unXor2 :: [Bool] -> [Bool] -> [Bool]
unXor2 _ [] = []
unXor2 (q:queue) (e:encodedBits) = newBit : unXor2 (queue ++ [newBit]) encodedBits
    where newBit = q /= e

main = return ()

-- decoded[4] = encoded[4] ^ decoded[2]

{-
if (c > 0) a[c] ^= a[c-1]; // on every char except the first one, XOR it with its predecessor
        a[c] ^= a[c] >> 3; // then XOR the character with itself right-shifted by 3
        a[c] ^= a[c] >> 2; // then XOR it with itself right-shifted by 2
        a[c] ^= a[c] >> 1; // then XOR itself right-shifted by 1
        printf("%02x", (unsigned int)(a[c])); // then print each char as (hex?)
-}
