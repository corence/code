
type Bit = Bool
type Byte = [Bit]

display = show . map bitToChar
    where bitToChar False = '0'
          bitToChar True = '1'
    
main = putStrLn $ output
    where encoded_hex = "642f54096b24"
          encoded_bytes = unhex encoded_hex
          decoded_bytes_1 = map decode_byte encoded_bytes
          decoded_bytes_2 = mingle encoded_bytes decoded_bytes_1
          output = hex decoded_bytes_2
    
decode_byte :: Byte -> Byte
decode_byte = (decode_with_offset 1) . (decode_with_offset 2) . (decode_with_offset 3)

decode_with_offset :: Int -> Byte -> Byte
decode_with_offset offset byte = reverse $ decode_with_modifiers offset (drop offset byte) (reverse $ take offset byte)
    --where decode_with_modifiers [] results = results
          --decode_with_modifiers (mod:modifiers) results = decode_with_modifiers modifiers (mod /= (results !! (offset - 1)))
          
decode_with_modifiers :: Int -> Byte -> Byte -> Byte
decode_with_modifiers _ [] results = results
decode_with_modifiers offset (mod:modifiers) results = decode_with_modifiers offset modifiers ((mod /= (results !! (offset - 1))) : results)

{-
decode_with_offset :: Int -> Byte -> Byte
decode_with_offset offset byte = take offset byte ++ d (drop offset byte) (replicate offset True)
    where d [] result = reverse result
          d (b:bytes) result = b /= result !! offset

dbyte :: Int -> Int -> Byte -> Byte -> Byte
dbyte offset index input result = error "wtf"
-}

{-
so encode `11111111` -> bitshifted  is `01111111` -> xor’d is `10000000` — this fits the pattern (`0` to repeat a digit)

[4:27] 
decoding `a[c] ^= a[c] >> 2` will be trickier

[4:32] 
`11101100` -> bitshifted is `00111011` -> xor’d is `11010111`
first 2 digits are equal to the decoded bits
3rd bit is `0`, this means 1st and 3rd bits are the same
4th bit is `1`, this means 2nd and 4th bits are different
5th bit is `0`, this means 3rd and 5th bits are the same (1)
6th bit is `1`, this means 4th and 6th bits are different 
so
3rd bit is (xord[1] != unxord[3]) (???)
etc
-}

hex :: [Byte] -> String
hex string = error "hex"

unhex :: String -> [Byte]
unhex bytes = error "unhex"

mingle :: [Byte] -> [Byte] -> [Byte]
mingle encoded_bytes bytes = error "mingle"

{-
bool* decode_byte_step(bool* bytes, int offset) {
    for(int i = offset; i < 8; ++i) {
        bytes[i] ^= bytes[i - offset];
    }
    return bytes;
}
-- decoded[4] = encoded[4] ^ decoded[2]
-}



{-
/*

{-
if (c > 0) a[c] ^= a[c-1]; // on every char except the first one, XOR it with its predecessor
        a[c] ^= a[c] >> 3; // then XOR the character with itself right-shifted by 3
        a[c] ^= a[c] >> 2; // then XOR it with itself right-shifted by 2
        a[c] ^= a[c] >> 1; // then XOR itself right-shifted by 1
        printf("%02x", (unsigned int)(a[c])); // then print each char as (hex?)
-}
*/

-}
