
#include <stdio.h>
#include <string.h>

int main() {
    char a[] = "XXXXXX";
    int b = strlen(a);
    for (int c = 0; c < b; c++) {
        printf("%02x", (unsigned int)(a[c]));
        a[c] ^= a[c] >> 1;
        a[c] ^= a[c] >> 2;
        a[c] ^= a[c] >> 3;
        if (c > 0) a[c] ^= a[c-1];
    }
    return 0;
}

char fixup(char input, int offset) {
    char output = \0;
    
    for(int i = 0; i < offset; ++i) {
        
    }
    
    for(int i = offset; i < 8; ++i) {
        
    }
}

/*
-- decoded[4] = encoded[4] ^ decoded[2]

{-
if (c > 0) a[c] ^= a[c-1]; // on every char except the first one, XOR it with its predecessor
        a[c] ^= a[c] >> 3; // then XOR the character with itself right-shifted by 3
        a[c] ^= a[c] >> 2; // then XOR it with itself right-shifted by 2
        a[c] ^= a[c] >> 1; // then XOR itself right-shifted by 1
        printf("%02x", (unsigned int)(a[c])); // then print each char as (hex?)
-}
*/
