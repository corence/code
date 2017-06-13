
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

char* unhex(char* hex_string) {
    //const char inp[] = "deadbeef10203040b00b1e50";
    //char* pos = hex_string;
    //unsigned char val[12];

    const int input_length = strlen(hex_string);
    if(input_length % 2 != 0) {
        exit(1); // damn girl
    }
    const int output_length = input_length / 2;

    char* output = calloc(output_length + 1, sizeof(char));

    /* WARNING: no sanitization or error-checking whatsoever */
    for(int i = 0; i < output_length; ++i) {
        sscanf(hex_string + (i * 2), "%2hhx", (output + i));
    }

    return output; // don't worry about cleaning up our memory - it will get cleaned up when the program terminates
}

bool** explode_bytes(char* string) {
    const int num_bytes = strlen(string);
    bool** result = calloc(num_bytes, sizeof(bool*));
    for(int i = 0; i < num_bytes; ++i) {
        char byte = string[i];
        result[i] = calloc(8, sizeof(bool));
        bool* bytes = result[i];
        for(int j = 0; j < 8; ++j) {
            bytes[j] = (bool)(byte & (1 << j));
            printf("%d", bytes[j]);
        }
        printf("\n");
    }
    return result;
}

char implode_byte(bool* byte) {
    char result = 0;
    for(int i = 0; i < 8; ++i) {
        result |= (byte[i] << i);
    }
    return result;
}

bool* decode_byte(bool* input, int offset);

int main() {
    char* encoded_hex = "642f54096b24";
    char* encoded_bytes = unhex(encoded_hex);
    const int num_bytes = strlen(encoded_bytes);
    bool** bytes = explode_bytes(encoded_bytes);
    char* output = calloc(num_bytes + 1, sizeof(char));

    printf("encoded_bytes[0]: %d, [1]: %d\n\n", encoded_bytes[0], encoded_bytes[1]);
    for(int i = 0; i < num_bytes; ++i) {
        printf("encoded_bytes[%d] = %d\n", i, encoded_bytes[i]);
    }
    printf("\n");

    for(int i = num_bytes - 1; i >= 0; --i) {
        bytes[i] = decode_byte(bytes[i], 1);
        bytes[i] = decode_byte(bytes[i], 2);
        bytes[i] = decode_byte(bytes[i], 3);
        output[i] = implode_byte(bytes[i]);
    }

    for(int i = num_bytes - 1; i >= 1; --i) {
        output[i] ^= output[i - 1];
    }

    printf("%zd: %s\n", strlen(output), output);
    return 0;
}

bool get_bit(char byte, int index) {
    const int shift = (7 - index);
    return (byte & (1 << shift)) >> shift;
}

bool set_bit(char byte, int index, bool value) {
    printf("set_bit(%d, %d, %d) = %d\n", byte, index, value, byte | (value << (7 - index)));
    return byte | ((char)value << (7 - index));
}

bool* decode_byte(bool* bytes, int offset) {
    for(int i = offset; i < 8; ++i) {
        bytes[i] ^= bytes[i - offset];
    }
    return bytes;
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

