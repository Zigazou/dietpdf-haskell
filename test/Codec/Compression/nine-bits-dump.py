#!/usr/bin/env python3

import sys

def read_and_process_input():
    # Read the binary input as bytes from stdin
    data = sys.stdin.buffer.read()

    # Initialize variables
    bit_buffer = 0
    bit_count = 0

    # Process byte by byte
    for byte in data:
        # Add the byte to the bit buffer
        bit_buffer = (bit_buffer << 8) | byte
        bit_count += 8

        # While we have enough bits (9 bits), extract 9 bits chunks
        while bit_count >= 9:
            # Extract the top 9 bits
            value = (bit_buffer >> (bit_count - 9)) & 0x1FF
            bit_count -= 9

            # Print the value in decimal and hexadecimal
            print(f"{value} (dec) / {hex(value)} (hex)")

    # If there are leftover bits (less than 9) in the buffer, you can choose to ignore or handle them
    if bit_count > 0:
        print(f"Remaining {bit_count} bits = {bit_buffer & ((2 ** bit_count) - 1)} (not enough for a full 9-bit value)")

if __name__ == "__main__":
    read_and_process_input()