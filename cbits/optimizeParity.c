#include <stdint.h>
#include <string.h>

/*
 * Modify a RGB triplet so that all components have the same parity (even/odd).
 * The parity is taken from the most used parity among the three components.
 */
static void sameParity(uint8_t *red, uint8_t *green, uint8_t *blue) {
  uint8_t redParity = *red & 1;
  uint8_t greenParity = *green & 1;
  uint8_t blueParity = *blue & 1;

  int parityCount = redParity + greenParity + blueParity;
  uint8_t targetParity;

  // Determine target parity: use the parity that appears 2+ times
  if (parityCount >= 2) {
    // If at least 2 components have odd parity (parityCount >= 2),
    // we need to check if it's odd. If parityCount is 3 (all odd) or 2,
    // targetParity = 1 (odd). If parityCount = 1, only one is odd.
    // If parityCount = 0, all are even.
    if (parityCount >= 2) {
      targetParity = 1; // Make all odd
    } else {
      targetParity = 0; // Make all even
    }
  } else {
    targetParity = 0; // Make all even
  }

  // Adjust each component to match target parity
  if ((*red & 1) != targetParity) {
    if (targetParity == 0) {
      *red = (*red & 1) ? *red - 1 : *red;
    } else {
      *red = (*red & 1) ? *red : *red + 1;
    }
  }

  if ((*green & 1) != targetParity) {
    if (targetParity == 0) {
      *green = (*green & 1) ? *green - 1 : *green;
    } else {
      *green = (*green & 1) ? *green : *green + 1;
    }
  }

  if ((*blue & 1) != targetParity) {
    if (targetParity == 0) {
      *blue = (*blue & 1) ? *blue - 1 : *blue;
    } else {
      *blue = (*blue & 1) ? *blue : *blue + 1;
    }
  }
}

/*
 * Optimize RGB triplets by adjusting component values to have the same parity.
 *
 * Parameters:
 *   - input: pointer to input buffer with RGB data
 *   - inputLen: length of input buffer in bytes
 *   - output: pointer to output buffer (must be at least inputLen bytes)
 *
 * Returns: number of bytes written to output buffer
 */
size_t optimizeParityFFI(const uint8_t *input, size_t inputLen,
                         uint8_t *output) {
  // If length is not a multiple of 3, just copy as-is
  if (inputLen % 3 != 0) {
    memcpy(output, input, inputLen);
    return inputLen;
  }

  size_t triplets = inputLen / 3;
  for (size_t i = 0; i < triplets; i++) {
    uint8_t red = input[i * 3];
    uint8_t green = input[i * 3 + 1];
    uint8_t blue = input[i * 3 + 2];

    sameParity(&red, &green, &blue);

    output[i * 3] = red;
    output[i * 3 + 1] = green;
    output[i * 3 + 2] = blue;
  }

  return inputLen;
}
