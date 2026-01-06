#include <stdint.h>
#include <string.h>
#include <stdbool.h>

/*
 * Check if RGB triplets are nearly gray, meaning the absolute differences
 * between R, G, and B components are at most 1.
 *
 * Parameters:
 *   - input: pointer to input buffer with RGB data
 *   - inputLen: length of input buffer in bytes
 *
 * Returns: true if all RGB triplets are nearly gray, false otherwise
 */
bool containsOnlyGrayFFI(const uint8_t *input, size_t inputLen) {
  // Check if length is a multiple of 3
  if (inputLen % 3 != 0) {
    return false;
  }

  for (size_t i = 0; i < inputLen; i += 3) {
    uint8_t red = input[i];
    uint8_t green = input[i + 1];
    uint8_t blue = input[i + 2];

    // Check if the absolute differences are within 1
    if (!((red >= green ? red - green : green - red) <= 1 &&
          (red >= blue ? red - blue : blue - red) <= 1 &&
          (green >= blue ? green - blue : blue - green) <= 1)) {
      return false;
    }
  }

  return true;
}

/*
 * Optimize RGB triplets into grayscale by taking the red component.
 *
 * If the input length is not a multiple of 3, the function copies the input
 * to the output as-is.
 *
 * Parameters:
 *   - input: pointer to input buffer with RGB data
 *   - inputLen: length of input buffer in bytes
 *   - output: pointer to output buffer (must be at least inputLen bytes)
 *
 * Returns: number of bytes written to output buffer
 */
size_t optimizeGrayFFI(const uint8_t *input, size_t inputLen, uint8_t *output) {
  // If length is not a multiple of 3, just copy as-is
  if (inputLen % 3 != 0) {
    memcpy(output, input, inputLen);
    return inputLen;
  }

  size_t outputLen = inputLen / 3;
  size_t rgbIndex = 0;
  for (size_t grayIndex = 0; grayIndex < outputLen; grayIndex++) {
    output[grayIndex] = input[rgbIndex];
    rgbIndex += 3;
  }

  return outputLen;
}
