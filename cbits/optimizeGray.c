#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

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

bool isNearlyGrayFFI(const uint8_t *cb_channel, const uint8_t *cr_channel,
                     const size_t inputLen) {
  for (size_t i = 0; i < inputLen; i++) {
    if (cb_channel[i] < 0x70 || cb_channel[i] > 0x8F || cr_channel[i] < 0x70 ||
        cr_channel[i] > 0x8F) {
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

/* Convert 8-bit gray to packed 4-bit gray using Floyd-Steinberg diffusion. Rows
 * are byte-aligned, with the first pixel in the high nibble and zero padding
 * for odd widths. Buffers must not overlap. Return zero on invalid dimensions
 * or allocation failure; output requires ceil(width / 2) * height.
 */
size_t ditherGray4FFI(const uint8_t *input, size_t width, size_t height,
                      uint8_t *output) {
  if (!input || !output || !width || !height || width > SIZE_MAX / height ||
      width > SIZE_MAX / (2 * sizeof(float)) - 2) {
    return 0;
  }

  size_t stride = width / 2 + width % 2;
  float *errors = calloc(2 * (width + 2), sizeof(float));

  if (!errors) {
    return 0;
  }

  float *current = errors;
  float *next = errors + width + 2;
  float error;
  float value;
  unsigned level;

  for (size_t y = 0; y < height; ++y) {
    const uint8_t *inputRow = input + y * width;
    uint8_t *outputRow = output + y * stride;

    for (size_t x = 0; x < width; ++x) {
      value = inputRow[x] + current[x + 1];

      if (value < 0) {
        value = 0;
      } else if (value > 255) {
        value = 255;
      }

      level = (unsigned)(value / 17.0f + 0.5f);
      if (x & 1) {
        outputRow[x / 2] |= (uint8_t)level;
      } else {
        outputRow[x / 2] = (uint8_t)(level << 4);
      }

      error = value - level * 17.0f;
      current[x + 2] += error * (7.0f / 16.0f);

      next[x] += error * (3.0f / 16.0f);
      next[x + 1] += error * (5.0f / 16.0f);
      next[x + 2] += error * (1.0f / 16.0f);
    }

    float *swap = current;

    current = next;
    next = swap;
    memset(next, 0, (width + 2) * sizeof(float));
  }

  free(errors);

  return stride * height;
}

/* Count distinct samples in an 8-bit grayscale buffer. */
size_t countGrayLevelsFFI(const uint8_t *input, size_t inputLen) {
  bool seen[256] = {false};
  size_t count = 0;

  for (size_t i = 0; i < inputLen; ++i) {
    if (!seen[input[i]]) {
      seen[input[i]] = true;
      ++count;
    }
  }

  return count;
}

/* Pack mapped grayscale samples MSB first, with zero-padded byte-aligned rows.
 * Input contains width * height bytes; lookup contains 256 sample mappings.
 * Output requires ceil(width / (8 / bits)) * height bytes. */
size_t packGrayFFI(const uint8_t *input, size_t width, size_t height,
                   unsigned bits, const uint8_t *lookup, uint8_t *output) {
  if (!input || !lookup || !output || !width || !height ||
      (bits != 1 && bits != 2 && bits != 4 && bits != 8) ||
      width > SIZE_MAX / height) {
    return 0;
  }

  size_t perByte = 8 / bits;
  size_t stride = width / perByte + (width % perByte != 0);

  memset(output, 0, stride * height);

  for (size_t y = 0; y < height; ++y) {
    for (size_t x = 0; x < width; ++x) {
      output[y * stride + x / perByte] |= lookup[input[y * width + x]]
                                          << (8 - bits * (x % perByte + 1));
    }
  }

  return stride * height;
}
