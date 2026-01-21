#include <stdbool.h>
#include <stdint.h>
#include <string.h>

/**
 * Validates TIFF image parameters to ensure they are within acceptable ranges
 * and that the buffer size is sufficient for the specified image dimensions.
 *
 * @param size The size of the image data buffer in bytes
 * @param width The width of the image in pixels
 * @param bits_per_component The number of bits per color component (must be 2,
 * 4, 8, or 16)
 * @param color_components The number of color components per pixel (must be
 * 1-4)
 *
 * @return true if the parameters are invalid, false if they are valid
 *
 * The function performs three checks:
 * 1. Validates that bits_per_component is one of: 2, 4, 8, or 16
 * 2. Validates that color_components is between 1 and 4 (inclusive)
 * 3. Ensures the buffer size is large enough to hold all pixel data
 *    (width × bits_per_component × color_components ≤ size × 8)
 */
bool tiff_invalid_parameters(size_t size, uint32_t width,
                             uint8_t bits_per_component,
                             uint8_t color_components) {

  /* Check bits per component values */
  if (bits_per_component != 2 && bits_per_component != 4 &&
      bits_per_component != 8 && bits_per_component != 16) {
    return true;
  }

  /* Check color components values */
  if (color_components < 1 || color_components > 4) {
    return true;
  }

  /* Check size and width */
  size_t total_bits_needed = width * bits_per_component * color_components;
  size_t total_bits_given = size * 8;
  if (total_bits_needed > total_bits_given) {
    return true;
  }

  return false;
}