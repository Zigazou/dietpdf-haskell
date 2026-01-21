#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "tiff_invalid_parameters.h"
#include "tiff_macros.h"

void tiff_unpredict_row_2bits_1component(uint8_t *row, size_t size,
                                         uint32_t width) {
  uint8_t current_pixel0 = 0, current_pixel1 = 0, current_pixel2 = 0,
          current_pixel3 = 0;
  uint8_t previous_pixel0 = 0, previous_pixel1 = 0, previous_pixel2 = 0,
          previous_pixel3 = 0;
  uint8_t boundary = 0;
  uint8_t last_pixels = row[size - 1];

  for (int64_t i = 0; i < (int64_t)size; i++) {
    previous_pixel0 = i > 0 ? G______XX(row[i - 1]) : 0;

    current_pixel0 = GXX______(row[i]) + previous_pixel0;
    current_pixel1 = G__XX____(row[i]) + current_pixel0;
    current_pixel2 = G____XX__(row[i]) + current_pixel1;
    current_pixel3 = G______XX(row[i]) + current_pixel2;

    row[i] = (uint8_t)(SXX______(current_pixel0) | S__XX____(current_pixel1) |
                       S____XX__(current_pixel2) | S______XX(current_pixel3));
  }

  boundary = width & 3;
  if (boundary == 1) {
    row[size - 1] = (row[size - 1] & 0b11000000) | G__XXXXXX(last_pixels);
  } else if (boundary == 2) {
    row[size - 1] = (row[size - 1] & 0b11110000) | G____XXXX(last_pixels);
  } else if (boundary == 3) {
    row[size - 1] = (row[size - 1] & 0b11111100) | G______XX(last_pixels);
  }
}

void tiff_unpredict_row_2bits_2components(uint8_t *row, size_t size,
                                          uint32_t width) {
  uint8_t current_component0 = 0, current_component1 = 0;
  uint8_t previous_component0 = 0, previous_component1 = 0;
  uint8_t nibble = 0;
  bool alternate;

  alternate = true;

  for (int64_t i = 0; width > 0 && i < (int64_t)size; width--, i++) {
    // 2 cases:
    if (alternate) {
      // Current and previous pixels are in two different bytes:
      //        row[i-1]         |          row[i]
      // __ __ __ __ P0 P0 P1 P1 | C0 C0 C1 C1 NN NN NN NN
      previous_component0 = i > 0 ? G____XX__(row[i - 1]) : 0;
      previous_component1 = i > 0 ? G______XX(row[i - 1]) : 0;

      current_component0 = GXX______(row[i]);
      current_component1 = G__XX____(row[i]);
      nibble = G____XXXX(row[i]);

      row[i] = (uint8_t)(SXX______(current_component0 + previous_component0) |
                         S__XX____(current_component1 + previous_component1) |
                         nibble);

      i--;
    } else {
      // Current and previous pixels are in the same byte:
      // P0 P0 P1 P1 C0 C0 C1 C1
      previous_component0 = GXX______(row[i]);
      previous_component1 = G__XX____(row[i]);

      current_component0 = G____XX__(row[i]);
      current_component1 = G______XX(row[i]);

      row[i] = (uint8_t)(S____XX__(current_component0 + previous_component0) |
                         S______XX(current_component1 + previous_component1) |
                         SXX______(previous_component0) |
                         S__XX____(previous_component1));
    }

    alternate = !alternate;
  }
}

void tiff_unpredict_row_2bits_3components(uint8_t *row, size_t size,
                                          uint32_t width) {
  uint8_t current_component0 = 0, current_component1 = 0,
          current_component2 = 0;
  uint8_t previous_component0 = 0, previous_component1 = 0,
          previous_component2 = 0;
  uint8_t nibble = 0;
  uint8_t last_pixel = row[size - 1];

  uint8_t boundary = 0;

  for (int64_t i = 0; width > 0 && i < (int64_t)size; width--, i++) {
    // 4 cases:
    if (boundary == 0) {
      // Current and previous pixels are in two bytes:
      //         row[i-1]        |          row[i]
      // __ __ P0 P0 P1 P1 P2 P2 | C0 C0 C1 C1 C2 C2 NN NN
      previous_component0 = i > 0 ? G__XX____(row[i - 1]) : 0;
      previous_component1 = i > 0 ? G____XX__(row[i - 1]) : 0;
      previous_component2 = i > 0 ? G______XX(row[i - 1]) : 0;

      current_component0 = GXX______(row[i]);
      current_component1 = G__XX____(row[i]);
      current_component2 = G____XX__(row[i]);
      nibble = G______XX(row[i]);

      row[i] = (uint8_t)(SXX______(current_component0 + previous_component0) |
                         S__XX____(current_component1 + previous_component1) |
                         S____XX__(current_component2 + previous_component2) |
                         nibble);

      i--;
    } else if (boundary == 1) {
      // Current and previous pixels are in two different bytes:
      //          row[i]         |         row[i+1]
      // P0 P0 P1 P1 P2 P2 C0 C0 | C1 C1 C2 C2 NN NN NN NN
      previous_component0 = GXX______(row[i]);
      previous_component1 = G__XX____(row[i]);
      previous_component2 = G____XX__(row[i]);

      current_component0 = G______XX(row[i]);
      current_component1 = i < (size - 1) ? GXX______(row[i + 1]) : 0;
      current_component2 = i < (size - 1) ? G__XX____(row[i + 1]) : 0;
      nibble = i < (size - 1) ? G____XXXX(row[i + 1]) : 0;

      row[i] = (uint8_t)(SXX______(previous_component0) |
                         S__XX____(previous_component1) |
                         S____XX__(previous_component2) |
                         S______XX(current_component0 + previous_component0));

      if (i < (size - 1)) {
        row[i + 1] =
            (uint8_t)(SXX______(current_component1 + previous_component1) |
                      S__XX____(current_component2 + previous_component2) |
                      nibble);
      }
    } else if (boundary == 2) {
      // Current and previous pixels are in three different bytes:
      //         row[i-1]        |          row[i]         |
      // __ __ __ __ __ __ P0 P0 | P1 P1 P2 P2 C0 C0 C1 C1 |
      //         row[i+1]
      // C2 C2 NN NN NN NN NN NN
      previous_component0 = i > 0 ? G______XX(row[i - 1]) : 0;
      previous_component1 = GXX______(row[i]);
      previous_component2 = G__XX____(row[i]);

      current_component0 = G____XX__(row[i]);
      current_component1 = G______XX(row[i]);
      current_component2 = i < (size - 1) ? GXX______(row[i + 1]) : 0;
      nibble = i < (size - 1) ? G__XXXXXX(row[i + 1]) : 0;

      row[i] = (uint8_t)(SXX______(previous_component1) |
                         S__XX____(previous_component2) |
                         S____XX__(current_component0 + previous_component0) |
                         S______XX(current_component1 + previous_component1));

      if (i < size - 1) {
        row[i + 1] =
            (uint8_t)(SXX______(current_component2 + previous_component2) |
                      nibble);
      }
    } else if (boundary == 3) {
      // Current and previous pixels are in two different bytes:
      //         row[i-1]        |          row[i]
      // __ __ __ __ P0 P0 P1 P1 | P2 P2 C0 C0 C1 C1 C2 C2
      previous_component0 = i > 0 ? G____XX__(row[i - 1]) : 0;
      previous_component1 = i > 0 ? G______XX(row[i - 1]) : 0;
      previous_component2 = GXX______(row[i]);

      current_component0 = G__XX____(row[i]);
      current_component1 = G____XX__(row[i]);
      current_component2 = G______XX(row[i]);

      row[i] = (uint8_t)(SXX______(previous_component2) |
                         S__XX____(current_component0 + previous_component0) |
                         S____XX__(current_component1 + previous_component1) |
                         S______XX(current_component2 + previous_component2));
    }

    boundary = (boundary + 1) & 3;
  }
}

void tiff_unpredict_row_2bits_4components(uint8_t *row, size_t size,
                                          uint32_t width) {
  uint8_t current_component0 = 0, current_component1 = 0,
          current_component2 = 0, current_component3 = 0;
  uint8_t previous_component0 = 0, previous_component1 = 0,
          previous_component2 = 0, previous_component3 = 0;

  for (int64_t i = 0; i < size; i++) {
    // Each pixel is in its own byte
    previous_component0 = i >= 1 ? GXX______(row[i - 1]) : 0;
    previous_component1 = i >= 1 ? G__XX____(row[i - 1]) : 0;
    previous_component2 = i >= 1 ? G____XX__(row[i - 1]) : 0;
    previous_component3 = i >= 1 ? G______XX(row[i - 1]) : 0;

    current_component0 = GXX______(row[i]);
    current_component1 = G__XX____(row[i]);
    current_component2 = G____XX__(row[i]);
    current_component3 = G______XX(row[i]);

    row[i] = (uint8_t)(SXX______(current_component0 + previous_component0) |
                       S__XX____(current_component1 + previous_component1) |
                       S____XX__(current_component2 + previous_component2) |
                       S______XX(current_component3 + previous_component3));
  }
}

void tiff_unpredict_row_4bits_1component(uint8_t *row, size_t size,
                                         uint32_t width) {
  uint8_t current_pixel = 0;
  uint8_t previous_pixel = 0;
  uint8_t nibble = 0;
  uint8_t last_pixel = row[size - 1];
  bool alternate;

  alternate = true;

  for (int64_t i = 0; i < size; i++) {
    // 2 cases:
    if (alternate) {
      // Current and previous pixels are in two different bytes:
      //       row[i-1]          |          row[i]
      // __ __ __ __ PP PP PP PP | CP CP CP CP NN NN NN NN
      previous_pixel = i > 0 ? G____XXXX(row[i - 1]) : 0;

      current_pixel = GXXXX____(row[i]);
      nibble = G____XXXX(row[i]);

      row[i] = (uint8_t)(SXXXX____(current_pixel + previous_pixel) | nibble);

      i--;
    } else {
      // Current and previous pixels are in the same byte:
      // PP PP PP PP CP CP CP CP
      previous_pixel = GXXXX____(row[i]);
      current_pixel = G____XXXX(row[i]);

      row[i] = (uint8_t)(S____XXXX(current_pixel + previous_pixel) |
                         SXXXX____(previous_pixel));
    }

    alternate = !alternate;
  }

  // Handle last pixel if width is odd.
  if ((width & 1) == 1) {
    row[size-1] = (row[size - 1] & 0b11110000) | (S____XXXX(last_pixel));
  }
}

void tiff_unpredict_row_4bits_2components(uint8_t *row, size_t size,
                                          uint32_t width) {
  uint8_t current_component0 = 0, current_component1 = 0;
  uint8_t previous_component0 = 0, previous_component1 = 0;

  for (int64_t i = 0; i < size; i++) {
    // Each pixel is in its own byte
    // C0 C0 C0 C0 C1 C1 C1 C1
    current_component0 = GXXXX____(row[i]);
    current_component1 = G____XXXX(row[i]);

    previous_component0 = i > 0 ? GXXXX____(row[i - 1]) : 0;
    previous_component1 = i > 0 ? G____XXXX(row[i - 1]) : 0;

    row[i] = (uint8_t)(SXXXX____(current_component0 + previous_component0) |
                       S____XXXX(current_component1 + previous_component1));
  }
}

void tiff_unpredict_row_4bits_3components(uint8_t *row, size_t size,
                                          uint32_t width) {
  uint8_t current_component0 = 0, current_component1 = 0,
          current_component2 = 0;
  uint8_t previous_component0 = 0, previous_component1 = 0,
          previous_component2 = 0;
  uint8_t nibble = 0;

  bool alternate = true;

  for (int64_t i = 0; i < size - 1; i++) {
    // 2 cases:
    if (alternate) {
      // Current and previous pixels are in four different bytes:
      //         row[i-2]        |          row[i-1]       |
      // __ __ __ __ P0 P0 P0 P0 | P1 P1 P1 P1 P2 P2 P2 P2 |
      //          row[i]         |         row[i+1]
      // C0 C0 C0 C0 C1 C1 C1 C1 | C2 C2 C2 C2 NN NN NN NN
      previous_component0 = i > 1 ? G____XXXX(row[i - 2]) : 0;
      previous_component1 = i > 0 ? GXXXX____(row[i - 1]) : 0;
      previous_component2 = i > 0 ? G____XXXX(row[i - 1]) : 0;

      current_component0 = GXXXX____(row[i]);
      current_component1 = G____XXXX(row[i]);
      current_component2 = GXXXX____(row[i + 1]);
      nibble = G____XXXX(row[i + 1]);

      row[i] = (uint8_t)(SXXXX____(current_component0 + previous_component0) |
                         S____XXXX(current_component1 + previous_component1));

      if (i < size - 1) {
        row[i + 1] =
            (uint8_t)(SXXXX____(current_component2 + previous_component2) |
                      nibble);
      }
    } else {
      // Current and previous pixels are in three different bytes:
      //         row[i-1]        |          row[i]         |
      // P0 P0 P0 P0 P1 P1 P1 P1 | P2 P2 P2 P2 C0 C0 C0 C0 |
      //          row[i+1]
      // C1 C1 C1 C1 C2 C2 C2 C2
      previous_component0 = i > 0 ? GXXXX____(row[i - 1]) : 0;
      previous_component1 = i > 0 ? G____XXXX(row[i - 1]) : 0;
      previous_component2 = GXXXX____(row[i]);

      current_component0 = G____XXXX(row[i]);
      current_component1 = GXXXX____(row[i + 1]);
      current_component2 = G____XXXX(row[i + 1]);

      row[i] = (uint8_t)(SXXXX____(previous_component2) |
                         S____XXXX(current_component0 + previous_component0));

      row[i + 1] =
          (uint8_t)(SXXXX____(current_component1 + previous_component1) |
                    S____XXXX(current_component2 + previous_component2));

      i++;
    }

    alternate = !alternate;
  }
}

void tiff_unpredict_row_4bits_4components(uint8_t *row, size_t size,
                                          uint32_t width) {

  uint8_t current_component0 = 0, current_component1 = 0,
          current_component2 = 0, current_component3 = 0;
  uint8_t previous_component0 = 0, previous_component1 = 0,
          previous_component2 = 0, previous_component3 = 0;

  for (int64_t i = 0; i < size; i += 2) {
    // Each pixel occupies two bytes.
    previous_component0 = i >= 3 ? GXXXX____(row[i - 3]) : 0;
    previous_component1 = i >= 3 ? G____XXXX(row[i - 3]) : 0;
    previous_component2 = i >= 2 ? GXXXX____(row[i - 2]) : 0;
    previous_component3 = i >= 2 ? G____XXXX(row[i - 2]) : 0;

    current_component0 = i >= 1 ? GXXXX____(row[i - 1]) : 0;
    current_component1 = i >= 1 ? G____XXXX(row[i - 1]) : 0;
    current_component2 = GXXXX____(row[i]);
    current_component3 = G____XXXX(row[i]);

    if (i >= 1) {
      row[i - 1] =
          (uint8_t)(SXXXX____(current_component0 + previous_component0) |
                    S____XXXX(current_component1 + previous_component1));
    }

    row[i] = (uint8_t)(SXXXX____(current_component2 + previous_component2) |
                       S____XXXX(current_component3 + previous_component3));
  }
}

void tiff_unpredict_row_8bits(uint8_t *row, size_t size, uint32_t width,
                              uint8_t color_components) {
  size_t row_bytes = width * color_components;

  for (int64_t i = 0; i < row_bytes; i++) {
    row[i] = (uint8_t)(row[i] +
                       (i >= color_components ? row[i - color_components] : 0));
  }
}

void tiff_unpredict_row_16bits(uint8_t *row, size_t size, uint32_t width,
                               uint8_t color_components) {
  int64_t row_words = width * color_components;
  uint16_t current_component = 0;
  uint16_t previous_component = 0;

  uint16_t *row16 = (uint16_t *)row;

  for (int64_t i = 0; i < row_words; i++) {
    current_component = SWAP16(row16[i]);
    previous_component =
        i >= color_components ? SWAP16(row16[i - color_components]) : 0;

    row16[i] = (uint16_t)(SWAP16(current_component + previous_component));
  }
}

/*
 * Apply TIFF prediction to a row of pixel data.
 *
 * TIFF prediction compares each pixel's color components to the previous
 * pixel's components and stores the difference. Components differences are
 * computed on unsigned values (of the size given par bits_per_component). The
 * "previous pixel" for the first pixel in the row is considered to have zero
 * for all its components.
 *
 * This function modifies the row in place.
 *
 * Parameters:
 * - row - Pointer to the row of pixel data.
 * - size - Number of bytes in the row.
 * - width - Number of pixels in the row.
 * - bits_per_component - Number of bits per color component (authorized values
 *   are 2, 4, 8 and 16).
 * - color_components - number of color components per pixel (authorized values
 *   are 1, 2, 3 and 4).
 */

void tiff_unpredict_row_ffi(uint8_t *row, size_t size, uint32_t width,
                            uint8_t bits_per_component,
                            uint8_t color_components) {

  /* Check parameters. */
  if (tiff_invalid_parameters(size, width, bits_per_component,
                              color_components)) {
    return;
  }

  /* Use dedicated functions for specific cases */
  if (bits_per_component == 2) {
    if (color_components == 1) {
      tiff_unpredict_row_2bits_1component(row, size, width);
    } else if (color_components == 2) {
      tiff_unpredict_row_2bits_2components(row, size, width);
    } else if (color_components == 3) {
      tiff_unpredict_row_2bits_3components(row, size, width);
    } else if (color_components == 4) {
      tiff_unpredict_row_2bits_4components(row, size, width);
    }
  } else if (bits_per_component == 4) {
    if (color_components == 1) {
      tiff_unpredict_row_4bits_1component(row, size, width);
    } else if (color_components == 2) {
      tiff_unpredict_row_4bits_2components(row, size, width);
    } else if (color_components == 3) {
      tiff_unpredict_row_4bits_3components(row, size, width);
    } else if (color_components == 4) {
      tiff_unpredict_row_4bits_4components(row, size, width);
    }
  } else if (bits_per_component == 8) {
    tiff_unpredict_row_8bits(row, size, width, color_components);
  } else if (bits_per_component == 16) {
    tiff_unpredict_row_16bits(row, size, width, color_components);
  }
}
