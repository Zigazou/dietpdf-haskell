#include <stdint.h>
#include <string.h>

#if defined(__SSE2__)
#include <emmintrin.h>
#endif

/*
 * Evaluate the uncompressed size of data encoded in run-length format.
 *
 * Parameters:
 *   input: pointer to input buffer with run-length encoded data
 *   inputLen: length of input buffer in bytes
 *
 * Returns: uncompressed size in bytes, or (size_t)-1 on error
 */
size_t runLengthEvaluateUncompressedSizeFFI(const uint8_t *input,
                                            size_t inputLen) {
  size_t readIdx = 0;
  size_t totalSize = 0;

  while (readIdx < inputLen) {
    uint8_t lengthByte = input[readIdx];
    readIdx++;

    if (lengthByte == 128) {
      // End of data marker
      break;
    } else if (lengthByte <= 127) {
      // Literal run: copy (lengthByte + 1) bytes
      size_t literalLength = lengthByte + 1;

      if (readIdx + literalLength > inputLen) {
        // Error: not enough input data
        return (size_t)-1;
      }

      totalSize += literalLength;
      readIdx += literalLength;
    } else {
      // Repeat run: copy next byte (257 - lengthByte) times
      if (readIdx >= inputLen) {
        // Error: expected a byte to repeat
        return (size_t)-1;
      }

      size_t repeatCount = 257 - lengthByte;
      totalSize += repeatCount;
      readIdx++;
    }
  }

  return totalSize;
}

/*
 * RunLengthDecode filter implementation according to Adobe PDF 32000-1:2008
 *
 * Decompresses data encoded in run-length format.
 * Each run consists of a length byte followed by data:
 * - Length 0-127: copy next (length + 1) bytes literally
 * - Length 129-255: copy next byte (257 - length) times
 * - Length 128: end of data (EOD)
 *
 * Parameters:
 *   input: pointer to input buffer with run-length encoded data
 *   inputLen: length of input buffer in bytes
 *   output: pointer to output buffer (must be large enough)
 *
 * Returns: number of bytes written to output buffer, or (size_t)-1 on error
 */
size_t runLengthDecompressFFI(const uint8_t *input, size_t inputLen,
                              uint8_t *output) {
  size_t readIdx = 0;
  size_t writeIdx = 0;

  while (readIdx < inputLen) {
    uint8_t lengthByte = input[readIdx];
    readIdx++;

    if (lengthByte == 128) {
      // End of data marker
      break;
    } else if (lengthByte <= 127) {
      // Literal run: copy (lengthByte + 1) bytes
      size_t literalLength = lengthByte + 1;

      if (readIdx + literalLength > inputLen) {
        // Error: not enough input data
        return (size_t)-1;
      }

      memcpy(output + writeIdx, input + readIdx, literalLength);
      writeIdx += literalLength;
      readIdx += literalLength;
    } else {
      // Repeat run: copy next byte (257 - lengthByte) times
      if (readIdx >= inputLen) {
        // Error: expected a byte to repeat
        return (size_t)-1;
      }

      uint8_t repeatByte = input[readIdx];
      readIdx++;

      size_t repeatCount = 257 - lengthByte;
      memset(output + writeIdx, repeatByte, repeatCount);
      writeIdx += repeatCount;
    }
  }

  return writeIdx;
}

/*
 * RunLengthEncode filter implementation according to Adobe PDF 32000-1:2008
 *
 * Compresses data using run-length encoding.
 * The encoder produces a sequence of runs, where each run is:
 * - For literal sequences: length byte (0-127) + (length + 1) data bytes
 * - For repeat sequences: length byte (129-255) + 1 data byte
 *
 * Strategy: Simple greedy approach
 * - Look for runs of identical bytes
 * - If run is 2+ bytes, encode as repeat run
 * - Otherwise, accumulate literal bytes and encode when maxed out (128) or
 *   when we hit three identical bytes
 *
 * Parameters:
 *   input: pointer to input buffer with raw data
 *   inputLen: length of input buffer in bytes
 *   output: pointer to output buffer (must be large enough)
 *
 * Returns: number of bytes written to output buffer
 */
size_t runLengthCompressFFI(const uint8_t *input, size_t inputLen,
                            uint8_t *output) {
  size_t readIdx = 0;
  size_t writeIdx = 0;

  while (readIdx < inputLen) {
    size_t remaining = inputLen - readIdx;
    size_t limit = remaining < 128 ? remaining : 128;
    const uint8_t *start = input + readIdx;

    /* Check if we have a repeat run starting at the current position. */
    if (limit >= 2 && start[0] == start[1]) {
      /* Check for a simple repeat run of exactly two bytes. */
      if (limit == 2 || start[2] != start[0]) {
        output[writeIdx++] = 255;
        output[writeIdx++] = start[0];
        readIdx += 2;

        continue;
      }

      size_t count = 3;

#if defined(__SSE2__)
      /* Check for a longer repeat run using SIMD if available. */
      const __m128i repeated = _mm_set1_epi8((char)start[0]);

      while (limit - count >= 16) {
        /* Load 16 bytes from the current position and compare with the repeated
         * byte. */
        __m128i bytes = _mm_loadu_si128((const __m128i *)(start + count));

        /* Compare the loaded bytes with the repeated byte to create a mask. */
        unsigned mask =
            (unsigned)_mm_movemask_epi8(_mm_cmpeq_epi8(bytes, repeated));

        /* If the mask is not all ones, it means not all bytes matched the
        repeated byte. */
        if (mask != 0xffffu) {
          break;
        }

        count += 16;
      }
#endif

      /* Check for any remaining repeat run beyond what SIMD could detect. */
      while (count < limit && start[count] == start[0]) {
        count++;
      }

      output[writeIdx++] = (uint8_t)(257 - count);
      output[writeIdx++] = start[0];
      readIdx += count;
    } else {
      /* Start counting literal bytes. */
      size_t count = 0;

      /* Only triples interrupt literals, including triples that straddle
       * the 128-byte packet boundary. Pairs inside literals stay literal. */
      size_t scanLimit = remaining > 2 ? remaining - 2 : 0;

      if (scanLimit > limit) {
        scanLimit = limit;
      }

#if defined(__SSE2__)
      /* Check for a literal run using SIMD if available. */
      while (scanLimit - count >= 16) {
        /* Load 16 bytes from the current position and the next two positions to
        check for triples. */
        __m128i a = _mm_loadu_si128((const __m128i *)(start + count));
        __m128i b = _mm_loadu_si128((const __m128i *)(start + count + 1));
        __m128i c = _mm_loadu_si128((const __m128i *)(start + count + 2));

        /* Check if any of the 16 bytes form a triple with the next two bytes.
         */
        unsigned mask = (unsigned)_mm_movemask_epi8(
            _mm_and_si128(_mm_cmpeq_epi8(a, b), _mm_cmpeq_epi8(b, c)));

        /* If any of the 16 bytes form a triple, the mask will be non-zero. */
        if (mask != 0) {
          break;
        }

        /* Advance the count by 16 as none of the 16 bytes formed a triple. */
        count += 16;
      }
#endif

      /* Check for any remaining literal run beyond what SIMD could detect. */
      while (count < scanLimit && !(start[count] == start[count + 1] &&
                                    start[count + 1] == start[count + 2])) {
        count++;
      }

      if (count == scanLimit) {
        count = limit;
      }

      /* Write the literal run to the output buffer. */
      output[writeIdx++] = (uint8_t)(count - 1);
      memcpy(output + writeIdx, start, count);

      /* Update the write and read indices after writing the literal run. */
      writeIdx += count;
      readIdx += count;
    }
  }

  return writeIdx;
}
