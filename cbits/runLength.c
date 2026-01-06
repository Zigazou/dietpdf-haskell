#include <stdint.h>
#include <string.h>

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
 *   when we hit a potential repeat run
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
    // Count how many bytes are identical starting at readIdx
    size_t runLength;
    for (runLength = 1;
         readIdx + runLength < inputLen &&
         input[readIdx + runLength] == input[readIdx] && runLength < 128;
         runLength++)
      ;

    if (runLength >= 2) {
      // Encode as repeat run
      // Length byte is (257 - runLength), followed by one byte to repeat
      uint8_t lengthByte = 257 - runLength;
      output[writeIdx++] = lengthByte;
      output[writeIdx++] = input[readIdx];
      readIdx += runLength;
    } else {
      // Start accumulating literal bytes
      size_t literalStart = writeIdx + 1; // +1 for the length byte
      size_t literalCount = 0;

      // Accumulate literals until we hit a repeat or reach max (128)
      while (readIdx < inputLen && literalCount < 128) {
        // Check if next 2+ bytes are identical (potential repeat run)
        size_t peekAhead1 = readIdx + 1;
        size_t peekAhead2 = readIdx + 2;

        if (peekAhead2 < inputLen && input[readIdx] == input[peekAhead1] &&
            input[peekAhead1] == input[peekAhead2]) {
          // Stop accumulating, we'll handle the repeat run next iteration
          break;
        }

        output[literalStart + literalCount] = input[readIdx];
        literalCount++;
        readIdx++;
      }

      // Write the literal run
      output[writeIdx] = literalCount - 1; // Length byte = literalCount - 1
      writeIdx = literalStart + literalCount;
    }
  }

  // Do not write EOD marker (PDF files may omit it)
  // output[writeIdx++] = 128;

  return writeIdx;
}
