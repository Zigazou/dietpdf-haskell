/* Standalone differential test and benchmark:
 * cc -O3 -Wall -Wextra -Werror test/cbits/runLengthCompress.c cbits/runLength.c -o /tmp/rle-test
 * /tmp/rle-test
 * Add -U__SSE2__ to exercise the scalar fallback, or
 * -fsanitize=address,undefined to check memory safety.
 */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

size_t runLengthCompressFFI(const uint8_t *, size_t, uint8_t *);
size_t runLengthDecompressFFI(const uint8_t *, size_t, uint8_t *);

/* Original greedy packet selection, retained as a compatibility oracle. */
static size_t reference(const uint8_t *input, size_t n, uint8_t *output) {
  size_t i = 0, o = 0;
  while (i < n) {
    size_t run = 1;
    while (i + run < n && input[i + run] == input[i] && run < 128)
      run++;
    if (run >= 2) {
      output[o++] = (uint8_t)(257 - run);
      output[o++] = input[i];
      i += run;
    } else {
      size_t header = o++, count = 0;
      while (i < n && count < 128) {
        if (i + 2 < n && input[i] == input[i + 1] && input[i + 1] == input[i + 2])
          break;
        output[o++] = input[i++];
        count++;
      }
      output[header] = (uint8_t)(count - 1);
    }
  }
  return o;
}

static uint32_t randomByte(void) {
  static uint32_t state = 123456789;
  state ^= state << 13;
  state ^= state >> 17;
  state ^= state << 5;
  return state;
}

static void check(const uint8_t *input, size_t n) {
  uint8_t *expected = malloc(2 * n + 1);
  size_t expectedLen = reference(input, n, expected);
  /* Exact allocations expose reads/writes beyond the contract to ASan. */
  uint8_t *actual = malloc(expectedLen ? expectedLen : 1);
  uint8_t *decoded = malloc(n ? n : 1);
  size_t actualLen = runLengthCompressFFI(input, n, actual);
  assert(actualLen == expectedLen);
  assert(memcmp(actual, expected, actualLen) == 0);
  assert(runLengthDecompressFFI(actual, actualLen, decoded) == n);
  assert(memcmp(input, decoded, n) == 0);
  free(expected);
  free(actual);
  free(decoded);
}

static volatile size_t sink;
static void benchmark(const char *name, uint8_t *input, size_t n) {
  uint8_t *output = malloc(2 * n + 1);
  double times[2];
  for (int version = 0; version < 2; version++) {
    clock_t begin = clock();
    for (int i = 0; i < 100; i++)
      sink = version ? runLengthCompressFFI(input, n, output)
                     : reference(input, n, output);
    times[version] = (double)(clock() - begin) / CLOCKS_PER_SEC;
  }
  printf("%-12s reference %.3fs optimized %.3fs speedup %.2fx\n",
         name, times[0], times[1], times[0] / times[1]);
  free(output);
}

int main(void) {
  assert(runLengthCompressFFI(NULL, 0, NULL) == 0);
  for (unsigned n = 0; n <= 12; n++) {
    uint8_t *input = malloc(n ? n : 1);
    for (unsigned bits = 0; bits < (1u << n); bits++) {
      for (size_t j = 0; j < n; j++) input[j] = (bits >> j) & 1;
      check(input, n);
    }
    free(input);
  }
  for (size_t n = 1; n <= 1024; n++) {
    uint8_t *input = malloc(n);
    for (int pattern = 0; pattern < 5; pattern++) {
      for (size_t j = 0; j < n; j++)
        input[j] = pattern == 0 ? 42 : pattern == 1 ? (uint8_t)j
                   : pattern == 2 ? (uint8_t)(j / 2)
                   : pattern == 3 ? randomByte() % 3 : randomByte();
      check(input, n);
    }
    /* Place triples at every offset around SIMD and packet boundaries. */
    for (size_t pos = 0; pos + 2 < n && pos < 260; pos++) {
      for (size_t j = 0; j < n; j++) input[j] = (uint8_t)j;
      input[pos + 1] = input[pos + 2] = input[pos];
      check(input, n);
    }
    free(input);
  }
  puts("Differential and round-trip checks passed.");
  size_t n = 1024 * 1024;
  uint8_t *input = malloc(n);
  memset(input, 42, n);
  benchmark("constant", input, n);
  for (size_t j = 0; j < n; j++) input[j] = (uint8_t)j;
  benchmark("literal", input, n);
  for (size_t j = 0; j < n; j++) input[j] = randomByte();
  benchmark("random", input, n);
  for (size_t j = 0; j < n; j++) input[j] = (uint8_t)(j / 2);
  benchmark("pairs", input, n);
  for (size_t j = 0; j < n; j++) input[j] = randomByte() % 3;
  benchmark("mixed", input, n);
  free(input);
  return 0;
}
