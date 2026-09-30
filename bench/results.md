# Measured resource removal results

Baseline: `364fc304d35dd6a38bdfa0ac7554dc4e0acb3942`.
Compiler: GHC 9.8.4, `-O2`, x86_64. Both versions compiled with the same flags.
Five runs per version; parsing/import and output serialization excluded.
Full output representations matched in all comparisons.

| Input | Baseline median | Optimized median | Speedup |
| --- | ---: | ---: | ---: |
| compilation.pdf | 1.803235 s | 0.001431 s | 1260.46x |
| rm555.pdf | 0.104011 s | 0.086553 s | 1.20x |
| hello.pdf | 0.000210 s | 0.000172 s | 1.22x |

These measure one function call, not the full optimization pipeline. The tiny
`hello.pdf` timings are too short for a reliable speedup claim. The unusually
large improvement on `compilation.pdf` comes from avoiding decompression and
graphics parsing of image sample streams. Image dictionary names are still
collected, and images referenced as mandatory content are still validated.

Before the image-stream fix, three preliminary runs of the first optimization
had a median of 1.794 s on `compilation.pdf`: changing sets and traversal alone
had little effect on this input.

Reproduce:

```sh
stack build dietpdf:lib
BASELINE_REV=364fc304d35dd6a38bdfa0ac7554dc4e0acb3942 \
  bash bench/remove-unused-resources.sh \
  /home/fred/Programmation/flux/compilation.pdf \
  /home/fred/test/ollama/rm555.pdf \
  /home/fred/test/hello.pdf
```

Input SHA-256 checksums:

```text
0a2bcd4d5d741bfa1c3823ec63d55ade2f982b82c0f2d1e26af9fe8e1d20ea37  compilation.pdf
f5015648b37c3405ae689ded05f24cee1e1af57d09f492f0e0aa0f547ea7e261  rm555.pdf
81d5609e9326122122db1c21a2068abb2211da1a986dd98a9b004b52fe313ed7  hello.pdf
```

Raw measurements:

```text
/home/fred/Programmation/flux/compilation.pdf
  baseline=1.826474s optimized=0.001347s identical=yes
  baseline=1.758024s optimized=0.001549s identical=yes
  baseline=1.803235s optimized=0.001433s identical=yes
  baseline=1.777512s optimized=0.001395s identical=yes
  baseline=1.810559s optimized=0.001431s identical=yes
  median: 1.803235s -> 0.001431s (1260.46x)
/home/fred/test/ollama/rm555.pdf
  baseline=0.106191s optimized=0.087298s identical=yes
  baseline=0.094955s optimized=0.083677s identical=yes
  baseline=0.105514s optimized=0.086553s identical=yes
  baseline=0.094154s optimized=0.089406s identical=yes
  baseline=0.104011s optimized=0.085024s identical=yes
  median: 0.104011s -> 0.086553s (1.20x)
/home/fred/test/hello.pdf
  baseline=0.000307s optimized=0.000195s identical=yes
  baseline=0.000208s optimized=0.000176s identical=yes
  baseline=0.000210s optimized=0.000168s identical=yes
  baseline=0.000209s optimized=0.000172s identical=yes
  baseline=0.000223s optimized=0.000168s identical=yes
  median: 0.000210s -> 0.000172s (1.22x)
```
