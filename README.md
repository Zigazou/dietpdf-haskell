# DietPDF

DietPDF - Reduce PDF file size (and more).

While there are many online and offline tools for compressing PDF files, they
usually do so by forcibly reducing image quality.

DietPDF, on the other hand, uses alternative methods to reduce PDF file size
while preserving image quality.

In fact, running DietPDF after using other tools will likely further reduce the
PDF file size.

## Requirements

- GhostScript `gs` (optional, only if `-g` is used)
- JpegTran `jpegtran` (required)
- ImageMagick `convert` (required)
- Grok image compression (`grk_compress`) as found on
  [Github Grok repository](https://github.com/GrokImageCompression/grok)
  (required)
- TTFAutoHint `ttfautohint` (required)

## Usage

Though DietPDF is mainly targeted at compressing PDF, it offers other tools
that have been implemented while debugging.

It supports options for optimizing, extracting information, encoding, decoding,
and more.

The `dietpdf` CLI commands take the following form:

    dietpdf <command> <command arguments>

### info

Prints a list of objects contained in a PDF file.

**Usage:** `dietpdf info <input_pdf_file>`

* *input_pdf_file*: The path to the PDF file to analyze.

## extract

Extracts the stream (unfiltered) of a specific object from a PDF file. The
content is sent to the standard output.

**Usage:** `dietpdf extract <object_number> <input_pdf_file>`

* *object_number*: The number of the object to extract from the PDF.
* *input_pdf_file*: The path to the PDF file to analyze.

### optimize

Optimizes a PDF file.

**Usage:** `dietpdf optimize <input_pdf_file> <output_pdf_file> [--gs-optimize | -g]`

* *input_pdf_file*: The path to the PDF file to process.
* *output_pdf_file*: The path to the optimized PDF file to create.
* --gs-optimize or -g: (Optional) Use GhostScript before optimizing.

### hash

Computes a hash of each stream in a PDF file.

**Usage:** `dietpdf hash <input_pdf_file>`

* *input_pdf_file*: The path to the PDF file to process.

### encode

Encodes a file as it would be in a PDF stream.

**Usage:** `dietpdf encode <codec> [input_pdf_file]`

* *codec*: The codec to use for encoding. Options include:
    * LZW
    * Deflate
    * NoCompress
    * RLE
    * Zopfli
    * Ascii85
    * Hex
* *input_pdf_file*: (Optional) The file to encode.

### decode

Decodes a file as it would be in a PDF stream.

**Usage:** `dietpdf decode <codec> [output_pdf_file]`

* *codec*: The codec to use for decoding. Options include:
    * LZW
    * Deflate
    * NoCompress
    * RLE
    * Zopfli
    * Ascii85
    * Hex
* *output_pdf_file*: (Optional) The file to decode.

### predict

Predicts a file as it would be in a PDF stream.

**Usage:** `dietpdf predict <predictor> <columns> <components> [input_pdf_file]`

* *predictor*: The predictor to use. Options include:
    * TIFFNoPrediction
    * TIFFPredictor2
    * PNGNone
    * PNGSub
    * PNGUp
    * PNGAverage
    * PNGPaeth
    * PNGOptimum
* *columns*: The width in pixels.
* *components*: The number of components (e.g., color channels).
* *input_pdf_file*: (Optional) The file to predict.

### unpredict

Unpredicts a file as it would be in a PDF stream.

**Usage:** `dietpdf unpredict <predictor> <columns> <components> [input_pdf_file]`

* *predictor*: The predictor to use. Options include:
    * TIFFNoPrediction
    * TIFFPredictor2
    * PNGNone
    * PNGSub
    * PNGUp
    * PNGAverage
    * PNGPaeth
    * PNGOptimum
* *columns*: The width in pixels.
* *components*: The number of components (e.g., color channels).
* *input_pdf_file*: (Optional) The file to unpredict.

## Limitations

### GhostScript

GhostScript may drop contents it does not natively handle. For example, if you
have embedded videos launched when clicking on an image, GhostScript will drop
them while rewriting the PDF.

On the other hand, with its rewriting engine, GhostScript often reduce PDF size.

GhostScript before version 10.02.1 may mess with document summary or with some
fonts. Check your version before enabling it. This is especially true for Debian
distributions since they tend to keep old versions of applications.

### Encrypted PDF

Dietpdf does not handle encrypted PDF. PDF files must be unencrypted before
using Dietpdf.

## Stack commands

### Compiling

```sh
stack build
stack exec -- whereis dietpdf
```

### Profiling

```sh
stack build --profile --ghc-options="-fprof-auto" # ou -fprof-auto-top
stack run --profile -- optimize in.pdf out.pdf +RTS -p
```

This produces a `dietpdf.prof` file.