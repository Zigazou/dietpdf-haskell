# DietPDF

Reduce PDF files

## Requirements

- GhostScript `gs` (optional, only if `-g` is used)
- JpegTran `jpegtran` (required)
- ImageMagick `convert` (required)
- Grok image compression (`grk_compress`) as found on
  [Github Grok repository](https://github.com/GrokImageCompression/grok)
  (required)
- TTFAutoHint `ttfautohint` (required)

## Options

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