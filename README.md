# DietPDF

Reduce PDF files

## Profiling

```sh
stack build --profile --ghc-options="-fprof-auto" # ou -fprof-auto-top
stack run --profile -- optimize in.pdf out.pdf +RTS -p
```

This produces a `dietpdf.prof` file.
