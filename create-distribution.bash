#!/bin/bash

mkdir --parent dist/lib
mkdir --parent dist/bin

executable=$(stack exec -- whereis dietpdf | sed 's/^dietpdf: //')

ldd "$executable" \
    | grep '=>' \
    | sed 's/^.*=> \([^ ]*\).*$/\1/g' \
    | while read -r lib
        do
            cp -- "$lib" dist/lib
        done

cp -- "$executable" dist/bin

patchelf --set-rpath '$ORIGIN/../lib' dist/bin/dietpdf
