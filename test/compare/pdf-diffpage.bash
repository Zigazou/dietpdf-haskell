#!/bin/bash

# This script 
#
# Requirements:
# - ImageMagick (compare)
# - Poppler (pdftoppm)
#
# Usage:
#   pdf-diffpage.bash page file1.pdf file2.pdf
#

DPI=300

# Force commands to return floating numbers with a dot as decimal separator.
export LANG=C

function error() {
    printf -- "ERROR: %s\n" "$1" >&2
    exit 1
}

function render_page() {
    local page="$1"
    local pdf="$2"
    local output="$3"

    pdftoppm \
        -r "$DPI" \
        -f "$page" \
        -l "$page" \
        -singlefile \
        -png \
        "$pdf" \
        "$output" \
        2> /dev/null
}

[ "$#" -ne 4 ] && error "$0 page file1.pdf file2.pdf diff.png"

page="$1"
pdf1="$2"
pdf2="$3"
diff="$4"

[ ! -f "$pdf1" ] && error "Cannot find $pdf1"
[ ! -f "$pdf2" ] && error "Cannot find $pdf2"

# Create a temporary directory.
tmpdir=$(mktemp -d)

# Convert each PDF to a series of PNG images.
printf "Extracting pages\n"
render_page "$page" "$pdf1" "$tmpdir/pdf1" &
render_page "$page" "$pdf2" "$tmpdir/pdf2" &

wait

compare \
    -fuzz 0.5% \
    "$tmpdir/pdf1.png" \
    "$tmpdir/pdf2.png" \
    "$diff"

# Clean up.
rm -rf "$tmpdir"