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

[ "$#" -lt 3 ] && error "$0 file1.pdf file2.pdf pageA [pageB [pageC...]]"

pdf1="$1"
pdf2="$2"

[ ! -f "$pdf1" ] && error "Cannot find $pdf1"
[ ! -f "$pdf2" ] && error "Cannot find $pdf2"

shift 2
pages="$*"  # Remaining arguments are the pages to compare.

# Create a temporary directory.
tmpdir=$(mktemp -d)

# Convert each PDF to a series of PNG images.
printf "Extracting pages\n"

for page in $pages
do
    render_page "$page" "$pdf1" "$tmpdir/pdf1-$page" &
    render_page "$page" "$pdf2" "$tmpdir/pdf2-$page" &
done

wait

printf "Comparing pages\n"
for page in $pages
do
    compare \
        -fuzz 0.5% \
        "$tmpdir/pdf1-$page.png" \
        "$tmpdir/pdf2-$page.png" \
        "$pdf2-$page.diff.png" \
        &
done

wait

# Clean up.
rm -rf "$tmpdir"