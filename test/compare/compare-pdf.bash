#!/bin/bash

PSNR_MIN=40

export LANG=C

function error() {
    printf -- "ERROR: %s\n" "$1" >&2
    exit 1
}

function bad_psnr() {
    local psnr="$1"

    [ "$psnr" == "inf" ] && return 1
    (( $(echo "$psnr > $PSNR_MIN" | bc -l) )) && return 1

    return 0
}

[ "$#" -ne 2 ] && error "$0 file1.pdf file2.pdf"

pdf1="$1"
pdf2="$2"

[ ! -f "$pdf1" ] && error "Cannot find $pdf1"
[ ! -f "$pdf2" ] && error "Cannot find $pdf2"

# Get number of pages.
printf "Getting number of pages in %s\n" "$pdf1"
pages1=$(pdfinfo "$pdf1" | grep "Pages" | awk '{print $2}')

printf "Getting number of pages in %s\n" "$pdf2"
pages2=$(pdfinfo "$pdf2" | grep "Pages" | awk '{print $2}')

[ "$pages1" -ne "$pages2" ] && error "different number of pages"

# Create a temporary directory.
tmpdir=$(mktemp -d)

# Convert each PDF to a series of PNG images.
printf "Extracting pages from %s\n" "$pdf1"
convert -density 72 "$pdf1" "$tmpdir/pdf1-%d.png" 2> /dev/null

printf "Extracting pages from %s\n" "$pdf2"
convert -density 72 "$pdf2" "$tmpdir/pdf2-%d.png" 2> /dev/null

# Compare each pair of images.
difference_found=0
for ((i=0; i<pages1; i++))
do
    if [ ! -f "$tmpdir/pdf1-$i.png" ] || [ ! -f "$tmpdir/pdf2-$i.png" ]
    then
        printf "Page %d: Missing\n" $((i + 1))
        difference_found=1
        continue
    fi

    compare \
        -metric PSNR \
        "$tmpdir/pdf1-$i.png" \
        "$tmpdir/pdf2-$i.png" null: \
        2> "$tmpdir/diff-$i.txt"

    psnr=$(cat "$tmpdir/diff-$i.txt")
    page_status="OK"
    bad_psnr "$psnr" && page_status="BAD"
    printf "Page %d: PSNR = %3.2f %s\n" $((i + 1)) "$psnr" "$page_status"

    bad_psnr "$psnr" && difference_found=1
done

# Clean up.
rm -rf "$tmpdir"

exit "$difference_found"