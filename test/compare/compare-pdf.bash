#!/bin/bash

# This script compares two PDF files page by page. It uses ImageMagick's compare
# command to calculate the Peak Signal-to-Noise Ratio (PSNR) between each pair
# of pages. The PSNR is a measure of the quality of the images. A PSNR value of
# 40 or higher is considered to be good. If the PSNR is below this threshold,
# the script considers the pages to be different.
#
# Requirements:
# - ImageMagick (compare)
# - Poppler (pdftoppm, pdfinfo)
# - bc
#
# Usage:
#   compare-pdf.bash file1.pdf file2.pdf
#
# The script will print the PSNR value for each pair of pages. If the PSNR is
# below the threshold, the script will print "BAD" next to the PSNR value.
# The script will return 0 if the PDF files are identical, and 1 otherwise.

# This value is the minimum PSNR value that is considered acceptable. Values
# below this threshold are considered to be bad.
PSNR_MIN=40

# Force commands to return floating numbers with a dot as decimal separator.
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
printf "Extracting pages from\n"
pdftoppm -r 150 -png "$pdf1" "$tmpdir/pdf1" 2> /dev/null &
pdftoppm -r 150 -png "$pdf2" "$tmpdir/pdf2" 2> /dev/null &

wait

# Compare each pair of images.
difference_found=0
pattern=$(printf "%s" "$pages1" | wc --bytes)
min_psnr_found=10000
max_psnr_found=0
psnr_cumulative=0
for ((i=1; i<=pages1; i++))
do
    page=$(printf "%0${pattern}d" $i)

    if [ ! -f "$tmpdir/pdf1-$page.png" ] || [ ! -f "$tmpdir/pdf2-$page.png" ]
    then
        printf "Page %d: Missing\n" "$i"
        difference_found=1
        continue
    fi

    compare \
        -metric PSNR \
        "$tmpdir/pdf1-$page.png" \
        "$tmpdir/pdf2-$page.png" null: \
        2> "$tmpdir/diff-$page.txt"

    psnr=$(cat "$tmpdir/diff-$page.txt")
    page_status="OK"
    bad_psnr "$psnr" && page_status="BAD"
    printf "Page %d: PSNR = %3.2f %s\n" "$i" "$psnr" "$page_status"

    [ $(echo "$psnr < $min_psnr_found" | bc) -eq 1 ] && min_psnr_found="$psnr"
    [ $(echo "$psnr > $max_psnr_found" | bc) -eq 1 ] && max_psnr_found="$psnr"
    psnr_cumulative=$(echo "$psnr_cumulative + $psnr" | bc)

    bad_psnr "$psnr" && difference_found=1
done

# Clean up.
rm -rf "$tmpdir"

# Print statistics.
psnr_average=$(echo "$psnr_cumulative / $pages1" | bc)
printf "Min PSNR: %3.2f\n" "$min_psnr_found"
printf "Max PSNR: %3.2f\n" "$max_psnr_found"
printf "Average PSNR: %3.2f\n" "$psnr_average"

exit "$difference_found"