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
#   pdf-compare.bash file1.pdf file2.pdf
#
# The script will print the PSNR value for each pair of pages. If the PSNR is
# below the threshold, the script will print "BAD" next to the PSNR value.
# The script will return 0 if the PDF files are identical, and 1 otherwise.

# This value is the minimum PSNR value that is considered acceptable. Values
# below this threshold are considered to be bad.
PSNR_MIN=40
DPI=150

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

function count_pages() {
    printf "%s" $#
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
printf "Extracting pages\n"
pdftoppm -r "$DPI" -png "$pdf1" "$tmpdir/pdf1" 2> /dev/null &
pdftoppm -r "$DPI" -png "$pdf2" "$tmpdir/pdf2" 2> /dev/null &

wait

# Compare each pair of images.
number_width=$(printf "%s" "$pages1" | wc --bytes)

psnr_min_found=1000000
psnr_max_found=0
psnr_cumulative=0
identical_pages=""
bad_pages=""
good_pages=""
missing_pages=""
worst_page=""
printf "Comparing pages\n"
for ((i=1; i<=pages1; i++))
do
    page=$(printf "%0${number_width}d" $i)

    if [ ! -f "$tmpdir/pdf1-$page.png" ] || [ ! -f "$tmpdir/pdf2-$page.png" ]
    then
        missing_pages="$missing_pages$i "
        continue
    fi

    compare \
        -metric PSNR \
        "$tmpdir/pdf1-$page.png" \
        "$tmpdir/pdf2-$page.png" null: \
        2> "$tmpdir/diff-$page.txt"

    psnr=$(cat "$tmpdir/diff-$page.txt")
    if [ "$psnr" == "inf" ]
    then
        # Keep track of pages that are identical.
        identical_pages="$identical_pages$i "
        continue
    fi

    # Keep track of the pages that are considered bad or good.
    if bad_psnr "$psnr"
    then
        bad_pages="$bad_pages$i "
    else
        good_pages="$good_pages$i "
    fi

    # Update min and max PSNR values.
    if [ $(echo "$psnr < $psnr_min_found" | bc) -eq 1 ]
    then
        psnr_min_found="$psnr"
        worst_page="$i"
    else
        psnr_max_found="$psnr"
    fi

    # Keep track of the cumulative PSNR in order to compute the average.
    psnr_cumulative=$(echo "$psnr_cumulative + $psnr" | bc)
done

# Clean up.
rm -rf "$tmpdir"

# Print statistics.
psnr_good=$(count_pages $good_pages)
psnr_bad=$(count_pages $bad_pages)
psnr_missing=$(count_pages $missing_pages)
psnr_identical=$(count_pages $identical_pages)

if [ "$psnr_good" -gt 0 ] || [ "$psnr_bad" -gt 0 ]
then
    psnr_average=$(echo "$psnr_cumulative / ($psnr_good.0 + $psnr_bad.0)" | bc)

    printf "PSNR minimum       :\t%3.2f\n" "$psnr_min_found"
    printf "PSNR average       :\t%3.2f\n" "$psnr_average"
    printf "PSNR maximum       :\t%3.2f\n" "$psnr_max_found"
else
    printf "PSNR minimum       :\tinf\n"
    printf "PSNR average       :\tinf\n"
    printf "PSNR maximum       :\tinf\n"
fi

printf "Pages ident. %5d :\t%s\n" "$psnr_identical" "$identical_pages"
printf "Pages good   %5d :\t%s\n" "$psnr_good"      "$good_pages"
printf "Pages bad    %5d :\t%s\n" "$psnr_bad"       "$bad_pages"
printf "Pages miss.  %5d :\t%s\n" "$psnr_missing"   "$missing_pages"
printf "Worst page         :\t%s\n" "$worst_page"

test -z "$bad_pages"