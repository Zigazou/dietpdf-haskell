#!/bin/bash

# This script uses ImageMagick and NetPBM to encode a big LZW stream to be used
# in DietPDF unit tests.
# It generates two files:
# - lzw_test.uncompressed
# - lzw_test.compressed

UNCOMPRESSED="lzw_test5.uncompressed"
COMPRESSED="lzw_test5.compressed"
POINTSIZE=96

# Extract a 32 bits value from a binary file and output it in decimal.
function get_value32() {
    local source_file="$1"
    local offset="$2"
    od \
        --skip-bytes="$offset" \
        --read-bytes=4 \
        --endian=little \
        --format=d4 \
        --address-radix=n \
        "$source_file"
}

# A substring-like function for binary files.
# Result is sent to the standard output.
function extract_bytes() {
    local source_file="$1"
    local offset="$2"
    local length="$3"

    dd \
        skip="$offset" \
        count="$length" \
        bs=1 \
        if="$source_file"
}

# Exit if a command is not installed on the current system.
function check_command() {
    local human_name="$1"
    local command="$2"
    local exit_code="$3"

    if ! command -v "$command" &> /dev/null
    then
        echo "$human_name is not installed" >&2
        exit "$exit_code"
    fi
}

check_command "Image Magick" convert 1
check_command "NetPBM" pnmtotiff 2

# Generate the base image.
# It uses Image Magick to draw text in an uncompressed NetPBM file.
convert \
    -set colorspace Gray \
    -depth 8 \
    -pointsize $POINTSIZE \
    label:DIETPDF \
    lzw_test.pgm

# Extract RAW data from NetPBM file by skipping the first 3 lines.
tail \
    --lines=+4 \
    lzw_test.pgm \
    > $UNCOMPRESSED

# Convert the uncompressed image to an LZW compressed image without predictor.
pnmtotiff \
    -lzw \
    -msb2lsb \
    -rowsperstrip 100000 \
    lzw_test.pgm \
    > lzw_test.tiff

# Extract the RAW compressed data from the TIFF file.
# The 32 bits IFD (Image File Directory) offset is found at offset 4.
# This script considers the image stream is contained between the 8th byte and
# this offset.
image_file_directory_offset=$(get_value32 lzw_test.tiff 4)
compressed_stream_length=$((image_file_directory_offset - 8))
extract_bytes lzw_test.tiff 8 "$compressed_stream_length" \
    2> /dev/null \
    > $COMPRESSED

# Delete images used to generate compressed/uncompressed streams.
rm lzw_test.tiff lzw_test.pgm
