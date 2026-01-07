#!/bin/bash
#
# Script to extract and display all vector objects from a PDF file
#
# This script provides utilities to identify and extract vector graphics objects
# from PDF files using the dietpdf utility.

# List all vector object IDs found in a PDF file
#
# Queries the PDF metadata using 'dietpdf info' to find all objects with 'Vector'
# in their description, then extracts and outputs their object IDs.
#
# Arguments:
#   $1: Path to the PDF file to analyze
#
# Output:
#   One object ID per line
#
# Returns:
#   0 on success (even if no vector objects are found)
function list-vector-object-ids() {
    local pdf_file="$1"

    dietpdf info "$pdf_file" \
        | grep 'Vector' \
        | cut --field=2 \
        | sort --numeric-sort \
        2> /dev/null

    return 0
}

# Extract and display a single vector object from a PDF file
#
# Retrieves the specified object from the PDF and formats it with a header
# showing the object ID. Useful for inspecting individual vector objects.
#
# Arguments:
#   $1: Path to the PDF file
#   $2: Object ID to extract
#
# Output:
#   Formatted output with object ID header followed by the object content
#
function extract-vector-object() {
    local pdf_file="$1"
    local object_id="$2"

    printf '==================== %d ====================\n' "$object_id"
    dietpdf extract "$object_id" "$pdf_file" 2> /dev/null
    printf '\n'

    return 0
}

# Extract all vector objects from a PDF file
#
# Main function that identifies all vector objects in a PDF file and extracts
# them one by one, displaying each with formatted output.
#
# Arguments:
#   $1: Path to the PDF file to process
#
# Output:
#   Extracted content for each vector object with separator headers
#
# Returns:
#   0 on success
#   1 if the PDF file is not found
#
function extract-all-vector-objects() {
    local pdf_file="$1"

    if test ! -f "$pdf_file"
    then
        printf 'File not found: %s\n' "$pdf_file" >&2
        return 1
    fi

    list-vector-object-ids "$pdf_file" | while read -r object_id
    do
        extract-vector-object "$pdf_file" "$object_id"
    done

    return 0
}

# Script entry point
#
# Processes the PDF file specified as the first command-line argument,
# extracting all vector objects it contains.
pdf_file="$1"

extract-all-vector-objects "$pdf_file"
