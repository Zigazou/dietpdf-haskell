#!/usr/bin/env python3
import sys
import os

def pdfcut(start_offset, pdf_file_path):
    with open(pdf_file_path, mode="rb") as input_file:
        pdf_raw = input_file.read()

    next_endobj_offset = pdf_raw.index(b"endobj", start_offset)

    with open(pdf_file_path + ".cut", mode="wb") as output_file:
        output_file.write(pdf_raw[0:next_endobj_offset + len(b"endobj")])

def main():
    args = sys.argv[1:]

    if len(args) != 2:
        print('usage: <Start offset> <PDF file path>')
        sys.exit(1)

    pdfcut(int(args[0]), args[1])

if __name__ == '__main__':
    main()