#!/usr/bin/env python3

import argparse
import sys
import zlib


def read_all(path: str | None) -> bytes:
    if path and path != "-":
        with open(path, "rb") as f:
            return f.read()
    return sys.stdin.buffer.read()


def hexdump(data: bytes, cols: int = 16) -> None:
    for offset in range(0, len(data), cols):
        chunk = data[offset: offset + cols]
        hexbytes = " ".join(f"{b:02x}" for b in chunk)
        # Pad hex column to align
        pad_len = cols - len(chunk)
        if pad_len > 0:
            hexbytes += " " * (pad_len * 3 - 1)
        ascii_part = "".join(chr(b) if 32 <= b < 127 else "." for b in chunk)
        print(f"{offset:08x}  {hexbytes}  |{ascii_part}|")


def main(argv: list[str]) -> int:
    p = argparse.ArgumentParser(
        description="Decode a raw DEFLATE stream and output a hex dump",
    )
    p.add_argument(
        "file",
        nargs="?",
        default="-",
        help="Input file containing compressed stream (use '-' or omit to read stdin)",
    )
    args = p.parse_args(argv)

    try:
        raw = read_all(args.file)
        gzip_header = b"\x1f\x8b\x08\x00\x00\x00\x00\x00"
        raw = gzip_header + raw
        out = zlib.decompress(raw, wbits=31)
    except Exception as e:
        print(f"Error: failed to decompress input: {e}", file=sys.stderr)
        return 1

    hexdump(out)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
