#!/usr/bin/env python3
import struct
import sys

def hex_dump(data, length=16):
    """Génère un dump hexadécimal formaté."""
    result = []
    for i in range(0, len(data), length):
        chunk = data[i:i + length]
        hex_part = " ".join(f"{b:02x}" for b in chunk)
        ascii_part = "".join(chr(b) if 32 <= b <= 126 else "." for b in chunk)
        result.append(f"{i:08x}  {hex_part:<47}  |{ascii_part}|")
    return "\n".join(result)

def parse_ttf(file_path):
    try:
        with open(file_path, "rb") as f:
            # Lecture de l'Offset Subtable (12 premiers octets)
            # scaler_type (4), numTables (2), searchRange (2), entrySelector (2), rangeShift (2)
            header = f.read(12)
            if len(header) < 12:
                print("Erreur : Fichier trop court.")
                return

            scaler_type, num_tables = struct.unpack(">I H", header[:6])
            #print(f"--- Analyse du fichier : {file_path} ---")
            #print(f"Scaler Type: {hex(scaler_type)}")
            #print(f"Nombre de tables détectées : {num_tables}\n")

            # Lecture du Table Directory
            tables = []
            for _ in range(num_tables):
                entry = f.read(16)
                # tag (4 chars), checksum (4), offset (4), length (4)
                tag, checksum, offset, length = struct.unpack(">4s I I I", entry)
                tables.append({
                    "tag": tag.decode('ascii', errors='ignore'),
                    "offset": offset,
                    "length": length
                })

            # Extraction et Dump de chaque table
            print("Tag\tOffset\tLength")
            for table in tables:
                print(f"{table['tag']}\t{table['offset']}\t{table['length']}")

                continue
                f.seek(table['offset'])
                table_data = f.read(table['length'])
                
                print(hex_dump(table_data))
                print("\n")

    except FileNotFoundError:
        print(f"Erreur : Le fichier '{file_path}' est introuvable.")
    except Exception as e:
        print(f"Une erreur est survenue : {e}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 ttf_parser.py <chemin_police.ttf>")
    else:
        parse_ttf(sys.argv[1])

