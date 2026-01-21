#ifndef TIFF_MACROS_H
#define TIFF_MACROS_H
// Macros to extract nibbles and bits from a byte.
// Macros working on 2-bit components.
#define GXX______(x) (((x)&0b11000000) >> 6)
#define G__XX____(x) (((x)&0b00110000) >> 4)
#define G____XX__(x) (((x)&0b00001100) >> 2)
#define G______XX(x) ((x)&0b00000011)

// Macros working on 4-bit components.
#define GXXXX____(x) (((x)&0b11110000) >> 4)
#define G____XXXX(x) ((x)&0b00001111)

// Macros working on 6-bit components.
#define GXXXXXX__(x) (((x)&0b11111100) >> 2)
#define G__XXXXXX(x) ((x)&0b00111111)

// Macros to set nibbles and bits.
// Macros working on 2-bit components.
#define SXX______(x) (((x)&0b00000011) << 6)
#define S__XX____(x) (((x)&0b00000011) << 4)
#define S____XX__(x) (((x)&0b00000011) << 2)
#define S______XX(x) ((x)&0b00000011)

// Macros working on 4-bit components.
#define SXXXX____(x) (((x)&0b00001111) << 4)
#define S____XXXX(x) ((x)&0b00001111)

// Macros working on 6-bit components.
#define SXXXXXX__(x) (((x)&0b00111111) << 2)
#define S__XXXXXX(x) ((x)&0b00111111)

// Macro to swap bytes in a 16-bit word in order to work with different
// endianness.
#define SWAP16(x) ((((x) & 0x00ff) << 8) | (((x) & 0xff00) >> 8))
#endif // TIFF_MACROS_H