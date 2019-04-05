LZSA is a byte-aligned compression format that is specifically engineered for very fast decompression on 8-bit systems. It can compress files of any size by using blocks of a maximum size of 64 Kb with block-interdependent compression and up to 64 Kb of back-references for matches. 

The LZSA compression tool uses an aggressive optimal packing strategy to try to find the sequence of commands that gives the smallest packed file that decompresses to the original while maintaining the maximum possible decompression speed.

Compression ratio comparison between LZSA and other optimal packers, for a workload composed of ZX Spectrum and C64 files:

    ZX7                  57,36%           (entropy coding)
    LZ5 1.4.1            59,82%
    LZSA                 60,84% <------   (single byte stream)
    Lizard -29           64,14%           (rep-match, 4 byte streams)
    LZ4_HC -19 -B4 -BD   64,5%            (single byte stream)
    Uncompressed         100%

Performance over well-known compression corpus files:

                         Uncompressed     LZ4_HC -19 -B4 -BD    LZSA
    Canterbury           2810784          935827 (33,29%)       855284 (30,43%)
    Silesia              211938580        77299725 (36,47%)     73793860 (34,82%)
    Calgary              3251493          1248780 (38,40%)      1196809 (36,80%)
    Large                11159482         3771025 (33,79%)      3648987 (32,70%)
    enwik9               1000000000       371841591 (37,18%)    355427361 (35,54%)
    
As an example of LZSA's simplicity, a size-optimized decompressor on 8088 has been implemented in 91 bytes.

The compressor is approximately 2X slower than LZ4_HC but compresses better while maintaining similar decompression speeds and decompressor simplicity.

The main differences with the LZ4 compression format are:

* The use of short (8-bit) match offsets where possible. The match-finder and optimizer cooperate to try and use the shortest match offsets possible.
* Shorter encoding of lengths. As blocks are maximum 64 Kb in size, lengths can only be up to 64 Kb.
* As a result of the smaller commands due to the possibly shorter match offsets, a minimum match size of 3 bytes instead of 4. The use of small matches is driven by the optimizer, and used where they provide gains.

Inspirations: 

* [LZ4](https://github.com/lz4/lz4) by Yann Collet. 
* [LZ5/Lizard](https://github.com/inikep/lizard) by Przemyslaw Skibinski and Yann Collet. 
* The suffix array intervals in [Wimlib](https://wimlib.net/git/?p=wimlib;a=tree) by Eric Biggers.

License:

* The LZSA code is available under the Zlib license. 
* The compressor (shrink.c) is available under the CC0 license due to using portions of code from Eric Bigger's Wimlib in the suffix array-based matchfinder.

# Stream format

The stream format is composed of:
* a header
* one or more frames
* a footer

# Header format

The 3-bytes header contains a signature and a traits byte:

    0    1                2
    0x7b 0x9e             0x00
    <--- signature --->   <- traits ->

The traits are set to 0x00 for this version of the format.

# Frame format

Each frame contains a 3-bytes length followed by block data that expands to up to 64 Kb of decompressed data.

    0    1    2
    DSZ0 DSZ1 U|DSZ2

* DSZ0 (length byte 0) contains bits 0-7 of the block data size
* DSZ1 (length byte 1) contains bits 8-15 of the block data size
* DSZ2 (bit 0 of length byte 2) contains bit 16 of the block data size
* U (bit 7 of length byte 2) is set if the block data is uncompressed, and clear if the block data is compressed.
* Bits 1..6 of length byte 2 are currently undefined and must be set to 0.

# Block data format

LZSA blocks are composed from consecutive commands. Each command follows this format:

* token: <O|LLL|MMMM>
* optional extra literal length
* literal values
* match offset low
* optional match offset high
* optional extra encoded match length

**token**

The token byte is broken down into three parts:

    7 6 5 4 3 2 1 0
    O L L L M M M M

* O: set for a 2-bytes match offset, clear for a 1-byte match offset
* L: 3-bit literals length (0-6, or 7 if extended). If the number of literals for this command is 0 to 6, the length is encoded in the token and no extra bytes are required. Otherwise, a value of 7 is encoded and extra bytes follow as 'optional extra literal length'
* M: 4-bit encoded match length (0-14, or 15 if extended). Likewise, if the encoded match length for this command is 0 to 14, it is directly stored, otherwise 15 is stored and extra bytes follow as 'optional extra encoded match length'. Except for the last command in a block, a command always contains a match, so the encoded match length is the actual match length offset by the minimum, which is 3 bytes. For instance, an actual match length of 10 bytes to be copied, is encoded as 7.

**optional extra literal length**

If the literals length is 7 or more, the 'L' bits in the token form the value 7, and an extra byte follows here, with three possible types of value:

* 0-253: the value is added to the 7 stored in the token, to compose the final literals length. For instance a length of 206 will be stored as 7 in the token + a single byte with the value of 199, as 7 + 199 = 206.
* 254: a second byte follows. The final literals value is 7 + 254 + the second byte. For instance, a literals length of 499 is encoded as 7 in the token, a byte with the value of 254, and a final byte with teh value of 238, as 7 + 254 + 238 = 499.
* 255: a second and third byte follow, forming a little-endian 16-bit value. The final literals value is 7 + 255 + that 16-bit value. For instance, a literals length of 1024 is stored as 7 in the token, then byte values of 255, 250 and 2, as 7 + 255 + 250 + (2 * 256) = 1024.

**literal values**

Literal bytes, whose number is specified by the literals length, follow here. There can be zero literals in a command.

Important note: the last command in a block ends here, as it always contains literals only.

**match offset low**

The low 8 bits of the match offset follows. 

**optional match offset high**

If the 'O' bit (bit 7) is set in the token, the high 8 bits of the match offset follow, otherwise they are understood to be all set to 0.

**important note regarding match offsets: off by 1**

Note that the match offset is *off by 1*: a value of 0 refers to the byte preceding the current output index (N-1). A value of 1 refers to two bytes before the current output index (N-2) and so on. This is so that match offsets up to 256 can be encoded as a single byte, for extra compression.

**optional extra encoded match length**

If the encoded match length is 15 or more, the 'M' bits in the token form the value 15, and an extra byte follows here, with three possible types of value.

* 0-253: the value is added to the 15 stored in the token.
* 254: a second byte follows. The final encoded match length is 15 + 254 + the second byte, which gives an actual match length of 3 + 15 + 254 + the second byte.
* 255: a second and third byte follow, forming a little-endian 16-bit value. The final encoded match length is 15 + 255 + that 16-bit value, which gives an actual match length of 3 + 15 + 255 + that 16-bit value.

# Footer format

The stream ends with the EOD frame: the 3 length bytes are set to 0x00, 0x00, 0x00, and no block data follows.
