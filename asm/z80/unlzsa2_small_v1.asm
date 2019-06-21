;
;  Size-optimized LZSA2 decompressor by spke (v.1 02-09/06/2019, 145 bytes)
;
;  The data must be compressed using the command line compressor by Emmanuel Marty
;  The compression is done as follows:
;
;  lzsa.exe -f2 -r <sourcefile> <outfile>
;
;  where option -r asks for the generation of raw (frame-less) data.
;
;  The decompression is done in the standard way:
;
;  ld hl,CompressedData
;  ld de,WhereToDecompress
;  call DecompressLZSA2
;
;  Of course, LZSA2 compression algorithm is (c) 2019 Emmanuel Marty,
;  see https://github.com/emmanuel-marty/lzsa for more information
;
;  Drop me an email if you have any comments/ideas/suggestions: zxintrospec@gmail.com
;

@DecompressLZSA2:
		xor a : ld b,a : exa : jr ReadToken

CASE0xx		ld d,#FF : cp %01000000 : jr c,CASE00x

CASE01x:	cp %01100000 : rl d

OffsetReadE:	ld e,(hl) : inc hl
		
SaveOffset:	ld iyl,e : ld iyh,d

MatchLen:	and %00000111 : add 2 : cp 9 : call z,ExtendedCode

CopyMatch:	ld c,a
		ex (sp),hl : push hl						; BC = len, DE = offset, HL = dest, SP ->[dest,src]
		add hl,de : pop de						; BC = len, DE = dest, HL = dest-offset, SP->[src]
		ldir : pop hl

ReadToken:	ld a,(hl) : ld ixl,a : inc hl
		and %00011000 : jr z,NoLiterals

		rrca : rrca : rrca
		call pe,ExtendedCode

		ld c,a
		ldir

NoLiterals:	push de : ld a,ixl
		or a : jp p,CASE0xx

CASE1xx		cp %11000000 : jr nc,CASE11x

CASE10x:	call ReadNibble
		ld d,a : ld a,c
		cp %10100000 : rl d
		dec d : dec d : jr OffsetReadE

CASE00x:	call ReadNibble
		ld e,a : ld a,c
		cp %00100000 : rl e : jr SaveOffset

CASE11x		cp %11100000 : jr c,CASE110

CASE111:	ld e,iyl : ld d,iyh : jr MatchLen

CASE110:	ld d,(hl) : inc hl : jr OffsetReadE

ExtendedCode:	call ReadNibble : inc a : jr z,ExtraByte
		sub #F0+1 : add c : ret
ExtraByte	ld a,15 : add c : add (hl) : inc hl : ret nc
		ld a,(hl) : inc hl
		ld b,(hl) : inc hl : ret nz
		pop de : pop de : ret

ReadNibble:	ld c,a : xor a : exa : ret m
UpdateNibble	ld a,(hl) : or #F0 : exa
		ld a,(hl) : inc hl : or #0F
		rrca : rrca : rrca : rrca : ret
















