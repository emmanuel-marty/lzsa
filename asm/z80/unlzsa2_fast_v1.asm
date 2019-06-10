;
;  Speed-optimized LZSA2 decompressor by spke (v.1 02-07/06/2019, 218 bytes)
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
		; A' stores next nibble as %1111.... or assumed to contain trash
		; B is assumed to be 0
		xor a : ld b,a : exa : jr ReadToken





LongerMatch:	exa : jp m,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : inc hl : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	sub #F0-9 : cp 15+9 : jr c,CopyMatch
		;inc a : jr z,LongMatch : sub #F0-9+1 : jp CopyMatch

LongMatch:	;ld a,24 : 
		add (hl) : inc hl : jr nc,CopyMatch
		ld c,(hl) : inc hl
		ld b,(hl) : inc hl
		jr nz,CopyMatch.useC
		pop de : ret




ManyLiterals:	ld a,18 : 
		add (hl) : inc hl : jr nc,CopyLiterals
		ld c,(hl) : inc hl
		ld a,b : ld b,(hl) : inc hl
		jr CopyLiterals.useBC




MoreLiterals:	ld b,(hl) : inc hl
		exa : jp m,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : inc hl : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	;sub #F0-3 : cp 15+3 : jr z,ManyLiterals
		inc a : jr z,ManyLiterals : sub #F0-3+1

CopyLiterals:	ld c,a
.useC		ld a,b : ld b,0
.useBC		ldir
		push de : or a : jp p,CASE0xx : jr CASE1xx





		; if "LL" of the byte token is equal to 0,
		; there are no literals to copy
NoLiterals:	xor (hl) : inc hl
		push de : jp m,CASE1xx

		; short (5 or 9 bit long) offsets
CASE0xx		ld d,#FF : cp %01000000 : jr c,CASE00x

		; "01x": the case of the 9-bit offset
CASE01x:	cp %01100000 : rl d

ReadOffsetE:	ld e,(hl) : inc hl

SaveOffset:	ld ixl,e : ld ixh,d

MatchLen:	inc a : and %00000111 : jr z,LongerMatch : inc a

CopyMatch:	ld c,a
.useC		ex (sp),hl : push hl					; BC = len, DE = offset, HL = dest, SP ->[dest,src]
		add hl,de : pop de					; BC = len, DE = dest, HL = dest-offset, SP->[src]
		ldir : pop hl

		; compressed data stream contains records
		; each record begins with the byte token "XYZ|LL|MMM"
ReadToken:	ld a,(hl) : and %00011000 : jr z,NoLiterals

		jp pe,MoreLiterals					; 00 has already been processed; this identifies the case of 11
		rrca : rrca : rrca

		ld c,a : ld a,(hl) : inc hl				; token is re-read for further processing
		ldir

		; the token and literals are followed by the offset
		push de : or a : jp p,CASE0xx

CASE1xx		cp %11000000 : jr nc,CASE11x

		; "10x": the case of the 5-bit offset
CASE10x:	ld c,a : xor a
		exa : jp m,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : inc hl : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	ld d,a : ld a,c
		cp %10100000 : rl d
		dec d : dec d : jr ReadOffsetE

		; "00x": the case of the 5-bit offset
CASE00x:	ld c,a : xor a
		exa : jp m,.noUpdate

			ld a,(hl) : or #F0 : exa
			ld a,(hl) : inc hl : or #0F
			rrca : rrca : rrca : rrca

.noUpdate	ld e,a : ld a,c
		cp %00100000 : rl e : jp SaveOffset

		; two remaining cases
CASE11x		cp %11100000 : jr c,CASE110

		; "111": repeated offset
CASE111:	ld e,ixl : ld d,ixh : jr MatchLen

		; "110": 16-bit offset
CASE110:	ld d,(hl) : inc hl : jr ReadOffsetE

;ReadNibble:	; 17 bytes, 44 t-state per nibble
;		exa : ret m				; 4+11 = 15t
;UpdateNibble:
;		ld a,(hl) : or #F0 : exa
;		ld a,(hl) : inc hl : or #0F
;		rrca : rrca : rrca : rrca : ret		; 4+5 + 7+7+4+7+6+7+4+4+4+4+10 = 73t














