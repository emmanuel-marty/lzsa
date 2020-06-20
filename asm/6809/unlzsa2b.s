;  unlzsa2.s - 6809 backward decompression routine for raw LZSA2 - 189 bytes
;  compress with lzsa -f2 -r -b <original_file> <compressed_file>
;
;  in:  x = last byte of compressed data
;       y = last byte of decompression buffer
;  out: y = first byte of decompressed data
;
;  Copyright (C) 2020 Emmanuel Marty
;
;  This software is provided 'as-is', without any express or implied
;  warranty.  In no event will the authors be held liable for any damages
;  arising from the use of this software.
;
;  Permission is granted to anyone to use this software for any purpose,
;  including commercial applications, and to alter it and redistribute it
;  freely, subject to the following restrictions:
;
;  1. The origin of this software must not be misrepresented; you must not
;     claim that you wrote the original software. If you use this software
;     in a product, an acknowledgment in the product documentation would be
;     appreciated but is not required.
;  2. Altered source versions must be plainly marked as such, and must not be
;     misrepresented as being the original software.
;  3. This notice may not be removed or altered from any source distribution.

decompress_lzsa2
         lsr <lz2nibct,pcr ; reset nibble available flag
         leax 1,x
         leay 1,y

lz2token ldb ,-x           ; load next token into B: XYZ|LL|MMM
         pshs b            ; save it

         clra              ; clear A (high part of literals count)
         andb #$18         ; isolate LLL (embedded literals count) in B
         beq lz2nolt       ; skip if no literals
         cmpb #$18         ; LITERALS_RUN_LEN_V2?
         bne lz2declt      ; if not, we have the complete count, go unshift

         bsr lz2nibl       ; get extra literals length nibble in B
         addb #$03         ; add LITERALS_RUN_LEN_V2
         cmpb #$12         ; LITERALS_RUN_LEN_V2 + 15 ?
         bne lz2gotlt      ; if not, we have the full literals count, go copy

         addb ,-x         ; add extra literals count byte + LITERALS_RUN_LEN + 15
         bcc lz2gotlt      ; if no overflow, we got the complete count, copy

         ldd ,--x          ; load 16 bit count in D (low part in B, high in A)
         bra lz2gotlt      ; we now have the complete count, go copy

lz2declt lsrb              ; shift literals count into place
         lsrb
         lsrb
 
lz2gotlt tfr x,u
         tfr d,x           ; transfer 16-bit count into X
lz2cpylt lda ,-u           ; copy literal byte
         sta ,-y 
         leax -1,x         ; decrement X and update Z flag
         bne lz2cpylt      ; loop until all literal bytes are copied
         tfr u,x

lz2nolt  ldb ,s            ; get token again, don't pop it from the stack

         lslb              ; push token's X flag bit into carry
         bcs lz2replg      ; if token's X bit is set, rep or large offset

         lslb              ; push token's Y flag bit into carry
         bcs lz2offs9      ; if token's Y bit is set, 9 bits offset

         lslb              ; push token's Z flag bit into carry
         tfr cc,a          ; preserve cpu flags (to preserve carry)
         bsr lz2nibl       ; get offset nibble in B
         tfr a,cc          ; restore cpu flags

         rolb              ; shift Z flag from carry into bit 0 of B
         eorb #$e1         ; set bits 5-7 of offset, reverse bit 0
         lda #$ff          ; set bits 8-15 of offset
         bra lz2gotof

lz2offs9 clra              ; clear A (to prepare for high 8 bits of offset)
         lslb              ; push token's Z flag bit into carry         
         rola              ; shift Z flag from carry into bit 0 of A
         coma              ; set bits 9-15 of offset, reverse bit 8
         bra lz2lowof

lz2nibct fcb $00           ; nibble ready flag

lz2nibl  ldb #$aa
         lsr <lz2nibct,pcr  ; nibble ready?
         bcs lz2gotnb

         inc <lz2nibct,pcr  ; flag nibble as ready for next time
         ldb ,-x           ; load two nibbles
         stb <lz2nibl+1,pcr ; store nibble for next time (low 4 bits)

         lsrb              ; shift 4 high bits of nibble down
         lsrb
         lsrb
         lsrb

lz2gotnb andb #$0f         ; only keep low 4 bits
lz2done  rts

lz2replg lslb              ; push token's Y flag bit into carry
         bcs lz2rep16      ; if token's Y bit is set, rep or 16 bit offset

         lslb              ; push token's Z flag bit into carry
         tfr cc,a          ; preserve cpu flags (to preserve carry)
         bsr lz2nibl       ; get offset nibble in B
         tfr a,cc          ; restore cpu flags

         rolb              ; shift Z flag from carry into bit 0 of B
         eorb #$e1         ; set bits 13-15 of offset, reverse bit 8
         tfr b,a           ; copy bits 8-15 of offset into A
         suba #$02         ; substract 512 from offset
         bra lz2lowof

lz2rep16 bmi lz2repof      ; if token's Z flag bit is set, rep match
         
         lda ,-x           ; load high 8 bits of (negative, signed) offset
lz2lowof ldb ,-x           ; load low 8 bits of offset

lz2gotof nega              ; reverse sign of offset in D
         negb
         sbca #0
         std <lz2repof+1,pcr ; store match offset

lz2repof ldd #$aaaa        ; load match offset
         leau d,y          ; put backreference start address in U (dst+offset)
         
         puls b            ; restore token
         
         clra              ; clear A (high part of match length)
         andb #$07         ; isolate MMM (embedded match length)
         addb #$02         ; add MIN_MATCH_SIZE_V2
         cmpb #$09         ; MIN_MATCH_SIZE_V2 + MATCH_RUN_LEN_V2?
         bne lz2gotln      ; no, we have the full match length, go copy

         bsr lz2nibl       ; get offset nibble in B
         addb #$09         ; add MIN_MATCH_SIZE_V2 + MATCH_RUN_LEN_V2
         cmpb #$18         ; MIN_MATCH_SIZE_V2 + MATCH_RUN_LEN_V2 + 15?
         bne lz2gotln      ; if not, we have the full match length, go copy

         ldb ,-x           ; load extra length byte
         addb #$18         ; add MIN_MATCH_SIZE_V2 + MATCH_RUN_LEN_V2 + 15
         bcc lz2gotln      ; if no overflow, we have the full length
         beq lz2done       ; detect EOD code

         ldd ,--x          ; load 16-bit len in D (low part in B, high in A)

lz2gotln pshs x            ; save source compressed data pointer
         tfr d,x           ; copy match length to X

lz2cpymt lda ,-u           ; copy matched byte
         sta ,-y 
         leax -1,x         ; decrement X
         bne lz2cpymt      ; loop until all matched bytes are copied

         puls x            ; restore source compressed data pointer
         lbra lz2token     ; go decode next token
