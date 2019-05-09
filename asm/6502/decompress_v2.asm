; -----------------------------------------------------------------------------
; Decompress raw LZSA2 block.
; Create one with lzsa -r -f2 <original_file> <compressed_file>
;
; in:
; * LZSA_SRC_LO and LZSA_SRC_HI contain the compressed raw block address
; * LZSA_DST_LO and LZSA_DST_HI contain the destination buffer address
;
; out:
; * LZSA_DST_LO and LZSA_DST_HI contain the last decompressed byte address, +1
; -----------------------------------------------------------------------------
;
;  Copyright (C) 2019 Emmanuel Marty
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
; -----------------------------------------------------------------------------

OFFSLO = $43                            ; zero-page location for temp offset
OFFSHI = $44
FIXUP = $4B
NIBBLES = $FB
NIBCOUNT = $FC

DECOMPRESS_LZSA2
   LDY #$00
   STY NIBBLES
   STY NIBCOUNT

DECODE_TOKEN
   JSR GETSRC                           ; read token byte: XYZ|LL|MMM
   PHA                                  ; preserve token on stack

   AND #$18                             ; isolate literals count (LL)
   BEQ NO_LITERALS                      ; skip if no literals to copy
   CMP #$18                             ; LITERALS_RUN_LEN_V2 << 3?
   BNE EMBEDDED_LITERALS                ; if less, count is directly embedded in token

   JSR GETNIBBLE                        ; get extra literals length nibble
   CLC                                  ; add nibble to len from token
   ADC #$03                             ; (LITERALS_RUN_LEN_V2)
   CMP #$12                             ; LITERALS_RUN_LEN_V2 + 15 ?
   BNE PREPARE_COPY_LITERALS            ; if less, literals count is complete

   JSR GETSRC                           ; get extra byte of variable literals count
   TAX                                  ; non-zero?
   BNE PREPARE_COPY_LITERALS_HIGH       ; if so, literals count is complete

                                        ; handle 16 bits literals count
                                        ; literals count = directly these 16 bits
   JSR GETLARGESRC                      ; grab low 8 bits in X, high 8 bits in A
   TAY                                  ; put high 8 bits in Y
   JMP PREPARE_COPY_LITERALS_HIGH

EMBEDDED_LITERALS
   LSR A                                ; shift literals count into place
   LSR A
   LSR A

PREPARE_COPY_LITERALS
   TAX
PREPARE_COPY_LITERALS_HIGH
   INY

COPY_LITERALS
   JSR GETPUT                           ; copy one byte of literals
   DEX
   BNE COPY_LITERALS
   DEY
   BNE COPY_LITERALS
   
NO_LITERALS
   PLA                                  ; retrieve token from stack
   PHA                                  ; preserve token again
   BMI REPMATCH_OR_LARGE_OFFSET         ; 1YZ: rep-match or 13/16 bit offset

   ASL                                  ; 0YZ: 5 or 9 bit offset
   BMI OFFSET_9_BIT         
    
                                        ; 00Z: 5 bit offset
   LSR A                                ; Shift Z (offset bit 4) in place
   LSR A
   AND #$10
   STA FIXUP

   JSR GETNIBBLE                        ; get nibble for offset bits 0-3
   ORA FIXUP                            ; merge offset bit 4
   ORA #$E0                             ; set offset bits 7-5 to 1
   TAX                                  ; store low byte of match offset
   LDA #$0FF                            ; set offset bits 15-8 to 1
   BNE GOT_OFFSET                       ; (*same as JMP GOT_OFFSET but shorter)
   
OFFSET_9_BIT                            ; 01Z: 9 bit offset
   ASL                                  ; shift Z (offset bit 8) in place
   ROL
   ROL
   ORA #$FE                             ; set offset bits 15-9 to 1
   STA OFFSHI

   JSR GETSRC                           ; get offset bits 0-7 from stream in A
   TAX                                  ; store low byte of match offset
   JMP GOT_OFFSET_LO                    ; go prepare match

REPMATCH_OR_LARGE_OFFSET
   ASL                                  ; 13 bit offset?
   BMI REPMATCH_OR_16_BIT               ; handle rep-match or 16-bit offset if not

                                        ; 10Z: 13 bit offset

   LSR A                                ; shift Z (offset bit 4) in place
   LSR A
   AND #$10
   STA FIXUP

   JSR GETSRC                           ; get offset bits 0-7 from stream in A
   TAX                                  ; store low byte of match offset

   JSR GETNIBBLE                        ; get nibble for offset bits 8-11
   ORA FIXUP                            ; merge offset bit 12
   CLC
   ADC #$DE                             ; set bits 13-15 to 1 and substract 2 (to substract 512)
   BNE GOT_OFFSET                       ; go prepare match (*same as JMP GOT_OFFSET but shorter)

REPMATCH_OR_16_BIT                      ; rep-match or 16 bit offset
   ASL                                  ; XYZ=111?
   BMI REP_MATCH                        ; reuse previous offset if so (rep-match)
   
                                        ; 110: handle 16 bit offset
   JSR GETLARGESRC                      ; grab low 8 bits in X, high 8 bits in A

GOT_OFFSET
   STA OFFSHI                           ; store final match offset
GOT_OFFSET_LO
   STX OFFSLO

REP_MATCH
   CLC                                  ; add dest + match offset
   LDA PUTDST+1                         ; low 8 bits
   ADC OFFSLO
   STA COPY_MATCH_LOOP+1                ; store back reference address
   LDA OFFSHI                           ; high 8 bits
   ADC PUTDST+2
   STA COPY_MATCH_LOOP+2                ; store high 8 bits of address
   
   PLA                                  ; retrieve token from stack again
   AND #$07                             ; isolate match len (MMM)
   CLC
   ADC #$02                             ; add MIN_MATCH_SIZE_V2
   CMP #$09                             ; MIN_MATCH_SIZE_V2 + MATCH_RUN_LEN_V2?
   BNE PREPARE_COPY_MATCH               ; if less, length is directly embedded in token

   JSR GETNIBBLE                        ; get extra match length nibble
   CLC                                  ; add nibble to len from token
   ADC #$09                             ; (MIN_MATCH_SIZE_V2 + MATCH_RUN_LEN_V2)
   CMP #$18                             ; MIN_MATCH_SIZE_V2 + MATCH_RUN_LEN_V2 + 15?
   BNE PREPARE_COPY_MATCH               ; if less, match length is complete

   JSR GETSRC                           ; get extra byte of variable match length
   TAX                                  ; non-zero?
   BNE PREPARE_COPY_MATCH_Y             ; if so, the match length is complete

                                        ; Handle 16 bits match length
   JSR GETLARGESRC                      ; grab low 8 bits in X, high 8 bits in A
   TAY                                  ; put high 8 bits in Y
                                        ; large match length with zero high byte?
   BEQ DECOMPRESSION_DONE               ; if so, this is the EOD code, bail
   TXA

PREPARE_COPY_MATCH
   TAX
PREPARE_COPY_MATCH_Y
   INY

COPY_MATCH_LOOP
   LDA $AAAA                            ; get one byte of backreference
   INC COPY_MATCH_LOOP+1
   BNE GETMATCH_DONE
   INC COPY_MATCH_LOOP+2
GETMATCH_DONE
   JSR PUTDST                           ; copy to destination
   DEX
   BNE COPY_MATCH_LOOP
   DEY
   BNE COPY_MATCH_LOOP
   JMP DECODE_TOKEN

GETNIBBLE
   DEC NIBCOUNT
   BPL HAS_NIBBLES

   LDA #$01
   STA NIBCOUNT
   JSR GETSRC                           ; get 2 nibbles
   STA NIBBLES
   LSR A
   LSR A
   LSR A
   LSR A
   RTS

HAS_NIBBLES
   LDA NIBBLES
   AND #$0F                             ; isolate low 4 bits of nibble
   RTS

GETPUT
   JSR GETSRC
PUTDST
LZSA_DST_LO = *+1
LZSA_DST_HI = *+2
   STA $AAAA
   INC PUTDST+1
   BNE PUTDST_DONE
   INC PUTDST+2
PUTDST_DONE
DECOMPRESSION_DONE
   RTS

GETLARGESRC
   JSR GETSRC                           ; grab low 8 bits
   TAX                                  ; move to X
                                        ; fall through grab high 8 bits

GETSRC
LZSA_SRC_LO = *+1
LZSA_SRC_HI = *+2
   LDA $AAAA
   INC GETSRC+1
   BNE GETSRC_DONE
   INC GETSRC+2
GETSRC_DONE
   RTS
