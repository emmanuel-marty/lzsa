; ***************************************************************************
; ***************************************************************************
;
; lzsa2_6502.s
;
; NMOS 6502 decompressor for data stored in Emmanuel Marty's LZSA2 format.
;
; This code is written for the ACME assembler.
;
; The code is 241 bytes for the small version, and 256 bytes for the normal.
;
; Copyright John Brandwood 2021.
;
; Distributed under the Boost Software License, Version 1.0.
; (See accompanying file LICENSE_1_0.txt or copy at
;  http://www.boost.org/LICENSE_1_0.txt)
;
; ***************************************************************************
; ***************************************************************************



; ***************************************************************************
; ***************************************************************************
;
; Decompression Options & Macros
;

                ;
                ; Choose size over decompression speed (within sane limits)?
                ;

LZSA_SMALL_SIZE =       0



; ***************************************************************************
; ***************************************************************************
;
; Data usage is last 11 bytes of zero-page.
;

lzsa_length     =       lzsa_winptr             ; 1 word.

lzsa_cmdbuf     =       $F5                     ; 1 byte.
lzsa_nibflg     =       $F6                     ; 1 byte.
lzsa_nibble     =       $F7                     ; 1 byte.
lzsa_offset     =       $F8                     ; 1 word.
lzsa_winptr     =       $FA                     ; 1 word.
lzsa_srcptr     =       $FC                     ; 1 word.
lzsa_dstptr     =       $FE                     ; 1 word.

lzsa_length     =       lzsa_winptr             ; 1 word.

LZSA_SRC_LO     =       $FC
LZSA_SRC_HI     =       $FD
LZSA_DST_LO     =       $FE
LZSA_DST_HI     =       $FF



; ***************************************************************************
; ***************************************************************************
;
; lzsa2_unpack - Decompress data stored in Emmanuel Marty's LZSA2 format.
;
; Args: lzsa_srcptr = ptr to compessed data
; Args: lzsa_dstptr = ptr to output buffer
;

DECOMPRESS_LZSA2_FAST:
lzsa2_unpack:   ldx     #$00                    ; Hi-byte of length or offset.
                ldy     #$00                    ; Initialize source index.
                sty     <lzsa_nibflg            ; Initialize nibble buffer.

                ;
                ; Copy bytes from compressed source data.
                ;
                ; N.B. X=0 is expected and guaranteed when we get here.
                ;

.cp_length:     !if     LZSA_SMALL_SIZE {

                jsr     .get_byte

                } else {

                lda     (lzsa_srcptr),y
                inc     <lzsa_srcptr + 0
                bne     .cp_skip0
                inc     <lzsa_srcptr + 1

                }

.cp_skip0:      sta     <lzsa_cmdbuf            ; Preserve this for later.
                and     #$18                    ; Extract literal length.
                beq     .lz_offset              ; Skip directly to match?

                lsr                             ; Get 2-bit literal length.
                lsr
                lsr
                cmp     #$03                    ; Extended length?
                bcc     .cp_got_len

                jsr     .get_length             ; X=0 for literals, returns CC.
                stx     .cp_npages + 1          ; Hi-byte of length.

.cp_got_len:    tax                             ; Lo-byte of length.

.cp_byte:       lda     (lzsa_srcptr),y         ; CC throughout the execution of
                sta     (lzsa_dstptr),y         ; of this .cp_page loop.
                inc     <lzsa_srcptr + 0
                bne     .cp_skip1
                inc     <lzsa_srcptr + 1
.cp_skip1:      inc     <lzsa_dstptr + 0
                bne     .cp_skip2
                inc     <lzsa_dstptr + 1
.cp_skip2:      dex
                bne     .cp_byte
.cp_npages:     lda     #0                      ; Any full pages left to copy?
                beq     .lz_offset

                dec     .cp_npages + 1          ; Unlikely, so can be slow.
                bcc     .cp_byte                ; Always true!

                ;
                ; Copy bytes from decompressed window.
                ;
                ; N.B. X=0 is expected and guaranteed when we get here.
                ;
                ; xyz
                ; ===========================
                ; 00z  5-bit offset
                ; 01z  9-bit offset
                ; 10z  13-bit offset
                ; 110  16-bit offset
                ; 111  repeat offset
                ;

.lz_offset:     lda     <lzsa_cmdbuf
                asl
                bcs     .get_13_16_rep

.get_5_9_bits:  dex                             ; X=$FF for a 5-bit offset.
                asl
                bcs     .get_9_bits             ; Fall through if 5-bit.

.get_13_bits:   asl                             ; Both 5-bit and 13-bit read
                php                             ; a nibble.
                jsr     .get_nibble
                plp
                rol                             ; Shift into position, clr C.
                eor     #$E1
                cpx     #$00                    ; X=$FF for a 5-bit offset.
                bne     .set_offset
                sbc     #2                      ; 13-bit offset from $FE00.
                bne     .set_hi_8               ; Always NZ from previous SBC.

.get_9_bits:    asl                             ; X=$FF if CC, X=$FE if CS.
                bcc     .get_lo_8
                dex
                bcs     .get_lo_8               ; Always CS from previous BCC.

.get_13_16_rep: asl
                bcc     .get_13_bits            ; Shares code with 5-bit path.

.get_16_rep:    bmi     .lz_length              ; Repeat previous offset.

.get_16_bits:   jsr     .get_byte               ; Get hi-byte of offset.

.set_hi_8:      tax

.get_lo_8:      !if     LZSA_SMALL_SIZE {

                jsr     .get_byte               ; Get lo-byte of offset.

                } else {

                lda     (lzsa_srcptr),y         ; Get lo-byte of offset.
                inc     <lzsa_srcptr + 0
                bne     .set_offset
                inc     <lzsa_srcptr + 1

                }

.set_offset:    sta     <lzsa_offset + 0        ; Save new offset.
                stx     <lzsa_offset + 1

.lz_length:     ldx     #1                      ; Hi-byte of length+256.

                lda     <lzsa_cmdbuf
                and     #$07
                clc
                adc     #$02
                cmp     #$09                    ; Extended length?
                bcc     .got_lz_len

                jsr     .get_length             ; X=1 for match, returns CC.
                inx                             ; Hi-byte of length+256.

.got_lz_len:    eor     #$FF                    ; Negate the lo-byte of length.
                tay
                eor     #$FF

.get_lz_dst:    adc     <lzsa_dstptr + 0        ; Calc address of partial page.
                sta     <lzsa_dstptr + 0        ; Always CC from previous CMP.
                iny
                bcs     .get_lz_win
                beq     .get_lz_win             ; Is lo-byte of length zero?
                dec     <lzsa_dstptr + 1

.get_lz_win:    clc                             ; Calc address of match.
                adc     <lzsa_offset + 0        ; N.B. Offset is negative!
                sta     <lzsa_winptr + 0
                lda     <lzsa_dstptr + 1
                adc     <lzsa_offset + 1
                sta     <lzsa_winptr + 1

.lz_byte:       lda     (lzsa_winptr),y
                sta     (lzsa_dstptr),y
                iny
                bne     .lz_byte
                inc     <lzsa_dstptr + 1
                dex                             ; Any full pages left to copy?
                bne     .lz_more

                jmp     .cp_length              ; Loop around to the beginning.

.lz_more:       inc     <lzsa_winptr + 1        ; Unlikely, so can be slow.
                bne     .lz_byte                ; Always true!

                ;
                ; Lookup tables to differentiate literal and match lengths.
                ;

.nibl_len_tbl:  !byte   3                       ; 0+3 (for literal).
                !byte   9                       ; 2+7 (for match).

.byte_len_tbl:  !byte   18 - 1                  ; 0+3+15 - CS (for literal).
                !byte   24 - 1                  ; 2+7+15 - CS (for match).

                ;
                ; Get 16-bit length in X:A register pair, return with CC.
                ;

.get_length:    jsr     .get_nibble
                cmp     #$0F                    ; Extended length?
                bcs     .byte_length
                adc     .nibl_len_tbl,x         ; Always CC from previous CMP.

.got_length:    ldx     #$00                    ; Set hi-byte of 4 & 8 bit
                rts                             ; lengths.

.byte_length:   jsr     .get_byte               ; So rare, this can be slow!
                adc     .byte_len_tbl,x         ; Always CS from previous CMP.
                bcc     .got_length
                beq     .finished

.word_length:   clc                             ; MUST return CC!
                jsr     .get_byte               ; So rare, this can be slow!
                pha
                jsr     .get_byte               ; So rare, this can be slow!
                tax
                pla
                bne     .got_word               ; Check for zero lo-byte.
                dex                             ; Do one less page loop if so.
.got_word:      rts

.get_byte:      lda     (lzsa_srcptr),y         ; Subroutine version for when
                inc     <lzsa_srcptr + 0        ; inlining isn't advantageous.
                bne     .got_byte
                inc     <lzsa_srcptr + 1
.got_byte:      rts

.finished:      pla                             ; Decompression completed, pop
                pla                             ; return address.
                rts

                ;
                ; Get a nibble value from compressed data in A.
                ;

.get_nibble:    lsr     <lzsa_nibflg            ; Is there a nibble waiting?
                lda     <lzsa_nibble            ; Extract the lo-nibble.
                bcs     .got_nibble

                inc     <lzsa_nibflg            ; Reset the flag.

                !if     LZSA_SMALL_SIZE {
                jsr     .get_byte

                } else {

                lda     (lzsa_srcptr),y
                inc     <lzsa_srcptr + 0
                bne     .set_nibble
                inc     <lzsa_srcptr + 1

                }

.set_nibble:    sta     <lzsa_nibble            ; Preserve for next time.
                lsr                             ; Extract the hi-nibble.
                lsr
                lsr
                lsr

.got_nibble:    and     #$0F
                rts
