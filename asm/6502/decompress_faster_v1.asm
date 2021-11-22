; ***************************************************************************
; ***************************************************************************
;
; lzsa1_6502.s
;
; NMOS 6502 decompressor for data stored in Emmanuel Marty's LZSA1 format.
;
; This code is written for the ACME assembler.
;
; The code is 165 bytes for the small version, and 191 bytes for the normal.
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
; Data usage is last 7 bytes of zero-page.
;

lzsa_cmdbuf     =       $F9                     ; 1 byte.
lzsa_winptr     =       $FA                     ; 1 word.
lzsa_srcptr     =       $FC                     ; 1 word.
lzsa_dstptr     =       $FE                     ; 1 word.

lzsa_offset     =       lzsa_winptr

LZSA_SRC_LO     =       $FC
LZSA_SRC_HI     =       $FD
LZSA_DST_LO     =       $FE
LZSA_DST_HI     =       $FF



; ***************************************************************************
; ***************************************************************************
;
; lzsa1_unpack - Decompress data stored in Emmanuel Marty's LZSA1 format.
;
; Args: lzsa_srcptr = ptr to compessed data
; Args: lzsa_dstptr = ptr to output buffer
;

DECOMPRESS_LZSA1_FAST:
lzsa1_unpack:   ldy     #0                      ; Initialize source index.
                ldx     #0                      ; Initialize hi-byte of length.

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
                and     #$70                    ; Extract literal length.
                lsr                             ; Set CC before ...
                beq     .lz_offset              ; Skip directly to match?

                lsr                             ; Get 3-bit literal length.
                lsr
                lsr
                cmp     #$07                    ; Extended length?
                bcc     .cp_got_len

                jsr     .get_length             ; X=0, CS from CMP, returns CC.
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

                !if     LZSA_SMALL_SIZE {

                ;
                ; Copy bytes from decompressed window.
                ;
                ; Shorter but slower version.
                ;
                ; N.B. X=0 is expected and guaranteed when we get here.
                ;

.lz_offset:     jsr     .get_byte               ; Get offset-lo.

.offset_lo:     adc     <lzsa_dstptr + 0        ; Always CC from .cp_page loop.
                sta     <lzsa_winptr + 0

                lda     #$FF
                bit     <lzsa_cmdbuf
                bpl     .offset_hi

                jsr     .get_byte               ; Get offset-hi.

.offset_hi:     adc     <lzsa_dstptr + 1        ; lzsa_winptr < lzsa_dstptr, so
                sta     <lzsa_winptr + 1        ; always leaves CS.

.lz_length:     lda     <lzsa_cmdbuf            ; X=0 from previous loop.
                and     #$0F
                adc     #$03 - 1                ; CS from previous ADC.
                cmp     #$12                    ; Extended length?
                bcc     .lz_got_len

                jsr     .get_length             ; CS from CMP, X=0, returns CC.
                stx     .lz_npages + 1          ; Hi-byte of length.

.lz_got_len:    tax                             ; Lo-byte of length.

.lz_byte:       lda     (lzsa_winptr),y         ; CC throughout the execution of
                sta     (lzsa_dstptr),y         ; of this .lz_page loop.
                inc     <lzsa_winptr + 0
                bne     .lz_skip1
                inc     <lzsa_winptr + 1
.lz_skip1:      inc     <lzsa_dstptr + 0
                bne     .lz_skip2
                inc     <lzsa_dstptr + 1
.lz_skip2:      dex
                bne     .lz_byte
.lz_npages:     lda     #0                      ; Any full pages left to copy?
                beq     .cp_length

                dec     .lz_npages + 1          ; Unlikely, so can be slow.
                bcc     .lz_byte                ; Always true!

                } else {

                ;
                ; Copy bytes from decompressed window.
                ;
                ; Longer but faster.
                ;
                ; N.B. X=0 is expected and guaranteed when we get here.
                ;

.lz_offset:     lda     (lzsa_srcptr),y         ; Get offset-lo.
                inc     <lzsa_srcptr + 0
                bne     .offset_lo
                inc     <lzsa_srcptr + 1

.offset_lo:     sta     <lzsa_offset + 0

                lda     #$FF                    ; Get offset-hi.
                bit     <lzsa_cmdbuf
                bpl     .offset_hi

                lda     (lzsa_srcptr),y
                inc     <lzsa_srcptr + 0
                bne     .offset_hi
                inc     <lzsa_srcptr + 1

.offset_hi:     sta     <lzsa_offset + 1

.lz_length:     lda     <lzsa_cmdbuf            ; X=0 from previous loop.
                and     #$0F
                adc     #$03                    ; Always CC from .cp_page loop.
                cmp     #$12                    ; Extended length?
                bcc     .got_lz_len

                jsr     .get_length             ; X=0, CS from CMP, returns CC.

.got_lz_len:    inx                             ; Hi-byte of length+256.

                eor     #$FF                    ; Negate the lo-byte of length
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

                }

                ;
                ; Get 16-bit length in X:A register pair, return with CC.
                ;
                ; N.B. X=0 is expected and guaranteed when we get here.
                ;

.get_length:    clc                             ; Add on the next byte to get
                adc     (lzsa_srcptr),y         ; the length.
                inc     <lzsa_srcptr + 0
                bne     .skip_inc
                inc     <lzsa_srcptr + 1

.skip_inc:      bcc     .got_length             ; No overflow means done.
                clc                             ; MUST return CC!
                tax                             ; Preserve overflow value.

.extra_byte:    jsr     .get_byte               ; So rare, this can be slow!
                pha
                txa                             ; Overflow to 256 or 257?
                beq     .extra_word

.check_length:  pla                             ; Length-lo.
                bne     .got_length             ; Check for zero.
                dex                             ; Do one less page loop if so.
.got_length:    rts

.extra_word:    jsr     .get_byte               ; So rare, this can be slow!
                tax
                bne     .check_length           ; Length-hi == 0 at EOF.

.finished:      pla                             ; Length-lo.
                pla                             ; Decompression completed, pop
                pla                             ; return address.
                rts

.get_byte:      lda     (lzsa_srcptr),y         ; Subroutine version for when
                inc     <lzsa_srcptr + 0        ; inlining isn't advantageous.
                bne     .got_byte
                inc     <lzsa_srcptr + 1        ; Inc & test for bank overflow.
.got_byte:      rts
