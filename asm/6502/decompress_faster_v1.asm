; ***************************************************************************
; ***************************************************************************
;
; lzsa1_6502.s
;
; NMOS 6502 decompressor for data stored in Emmanuel Marty's LZSA1 format.
;
; This code is written for the ACME assembler.
;
; The code is 167 bytes for the small version, and 192 bytes for the normal.
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
; Data usage is last 8 bytes of zero-page.
;

lzsa_length     =       $F8                     ; 1 byte.
lzsa_cmdbuf     =       $F9                     ; 1 byte.
lzsa_winptr     =       $FA                     ; 1 word.
lzsa_srcptr     =       $FC                     ; 1 word.
lzsa_dstptr     =       $FE                     ; 1 word.

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
; Uses: lots!
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
                bcc     .inc_cp_len

                jsr     .get_length             ; CS from CMP, X=0.

                ora     #0                      ; Check the lo-byte of length
                beq     .put_cp_len             ; without effecting CC.

.inc_cp_len:    inx                             ; Increment # of pages to copy.

.put_cp_len:    stx     <lzsa_length
                tax

.cp_page:       lda     (lzsa_srcptr),y         ; CC throughout the execution of
                sta     (lzsa_dstptr),y         ; of this .cp_page loop.
                inc     <lzsa_srcptr + 0
                bne     .cp_skip1
                inc     <lzsa_srcptr + 1
.cp_skip1:      inc     <lzsa_dstptr + 0
                bne     .cp_skip2
                inc     <lzsa_dstptr + 1
.cp_skip2:      dex
                bne     .cp_page
                dec     <lzsa_length            ; Any full pages left to copy?
                bne     .cp_page

                ;
                ; Copy bytes from decompressed window.
                ;
                ; N.B. X=0 is expected and guaranteed when we get here.
                ;

.lz_offset:     !if     LZSA_SMALL_SIZE {

                jsr     .get_byte

                } else {

                lda     (lzsa_srcptr),y
                inc     <lzsa_srcptr + 0
                bne     .offset_lo
                inc     <lzsa_srcptr + 1

                }

.offset_lo:     adc     <lzsa_dstptr + 0        ; Always CC from .cp_page loop.
                sta     <lzsa_winptr + 0

                lda     #$FF
                bit     <lzsa_cmdbuf
                bpl     .offset_hi

                !if     LZSA_SMALL_SIZE {
                jsr     .get_byte

                } else {

                lda     (lzsa_srcptr),y
                inc     <lzsa_srcptr + 0
                bne     .offset_hi
                inc     <lzsa_srcptr + 1

                }

.offset_hi:     adc     <lzsa_dstptr + 1        ; lzsa_winptr < lzsa_dstptr, so
                sta     <lzsa_winptr + 1        ; always leaves CS.

                !if     LZSA_SMALL_SIZE {

.lz_length:     lda     <lzsa_cmdbuf            ; X=0 from previous loop.
                and     #$0F
                adc     #$03 - 1                ; CS from previous ADC.
                cmp     #$12                    ; Extended length?
                bcc     .inc_lz_len

                jsr     .get_length             ; CS from CMP, X=0, returns CC.

                ora     #0                      ; Check the lo-byte of length
                beq     .put_lz_len             ; without effecting CC.

.inc_lz_len:    inx                             ; Increment # of pages to copy.

.put_lz_len:    stx     <lzsa_length
                tax

.lz_page:       lda     (lzsa_winptr),y         ; CC throughout the execution of
                sta     (lzsa_dstptr),y         ; of this .lz_page loop.
                inc     <lzsa_winptr + 0
                bne     .skip3
                inc     <lzsa_winptr + 1
.skip3:         inc     <lzsa_dstptr + 0
                bne     .skip4
                inc     <lzsa_dstptr + 1
.skip4:         dex
                bne     .lz_page
                dec     <lzsa_length            ; Any full pages left to copy?
                bne     .lz_page

                jmp     .cp_length              ; Loop around to the beginning.

                } else {

.lz_length:     lda     <lzsa_cmdbuf            ; X=0 from previous loop.
                and     #$0F
                adc     #$03 - 1                ; CS from previous ADC.
                cmp     #$12                    ; Extended length?
                bcc     .got_lz_len

                jsr     .get_length             ; CS from CMP, X=0, returns CC.

.got_lz_len:    tay                             ; Check the lo-byte of length.
                beq     .lz_page

                inx                             ; Increment # of pages to copy.

.get_lz_win:    adc     <lzsa_winptr + 0        ; Calc address of partial page.
                sta     <lzsa_winptr + 0        ; Always CC from previous CMP.
                bcs     .get_lz_dst
                dec     <lzsa_winptr + 1

.get_lz_dst:    tya                             ; Calc address of partial page.
                clc
                adc     <lzsa_dstptr + 0
                sta     <lzsa_dstptr + 0
                bcs     .get_lz_idx
                dec     <lzsa_dstptr + 1

.get_lz_idx:    tya                             ; Negate the lo-byte of length.
                eor     #$FF
                tay
                iny

.lz_page:       lda     (lzsa_winptr),y
                sta     (lzsa_dstptr),y
                iny
                bne     .lz_page
                inc     <lzsa_winptr + 1
                inc     <lzsa_dstptr + 1
                dex                             ; Any full pages left to copy?
                bne     .lz_page

                jmp     .cp_length              ; Loop around to the beginning.

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
                cmp     #$01                    ; Overflow to 256 or 257?
                bcc     .extra_word

.extra_byte:    clc                             ; MUST return CC!
                inx
                bne     .get_byte               ; Always NZ from previous INX.

.extra_word:    jsr     .get_byte               ; So rare, this can be slow!
                pha
                jsr     .get_byte               ; So rare, this can be slow!
                tax
                beq     .finished               ; Length-hi == 0 at EOF.
                pla                             ; Length-lo.
                rts

.get_byte:      lda     (lzsa_srcptr),y         ; Subroutine version for when
                inc     <lzsa_srcptr + 0        ; inlining isn't advantageous.
                beq     .next_page
.got_length:    rts

.next_page:     inc     <lzsa_srcptr + 1        ; Inc & test for bank overflow.
                rts

.finished:      pla                             ; Length-lo.
                pla                             ; Decompression completed, pop
                pla                             ; return address.
                rts
