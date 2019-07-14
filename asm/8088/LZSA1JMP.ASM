;  lzsa1fta.asm time-efficient decompressor implementation for 8086 CPUs.
;  Turbo Assembler IDEAL mode dialect; can also be assembled with NASM.
;
;  Usual DOS assembler SMALL model assumptions apply.  This code:
;  - Assumes it was invoked via NEAR call (change RET to RETF for FAR calls)
;  - Is interrupt-safe
;  - Is not re-entrant (do not decompress while already running decompression)
;  - Trashes all data and segment registers
;
;  Copyright (C) 2019 Jim Leonard, Emmanuel Marty
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

        IDEAL
        P8086

SEGMENT CODE para public

ASSUME  cs:CODE, ds:CODE

PUBLIC  lzsa1_decompress_speed_jumptable

;  ---------------------------------------------------------------------------
;  Decompress raw LZSA1 block
;  inputs:
;  * ds:si: raw LZSA1 block
;  * es:di: output buffer
;  output:
;  * ax:    decompressed size
;  ---------------------------------------------------------------------------

;Jump table for handling LLL bits in initial LZSA1 tokens.
;Previous code would SHR val,4 to get a count from 0 to 7, then rep movsb.
;We can overload the shift operation into a jump table that jumps directly
;to optimized copying routine for 0-7 bytes.  Must declare in code segment.
;Note: If this looks strange for declaring a jump table, that's because it
;is a workaround for the Turbo Pascal harness that tests it.  Turbo Pascal
;treats OFFSET (label) as a relocatble item and throws an error, so we fool
;it by building the table with absolute EQU/literals instead.
L0b EQU OFFSET check_offset_size
L1b EQU OFFSET copy1b
L2b EQU OFFSET copy2b
L3b EQU OFFSET copy3b
L4b EQU OFFSET copy4b
L5b EQU OFFSET copy5b
L6b EQU OFFSET copy6b
L7b EQU OFFSET need_length_byte
copytable DW L0b,L0b,L0b,L0b,L0b,L0b,L0b,L0b
          DW L1b,L1b,L1b,L1b,L1b,L1b,L1b,L1b
          DW L2b,L2b,L2b,L2b,L2b,L2b,L2b,L2b
          DW L3b,L3b,L3b,L3b,L3b,L3b,L3b,L3b
          DW L4b,L4b,L4b,L4b,L4b,L4b,L4b,L4b
          DW L5b,L5b,L5b,L5b,L5b,L5b,L5b,L5b
          DW L6b,L6b,L6b,L6b,L6b,L6b,L6b,L6b
          DW L7b,L7b,L7b,L7b,L7b,L7b,L7b,L7b

PROC    lzsa1_decompress_speed_jumptable  NEAR

lzsa1_start:
        push    di              ;remember decompression offset
        cld                     ;ensure string ops move forward
        xor     cx,cx

@@decode_token:
        xchg    cx,ax           ;clear ah (cx = 0 from match copy's rep movsb)
        lodsb                   ;read token byte: O|LLL|MMMM
        mov     dx,ax           ;copy our token to dl for later MMMM handling

        and     al,070H         ;isolate literals length in token (LLL)
        jz      check_offset_size ;if LLL=0, we have no literals; goto match

; Jump to short copy routine for LLL=1 though 6, need_length_byte for LLL=7
        mov     bx,ax           ;prep for table lookup (must copy, don't XCHG!)
        jmp     [cs:copytable+bx]

need_length_byte:
        lodsb                   ;grab extra length byte
        add     al,07H          ;add LITERALS_RUN_LEN
        jnc     @@got_literals_exact ;if no overflow, we have full count
        je      @@big_literals

@@mid_literals:
        lodsb                   ;grab single extra length byte
        inc     ah              ;add 256
        xchg    cx,ax           ;with longer counts, we can save some time
        shr     cx,1            ;by doing a word copy instead of a byte copy.
        rep     movsw           ;We don't need to account for overlap because
        adc     cx,0            ;source for literals isn't the output buffer.
        rep     movsb
        jmp     check_offset_size

@@big_literals:
        lodsw                   ;grab 16-bit extra length
        xchg    cx,ax           ;with longer counts, we can save some time
        shr     cx,1            ;by doing a word copy instead of a byte copy.
        rep     movsw
        adc     cx,0
        rep     movsb
        jmp     check_offset_size

; Used for counts 7-248. In test data, average value around 1Ah.  YMMV.
@@got_literals_exact:
        xchg    cx,ax
        rep     movsb           ;copy cx literals from ds:si to es:di
        jmp     check_offset_size

;Literal copy sequence for lengths 1-6:
copy6b: movsb
copy5b: movsb
copy4b: movsb
copy3b: movsb
copy2b: movsb
copy1b: movsb

;Literals done; fall through to match offset determination
check_offset_size:
        test    dl,dl           ;check match offset size in token (O bit)
        js      @@get_long_offset ;load absolute 16-bit match offset

        mov     ah,0ffh         ;set up high byte
        lodsb                   ;load low byte

@@get_match_length:
        xchg    dx,ax           ;dx: match offset  ax: original token
        and     al,0FH          ;isolate match length in token (MMMM)
        cmp     al,0FH          ;MATCH_RUN_LEN?
        jne     @@got_matchlen_short  ;no, we have the full match length from the token, go copy

        lodsb                   ;grab extra length byte
        add     al,012H         ;add MIN_MATCH_SIZE + MATCH_RUN_LEN
        jnc     @@do_long_copy  ;if no overflow, we have the entire length
        jne     @@mid_matchlen

        lodsw                   ;grab 16-bit length
        xchg    cx,ax           ;get ready to do a long copy
        jcxz    @@done_decompressing ;wait, is it the EOD marker? Exit if so
        jmp     @@copy_len_preset ;otherwise, do the copy

@@got_matchlen_short:
        add     al,3            ;add MIN_MATCH_SIZE
        xchg    cx,ax           ;copy match length into cx
        mov     bp,ds           ;save ds
        mov     ax,es
        mov     ds,ax           ;ds=es
        xchg    ax,si           ;save si
        mov     si,di           ;ds:si now points at back reference in output data
        add     si,dx
        rep     movsb           ;copy match
        xchg    si,ax           ;restore si
        mov     ds,bp           ;restore ds
        jmp     @@decode_token  ;go decode another token

@@done_decompressing:
        pop     ax              ;retrieve the original decompression offset
        xchg    di,ax           ;compute decompressed size
        sub     ax,di
        ret                     ;done decompressing, exit to caller

;These are called less often; moved here to optimize the fall-through case
@@get_long_offset:
        lodsw                   ;Get 2-byte match offset
        jmp     @@get_match_length

;With a confirmed longer match length, we have an opportunity to optimize for
;the case where a single byte is repeated long enough that we can benefit
;from rep movsw to perform the run (instead of rep movsb).
@@mid_matchlen:
        lodsb                   ;grab single extra length byte
        inc     ah              ;add 256
@@do_long_copy:
        xchg    cx,ax           ;copy match length into cx
@@copy_len_preset:
        push    ds              ;save ds
        mov     bp,es
        mov     ds,bp           ;ds=es
        mov     bp,si           ;save si
        mov     si,di           ;ds:si now points at back reference in output data
        add     si,dx
        cmp     dx,-2           ;do we have a byte/word run to optimize?
        jae     @@do_run        ;perform a run
;You may be tempted to change "jae" to "jge" because DX is a signed number.
;Don't!  The total window is 64k, so if you treat this as a signed comparison,
;you will get incorrect results for offsets over 32K.

;If we're here, we have a long copy and it isn't byte-overlapping (if it
;overlapped, we'd be in @@do_run_1)  So, let's copy faster with REP MOVSW.
;This won't affect 8088 that much, but it speeds up 8086 and higher.
        shr     cx,1
        rep     movsw
        adc     cx,0
        rep     movsb
        mov     si,bp           ;restore si
        pop     ds
        jmp     @@decode_token  ;go decode another token

@@do_run:
        je      @@do_run_2      ;fall through to byte (common) if not word run

@@do_run_1:
        lodsb                   ;load first byte of run into al
        mov     ah,al
        shr     cx,1
        rep     stosw           ;perform word run
        adc     cx,0
        rep     stosb           ;finish word run
        mov     si,bp           ;restore si
        pop     ds
        jmp     @@decode_token  ;go decode another token

@@do_run_2:
        lodsw                   ;load first word of run
        shr     cx,1
        rep     stosw           ;perform word run
        adc     cx,0            ;despite 2-byte offset, compressor might
        rep     stosb           ;output odd length. better safe than sorry.
        mov     si,bp           ;restore si
        pop     ds
        jmp     @@decode_token  ;go decode another token

ENDP    lzsa1_decompress_speed_jumptable

ENDS    CODE

END

;Speed optimization history (decompression times in microseconds @ 4.77 MHz):
; original E. Marty code    shuttle 123208 alice 65660 robotron 407338 ***
; table for shr al,4        shuttle 120964 alice 63230 robotron 394733 +++
; push/pop to mov/mov       shuttle 118176 alice 61835 robotron 386762 +++
; movsw for literalcpys     shuttle 124102 alice 64908 robotron 400220 --- rb
; stosw for byte runs       shuttle 118897 alice 65040 robotron 403518 --- rb
; better stosw for runs     shuttle 117712 alice 65040 robotron 403343 +--
; disable RLE by default    shuttle 116924 alice 60783 robotron 381226 +++
; optimize got_matchlen     shuttle 115294 alice 59588 robotron 374330 +++
; fall through to getML     shuttle 113258 alice 59572 robotron 372004 +++
; fall through to midLI     shuttle 113258 alice 59572 robotron 375060 ..- rb
; fall through midMaLen     shuttle 113247 alice 59572 robotron 372004 +.+
; movsw for litlen > 255    shuttle 113247 alice 59572 robotron 371612 ..+
; rep stosw for long runs   shuttle 113247 alice 59572 robotron 371612 ...
; rep movsw for long cpys   shuttle 113247 alice 59572 robotron 371035 ..+
; xchg/dec ah -> mov ah,val shuttle 112575 alice 59272 robotron 369198 +++
; force >12h len.to longcpy shuttle 101998 alice 59266 robotron 364459 +.+
; more efficient run branch shuttle 102239 alice 59297 robotron 364716 --- rb
; even more eff. run branch shuttle 101998 alice 59266 robotron 364459 ***
; BUGFIX - bad sign compare shuttle 101955 alice 59225 robotron 364117 +++
; reverse 16-bit len compar shuttle 102000 alice 59263 robotron 364460 --- rb
; jcxz for EOD detection    no change to speed, but is 1 byte shorter  +++
; force movsw for literals  shuttle 107183 alice 62555 robotron 379524 --- rb
; defer shr4 until necessry shuttle 102069 alice 60236 robotron 364096 ---
; skip literals if LLL=0    shuttle  98655 alice 57849 robotron 363358 ---
; fall through to mid_liter shuttle  98595 alice 57789 robotron 361998 +++
; == jumptable experiments begin ==
; jumptable for small copys shuttle 101594 alice 61078 robotron 386018 ---
; start:xchg instead of mov shuttle 100948 alice 60467 robotron 381112 +++
; use table for LLL=0 check shuttle 106972 alice 63333 robotron 388304 --- rb
; jmptbl to fallthrough mov shuttle 102532 alice 60760 robotron 383070 ---
; cpy fallthrough check_ofs shuttle  98939 alice 58917 robotron 371019 +**
; single jumptable jump     shuttle  97528 alice 57264 robotron 362194 ++*
; conditional check for L=7 shuttle  98610 alice 58521 robotron 368153 --- rb
; defer add MIN_MATCH_SIZE  shuttle  97207 alice 57200 robotron 362884 ++*
