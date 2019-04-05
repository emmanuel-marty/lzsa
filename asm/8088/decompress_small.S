;  decompress_small.S - space-efficient decompressor implementation for 8088
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

   segment .text
   bits 16

;  ---------------------------------------------------------------------------
;  Decompress raw LZSA block
;  inputs:
;  * ds:si: raw LZSA block
;  * es:di: output buffer
;  output:
;  * ax:    decompressed size
;  ---------------------------------------------------------------------------

lzsa_decompress:
   push di                 ; remember decompression offset
   cld                     ; make string operations (lods, movs, stos..) move forward

   xor cx,cx

.decode_token:
   mov ax,cx               ; clear ah - cx is zero from above or from after rep movsb in .copy_match
   lodsb                   ; read token byte: LLL|MMMM|O
   mov dx,ax               ; keep token in dl
   
   and al,070H             ; isolate literals length in token (LLL)
   mov cl,4
   shr al,cl               ; shift literals length into place

   mov cx,ax               ; copy literals length into cx
   cmp al,07H              ; LITERALS_RUN_LEN?
   jne .copy_literals      ; no, we have the full literals count from the token, go copy

   call .get_varlen        ; get complete literals length

.copy_literals:
   rep movsb               ; copy cx literals from ds:si to es:di

   test dl,dl              ; check match offset size in token (O bit)
   js .get_long_offset

   xchg ax,cx              ; clear ah - cx is zero from the rep movsb above
   lodsb
   jmp short .get_match_length

.get_long_offset:
   lodsw                   ; Get 2-byte match offset

.get_match_length:
   inc ax                  ; the match offset is stored off-by-1, increase it
   xchg ax,dx              ; dx: match offset  ax: original token
   and al,0FH              ; isolate match length in token (MMMM)

   mov cx,ax               ; copy match length into cx
   cmp al,0FH              ; MATCH_RUN_LEN?
   jne .copy_match         ; no, we have the full match length from the token, go copy
   
   call .get_varlen        ; get complete match length
   test cx,cx
   je short .done_decompressing ; bail if we hit EOD

.copy_match:
   add cx,3                ; add MIN_MATCH_SIZE to get the final match length to copy
   push ds                 ; save ds:si (current pointer to compressed data)
   xchg si,ax          
   push es
   pop ds
   mov si,di               ; ds:si now points at back reference in output data
   sub si,dx
   rep movsb               ; copy match
   xchg si,ax              ; restore ds:si
   pop ds
   jmp short .decode_token ; go decode another token

.done_decompressing:
   pop ax                  ; retrieve the original decompression offset
   xchg ax,di              ; compute decompressed size
   sub ax,di
   ret                     ; done

.get_varlen:
   lodsb                   ; grab extra length byte

   cmp al,0FFH             ; 3-byte extra length?
   je .large_varlen        ; yes, go grab it

   add cx,ax               ; add extra length byte to length from token
   cmp al,0FEH             ; 2-byte extra length?
   jne .varlen_done        ; no, we have the full length now, bail

   lodsb                   ; grab extra length byte
   add cx,ax               ; add to length from token
.varlen_done:
   ret                     ; bail

.large_varlen:
   lodsw                   ; grab 16-bit extra length
   xchg cx,ax
   ret
