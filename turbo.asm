
;  Copyright 2021, David S. Madole <david@madole.net>
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program.  If not, see <https://www.gnu.org/licenses/>.


           ; Include kernal API entry points

           include bios.inc
           include kernel.inc


           ; Define non-public API elements

d_incofs1  equ     046ah
d_append   equ     046dh
k_lmpmask  equ     0473h


           ; Executable program header

           org     2000h-6
           dw      start
           dw      end-start
           dw      start

start:     br      entry


           ; Build information

           db      8+80h              ; month
           db      6                  ; day
           dw      2021               ; year
           dw      0                  ; build

           db      'See github.com/dmadole/Elfos-turbo for more info',0


           ; Check if hook points have already been patched and do not
           ; install if so, since we don't know what it is or what the
           ; impact might be of disconnecting it.

entry:     ldi     high hooklist      ; Get point to table of patch points
           phi     rd
           ldi     low hooklist
           plo     rd

chekloop   lda     rd                 ; a zero marks end of the table
           lbz     chekvers

           phi     rf                 ; get pointer to patch point
           lda     rd
           plo     rf

           inc     rf                 ; skip the lbr opcode

           ldn     rd                 ; if points into kernel then ok
           smi     20h
           lbdf    cheknext

           sep     scall              ; quit with error message
           dw      o_inmsg
           db      'ERROR: Read or write hooks already installed',13,10,0
           sep     sret

cheknext:  inc     rd                 ; skip target address in table
           inc     rd

           lbr     chekloop           ; repeat for all


           ; Check minimum needed kernel version 0.4.0 in order to have
           ; heap manager available, also lmp_mask moved in that version

chekvers:  ldi     high k_ver         ; pointer to installed kernel version
           phi     rd
           ldi     low k_ver
           plo     rd

           lda     rd                 ; if major is non-zero then good
           lbnz    allocmem

           lda     rd                 ; if minor is 4 or more then good
           smi     4
           lbdf    allocmem

           sep     scall              ; quit with error message
           dw      o_inmsg
           db      'ERROR: Needs kernel version 0.4.0 or higher',13,10,0
           sep     sret


           ; Allocate a page-aligned block from the heap for storage of
           ; the persistent code module. Make it permanent so it will
           ; not get cleaned up at program exit.

allocmem:  ldi     high end-module     ; length of persistent module
           phi     rc
           ldi     low end-module
           plo     rc

           ldi     255                 ; page-aligned
           phi     r7
           ldi     4                   ; permanent
           plo     r7

           sep     scall               ; request memory block
           dw      o_alloc
           lbnf    gotalloc

           sep     scall               ; return with error
           dw      o_inmsg
           db      'ERROR: Could not allocate memeory from heap',13,10,0
           sep     sret

gotalloc:  ghi     rf                  ; Offset to adjust addresses with
           smi     high module
           stxd


           ; Copy module code into the permanent heap block

           ldi     high end-module     ; length of code to copy
           phi     rc
           ldi     low end-module
           plo     rc

           ldi     high module         ; get source address
           phi     rd
           ldi     low module
           plo     rd

copycode:  lda     rd                  ; copy code to destination address
           str     rf
           inc     rf
           dec     rc
           glo     rc
           lbnz    copycode
           ghi     rc
           lbnz    copycode


           ; Update kernel hooks to point to the copied module code

           ldi     high hooklist      ; Get point to table of patch points
           phi     rd
           ldi     low hooklist
           plo     rd

           inc     r2                 ; point to page offset on stack

hookloop:  lda     rd                 ; a zero marks end of the table
           lbz     finished

           phi     rf                 ; get pointer to vector to hook
           lda     rd
           plo     rf

           inc     rf                 ; skip the lbr opcode

           lda     rd                 ; add offset to get copy address
           add                        ;  and update into vector
           str     rf
           inc     rf
           lda     rd
           str     rf

           lbr     hookloop           ; repeat for all


           ; Installation is complete, show banner and return

finished:  sep     scall              ; output message
           dw      o_inmsg
           db      'Turbo Filesystem Module Build 0 for Elf/OS',13,10,0
           sep     sret


           ; Table giving addresses of jump vectors we need to update
           ; to point to us instead, along with address in the module
           ; before copying to repoint those too. The address will be
           ; adject to the copy when patching is done.

hooklist:  dw      o_read, read
           dw      o_write, write
           db      0


           org     $ + 0ffh & 0ff00h

module:    ; Code for persistent module starts here

           ; Read bytes from file
           ;
           ; Input:
           ;   RC - Number of bytes to read
           ;   RD - Pointer to file descriptor
           ;   RF - Pointer to read buffer
           ;
           ; Output:
           ;   RC - Number of bytes actually read
           ;   RD - Unchanged
           ;   RF - Points after last byte read
           ;   DF - Set if error occurred
           ;   D  - Error code

read:      glo     rd                  ; advance to flags, but save before
           stxd
           adi     8
           plo     rd
           ghi     rd
           str     r2
           adci    0
           phi     rd                  ; rd = fd+8 (flags)

           ldn     rd
           plo     re

           lda     r2                  ; restore original rd
           phi     rd
           ldn     r2
           plo     rd                  ; rd = fd+0 (base)

           glo     re
           ani     8
           bnz     readvlid

           ldi     2<<1 + 1            ; return d=2, df=1, invalid fd
           br      readsret

readvlid:  glo     rc                  ; if there is nothing to do, return
           bnz     readnot0
           ghi     rc
           bz      readsret            ; return d=0, df=0, success

readnot0:  glo     r8                  ; save r8.0 to use for flags
           stxd

           glo     r9                  ; save r9 to use for dta pointer
           stxd
           ghi     r9
           stxd

           glo     ra                  ; save ra for bytes requested
           stxd
           ghi     ra
           stxd

           glo     rb                  ; save rb to use for loop counter
           stxd
           ghi     rb
           stxd

           glo     rc                  ; copy bytes requested to ra
           plo     ra
           ghi     rc
           phi     ra

           ldi     0
           plo     r8                  ; clear flags byte
           plo     rc                  ; clear bytes read counter
           phi     rc

           ; loops back to here

readloop:  inc     rd
           inc     rd                  ; rd = fd+2 (file offset nlsb)

           ; Check if we have already checked for an eof adjustment to 
           ; reduce the read bytes requested, if so, dont do it again.

           glo     r8
           ani     1
           bnz     readdata

           ; The following checks if we are in the "final lump" which is the
           ; last allocation unit in the file, and if so, we are near eof.
           ; Its not actually easily possible to know how much data is
           ; remaining in the file until we get to this point, as eof is only
           ; stored relative to the start of this final allocation unit.

           glo     rd                  ; this way we dont have to fix the
           adi     6                   ; result back if the branch below not
           plo     rb                  ; taken, also the separate copy is
           ghi     rd                  ; used even if the branch is taken
           adci    0
           phi     rb                  ; rb = fd+8 (flags)

           ldn     rb                  ; check final lump flag
           ani     4
           bz      readdata

           ; If we are in the final lump, then calculate how much data is
           ; remaining in the file and if more data has been requested than
           ; is in the file, reduce the request to match what is available.
           ; Since the request size is kept across loops, this adjustment
           ; only needs to be done once, and only can be done once.

           inc     r8                  ; remember weve already done this

           ldi     high k_lmpmask
           phi     r9
           ldi     low k_lmpmask
           plo     r9                  ; r9 = lmpmask
           ldn     r9
           plo     re

           dec     rb                  ; rb = fd+7 (eof offset lsb)
           inc     rd                  ; rd = fd+3 (file offset lsb)

           ldn     rb                  ; get eof offset lsb and subtract file
           sex     rd                  ; offset lsb from it
           sm 
           plo     r9

           dec     rd                  ; rd = fd+2 (file offset nlsb)
           dec     rb                  ; rb = fd+6 (eof offset msb)

           glo     re                  ; and lump mask msb with file offset
           and                         ; nlsb, then subtract from eof offset
           sex     rb                  ; msb
           sdb
           phi     r9                  ; r9 = bytes to eof
           sex     r2

           bnz     readneof            ; if bytes remaining to eof are not
           glo     r9                  ; zero then continue reading
           bz      readpopr

readneof:  glo     r9
           str     r2
           glo     ra                  ; compare bytes left in file to bytes
           sd                          ; requested to read (ra)
           ghi     r9
           str     r2
           ghi     ra
           sdb
           bdf     readdata            ; if ra <= bytes left leave as-is

           ghi     r9                  ; else replace request count with
           phi     ra                  ; what is actually left in file
           glo     r9 
           plo     ra

           ; Setup the source copy pointer into the current sector in memory
           ; and determine how much data we are going to copy, which will be
           ; the lesser of whats left in the sector or what was requested.

readdata:  lda     rd                  ; get sector offset as low 9 bits of
           ani     1
           phi     rb
           lda     rd                  ; rd = fd+4 (dta msb)
           plo     rb                  ; rb = sector offset

           sex     rd                  ; add dta address to sector offset
           inc     rd                  ; in rb and put result into r9
           glo     rb                  ; as copy source pointer
           add
           plo     r9
           dec     rd
           ghi     rb
           adc
           phi     r9                  ; rd = fd+4 (dta msb)
           sex     r2

           glo     rb                  ; find what is left in sector by
           sdi     low 512             ; subtracting sector offset from 512
           plo     rb                  ; overwrite original value
           ghi     rb
           sdbi    high 512
           phi     rb

           glo     rb                  ; compare bytes requested to bytes
           str     r2
           glo     ra                  ; left in sector
           sm 
           ghi     rb
           str     r2
           ghi     ra
           smb
           bdf     readleft            ; if fewer in sector, read that many

           ghi     ra                  ; otherwise read what was requested
           phi     rb
           glo     ra
           plo     rb
           br      readupdt

readleft:  inc     r8                  ; set flag to load more data
           inc     r8

readupdt:  glo     rb
           str     r2
           glo     ra                  ; subtract bytes we are going to copy 
           sm                          ; from bytes requested and at the 
           plo     ra                  ; same time put into loop counter rb
           ghi     rb
           str     r2
           ghi     ra
           smb
           phi     ra

           glo     rb                  ; add bytes we are going to copy to rc
           str     r2
           glo     rc
           add
           plo     rc
           ghi     rb
           str     r2
           ghi     rc
           adc
           phi     rc

           dec     rd                  ; rd = fd+3 (file offset lsb)

           sex     rd                  ; add the amount we are going to copy
           glo     rb                  ; onto the current file offset
           add
           stxd
           ghi     rb
           adc
           stxd
           ldi     0
           adc
           stxd
           ldi     0
           adc
           str     rd                  ; rd = fd+0 (base)
           sex     r2

readcopy:  lda     r9                  ; copy rb bytes from dta at m(r9)
           str     rf                  ; to user buffer at m(rf)
           inc     rf
           dec     rb
           glo     rb
           bnz     readcopy
           ghi     rb
           bnz     readcopy

           glo     r8                  ; check if flag is set to read data
           ani     2
           bz      readretn            ; if not, we are done

           dec     r8                  ; clear read data flag
           dec     r8

           sep     scall               ; get another sector
           dw      d_incofs1

           glo     ra
           bnz     readloop
           ghi     ra
           bnz     readloop            ; and finish satisfying request

           br      readretn

readpopr:  dec     rd
           dec     rd                  ; rd = fd+0 (start)

readretn:  inc     r2

           lda     r2                  ; restore saved rb
           phi     rb
           lda     r2
           plo     rb

           lda     r2                  ; restore saved ra
           phi     ra
           lda     r2
           plo     ra

           lda     r2                  ; restore saved r9
           phi     r9
           lda     r2
           plo     r9

           ldn     r2
           plo     r8

           ldi     0                   ; return d=0, df=0, success
readsret:  shr
           sep     sret


           ; Write bytes to file
           ;
           ; Input:
           ;   RC - Number of bytes to write
           ;   RD - Pointer to file descriptor
           ;   RF - Pointer to write buffer
           ;
           ; Output:
           ;   RC - Number of bytes actually written
           ;   RD - Unchanged
           ;   RF - Points after last byte written
           ;   DF - Set if error occurred
           ;   D  - Error code

write:     glo     rd                  ; advance to flags, but save before
           stxd
           adi     8
           plo     rd
           ghi     rd
           str     r2
           adci    0
           phi     rd                  ; rd = fd+8 (flags)

           ldn     rd
           plo     re

           lda     r2                  ; restore original rd
           phi     rd
           ldn     r2
           plo     rd                  ; rd = fd+0 (base)

           glo     re                  ; chec if fd is valid
           ani     8
           bnz     writvlid

           ldi     2<<1 + 1            ; return d=2, df=1, invalid fd
           br      writsret

writvlid:  glo     re                  ; check if fd is read-only
           ani     2
           bz      writwrit

           ldi     1<<1 + 1            ; return d=2, df=1, invalid fd
           br      writsret

writwrit:  glo     rc                  ; if there is nothing to do, return
           bnz     writnot0
           ghi     rc
           bz      writsret            ; return d=0, df=0, success

writnot0:  glo     r6                  ; save r9 to use for dta pointer
           stxd
           ghi     r6
           stxd

           glo     r7                  ; save r9 to use for dta pointer
           stxd
           ghi     r7
           stxd

           glo     r8                  ; save r8.0 to use for flags
           stxd

           glo     r9                  ; save r9 to use for dta pointer
           stxd
           ghi     r9
           stxd

           glo     ra                  ; save ra for bytes requested
           stxd
           ghi     ra
           stxd

           glo     rb                  ; save rb to use for loop counter
           stxd
           ghi     rb
           stxd

           glo     rc                  ; copy bytes requested to ra
           plo     ra
           ghi     rc
           phi     ra

           ldi     0
           plo     r8                  ; clear flags byte
           plo     rc                  ; clear bytes read counter
           phi     rc

           ; loops back to here

writloop:  inc     rd
           inc     rd                  ; rd = fd+2 (file offset nlsb)

           lda     rd                  ; get sector offset as low 9 bits of
           ani     1                   ; file offset, save in rb
           phi     rb
           lda     rd                  ; rd = fd+4 (dta msb)
           plo     rb

           sex     rd
           inc     rd                  ; add dta address to sector offset
           glo     rb
           add                         ; on stack and put result into r9
           plo     r9                  ; as copy destination pointer
           dec     rd
           ghi     rb
           adc
           phi     r9
           sex     r2

           glo     rb                  ; find the space left in sector by
           sdi     low 512             ; subtracting sector offset from 512
           plo     rb                  ; overwrite original value
           ghi     rb
           sdbi    high 512
           phi     rb

           glo     rb                  ; compare bytes to write to bytes
           str     r2                  ; left in sector
           glo     ra
           sm 
           ghi     rb
           str     r2
           ghi     ra
           smb
           bdf     writleft            ; if fewer in sector, write that many

           ghi     ra                  ; otherwise write what was requested
           phi     rb
           glo     ra
           plo     rb
           br      writupdt

writleft:  inc     r8                  ; set flag to load more data
           inc     r8

writupdt:  glo     rb
           str     r2
           glo     ra                  ; subtract bytes we are going to copy 
           sm                          ; from bytes requested and at the 
           plo     ra                  ; same time put into loop counter rb
           ghi     rb
           str     r2
           ghi     ra
           smb
           phi     ra

           glo     rb
           str     r2
           glo     rc
           add
           plo     rc
           ghi     rb
           str     r2
           ghi     rc
           adc
           phi     rc

           dec     rd                  ; rd = fd+3 (file offset lsb)

           sex     rd                  ; add the amount we are going to copy
           glo     rb                  ; onto the current file offset
           add
           stxd
           ghi     rb
           adc
           stxd
           ldi     0
           adc
           stxd
           ldi     0
           adc
           str     rd                  ; rd = fd+0 (base)
           sex     r2

           ; The following checks if we are in the "final lump" which is the
           ; last allocation unit in the file, and if so, we are near eof.
           ; Its not actually easily possible to know how much data is
           ; remaining in the file until we get to this point, as eof is only
           ; stored relative to the start of this final allocation unit.

           glo     rd                  ; this way we dont have to fix the
           adi     8                   ; result back if the branch below not
           plo     r7                  ; taken, also the separate copy is
           ghi     rd                  ; used even if the branch is taken
           adci    0
           phi     r7                  ; r7 = fd+8 (flags)

           ldn     r7                  ; get flags 
           ori     16+1                ; mark sector and file as written to
           str     r7
           ani     4                   ; check if in final lump
           bz      writcopy

           ; If we are in the final lump, then find if the file offset is
           ; past the eof offset, if it is, then update the eof offset to
           ; match the file offset since we are extending the file.

           ldi     high k_lmpmask
           phi     r6
           ldi     low k_lmpmask
           plo     r6                  ; r6 = lmpmask
           ldn     r6
           plo     re

           dec     r7                  ; r7 = fd+7 (eof offset lsb)

           inc     rd
           inc     rd
           inc     rd                  ; rd = fd+3 (file offset lsb)

           ldn     rd                  ; get file offset lsb and subtract eof
           plo     r6
           sex     r7                  ; offset lsb from it
           sd

           dec     rd                  ; rd = fd+2 (file offset nlsb)
           dec     r7                  ; r7 = fd+6 (eof offset msb)

           glo     re                  ; and lump mask msb with file offset
           sex     rd
           and                         ; nlsb, then subtract eof offset from it
           phi     r6
           sex     r7                  ; msb
           sdb
           sex     r2

           dec     rd
           dec     rd                  ; rd = fd+0 (begin)

           glo     r6
           bnz     writnapp
           ghi     r6
           bnz     writnapp

           sep     scall               ; append a new lump if eof offset
           dw      d_append            ; wrapped to zero

writnapp:  bdf     writcopy            ; if eof offset is larger or equal

           ghi     r6
           str     r7
           inc     r7
           glo     r6
           str     r7

           ; Setup the destination copy pointer into the current sector in
           ; memory and determine how much data we are going to copy, which
           ; will be the lesser of whats left in the sector or what was
           ; requested.

writcopy:  lda     rf                  ; copy rb bytes from dta at m(r9)
           str     r9                  ; to user buffer at m(rf)
           inc     r9
           dec     rb
           glo     rb
           bnz     writcopy
           ghi     rb
           bnz     writcopy

           glo     r8                  ; check if flag is set to read data
           ani     2
           bz      writretn            ; if not, we are done

           dec     r8                  ; clear read data flag
           dec     r8

           sep     scall               ; get another sector
           dw      d_incofs1

           glo     ra
           bnz     writloop
           ghi     ra
           bnz     writloop            ; and finish satisfying request

writretn:  inc     r2

           lda     r2                  ; restore saved rb
           phi     rb
           lda     r2
           plo     rb

           lda     r2                  ; restore saved ra
           phi     ra
           lda     r2
           plo     ra

           lda     r2                  ; restore saved r9
           phi     r9
           lda     r2
           plo     r9

           lda     r2
           plo     r8

           lda     r2                  ; restore saved r9
           phi     r7
           lda     r2
           plo     r7

           lda     r2                  ; restore saved r9
           phi     r6
           ldn     r2
           plo     r6

           ldi     0                   ; return d=0, df=0, success
writsret:  shr
           sep     sret

end:       ; That's all, folks!

