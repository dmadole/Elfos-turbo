
; This software is copyright 2021 by David S. Madole.
; You have permission to use, modify, copy, and distribute
; this software so long as this copyright notice is retained.
; This software may not be used in commercial applications
; without express written permission from the author.
;
; The author grants a license to Michael H. Riley to use this
; code for any purpose he sees fit, including commercial use,
; and without any need to include the above notice.


           ; Include kernal API entry points

           include kernel.inc

           ; Define non-public API elements

version    equ     0400h
himem      equ     0442h
incofs1    equ     046ah
append     equ     046dh
lmpmask    equ     047ch

scall      equ     4
sret       equ     5

           ; Executable program header

           org     2000h-6
           dw      start
           dw      end-start
           dw      start

start:     org     2000h
           br      entry

           ; Build information

           db      1+80h              ; month
           db      12                 ; day
           dw      2021               ; year
           dw      1                  ; build
           db      'Written by David S. Madole',0

           ; Installer code

entry:     ldi     0                  ; by default, load to high mem
           plo     re

skipspac:  lda     ra                 ; look for -k option to load to
           bz      endopts            ; kernel memory
           smi     ' '
           bz      skipspac
           smi     '-'-' '
           bnz     optfail

           lda     ra
           smi     'k'
           bnz     optfail

           lda     ra
           bnz     optfail

           inc     re                 ; set flag to install to kernel mem

           glo     r2                 ; check that there is enough space
           smi     0f8h
           ghi     r2
           smbi    01dh
           bdf     kernfail

endopts:   ldi     o_read.1           ; check if read or write already
           phi     r7                 ; point outside the kernel, dont
           ldi     o_read.0           ; install if so
           plo     r7

           inc     r7
           ldn     r7
           smi     1eh
           bdf     hookfail

           ldi     o_write.1
           phi     r7
           ldi     o_write.0
           plo     r7

           inc     r7
           ldn     r7
           smi     1eh
           bdf     hookfail

           ldi     minver.1           ; pointer to minimum version needed
           phi     r7
           ldi     minver.0
           plo     r7

           ldi     version.1          ; pointer to installed kernel version
           phi     r8
           ldi     version.0
           plo     r8

           ldi     3
           plo     rf

verloop:   lda     r7                 ; subtract min version from installed
           sex     r8
           sd
           irx
           sex     r2
           bnf     verfail            ; negative, install < min, so fail
           bnz     verpass            ; positive, install > min, so pass

           dec     rf                 ; equal, so keep checking
           glo     rf
           bnz     verloop            ; if we exit this versions are same

verpass:   ldi     himem.1            ; pointer to top of memory variable
           phi     r7
           ldi     himem.0
           plo     r7

           ldi     (end-read).1       ; get length of code to install
           phi     rf
           ldi     (end-read).0
           plo     rf

           sex     r7

           glo     rf                 ; subtract size to install from himem
           inc     r7                 ; point to himem lsb
           sd                         ; discard lsb result
           ghi     rf
           dec     r7                 ; point to himem msb
           sdb
           phi     r8                 ; r8 is address to install to
           phi     r9                 ; save extra copy in r9

           sex     r2

           ldi     0                  ; round result down to page boundary
           plo     r8
           plo     r9                 ; extra copy

           glo     re                 ; check where we are installing
           bz      usehimem

           ldi     1eh                ; discard prior himem calculation
           phi     r8
           phi     r9
           br      getsourc

usehimem:  dec     r8                 ; set himem to one less than install
           ghi     r8                 ; address to reserve memory block
           str     r7
           inc     r7
           glo     r8
           str     r7
           inc     r8                 ; restore to destination address

getsourc:  ldi     read.1             ; get source address
           phi     r7
           ldi     read.0
           plo     r7

copycode:  lda     r7                 ; copy code to destination address
           str     r8
           inc     r8
           dec     rf
           glo     rf
           bnz     copycode
           ghi     rf
           bnz     copycode

           ldi     o_read.1           ; patch o_read to point to new
           phi     r7
           ldi     o_read.0
           plo     r7

           inc     r7
           ghi     r9
           str     r7
           inc     r7
           glo     r9
           str     r7

           glo     r9
           adi     (write-read).0
           plo     r9
           ghi     r9
           adci    (write-read).1
           phi     r9

           ldi     o_write.1         ; patch o_write to point to new
           phi     r7
           ldi     o_write.0
           plo     r7

           inc     r7
           ghi     r9
           str     r7
           inc     r7
           glo     r9
           str     r7

           ldi     success.1
           stxd
           ldi     success.0
           stxd

output:    ldi     message.1
           phi     rf
           ldi     message.0
           plo     rf

           sep     scall
           dw      o_msg

           inc     r2
           lda     r2
           plo     rf
           ldn     r2
           phi     rf

           sep     scall
           dw      o_msg

           sep     sret

hookfail:  ldi     hookmsg.1
           stxd
           ldi     hookmsg.0
           stxd
           br      output

verfail:   ldi     vermsg.1
           stxd
           ldi     vermsg.0
           stxd
           br      output

optfail:   ldi     optmsg.1
           stxd
           ldi     optmsg.0
           stxd
           br      output

kernfail:  ldi     kernmsg.1
           stxd
           ldi     kernmsg.0
           stxd
           br      output

message:   db      'Turbo Filesystem Module Build 1 for Elf/OS',13,10,0
success:   db      'Copyright 2021 by David S Madole',13,10,0
optmsg:    db      'ERROR: Invalid option specified for command',13,10,0
vermsg:    db      'ERROR: Needs kernel version 0.3.1 or higher',13,10,0
hookmsg:   db      'ERROR: Read or write routines already hooked',13,10,0
kernmsg:   db      'ERROR: Insufficient kernel memory to install',13,10,0

minver:    db      0,3,1              ; minimum kernel version

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

           org     $ + 0ffh & 0ff00h

read:      glo     rd                   ; advance to flags, but save before
           stxd
           adi     8
           plo     rd
           ghi     rd
           str     r2
           adci    0
           phi     rd                   ; rd = fd+8 (flags)

           ldn     rd
           plo     re

           lda     r2                   ; restore original rd
           phi     rd
           ldn     r2
           plo     rd                   ; rd = fd+0 (base)

           glo     re
           ani     8
           bnz     readvlid

           ldi     2<<1 + 1		; return d=2, df=1, invalid fd
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

           ldi     lmpmask.1
           phi     r9
           ldi     lmpmask.0
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
           sdi     512.0               ; subtracting sector offset from 512
           plo     rb                  ; overwrite original value
           ghi     rb
           sdbi    512.1
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
           glo	   ra                  ; subtract bytes we are going to copy 
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

           glo     r8                 ; check if flag is set to read data
           ani     2
           bz      readretn           ; if not, we are done

           dec     r8                 ; clear read data flag
           dec     r8

           sep     scall              ; get another sector
           dw      incofs1

           glo     ra
           bnz     readloop
           ghi     ra
           bnz     readloop           ; and finish satisfying request

           br      readretn

readpopr:  dec     rd
           dec     rd                 ; rd = fd+0 (start)

readretn:  inc     r2

           lda     r2                 ; restore saved rb
           phi     rb
           lda     r2
           plo     rb

           lda     r2                 ; restore saved ra
           phi     ra
           lda     r2
           plo     ra

           lda     r2                 ; restore saved r9
           phi     r9
           lda     r2
           plo     r9

           ldn     r2
           plo     r8

           ldi     0                  ; return d=0, df=0, success
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
           sdi     512.0               ; subtracting sector offset from 512
           plo     rb                  ; overwrite original value
           ghi     rb
           sdbi    512.1
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
           glo	   ra                  ; subtract bytes we are going to copy 
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

           ldi     lmpmask.1
           phi     r6
           ldi     lmpmask.0
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
           dw      append              ; wrapped to zero

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

           glo     r8                 ; check if flag is set to read data
           ani     2
           bz      writretn           ; if not, we are done

           dec     r8                 ; clear read data flag
           dec     r8

           sep     scall              ; get another sector
           dw      incofs1

           glo     ra
           bnz     writloop
           ghi     ra
           bnz     writloop           ; and finish satisfying request

writretn:  inc     r2

           lda     r2                 ; restore saved rb
           phi     rb
           lda     r2
           plo     rb

           lda     r2                 ; restore saved ra
           phi     ra
           lda     r2
           plo     ra

           lda     r2                 ; restore saved r9
           phi     r9
           lda     r2
           plo     r9

           lda     r2
           plo     r8

           lda     r2                 ; restore saved r9
           phi     r7
           lda     r2
           plo     r7

           lda     r2                 ; restore saved r9
           phi     r6
           ldn     r2
           plo     r6

           ldi     0                  ; return d=0, df=0, success
writsret:  shr
           sep     sret

end:       ; That's all, folks!

