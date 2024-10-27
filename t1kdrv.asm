;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; T1KDRV.SYS
;;
;; Makes the BIOS ROM drive on Tandy 1000 systems visible to DOS.
;;
;; Tandy DOS specific to 1000-series systems is the only DOS with built-in
;; support for the ROM drive.  Even Tandy-branded MS-DOS 5 has no support.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ORG 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structure definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DOS Device Request Packet Header
struc t_reqhdr
    .len:           resb 1      ; Length of packet
    .unit:          resb 1      ; DOS logical drive (0=A:)
    .command:       resb 1
    .status:        resw 1
                    resb 8
endstruc

; DOS Device Init Request Packet
struc t_initcmd
    .reqhdr:        resb t_reqhdr_size
    .numunit:       resb 1
    .endaddr:       resw 2
    .bpbptr:        resw 2
    .drvnum:        resb 1
endstruc

; Standard DOS 3 BPB
struc t_bpb_v3x
    .bps:           resw 1
    .spc:           resb 1
    .nreserved:     resw 1
    .nfat:          resb 1
    .ndirent:       resw 1
    .nsect16:       resw 1
    .mediaid:       resb 1
    .spf:           resw 1
    .spt:           resw 1
    .nheads:        resw 1
    .nhidden:       resw 1
endstruc

; Standard DOS 3.31+ FAT16 BPB.
struc t_bpb_v331
    .bps:           resw 1
    .spc:           resb 1
    .nreserved:     resw 1
    .nfat:          resb 1
    .ndirent:       resw 1
    .nsect16:       resw 1
    .mediaid:       resb 1
    .spf:           resw 1
    .spt:           resw 1
    .nheads:        resw 1
    .nhidden:       resd 1
    .nsect32:       resd 1
endstruc

; DOS Logical Drive structure,
; Defined by INT 2Fh, AX=0802h
struc t_drive_data_v3x
    .next_table:        resw 2
    .biosdrv:           resb 1
    .dosdrv:            resb 1
    .bpb:               resb t_bpb_v3x_size
    .flags:             resb 1
    .opencount:         resw 1
    .volname:           resb 11
                        resb 1
    .device_type:       resb 1
    .device_flags:      resw 1
    .tracks:            resw 1
    .bpb_max:           resb t_bpb_v3x_size
                        resb 3
    .fstypestr:         resb 8
                        resb 1
    .access_cyl:        resb 1
    .access:            resw 2
endstruc


struc t_drive_data_v331
    .next_table:        resw 2
    .biosdrv:           resb 1
    .dosdrv:            resb 1
    .bpb:               resb t_bpb_v331_size
                        resb 6
    .flags:             resb 1
    .opencount:         resw 1
    .volname:           resb 11
                        resb 1
    .device_type:       resb 1
    .device_flags:      resw 1
    .tracks:            resw 1
    .bpb_max:           resb t_bpb_v331_size
                        resb 6
    .access_cyl:        resb 1
    .access:            resw 2
endstruc

; Drive Data for DOS 4+ at least until DOS 7,
; maybe not 7.1 with FAT32
struc t_drive_data_v4
    .next_table:        resw 2
    .biosdrv:           resb 1
    .dosdrv:            resb 1
    .bpb:               resb t_bpb_v331_size
    .flags:             resb 1
    .opencount:         resw 1
    .device_type:       resb 1
    .device_flags:      resw 1
    .tracks:            resw 1
    .bpb_max:           resb t_bpb_v331_size
                        resb 6
    .access_cyl:        resb 1
    .access:            resw 2
    .volname:           resb 11
                        resb 1
    .serial:            resd 1
    .fstypestr:         resb 8
                        resb 1
endstruc

; Beginning of a FAT boot sector
struc t_bootsect
    .jmp:               resb 3
    .oem:               resb 8
    .bpb:
endstruc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resident code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Device Header defined by DOS Technical Reference 2-6
header:
    dw  0xFFFF                  ; Next Device SEG:OFF
    dw  0xFFFF
    dw  0x40                    ; Attribute flags
                                ; bit 6 = ??
    dw  STRATEGY                ; Strategy entrypoint
    dw  INTERRUPT               ; Interrupt entrypoint
.NUNIT: ; Name/Unit field
    ; Per DOS Techncal Reference 2-9
    ; > This [NUNIT] is optional because DOS fills in this location with the
    ; > value returned by the driver's INIT code.
    db  1                       ; Block: number of units handled
; end of header

; Saved pointer to DOS request packet
PTRSAV:
    dw  0
    dw  0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategy entrypoint
; Saves a pointer to the DOS request packet
; DOS will follow-up to initiate processing
STRATEGY:
    mov cs:[PTRSAV], bx
    mov cs:[PTRSAV + 2], es
    RETF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interrupt entrypoint
; Process the previously saved packet
INTERRUPT:
    ; Save the registers we plan to use
    push es
    push bx
    push ax

    ; ES:BX -> the saved packet
    les bx, cs:[PTRSAV]

    ; Check for init call
    cmp byte es:[bx + t_reqhdr.command], 0
    jne .not_init
.is_init:
    ; To be replaced by INT3 opcode
    nop

    ; Jump to init which is a pseudo-procedure.  Since we are the
    ; only "call" it is hard-coded to jump back to our .exit label.
    jmp init

.not_init:
    ; DOS makes the request to us for subunit 0, but
    ; implements INT 2Fh/AX=0802h expecting the requested unit
    ; to be the DOS drive number, not a subunit.
    ; Copy the previously saved DOS drive number
    mov al, cs:[drive_data + t_drive_data_v4.dosdrv]
    mov es:[bx + t_reqhdr.unit], al

    ; Forward the request packet to the built-in DRIVER.SYS code
    mov ax, 0x802
    int 0x2F
    ; Clean up stack
    pushf                               ; Push resulting flags
    pop bx                              ; Save flags to BX
    add sp, 2                           ; Discard original flags
    push bx                             ; Results back on stack
    popf                                ; Restore resulting flags

.exit:
    pop ax
    pop bx
    pop es
    retf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resident Data Section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DOS logical drive data structure
align 2
drive_data:
    istruc t_drive_data_v4
        at .next_table,         dw 0xFFFF
                                dw 0xFFFF
        at .biosdrv,            db 8
        at .dosdrv,             db 0xFF
        at .volname,            db 'NO NAME    '
        ; Remainder uninitialized (0)
;        at .device_type,        db 5
;        at .device_flags,       db 0x2d
;        at .access_cyl,         dw 0xFF
;        at .access,             dw 0xFFFF
;                                dw 0xFFFF
    iend

; Array of BPBs, one per unit (one unit)
align 2
bpb_ptrs:
    dw drive_data + t_drive_data_v4.bpb

resident_end:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-Resident code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Init command handler
init:
    push si
    push cx
    push dx
    push di
    push bp
    push ds

    ; DS:BX -> DOS Request Packet
    ; lds bx, cs:[PTRSAV]
    ; mov al, [bx + t_reqhdr.unit]
    ; mov ah, [bx + t_initcmd.numunit]
    ; mov cx, [bx + t_initcmd.bpbptr]     ; Args segment
    ; mov dx, [bx + t_initcmd.bpbptr + 2] ; Args offset
    ; les di, [bx + t_initcmd.endaddr]    ; ES:DI -> endaddr

    push cs
    pop ds
    cld

    ; Print Startup Banner
    mov dx, s_banner
    call dos_puts_cs_dx

    ; Get DOS version
    mov ax, 0x3000
    int 0x21

    ; Set up DS ES, Direction for doing data rearranging
    push cs
    pop ds
    push cs
    pop es
    cld

    ; Is DOS >= 4
    cmp al, 4
    jb .dos_lt_4
    jmp .vers_ok  ; >= 4 is the default

.dos_lt_4:
    ; Is DOS 3?
    cmp al, 3
    jne .unsupported_dos

    ; Is DOS 3.31?
    cmp ah, 31
    jb .dos_3 ; 3.x but not 3.31
    jmp .dos_331

.dos_3:
    ; DOS 3 BPB is shorter (only 19 or 0x13 bytes)
    mov word [bpb_size], t_bpb_v3x_size

    ; Compute this DOS's location for flags and opencount
    lea di, [drive_data + t_drive_data_v3x.flags]
    mov [ptr_drive_data_flags], di

    ; Compute this DOS's location for device_type through bpb_max
    lea di, [drive_data + t_drive_data_v3x.device_type]
    mov [ptr_drive_data_device_type], di

    ; Compute this DOS's location for access_cyl and access
    lea di, [drive_data + t_drive_data_v3x.access_cyl]
    mov [ptr_drive_data_access_cyl], di

    ; Compute this DOS's location for the volname
    lea di, [drive_data + t_drive_data_v3x.volname]
    mov [ptr_drive_data_volname], di

    ; Continue with common rearranging code
    jmp .rearrange_drive_data

.unsupported_dos:
    mov dx, s_unsupported_dos
    call dos_puts_cs_dx

    jmp .err_incompat

.dos_331:
    ; Compute this DOS's location for flags and opencount
    lea di, [drive_data + t_drive_data_v331.flags]
    mov [ptr_drive_data_flags], di

    ; Compute this DOS's location for device_type through bpb_max
    lea di, [drive_data + t_drive_data_v331.device_type]
    mov [ptr_drive_data_device_type], di

    ; Compute this DOS's location for access_cyl and access
    lea di, [drive_data + t_drive_data_v331.access_cyl]
    mov [ptr_drive_data_access_cyl], di

    ; Compute this DOS's location for the volname
    lea di, [drive_data + t_drive_data_v331.volname]
    mov [ptr_drive_data_volname], di

    ; Continue with common rearranging code
;    jmp .rearrange_drive_data
.rearrange_drive_data:

    ; DI = Correct location of volname

    ; Copy from offset 4Bh
    lea si, [drive_data + t_drive_data_v4.volname]
    mov cx, 11
    rep movsb

    ; Zero the DOS 4 volume name area
    xor ax, ax
    mov si, di
    rep movsb

.vers_ok:

    ; Set the remaining fields
    mov di, [ptr_drive_data_device_type]
    mov byte [di], 5

    inc di
    mov byte [di], 0x2d
    ; 0x2d = 10 1101
    ; IMPORTANT: bit 5 is documented in MS-DOS 4 source to indicate that this
    ; logical drive is the current owner of the physical (BIOS) drive.  It is
    ; critically important that exactly one entry in the BDS list have this flag,
    ; especially if it is the only one.
    ; If bit 5 clear, internal DOS code in IOCTL$GETOWN assumes there must be
    ; more entries and will attempt to follow FFFF:FFFFh and fail.
    ; Bit 3, 2, and 0 are much more obvious, corrsponding to a consistent
    ; number of sectors per track, a fixed BPB (no need to read sector 0), and
    ; non-removable media.

    mov di, [ptr_drive_data_access_cyl]
    xor ax, ax
    dec ax ; AX=FFFFh
    mov byte [di], al

    inc di
    mov word [di], ax
    mov word [di + 2], ax

    ; DRIVER.SYS resident installation check
    ; Built-in to DOS 3.2+, so this should never fail
;    mov ax, 0x800
;    int 0x2f
;    cmp al, 0xff
;    jne .err_incompat

    ; DS:BX -> DOS Request Packet
    les bx, cs:[PTRSAV]
    ; AL = next available drive
    mov al, byte es:[bx + t_initcmd.drvnum]
    ; Set the logical drive letter
    mov byte cs:[drive_data + t_drive_data_v4.dosdrv], al

    ; Convert it to a letter and update the strings
    add al, byte 'A'
    mov byte cs:[s_installing.letter], al
    mov byte cs:[s_installed.letter], al

    ; Get Drive Parameters
    mov ax, 0x800
    mov dx, 8
    ; ES:DI = 0 in case replaced
    xor di, di
    mov es, di
    int 0x13

    jnc .drive_parameters_ok

; No drive parameters for BIOS device 8
    mov dx, s_no_drive_parameters
    call dos_puts_cs_dx

    jmp .err_incompat

.drive_parameters_ok:

    ; WTF: shr cl, 6 doesn't work (on 86Box), nor does shr al, 6.
    ; So do this dance of moving to AL, loading the shift count
    ; into CL, shifting, then put it back in CL
    ; AL = High-2 cylinder [7:6], Max SPT [5:0]
;    mov al, cl
    ; Load shift count
;    mov cl, 6
    ; AL = High cylinder [1:0]
;    shr al, cl
    ; CL = High cylinder [1:0]
;    mov cl, al

    ; CX = Cylinders[9:0]
;    xchg ch, cl

    ; Update the DOS tracks field with the number of cylinders
;    mov word cs:[drive_data + t_drive_data_331.tracks], cx

    ; Read the boot sector to ES:BX
    mov ax, 0x201
    mov dx, 8
    mov cx, 1
    mov bx, bss_data
    push cs
    pop es
    int 0x13
    jnc .read_ok

; Read failed
    mov dx, s_bad_read
    call dos_puts_cs_dx

    jmp .err_incompat

.read_ok:
    ; Copy from this segment to this segment
    push cs
    pop ds
    push cs
    pop es
    cld

    ; Copy the BPB to the active BPB
    mov si, bss_data + t_bootsect.bpb
    ; First BPB always starts at the same location
    mov di, drive_data + t_drive_data_v4.bpb
    mov cx, [bpb_size]
    rep movsb

    ; Copy the BPB to the max BPB
    mov si, bss_data + t_bootsect.bpb
    mov di, word[ptr_drive_data_device_type]
    ; Offset of bpb_max is always the same relative to device_type
    add di, t_drive_data_v4.bpb_max - t_drive_data_v4.device_type
    mov cx, [bpb_size]
    rep movsb

    ; Print progress
    mov dx, s_installing
    call dos_puts_cs_dx

    ; Create the DOS device, using our resident drive_data as storage
    mov ax, 0x801
    push cs
    pop ds
    mov di, drive_data
    int 0x2F

    lds bx, cs:[PTRSAV]
    mov ah, 1
    ; mov byte cs:[header.NUNIT], ah

    ; Update the number of units to 1
    mov byte [bx + t_initcmd.numunit], ah
    ; Update the resident end to the "next byte after"
    ; NOTE: It may be the case that the BPB pointers don't
    ; actually need to stay resident so we could save 4 byes.
    mov word [bx + t_initcmd.endaddr], resident_end
    mov word [bx + t_initcmd.endaddr + 2], cs
    ; Tell DOS about the BPBs
    mov word [bx + t_initcmd.bpbptr], bpb_ptrs
    mov word [bx + t_initcmd.bpbptr + 2], cs

    ; Retrieve the DOS logical drive actually used
    mov al, byte cs:[drive_data + t_drive_data_v4.dosdrv]

    ; Convert it to a letter and update the strings
    add al, byte 'A'
    mov byte cs:[s_installed.letter], al

    ; Because the init implementation is in the non-resident code section,
    ; overwrite the code that called it to instead raise a debug exception
    ; before forwarding to DOS like all other calls.
    mov byte cs:[INTERRUPT.is_init], 0xcc ; INT3
    mov byte cs:[INTERRUPT.is_init + 1], 0xeb ; JMP SHORT
    mov byte cs:[INTERRUPT.is_init + 2], INTERRUPT.not_init - (INTERRUPT.is_init + 1)

    ; Print success
    mov dx, s_installed
    call dos_puts_cs_dx

    xor ax, ax
    jmp .exit_done

; Exit and do not stay resident
.err_incompat:
    lds bx, cs:[PTRSAV]
    xor ax, ax
    mov byte [bx + t_initcmd.numunit], ah
    mov word [bx + t_initcmd.endaddr], ax
    mov word [bx + t_initcmd.endaddr + 2], cs
    stc
    jmp .exit_error_done
; Exit with error code in AL
.exit_error_done:
    ; Status done with error (failure)
    mov ah, 0x81
    jmp .exit_status_ah
; Exit success
.exit_done:
    ; Status DONE (success), AL is ignored
    mov ah, 1
; All exits
.exit_status_ah:
    lds bx, cs:[PTRSAV]
    mov word [bx + t_reqhdr.status], ax

    pop ds
    pop bp
    pop di
    pop dx
    pop cx
    pop si
    ; "Return" to the caller
    jmp INTERRUPT.exit

; Write a DOS string in CS:DX to CON
dos_puts_cs_dx:
    push ax
    push ds

    push cs
    pop ds

    mov ah, 9
    int 0x21

    pop ds
    pop ax
    ret


; ------------
; String table

s_banner:
    db 'T1KDRV ', __?DATE?__, ' ', __?TIME?__, 13, 10, '$'

s_installing:
    db 'Installing Drive '
.letter:
    db '::', 13, 10, '$'

s_installed:
    db 'Tandy ROM drive at '
.letter:
    db '::', 13, 10, '$'

s_unsupported_dos:
    db 'Unsupported DOS Version', 13, 10, '$'

s_no_drive_parameters:
    db 'Unable to read drive 8 parameters', 13, 10, '$'

s_bad_read:
    db 'Unable to read drive 8 sector 0', 13, 10, '$'

; -----------------
; Non-Resident data

ptr_drive_data_flags:
    dw drive_data + t_drive_data_v4.flags
ptr_drive_data_device_type:
    dw drive_data + t_drive_data_v4.device_type
ptr_drive_data_volname:
    dw drive_data + t_drive_data_v4.volname
ptr_drive_data_access_cyl:
    dw drive_data + t_drive_data_v4.access_cyl
bpb_size:
    dw t_bpb_v331_size

align 16, db 0
bss_data:
