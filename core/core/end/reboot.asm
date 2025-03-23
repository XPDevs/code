[org 0x7C00]        ; Bootloader starts at memory address 0x7C00
jmp start            ; Jump to start label to begin execution

start:
    ; Do any initialization here if necessary (optional)

    ; Trigger a software reset by calling INT 0x19
    mov ah, 0x00     ; Function 0x19 is system reset
    int 0x19         ; Call BIOS interrupt for reset

times 510 - ($ - $$) db 0   ; Fill the remaining space with zeros to make the total size 512 bytes
dw 0xAA55                   ; Bootloader signature

