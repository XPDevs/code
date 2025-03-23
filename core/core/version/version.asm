; Bootloader.asm
BITS 16                 ; Set 16-bit mode for real mode

ORG 0x7C00             ; The bootloader will be loaded at memory address 0x7C00 by the BIOS

start:
    ; Initialize the video mode for text output (80x25 VGA text mode)
    mov ah, 0x0E       ; teletype output function for BIOS interrupt
    mov al, ' '        ; print a space to clear the screen
    int 0x10           ; call BIOS video interrupt

    ; Print "Name: DoorsOS"
    mov si, Name       ; Point to the string "Name: DoorsOS"
    call PrintString

    ; Print "Version: xxxxxx"
    mov si, Version    ; Point to the string "Version: xxxxxx"
    call PrintString

    ; Print "Build Date: xxxxxx"
    mov si, BuildDate  ; Point to the string "Build Date: xxxxxx"
    call PrintString

    ; Print "Architecture: x86"
    mov si, Architecture ; Point to the string "Architecture: x86"
    call PrintString

    ; Print prompt for the user to press Enter
    mov si, Prompt     ; Point to the prompt string
    call PrintString

    ; Wait for Enter key press (0x0D)
WaitForEnter:
    mov ah, 0x00       ; BIOS keyboard input function (wait for keypress)
    int 0x16           ; Wait for key press
    cmp al, 0x0D       ; Compare the key with Enter (0x0D)
    jne WaitForEnter    ; If not Enter, keep waiting

    ; System Reset by calling BIOS interrupt 0x19
    mov ax, 0x1234     ; BIOS reset code (can be any non-zero value)
    int 0x19           ; Call BIOS reset interrupt to restart the system

; PrintString function that prints a string in SI register
PrintString:
    mov ah, 0x0E       ; BIOS teletype output function
.next_char:
    lodsb              ; Load next byte from SI into AL
    or al, al          ; Check if we reached the end of the string
    jz .done
    int 0x10           ; Print the character in AL
    jmp .next_char
.done:
    ret

Name:      db 'Name: Core', 0x0D, 0x0A, 0
Version:    db 'Version: 1.2.0', 0x0D, 0x0A, 0
BuildDate:  db 'Build Date: 22/03/2025', 0x0D, 0x0A, 0
Architecture: db 'Architecture: x86', 0x0D, 0x0A, 0
Prompt:     db 'Press <Enter> to return to boot menu...', 0x0D, 0x0A, 0

; Bootloader signature: 0xAA55
TIMES 510 - ($ - $$) db 0   ; Fill with zero bytes until 510th byte
dw 0xAA55                   ; Boot signature (0xAA55)

