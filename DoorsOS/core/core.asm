[org 0x7C00]           ; The bootloader is loaded at address 0x7C00

; Set up spinner variables
spinner:
    ; Define the fixed position of the spinner (center of the screen)
    ; In VGA text mode, each character is 2 bytes, so 12th row (row 12, column 40):
    ; Position = (12 * 80 + 40) * 2 = 1920 (in memory offset)
    mov di, 1920         ; Position at row 12, column 40 in VGA text mode
    mov si, spinner_chars ; Address of spinner character sequence
    mov cx, 4             ; We have 4 spinner characters (|, /, -, \)

spinner_loop:
    ; Get the current spinner character
    mov al, [si]
    ; Write the character to the screen
    mov ah, 0x0E          ; BIOS teletype output function
    mov bh, 0x00          ; Page number (0)
    mov bl, 0x0F          ; Text color (white on black)
    mov dx, di            ; Use the fixed position (row 12, column 40)
    int 0x10              ; BIOS interrupt to print character

    ; Move to the next spinner character
    inc si                ; Move to the next spinner character
    cmp byte [si], 0      ; Check if we’ve reached the end of the spinner string
    je spinner_chars      ; If so, restart the spinner sequence

    ; Call delay function to introduce a 1-second delay
    call delay_1_second

    ; Repeat spinner loop
    loop spinner_loop

; Spinner character sequence: "|/-\"
spinner_chars:
    db '|', '/', '-', '\', 0

; Delay function to introduce a 1-second delay
delay_1_second:
    ; Approximate 1-second delay, assuming a 1.19 MHz CPU or suitable speed
    ; This busy-wait loop is not precise but works for small delays in real mode
    mov cx, 0xFFFFF      ; Large enough value for a noticeable delay
delay_loop_1sec:
    loop delay_loop_1sec
    ret

; Infinite loop to prevent the bootloader from doing anything else
hang:
    jmp hang

; Boot sector signature (required for bootloaders)
times 510 - ($ - $$) db 0
dw 0xAA55

