; Bootloader Assembly Code
; This bootloader will print a joke based on a random number (1-4)
; and wait for the user to press Enter before rebooting using INT 0x19.
; It will print "Press <Enter> to restart" on a new line before waiting.

org 0x7c00       ; Bootloader will be loaded at this address

; BIOS interrupt to print characters and strings
mov ah, 0x0e      ; BIOS teletype function for printing characters

start:
    ; Generate a random number between 1 and 4 (using a simple approach)
    mov ah, 0x2c    ; BIOS interrupt to get system tick count
    int 0x21        ; Call interrupt to get the tick count
    xor dx, dx      ; Clear dx (higher word)
    mov dl, al      ; Copy low byte (AL) to DL
    and dl, 0x03    ; Mask lower 2 bits to get a number between 0 and 3
    add dl, 1       ; Shift the result to get a number between 1 and 4

    ; Check the value of DL and jump to the correct message
    cmp dl, 1
    je message1
    cmp dl, 2
    je message2
    cmp dl, 3
    je message3
    cmp dl, 4
    je message4

message1:
    ; Print message 1
    mov si, message1_text
    call PrintString
    jmp print_restart_message

message2:
    ; Print message 2
    mov si, message2_text
    call PrintString
    jmp print_restart_message

message3:
    ; Print message 3
    mov si, message3_text
    call PrintString
    jmp print_restart_message

message4:
    ; Print message 4
    mov si, message4_text
    call PrintString
    jmp print_restart_message

PrintString:
    ; Print the string pointed to by SI
print_char:
    lodsb            ; Load the next byte (character) into AL
    or al, al        ; Check if the character is null terminator
    jz done_printing ; If zero (null terminator), done printing
    mov ah, 0x0e     ; BIOS teletype function
    int 0x10         ; Print the character in AL
    jmp print_char   ; Repeat for the next character

done_printing:
    ret

print_restart_message:
    ; Print a new line (Carriage Return and Line Feed)
    mov ah, 0x0e     ; BIOS teletype function for printing characters
    mov al, 0x0D     ; Carriage return (new line)
    int 0x10         ; Print carriage return
    mov al, 0x0A     ; Line feed (move to the next line)
    int 0x10         ; Print line feed

    ; Print the "Press <Enter> to restart" message
    mov si, restart_message
    call PrintString
    jmp wait_for_enter

wait_for_enter:
    ; Wait for the user to press Enter (Carriage Return - 0x0D)
    mov ah, 0x00     ; BIOS keyboard input function
wait_key:
    int 0x16         ; Call BIOS to get a key press
    cmp al, 0x0D     ; Check if the key is Enter (0x0D)
    jne wait_key     ; If not Enter, keep waiting

    ; Reboot the system by calling INT 0x19 (reboot)
    mov ah, 0x19     ; Set up for INT 0x19 (reboot)
    int 0x19         ; Call BIOS interrupt to reboot

done:
    ; Halt the system by entering an infinite loop if something goes wrong
    cli
    hlt
    jmp $

; Messages
message1_text db "Why did the skeleton skip the music class? Because he couldn't find his bone to play with!", 0
message2_text db "So a skeleton walks into a bar and gets served a rib... get it?", 0
message3_text db "I once tried to make a joke about a broken pencil... but it was pointless... just like my diet", 0
message4_text db "Hey paps, I think we may have an issue, I burnt the water", 0
restart_message db "Press <Enter> to restart", 0

; Bootloader end, 0x55AA signature
times 510 - ($ - $$) db 0
dw 0xAA55

