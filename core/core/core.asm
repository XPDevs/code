; kernel.asm - ExploreOS Kernel
[BITS 16]
[ORG 0x0]

; Status Bar Constants
STATUS_BAR_ROW    equ 0
STATUS_BAR_COL    equ 0
STATUS_BAR_WIDTH  equ 80
STATUS_BAR_COLOR  equ 0x70    ; White background, black text
TIMER_INTERRUPT   equ 0x08    ; Timer interrupt vector
KEYBOARD_INT      equ 0x09    ; Keyboard interrupt vector
UPDATE_FREQUENCY  equ 1

; Boot Screen Constants
BOOT_SCREEN_ACTIVE   equ 1        ; Flag to track if boot screen is active
BOOT_SCREEN_INACTIVE equ 0
PROGRESS_BAR_WIDTH   equ 50       ; Width of progress bar in characters
PROGRESS_BAR_STEPS   equ 20       ; Number of steps for progress animation

; Music generation constants
SCALE_LENGTH equ 7
RHYTHM_LENGTH equ 4

; PRNG Constants and Variables
PRNG_MULTIPLIER    equ 0x19660D   ; Good multiplier for PRNG
PRNG_INCREMENT     equ 0x3C6EF35F ; Prime increment
PRNG_STATE_SIZE    equ 4          ; 32-bit PRNG state

; USB Constants
BOOT_DRIVE      equ 0x7E00
EXT_SUPPORT     equ 0x7E01

; Morse code timing and frequency constants
MORSE_FREQ      equ 550    ; Frequency
DOT_LENGTH      equ 2750   ; Length of a dot
DASH_LENGTH     equ 8250   ; Length of a dash (3x dot)
SYMBOL_SPACE    equ 125    ; Space between symbols
LETTER_SPACE    equ 375    ; Space between letters
WORD_SPACE      equ 3500   ; Space between words

; Pong constants
PADDLE_HEIGHT equ 3
COURT_WIDTH  equ 80    ; Width of playing field
COURT_HEIGHT equ 25    ; Height of playing field
P1_X         equ 2     ; Player 1 paddle X position
P2_X         equ 77    ; Player 2 paddle X position
DEFAULT_SPEED equ 1000  ; Default game speed

; Command history constants
HISTORY_SIZE     equ 20   ; Number of commands to store
HISTORY_CMD_SIZE equ 77   ; Max size of each command

; Text editor constants
MAX_BUFFER_SIZE  equ 4096
SCREEN_HEIGHT    equ 25
SCREEN_WIDTH     equ 80

; Memory management constants
HEAP_START      equ 0x5000  ; Start of heap memory
HEAP_SIZE       equ 0x8000  ; 32KB heap size
MIN_BLOCK_SIZE  equ 16      ; Minimum allocation size
BLOCK_HEADER_SIZE equ 4     ; Size of block header (2 bytes size + 2 bytes flags)
HEAP_END        equ HEAP_START + HEAP_SIZE

; Block flags
BLOCK_FREE      equ 0x0000
BLOCK_USED      equ 0x8000  ; High bit indicates used block

; Memory block structure offsets
BLOCK_SIZE      equ 0       ; Offset for size field
BLOCK_FLAGS     equ 2       ; Offset for flags field

; Kernel entry point
global kernel_entry
kernel_entry:
    mov byte [BOOT_DRIVE], dl
    mov byte [EXT_SUPPORT], dh

    mov ax, 0x1000
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0xFFFE

    cld

    call init_memory_manager

    call display_boot_screen

    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10

    mov ah, 0x01
    mov cx, 0x0607
    int 0x10

    mov ah, 0x02
    mov bh, 0
    mov dx, 0
    int 0x10

    mov ah, 0x02
    mov bh, 0
    mov dx, 0x0100
    int 0x10

    call init_status_bar

    mov si, welcome_msg
    call print_string
    
    mov si, license_msg
    call print_string
    
    mov si, newline
    call print_string

main_loop:
    call update_status_bar

    mov si, prompt_msg
    call print_string

    mov di, input_buffer
    call read_line

    mov si, newline
    call print_string

    mov si, input_buffer
    call process_command

    mov si, newline
    call print_string

    jmp main_loop

init_status_bar:
    pusha
    
    ; Save original timer interrupt vector
    push es
    mov ax, 0
    mov es, ax
    mov ax, [es:TIMER_INTERRUPT*4]      ; Get offset
    mov [old_timer_vector], ax
    mov ax, [es:TIMER_INTERRUPT*4+2]    ; Get segment
    mov [old_timer_vector+2], ax
    
    ; Install our timer handler
    cli                              ; Disable interrupts
    mov word [es:TIMER_INTERRUPT*4], timer_handler
    mov [es:TIMER_INTERRUPT*4+2], cs
    sti                              ; Enable interrupts
    pop es
    
    ; Initialize the counter
    mov word [timer_counter], 0
    
    ; Draw initial status bar
    call update_status_bar
    
    popa
    ret

; Function to restore original timer handler (call before exiting)
restore_timer:
    pusha
    push es
    
    ; Restore original timer interrupt vector
    mov ax, 0
    mov es, ax
    cli                              ; Disable interrupts
    mov ax, [old_timer_vector]
    mov [es:TIMER_INTERRUPT*4], ax
    mov ax, [old_timer_vector+2]
    mov [es:TIMER_INTERRUPT*4+2], ax
    sti                              ; Enable interrupts
    
    pop es
    popa
    ret

; Timer interrupt handler
timer_handler:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push ds
    push es
    
    ; Setup data segment
    mov ax, cs
    mov ds, ax
    
    ; Increment counter
    inc word [timer_counter]
    
    ; Check if status bar is enabled
    cmp byte [status_bar_enabled], 0
    je .skip_update
    
    ; Check if it's time to update the status bar
    mov ax, [timer_counter]
    xor dx, dx
    mov bx, UPDATE_FREQUENCY
    div bx
    cmp dx, 0           ; Check remainder
    jne .skip_update
    
    ; Update the status bar
    call update_status_bar
    
.skip_update:
    ; Chain to the original timer interrupt
    pop es
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ; Jump to original timer handler
    jmp far [cs:old_timer_vector]

; Function to disable status bar updates (call before entering game modes)
disable_status_bar:
    pusha
    
    ; Save original timer interrupt vector if not already saved
    push es
    mov ax, 0
    mov es, ax
    
    ; Set a flag to indicate status bar is disabled
    mov byte [status_bar_enabled], 0
    
    ; Clear the status bar area with blank space
    mov ah, 0x02
    mov bh, 0
    mov dx, 0x0000    ; Row 0, Col 0
    int 0x10
    
    mov cx, STATUS_BAR_WIDTH
    
.clear_loop:
    mov ah, 0x0E
    mov al, ' '
    int 0x10
    loop .clear_loop
    
    pop es
    popa
    ret

; Function to re-enable status bar (call after exiting game modes)
enable_status_bar:
    pusha
    
    ; Set flag to indicate status bar is enabled
    mov byte [status_bar_enabled], 1
    
    ; Immediately redraw the status bar
    call update_status_bar
    
    popa
    ret

; Function to update the status bar
update_status_bar:
    ; First check if status bar is disabled
    cmp byte [status_bar_enabled], 0
    je .early_exit      ; Skip the entire function if disabled
    
    pusha
    push ds
    push es
    
    ; Setup segments
    mov ax, cs
    mov ds, ax
    mov es, ax
    
    ; Save current cursor position
    mov ah, 0x03
    mov bh, 0
    int 0x10
    mov [saved_cursor_row], dh
    mov [saved_cursor_col], dl
    
    ; Clear status bar buffer
    mov di, status_buffer
    mov cx, 80
    mov al, ' '
    rep stosb
    mov byte [status_buffer + 80], 0
    
    ; Get current date - reuse existing code
    mov ah, 0x04
    int 0x1A
    jc .skip_date
    
    ; Format date as "YY-MM-DD"
    mov di, status_buffer
    mov byte [di], ' '
    inc di
    mov al, ch      ; Year (century)
    call format_bcd_to_buffer
    mov al, cl      ; Year
    call format_bcd_to_buffer
    mov byte [di], '-'
    inc di
    mov al, dh      ; Month
    call format_bcd_to_buffer
    mov byte [di], '-'
    inc di
    mov al, dl      ; Day
    call format_bcd_to_buffer
    
.skip_date:
    ; Add spacing
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di    
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di    
    
    ; Get current time - reuse existing code
    mov ah, 0x02
    int 0x1A
    jc .skip_time
    
    ; Format time as "HH:MM:SS"
    mov al, ch      ; Hour
    call format_bcd_to_buffer
    mov byte [di], ':'
    inc di
    mov al, cl      ; Minute
    call format_bcd_to_buffer
    mov byte [di], ':'
    inc di
    mov al, dh      ; Second
    call format_bcd_to_buffer
    
.skip_time:
    ; Add spacing
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di    
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    mov byte [di], ' '
    inc di
    
    ; Add version info
    mov si, ver_short
    call strcpy_buffer
    
    ; Draw the status bar (direct video memory approach to avoid flickering)
    push es
    mov ax, 0xB800      ; Video memory segment
    mov es, ax
    mov di, 0           ; Top row, start of video memory
    mov si, status_buffer
    mov ah, STATUS_BAR_COLOR
    
.draw_loop:
    lodsb               ; Load character from buffer
    cmp al, 0
    je .done_draw
    stosw               ; Store character and attribute
    jmp .draw_loop
    
.done_draw:
    ; Fill the rest of the row with spaces
    mov al, ' '
    mov cx, STATUS_BAR_WIDTH
    mov ax, di
    shr ax, 1   ; Divide by 2 (shift right)
    sub cx, ax
    jle .restore_cursor ; Skip if already filled
    rep stosw
    
.restore_cursor:
    pop es
    
    ; Restore cursor position
    mov ah, 0x02
    mov bh, 0
    mov dh, [saved_cursor_row]
    mov dl, [saved_cursor_col]
    int 0x10
    
    pop es
    pop ds
    popa
    ret

.early_exit:
    ret

; Helper function to format BCD value to ASCII in buffer
format_bcd_to_buffer:
    push ax
    
    mov ah, al
    shr ah, 4
    and al, 0x0F
    
    add ah, '0'
    add al, '0'
    
    mov [di], ah
    inc di
    mov [di], al
    inc di
    
    pop ax
    ret

; Helper function to copy string to buffer
strcpy_buffer:
.copy_loop:
    lodsb
    cmp al, 0
    je .done_copy
    mov [di], al
    inc di
    jmp .copy_loop
.done_copy:
    ret

; Read a line of input from keyboard
read_line:
    xor cx, cx
    mov word [history_position], 0   ; Reset history position
    
.read_char:
    xor ah, ah
    int 0x16
    
    ; Check for special keys
    cmp ah, 0x48    ; Up arrow
    je .handle_up_arrow
    cmp ah, 0x50    ; Down arrow
    je .handle_down_arrow
    
    cmp al, 0x08    ; Backspace
    je .process_char
    cmp al, 0x0D    ; Enter
    je .process_char
    cmp al, 0x1F    ; Control chars
    jbe .read_char
    
    cmp cx, 76          ; Our new 76 character limit
    jae .read_char      ; If already at limit, ignore the character
    
.process_char:
    cmp al, 0x0D
    je .done_reading
    cmp al, 0x08
    je .handle_backspace
    mov [di], al
    inc di
    inc cx
    mov ah, 0x0E
    int 0x10
    jmp .read_char

.handle_up_arrow:
    ; Save current input first time we press up
    cmp word [history_position], 0
    jne .continue_up
    push di
    push cx
    mov si, input_buffer
    mov di, current_input
    rep movsb
    pop cx
    pop di
    
.continue_up:
    mov ax, [history_count]
    test ax, ax              ; Check if history is empty
    jz .read_char
    
    mov ax, [history_position]
    inc ax
    cmp ax, [history_count]  ; Don't go beyond oldest command
    ja .read_char
    mov [history_position], ax
    
    call .clear_current_line
    call .show_history_entry
    jmp .read_char

.handle_down_arrow:
    mov ax, [history_position]
    test ax, ax              ; Check if we're already at newest command
    jz .restore_current
    dec ax
    mov [history_position], ax
    jnz .show_from_history
    
.restore_current:
    call .clear_current_line
    mov si, current_input
    mov di, input_buffer
    call .copy_and_show
    jmp .read_char
    
.show_from_history:
    call .clear_current_line
    call .show_history_entry
    jmp .read_char

.show_history_entry:
    ; Calculate source address in history buffer
    mov ax, [history_count]
    sub ax, [history_position]
    mov bx, HISTORY_CMD_SIZE
    mul bx
    mov si, history_buffer
    add si, ax
    mov di, input_buffer
    call .copy_and_show
    ret

.copy_and_show:
    xor cx, cx
.copy_loop:
    lodsb
    test al, al
    jz .copy_done
    mov [di], al
    inc di
    inc cx
    mov ah, 0x0E
    int 0x10
    jmp .copy_loop
.copy_done:
    mov byte [di], 0
    ret

.clear_current_line:
    mov ax, cx              ; Save current count
    push ax
.clear_loop:
    test cx, cx
    jz .clear_done
    mov ah, 0x0E
    mov al, 0x08           ; Backspace
    int 0x10
    mov al, ' '            ; Space
    int 0x10
    mov al, 0x08           ; Backspace again
    int 0x10
    dec cx
    jmp .clear_loop
.clear_done:
    pop cx
    mov di, input_buffer   ; Reset input buffer pointer
    ret

.handle_backspace:
    cmp cx, 0
    je .read_char
    dec di
    dec cx
    mov ah, 0x0E
    mov al, 0x08
    int 0x10
    mov al, ' '
    int 0x10
    mov al, 0x08
    int 0x10
    jmp .read_char

.done_reading:
    mov byte [di], 0
    cmp cx, 0
    je .empty_input
    
    ; Add command to history if not empty
    push si
    push di
    push cx
    
    ; Calculate destination in history buffer
    mov ax, [history_index]
    mov bx, HISTORY_CMD_SIZE
    mul bx
    mov di, history_buffer
    add di, ax
    
    ; Copy command to history
    mov si, input_buffer
    rep movsb
    mov byte [di], 0
    
    ; Update history index and count
    inc word [history_index]
    mov ax, [history_index]
    cmp ax, HISTORY_SIZE
    jb .no_wrap
    mov word [history_index], 0
.no_wrap:
    mov ax, [history_count]
    cmp ax, HISTORY_SIZE
    jae .skip_inc_count
    inc word [history_count]
.skip_inc_count:
    
    pop cx
    pop di
    pop si
    ret

.empty_input:
    ret

; Process entered command
process_command:
    mov si, input_buffer
    lodsb                   ; Load the first character from the input buffer
    cmp al, 0               ; Check if it's null (empty input)
    je .empty_input         ; If empty, handle it without printing anything

    mov si, input_buffer
    mov di, easter_egg1
    call strcmp
    jne .check_egg2_3     

    ; First easter egg phrase
    cmp byte [easter_egg_step], 0
    jne .reset_egg
    mov byte [easter_egg_step], 1
    ret        
    
    
    ; If no command matches, print unknown command message
    mov si, input_buffer
    call print_unknown_message
    ret

.empty_input:
    ret

.restart_system:
    call restore_timer
    
    jmp 0xFFFF:0x0000
    
.beep_sound:
    mov bx, 2415  ; C5 (494 Hz)
    mov cx, 0x2000
    call play_startup_note_with_duration
    ret

.check_egg2_3:
    mov si, input_buffer
    mov di, easter_egg2
    call strcmp
    jne .check_egg3
    
    ; Second easter egg phrase
    cmp byte [easter_egg_step], 1
    jne .reset_egg
    mov byte [easter_egg_step], 2
    ret
    
.check_egg3:
    mov si, input_buffer
    mov di, easter_egg3
    call strcmp
    jne .reset_egg
    
    ; Third easter egg phrase - trigger credits
    cmp byte [easter_egg_step], 2
    jne .reset_egg
    
    ; Show credits
    call show_credits
    mov byte [easter_egg_step], 0
    ret
    
.reset_egg:
    mov byte [easter_egg_step], 0
    
    ; Continue with regular command processing
    ; Compare input with known commands
    mov si, input_buffer
    mov di, echo_cmd
    call strcmp_prefix
    je .echo_command

    mov si, input_buffer
    mov di, beep_cmd
    call strcmp
    je .beep_sound

    mov si, input_buffer
    mov di, clear_cmd
    call strcmp
    je .clear_screen

    mov si, input_buffer
    mov di, ver_cmd
    call strcmp
    je .system_ver

    mov si, input_buffer
    mov di, rand_cmd
    call strcmp
    je .generate_random

    mov si, input_buffer
    mov di, restart_cmd
    call strcmp
    je .restart_system

    mov si, input_buffer
    mov di, halt_cmd
    call strcmp
    je .halt_system
    
    mov si, input_buffer
    mov di, battery_cmd
    call strcmp
    je .check_battery

    mov si, input_buffer
    mov di, date_cmd
    call strcmp
    je .show_date

    mov si, input_buffer
    mov di, time_cmd
    call strcmp
    je .show_time

    mov si, input_buffer
    mov di, license_cmd
    call strcmp
    je .show_license

    mov si, input_buffer
    mov di, help_cmd
    call strcmp_prefix
    je .help_response
    
    mov si, input_buffer
    mov di, meminfo_cmd
    call strcmp
    je show_memory_info

    mov si, input_buffer
    mov di, malloc_cmd
    call strcmp
    je malloc_test

    mov si, input_buffer
    mov di, free_cmd
    call strcmp
    je free_test   
    
    mov si, input_buffer
    mov di, memtest_cmd
    call strcmp
    je memory_test
    
    mov si, input_buffer
    mov di, bios_cmd
    call strcmp
    je bios_info
    
    mov si, input_buffer
    mov di, screen_cmd
    call strcmp
    je screen_saver
    
    mov si, input_buffer
    mov di, calc_cmd
    call strcmp_prefix
    je calculator
    
    mov si, input_buffer
    mov di, edit_cmd
    call strcmp
    je edit_command     
    
    mov si, input_buffer
    mov di, snake_cmd
    call strcmp
    je snake_game        
    
    mov si, input_buffer
    mov di, ngg_cmd
    call strcmp
    je number_guessing_game 
    
    mov si, input_buffer
    mov di, piano_cmd
    call strcmp
    je piano_mode   
    
    mov si, input_buffer
    mov di, paint_cmd
    call strcmp
    je paint_program    
    
    mov si, input_buffer
    mov di, timer_cmd
    call strcmp_prefix
    je timer_utility
    
    mov si, input_buffer
    mov di, pong_cmd
    call strcmp
    je pong_game    
        
    mov si, input_buffer
    mov di, morse_cmd
    call strcmp_prefix
    je morse_command        
        
    mov si, input_buffer
    mov di, boot_cmd
    call strcmp_prefix
    je .boot_command    

    mov si, input_buffer
    mov di, history_cmd
    call strcmp
    je .show_history

    mov si, input_buffer
    mov di, music_cmd
    call strcmp
    je .generate_music

    mov si, input_buffer
    mov di, defrag_cmd
    call strcmp
    je defragment_memory

    mov si, input_buffer
    mov di, hexdump_cmd
    call strcmp_prefix
    je .hexdump_memory

    mov si, input_buffer
    mov di, ttt_cmd
    call strcmp
    je tictactoe_game

    ; If no command matches, print unknown command message
    mov si, input_buffer
    call print_unknown_message
    ret

.show_license:
    mov si, license_msg
    call print_string
    ret

.generate_music:
    push ax
    push bx
    push cx
    push dx
    push si

    ; Seed the random number generator with the current time
    mov ah, 0x00
    int 0x1A
    mov [prng_state], dx

    ; Determine total notes (between 75-99)
    call .music_generate_random
    mov al, [prng_state]
    and al, 0x1F  ; Limit to 0-31
    add al, 75    ; Adjust to 75-99 range
    mov byte [total_notes], al

    ; Reset note counter
    mov byte [current_note], 0

.music_loop:
    ; Generate note
    call .music_generate_random
    mov ax, [prng_state]
    and ax, 0x03  ; Bias towards lower randomness
    
    ; Calculate offset manually
    mov si, scale_notes
    shl ax, 1     ; Multiply by 2 for word-sized table
    add si, ax
    mov bx, [si]  ; Get note frequency

    ; Generate duration using rhythm pattern
    call .music_generate_random
    mov ax, [prng_state]
    and ax, 0x03  ; Select from rhythm patterns
    
    ; Calculate rhythm pattern offset
    mov si, rhythm_patterns
    add si, ax
    mov cl, [si]  ; Load rhythm pattern
    mov ch, 0     ; Clear high byte
    shl cx, 10    ; Increase duration 

    ; Play the note
    call play_startup_note_with_duration

    ; Long pauses between notes
    mov cx, 0x0250
.pause_loop:
    loop .pause_loop

    ; Increment note counter
    inc byte [current_note]
    mov al, [current_note]
    cmp al, [total_notes]
    jl .music_loop

    ; Final message
    mov si, music_generated_msg
    call print_string

    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

.music_generate_random:
    push eax
    push ebx
    push ecx
    push edx
    
    ; Load current state into eax (32-bit)
    mov eax, [prng_state]
    
    ; Xorshift algorithm:
    ; x ^= x << 13
    mov ebx, eax
    shl ebx, 13
    xor eax, ebx
    
    ; x ^= x >> 17
    mov ebx, eax
    shr ebx, 17
    xor eax, ebx
    
    ; x ^= x << 5
    mov ebx, eax
    shl ebx, 5
    xor eax, ebx
    
    ; Save updated state
    mov [prng_state], eax
    
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

.halt_system:
    mov si, halt_msg
    call print_string

    call restore_timer

    cli
    hlt

.boot_command:
    mov si, input_buffer
    add si, 4  ; Skip "BOOT" prefix
    
    ; Skip spaces
.boot_skip_spaces:
    lodsb
    cmp al, ' '
    je .boot_skip_spaces
    cmp al, 0
    je .boot_usage

    ; Convert to uppercase if lowercase
    cmp al, 'a'
    jb .boot_check_drive
    cmp al, 'z'
    ja .boot_check_drive
    sub al, 32

.boot_check_drive:
    ; Check for ROM boot
    cmp al, 'R'
    je .boot_rom

    ; Check for valid drive letter
    cmp al, 'A'
    jb .boot_usage
    cmp al, 'D'
    ja .boot_usage

    ; Convert letter to BIOS drive number
    mov dl, al        ; Store drive letter temporarily
    sub dl, 'A'      ; Convert to number (A=0, B=1, C=2, D=3)
    cmp dl, 2        ; Check if C: or higher
    jb .boot_drive   ; If A: or B:, number is already correct
    mov dl, 0x80     ; C: = first hard disk (0x80)
    cmp al, 'C'      ; If it was D:, increment to second hard disk
    jne .next_hdd
    jmp .boot_drive
.next_hdd:
    inc dl           ; D: = second hard disk (0x81)

.boot_drive:
    call restore_timer

    ; Print boot message
    push dx
    mov si, boot_drive_msg
    call print_string
    
    ; Print drive letter (converting drive number back to letter)
    mov al, [input_buffer + 5]  ; Get original letter
    mov ah, 0x0E
    int 0x10
    
    mov si, boot_dots_msg
    call print_string
    pop dx

    ; Reset disk system
    push dx         ; Save drive number
    xor ah, ah
    int 0x13
    pop dx          ; Restore drive number
    jc .boot_error

    ; Read boot sector
    push dx         ; Save drive number again
    mov ah, 0x02    ; Read sectors
    mov al, 1       ; Number of sectors to read
    mov ch, 0       ; Cylinder 0
    mov cl, 1       ; Sector 1
    mov dh, 0       ; Head 0
    mov bx, 0x7C00  ; Load to standard boot location
    int 0x13
    pop dx          ; Restore drive number
    jc .boot_error

    ; Jump to boot sector, preserving DL as boot drive
    jmp 0:0x7C00

.boot_rom:
    call restore_timer

    mov si, boot_rom_msg
    call print_string
    
    ; Get ROM configuration - start at C000:0000
    mov ax, 0xC000        ; Segment address for ROM area (C0000h >> 4)
    mov es, ax
    xor bx, bx            ; Offset 0

.find_rom:
    ; Check for ROM signature (0x55 0xAA)
    cmp word [es:bx], 0xAA55
    jne .next_rom
    
    ; Found ROM, prepare to boot
    push es
    push bx
    
    ; Initialize ROM
    mov ah, 0x02          ; ROM initialization function
    int 0x18              ; ROM BASIC/Boot interrupt
    
    ; If we get here, try direct jump to ROM
    pop bx
    pop es
    add bx, 3             ; Skip header
    call far [es:bx]      ; Call ROM entry point
    
.next_rom:
    ; Move to next potential ROM location (512 byte boundary)
    mov ax, es
    add ax, 0x20          ; Add 512 bytes (32 paragraphs)
    mov es, ax
    cmp ax, 0xF000        ; Check if we've reached end of ROM area
    jb .find_rom
    
    ; If we get here, ROM boot failed
    mov si, boot_rom_error_msg
    call print_string
    ret

.boot_error:
    mov si, boot_error_msg
    call print_string
    ret

.boot_usage:
    mov si, boot_usage_msg
    call print_string
    ret

.clear_screen:
    ; Clear everything except the status bar (start from row 1)
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0x0100    ; Row 1, Column 0
    mov dx, 0x184F
    int 0x10

    ; Set cursor to row 1, column 0
    mov ah, 0x02
    mov bh, 0
    mov dx, 0x0100    ; Row 1, Column 0
    int 0x10
    
    ; No need to manually refresh status bar as timer will handle it
    ret

.system_ver:
    mov si, ver_msg
    call print_string
    ret

.help_response:
    mov si, input_buffer
    add si, 4          ; Skip "HELP"
    call skip_spaces
    lodsb
    cmp al, 0          ; If no command specified
    je .show_general_help
    
    dec si             ; Go back one character
    mov di, ver_cmd
    call strcmp
    je .show_ver_help
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, help_cmd
    call strcmp
    je .show_help_help    
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, clear_cmd
    call strcmp
    je .show_clear_help
   
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, halt_cmd
    call strcmp
    je .show_halt_help
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, restart_cmd
    call strcmp
    je .show_restart_help
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, rand_cmd
    call strcmp
    je .show_rand_help
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, echo_cmd
    call strcmp
    je .show_echo_help    

    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, meminfo_cmd
    call strcmp
    je .show_meminfo_help
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, malloc_cmd
    call strcmp
    je .show_malloc_help
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, free_cmd
    call strcmp
    je .show_free_help    
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, battery_cmd
    call strcmp
    je .show_battery_help    
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, date_cmd
    call strcmp
    je .show_date_help

    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, time_cmd
    call strcmp
    je .show_time_help

    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, beep_cmd
    call strcmp
    je .show_beep_help

    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, license_cmd
    call strcmp
    je .show_license_help

    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, memtest_cmd
    call strcmp
    je .show_memtest_help
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, bios_cmd
    call strcmp
    je .show_bios_help    
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, screen_cmd
    call strcmp
    je .show_screen_help    
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, calc_cmd
    call strcmp
    je .show_calc_help    
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, edit_cmd
    call strcmp
    je .show_edit_help
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, snake_cmd
    call strcmp
    je .show_snake_help    
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, ngg_cmd
    call strcmp
    je .show_ngg_help    
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, piano_cmd
    call strcmp
    je .show_piano_help
   
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, paint_cmd
    call strcmp
    je .show_paint_help
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, timer_cmd
    call strcmp
    je .show_timer_help         
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, pong_cmd
    call strcmp
    je .show_pong_help  
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, morse_cmd
    call strcmp
    je .show_morse_help  
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, boot_cmd
    call strcmp
    je .show_boot_help  
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, history_cmd
    call strcmp
    je .show_history_help 
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, hexdump_cmd
    call strcmp
    je .show_hexdump_help 
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, music_cmd
    call strcmp
    je .show_genmsc_help 
    
    mov si, input_buffer
    add si, 4
    call skip_spaces
    mov di, ttt_cmd
    call strcmp
    je .show_ttt_help                        
    
    ; If command not found
    mov si, input_buffer
    add si, 4
    call skip_spaces
    call print_unknown_help
    ret

.show_general_help:
    mov si, help_msg
    call print_string
    ret

.show_help_help:
    mov si, help_help
    call print_string
    ret

.show_ver_help:
    mov si, help_ver
    call print_string
    ret

.show_clear_help:
    mov si, help_clear
    call print_string
    ret

.show_restart_help:
    mov si, help_restart
    call print_string
    ret

.show_license_help:
    mov si, help_license
    call print_string
    ret

.show_rand_help:
    mov si, help_rand
    call print_string
    ret

.show_halt_help:
    mov si, help_halt
    call print_string
    ret

.show_echo_help:
    mov si, help_echo
    call print_string
    ret

.show_meminfo_help:
    mov si, help_meminfo
    call print_string
    ret
    
.show_genmsc_help:
    mov si, help_genmsc
    call print_string
    ret    

.show_malloc_help:
    mov si, help_malloc
    call print_string
    ret

.show_free_help:
    mov si, help_free
    call print_string
    ret

.show_battery_help:
    mov si, help_battery
    call print_string
    ret

.show_date_help:
    mov si, help_date
    call print_string
    ret

.show_time_help:
    mov si, help_time
    call print_string
    ret

.show_beep_help:
    mov si, help_beep
    call print_string
    ret

.show_memtest_help:
    mov si, help_memtest
    call print_string
    ret

.show_bios_help:
    mov si, help_bios
    call print_string
    ret

.show_screen_help:
    mov si, help_screen
    call print_string
    ret

.show_calc_help:
    mov si, help_calc
    call print_string
    ret

.show_edit_help:
    mov si, help_edit
    call print_string
    ret

.show_snake_help:
    mov si, help_snake
    call print_string
    ret

.show_ngg_help:
    mov si, help_ngg
    call print_string
    ret

.show_piano_help:
    mov si, help_piano
    call print_string
    ret

.show_paint_help:
    mov si, help_paint
    call print_string
    ret
    
.show_timer_help:
    mov si, help_timer
    call print_string
    ret
    
.show_pong_help:
    mov si, help_pong
    call print_string
    ret
    
.show_morse_help:
    mov si, help_morse
    call print_string
    ret
    
.show_boot_help:
    mov si, help_boot
    call print_string
    ret
    
.show_history_help:
    mov si, help_history
    call print_string
    ret
   
.show_hexdump_help:
    mov si, help_hexdump
    call print_string
    ret
    
.show_ttt_help:
    mov si, help_ttt
    call print_string
    ret

.generate_random:
    push eax
    push ebx
    push ecx
    push edx
    
    ; Load current state into eax (32-bit)
    mov eax, [prng_state]
    
    ; Xorshift algorithm:
    ; x ^= x << 13
    mov ebx, eax
    shl ebx, 13
    xor eax, ebx
    
    ; x ^= x >> 17
    mov ebx, eax
    shr ebx, 17
    xor eax, ebx
    
    ; x ^= x << 5
    mov ebx, eax
    shl ebx, 5
    xor eax, ebx
    
    ; Save updated state
    mov [prng_state], eax
    
    ; Generate a number between 0-99
    xor edx, edx
    mov ebx, 100
    div ebx      ; eax / 100, remainder in dx
    
    ; Convert to ASCII and print
    mov ax, dx   ; Move remainder to ax for printing
    
    ; Division by 10 to get tens and units
    xor dx, dx
    mov bx, 10
    div bx       ; al = tens, ah = units
    
    ; Print tens digit (if not zero)
    test al, al
    jz .single_digit  ; Skip if tens digit is zero
    
    add al, '0'
    mov ah, 0x0E
    int 0x10
    
.single_digit:
    ; Print units digit
    mov al, dl
    add al, '0'
    mov ah, 0x0E
    int 0x10
    
    ; Print newline
    mov al, 0x0D
    int 0x10
    mov al, 0x0A
    int 0x10
    
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

.show_history:
    mov si, history_header
    call print_string
    
    mov cx, [history_count]
    test cx, cx
    jz .no_history
    
    ; Start from most recent command
    mov ax, [history_index]
    dec ax
    js .wrap_index  ; If negative, wrap around
    jmp .continue_history
    
.wrap_index:
    mov ax, HISTORY_SIZE
    dec ax
    
.continue_history:
    mov di, 1  ; Start numbering from 1
    
.history_loop:
    push ax
    
    ; Print entry number
    mov ax, di
    call print_number
    mov si, dot_space
    call print_string
    
    ; Calculate address of history entry
    pop ax
    push ax
    
    mov bx, HISTORY_CMD_SIZE
    mul bx
    mov si, history_buffer
    add si, ax
    
    ; Check if this entry contains a command
    cmp byte [si], 0
    je .skip_entry
    
    ; Print the command
    call print_string
    mov si, newline
    call print_string
    
.next_entry:
    pop ax
    dec ax  ; Move to previous command
    js .wrap_again
    jmp .continue_next
    
.wrap_again:
    mov ax, HISTORY_SIZE
    dec ax
    
.continue_next:
    inc di  ; Increment counter
    loop .history_loop
    ret
    
.skip_entry:
    pop ax  ; Clean up stack
    ret
    
.no_history:
    mov si, no_history_msg
    call print_string
    ret

.hexdump_memory:
    ; Parse the address parameter (expected format: HEXDUMP XXXX)
    mov si, input_buffer
    add si, 7  ; Skip "HEXDUMP" command
    
.skip_spaces_hexdump:
    lodsb
    cmp al, ' '
    je .skip_spaces_hexdump
    cmp al, 0
    je .missing_address
    
    ; Back up one character
    dec si
    
    ; Convert hex string to number
    xor bx, bx  ; Clear BX to store the address
    mov dx, 0   ; Counter for actual digits entered
    
.parse_hex:
    lodsb
    cmp al, 0
    je .check_digit_count
    cmp al, ' '
    je .check_digit_count
    
    ; Count this digit
    inc dx
    
    ; Check for too many digits
    cmp dx, 5
    jge .too_many_digits
    
    ; Convert ASCII to hex value
    cmp al, '0'
    jb .invalid_hex
    cmp al, '9'
    jbe .digit_0_9
    
    ; Handle A-F (both upper and lower case)
    or al, 0x20  ; Convert to lowercase
    cmp al, 'a'
    jb .invalid_hex
    cmp al, 'f'
    ja .invalid_hex
    
    ; Convert a-f to values 10-15
    sub al, 'a' - 10
    jmp .valid_digit
    
.digit_0_9:
    sub al, '0'
    
.valid_digit:
    ; Shift existing value left 4 bits and add new digit
    shl bx, 4    ; Shift left by 4 bits
    or bl, al    ; Add the new digit
    jmp .parse_hex
    
.check_digit_count:
    cmp dx, 4
    jne .invalid_length
    jmp .parse_done
    
.too_many_digits:
    mov si, too_many_digits_msg
    call print_string
    ret
    
.invalid_length:
    mov si, invalid_length_msg
    call print_string
    ret
    
.invalid_hex:
    mov si, invalid_hex_msg
    call print_string
    ret
    
.missing_address:
    mov si, hexdump_usage_msg
    call print_string
    ret
    
.parse_done:
    ; Check if address is divisible by 0x10 (16)
    test bl, 0x0F
    
    jnz .not_aligned
    mov si, newline
    call print_string
    
    ; Print 8 rows of 16 bytes
    mov cx, 8   ; 8 rows
    
.hexdump_row:
    push cx
    
    ; Print address
    mov ax, bx
    call print_hex
    mov si, colon_space
    call print_string
    
    ; Save start address of row for ASCII dump
    push bx
    
    ; Print 16 bytes in hex
    mov cx, 16  ; 16 bytes per row
    
.hexdump_byte:
    mov al, [bx]
    call print_byte_hex
    mov ah, 0x0E
    mov al, ' '
    int 0x10
    
    inc bx
    loop .hexdump_byte
    
    ; Print ASCII representation
    mov si, separator
    call print_string
    
    ; Retrieve start address of row
    pop si
    mov cx, 16  ; 16 bytes
    
.hexdump_ascii:
    lodsb
    cmp al, 32  ; Below space
    jl .nonprintable
    cmp al, 126 ; Above tilde
    jg .nonprintable
    jmp .print_ascii_char
    
.not_aligned:
    mov si, not_aligned_msg
    call print_string
    ret    
    
.nonprintable:
    mov al, '.' ; Replace non-printable with dot
    
.print_ascii_char:
    mov ah, 0x0E
    int 0x10
    loop .hexdump_ascii
    
    mov si, newline
    call print_string
    
    pop cx
    dec cx
    jnz .hexdump_row
    
    ret

.echo_command:
    ; Move past 'ECHO'
    mov si, input_buffer
    add si, 4           ; Move past 'ECHO'
    
    ; Skip leading spaces
.skip_spaces:
    lodsb
    cmp al, ' '
    je .skip_spaces
    cmp al, 0           ; Check if we've reached the end
    je .empty_echo
    
    ; Decrement SI to point to the first non-space character
    dec si

    ; Print the rest of the input
.print_echo:
    lodsb
    cmp al, 0           ; Check for end of string
    je .done_echo

    mov ah, 0x0E        ; BIOS teletype output
    int 0x10            ; Print the current character
    jmp .print_echo

.done_echo:
    ; Print newline
    mov al, 0x0D
    int 0x10
    mov al, 0x0A
    int 0x10
    ret

.empty_echo:
    ; If no text to echo, just print a newline
    mov si, newline
    call print_string
    ret
    
.check_battery:
    ; First check if APM (Advanced Power Management) is supported
    mov ah, 53h            ; APM Installation Check
    mov al, 00h            ; Function: Installation check
    xor bx, bx             ; Device ID: APM BIOS
    int 15h                ; APM BIOS interrupt
    jc .no_battery         ; If carry flag set, APM not supported
    
    ; Get power status
    mov ah, 53h            ; APM
    mov al, 09h            ; Function: Get Power Status
    mov bx, 0001h          ; Device ID: All devices
    mov cx, 0              ; Clear CX
    int 15h                ; APM BIOS interrupt
    jc .no_battery         ; If carry flag set, error occurred
    
    ; Check if battery exists
    test bh, bh            ; Check if AC line status is known
    jz .no_battery         ; If zero, battery status unknown
    
    ; Extract battery percentage from BL
    mov al, bl             ; BL contains battery percentage
    cmp al, 0FFh           ; Check if battery percentage is unknown
    je .unknown_level
    
    ; Convert percentage to string and print
    mov si, battery_msg
    call print_string
    
    xor ah, ah             ; Clear AH for division
    mov bl, 10             ; Divide by 10
    div bl                 ; AL = quotient, AH = remainder
    
    ; Print first digit
    push ax                ; Save AH (remainder)
    mov al, al             ; Get quotient
    add al, '0'            ; Convert to ASCII
    mov ah, 0Eh            ; BIOS teletype
    int 10h
    
    ; Print second digit
    pop ax                 ; Restore AH (remainder)
    mov al, ah             ; Get remainder
    add al, '0'            ; Convert to ASCII
    mov ah, 0Eh            ; BIOS teletype
    int 10h
    
    ; Print percentage symbol
    mov si, percent_msg
    call print_string
    ret

.no_battery:
    mov si, no_battery_msg
    call print_string
    ret

.unknown_level:
    mov si, unknown_battery_msg
    call print_string
    ret
    
.show_date:
    ; Get current date from RTC
    mov ah, 0x04        ; RTC function: Get date
    int 0x1A
    jc .date_error

    ; Convert BCD to ASCII and print
    mov al, ch          ; Century
    call print_bcd
    mov al, cl          ; Year
    call print_bcd
    mov al, '-'         ; Date separator
    mov ah, 0x0E
    int 0x10
    
    mov al, dh          ; Month
    call print_bcd
    mov al, '-'         ; Date separator
    mov ah, 0x0E
    int 0x10
    
    mov al, dl          ; Day
    call print_bcd
    
    mov si, newline
    call print_string
    ret

.date_error:
    mov si, date_error_msg
    call print_string

.show_time:
    ; Get current time from RTC
    mov ah, 0x02        ; RTC function: Get time
    int 0x1A
    jc .time_error

    ; Convert BCD to ASCII and print
    mov al, ch          ; Hours
    call print_bcd
    mov al, ':'         ; Time separator
    mov ah, 0x0E
    int 0x10
    
    mov al, cl          ; Minutes
    call print_bcd
    mov al, ':'         ; Time separator
    mov ah, 0x0E
    int 0x10
    
    mov al, dh          ; Seconds
    call print_bcd
    
    mov si, newline
    call print_string
    ret

.time_error:
    mov si, time_error_msg
    call print_string
    ret

; Helper function for printing BCD numbers
print_bcd:
    push ax
    push bx
    mov bl, al          ; Save BCD number
    
    ; Print high nibble
    shr al, 4           ; Move high nibble to low
    add al, '0'         ; Convert to ASCII
    mov ah, 0x0E        ; BIOS teletype output
    int 0x10
    
    ; Print low nibble
    mov al, bl          ; Restore BCD number
    and al, 0x0F        ; Mask off high nibble
    add al, '0'         ; Convert to ASCII
    mov ah, 0x0E        ; BIOS teletype output
    int 0x10
    
    pop bx
    pop ax
    ret

; Helper function to print a byte in hex
print_byte_hex:
    push ax
    push cx
    
    mov ah, al
    shr al, 4
    and al, 0x0F
    add al, '0'
    cmp al, '9'
    jle .print_high_nibble
    add al, 7
.print_high_nibble:
    mov cl, al  ; Save high nibble
    
    mov al, ah
    and al, 0x0F
    add al, '0'
    cmp al, '9'
    jle .print_low_nibble
    add al, 7
.print_low_nibble:
    mov ah, al  ; Save low nibble
    
    mov al, cl  ; Restore high nibble
    mov cl, ah  ; Save low nibble
    
    mov ah, 0x0E
    int 0x10
    mov al, cl  ; Restore low nibble
    int 0x10
    
    pop cx
    pop ax
    ret

morse_command:
    mov si, input_buffer
    add si, 5          ; Skip "MORSE"
    call skip_spaces   ; Skip any spaces after command
    cmp byte [si], 0   ; Check if any text provided
    je .no_text
    
.process_text:
    lodsb
    cmp al, 0
    je .done
    
    cmp al, ' '
    je .word_space
    
    call play_morse_char
    mov cx, LETTER_SPACE
    call delay_ms
    jmp .process_text
    
.word_space:
    mov cx, WORD_SPACE
    call delay_ms
    jmp .process_text
    
.no_text:
    ret
    
.done:
    ret

; Play a single character in morse code
play_morse_char:
    push si
    push ax
    
    ; Convert to uppercase
    cmp al, 'a'
    jb .find_pattern
    cmp al, 'z'
    ja .find_pattern
    sub al, 32
    
.find_pattern:
    ; Find pattern index
    cmp al, 'A'
    jb .check_number
    cmp al, 'Z'
    ja .invalid_char
    sub al, 'A'
    jmp .get_pattern
    
.check_number:
    cmp al, '0'
    jb .invalid_char
    cmp al, '9'
    ja .invalid_char
    sub al, '0'
    add al, 26         ; Offset past letters
    
.get_pattern:
    mov bl, al
    xor bh, bh
    mov si, morse_patterns
    
.find_loop:            ; Skip to correct pattern
    test bl, bl
    jz .play_pattern
.skip_pattern:
    lodsb
    test al, al
    jnz .skip_pattern
    dec bl
    jmp .find_loop
    
.play_pattern:
    lodsb              ; Get next symbol
    test al, al
    jz .pattern_done
    
    cmp al, '.'
    je .play_dot
    cmp al, '-'
    je .play_dash
    jmp .play_pattern
    
.play_dot:
    push si
    mov cx, DOT_LENGTH
    call beep_ms
    mov cx, SYMBOL_SPACE
    call delay_ms
    pop si
    jmp .play_pattern
    
.play_dash:
    push si
    mov cx, DASH_LENGTH
    call beep_ms
    mov cx, SYMBOL_SPACE
    call delay_ms
    pop si
    jmp .play_pattern
    
.pattern_done:
.invalid_char:
    pop ax
    pop si
    ret

; Beep for specified milliseconds
beep_ms:
    push ax
    ; Turn speaker on
    in al, 61h
    or al, 03h
    out 61h, al
    
    ; Set frequency
    mov al, 0B6h
    out 43h, al
    mov ax, 1193180 / MORSE_FREQ
    out 42h, al
    mov al, ah
    out 42h, al
    
    call delay_ms
    
    ; Turn speaker off
    in al, 61h
    and al, 0FCh
    out 61h, al
    
    pop ax
    ret

; Delay for cx milliseconds
delay_ms:
    push ax
    push cx
.outer_loop:
    push cx
    mov cx, 1000
.inner_loop:
    loop .inner_loop
    pop cx
    loop .outer_loop
    pop cx
    pop ax
    ret

number_guessing_game:
    ; Clear screen
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    
    ; Reset cursor
    mov ah, 0x02
    mov bh, 0
    mov dx, 0
    int 0x10

.new_game:
    ; Generate random number between 1 and 100
    mov ah, 0x00
    int 0x1A        ; Get system time in CX:DX
    mov ax, dx
    xor dx, dx
    mov cx, 100
    div cx          ; Divide by 100
    inc dx          ; Add 1 to remainder (1-100 instead of 0-99)
    mov [ngg_target], dx
    
    ; Reset guess counter
    mov word [ngg_num_guesses], 0
    
    ; Display welcome message
    mov si, ngg_welcome
    call print_string

.guess_loop:
    ; Display prompt
    mov si, ngg_prompt
    call print_string
    
    ; Read user input
    mov di, input_buffer
    call read_line
    mov si, newline
    call print_string
    
    ; Convert input to number
    mov si, input_buffer
    call parse_decimal
    jc .invalid_input
    
    ; Validate input range
    cmp ax, 1
    jl .invalid_input
    cmp ax, 100
    jg .invalid_input
    
    ; Increment guess counter
    inc word [ngg_num_guesses]
    
    ; Compare with target
    cmp ax, [ngg_target]
    je .correct_guess
    jl .too_low
    jg .too_high

.invalid_input:
    mov si, ngg_invalid
    call print_string
    jmp .guess_loop

.too_high:
    mov si, ngg_high
    call print_string
    jmp .guess_loop

.too_low:
    mov si, ngg_low
    call print_string
    jmp .guess_loop

.correct_guess:
    mov si, ngg_correct
    call print_string
    
    mov ax, [ngg_num_guesses]
    call print_decimal
    
    mov si, ngg_guesses
    call print_string
    
    ; Ask to play again
    mov si, ngg_play_again
    call print_string
    
.get_play_again:
    xor ah, ah
    int 0x16        ; Get keypress
    
    cmp al, 'Y'
    je .new_game
    cmp al, 'y'
    je .new_game
    cmp al, 'N'
    je .exit_game
    cmp al, 'n'
    je .exit_game
    jmp .get_play_again

.exit_game:
    mov si, newline
    mov si, newline
    call print_string
    ret

paint_program:
    ; Save current screen
    pusha
      
    call disable_status_bar 
        
    ; Clear screen
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    
    mov ah, 0x02
    mov bh, 0
    mov dx, 0
    int 0x10    
    
    ; Show help
    mov si, paint_help
    call print_string
    
    ; Initialize position
    mov word [paint_x], 40
    mov word [paint_y], 12
    mov byte [paint_mode], 0
    
.paint_loop:
    ; Draw cursor
    mov ah, 0x02
    mov bh, 0
    mov dh, [paint_y]
    mov dl, [paint_x]
    int 0x10
    
    ; Draw character if in drawing mode
    cmp byte [paint_mode], 1
    jne .check_input
    mov ah, 0x09
    mov al, [paint_char]
    mov bl, [paint_color]
    mov cx, 1
    int 0x10
    
.check_input:
    mov ah, 0x01
    int 0x16
    jz .paint_loop
    
    mov ah, 0x00
    int 0x16
    
    ; Check for ESC
    cmp al, 27
    je .exit_paint
    
    ; Check for space (toggle drawing)
    cmp al, ' '
    je .toggle_mode
    
    ; Check for 'C' (clear screen)
    cmp al, 'c'
    je .clear_screen
    cmp al, 'C'
    je .clear_screen
    
    ; Check for 'S' (change symbol)
    cmp al, 's'
    je .change_symbol
    cmp al, 'S'
    je .change_symbol
    
    ; Check for number keys (1-9) for colors
    cmp al, '1'
    jl .check_arrows
    cmp al, '9'
    jg .check_arrows
    sub al, '0'
    mov [paint_color], al
    jmp .paint_loop
    
.check_arrows:
    ; Check arrow keys
    cmp ah, 0x48    ; Up
    je .move_up
    cmp ah, 0x50    ; Down
    je .move_down
    cmp ah, 0x4B    ; Left
    je .move_left
    cmp ah, 0x4D    ; Right
    je .move_right
    jmp .paint_loop
    
.move_up:
    cmp word [paint_y], 0
    je .paint_loop
    dec word [paint_y]
    jmp .paint_loop
    
.move_down:
    cmp word [paint_y], 24
    je .paint_loop
    inc word [paint_y]
    jmp .paint_loop
    
.move_left:
    cmp word [paint_x], 0
    je .paint_loop
    dec word [paint_x]
    jmp .paint_loop
    
.move_right:
    cmp word [paint_x], 79
    je .paint_loop
    inc word [paint_x]
    jmp .paint_loop
    
.toggle_mode:
    xor byte [paint_mode], 1
    jmp .paint_loop
    
.change_symbol:
    mov si, paint_symbols
.find_current:
    lodsb
    cmp al, [paint_char]
    je .next_symbol
    cmp al, 0
    je .reset_symbols
    jmp .find_current
.next_symbol:
    lodsb
    cmp al, 0
    je .reset_symbols
    mov [paint_char], al
    jmp .paint_loop
.reset_symbols:
    mov al, [paint_symbols]
    mov [paint_char], al
    jmp .paint_loop
    
.clear_screen:
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    jmp .paint_loop
    
.exit_paint:
    ; Clear screen
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    
    ; Reset cursor position
    mov ah, 0x02
    mov bh, 0
    mov dx, 0
    int 0x10
    
    call enable_status_bar
    
    mov si, newline
    mov si, newline
    call print_string
    
    popa
    ret

; BIOS Info command
bios_info:
    ; Get BIOS date string using F000:FFF5
    mov ax, 0xF000
    mov es, ax
    mov bx, 0xFFF5
    
    mov si, bios_msg
    call print_string
    
    ; Print BIOS date string
    mov cx, 8        ; Length of date string
.print_date:
    mov al, [es:bx]
    mov ah, 0x0E
    int 0x10
    inc bx
    loop .print_date
    
    mov si, newline
    call print_string
    ret


piano_mode:
    ; Check if boot screen is active - if so, skip piano mode
    cmp byte [boot_screen_state], BOOT_SCREEN_ACTIVE
    je .exit_piano
    
    mov si, piano_msg
    call print_string
    
.piano_loop:
    mov ah, 0x00          ; Wait for keypress
    int 0x16
    
    cmp al, 27            ; ESC key
    je .exit_piano
    
    ; Check if it's a valid piano key
    cmp al, 'a'
    jb .skip_sound
    cmp al, 'k'
    ja .skip_sound
    
    ; Get timer value for note
    sub al, 'a'           ; Convert to 0-based index
    mov bl, 2             ; Multiply by 2 for word-sized table lookup
    mul bl
    mov bx, ax
    mov bx, [timer_values + bx]  ; Get pre-calculated timer value
    
    mov cx, 0x0750        ; Duration similar to startup sound
    call play_startup_note_with_duration
    
.skip_sound:
    ; Small delay before accepting next key
    mov cx, 0x0100
.key_delay:
    loop .key_delay
    
    jmp .piano_loop
    
.exit_piano:
    ; Ensure speaker is off when exiting
    in al, 0x61
    and al, 0xFC
    out 0x61, al
    ret

calculator:
    ; Skip command name
    mov si, input_buffer
    add si, 4           ; Move past "CALC"
    
    ; Check if there's any input after "CALC"
    lodsb
    cmp al, 0          ; Check if end of string
    je .show_help      ; If no input, show help
    
    ; Check if it's just spaces after CALC
    call skip_spaces
    lodsb
    cmp al, 0          ; Check if end of string after spaces
    je .show_help      ; If no input after spaces, show help
    
    ; Check if help requested
    cmp al, '?'
    je .show_help
    
    ; Move back for parsing
    dec si
    
    ; Parse first number (now 32-bit)
    call parse_decimal
    jc .invalid_input  ; If carry flag set, invalid number
    push edx           ; Save high 32 bits (if any)
    push eax           ; Save low 32 bits
    
    ; Skip spaces and get operator
    call skip_spaces
    lodsb
    cmp al, 0          ; Check if string ended before operator
    je .incomplete_expression
    mov bl, al         ; Save operator
    
    ; Validate operator
    cmp bl, '+'
    je .valid_op
    cmp bl, '-'
    je .valid_op
    cmp bl, '*'
    je .valid_op
    cmp bl, '/'
    je .valid_op
    
    ; If we get here, operator is invalid
    pop eax            ; Clean up stack
    pop edx            ; Clean up stack
    jmp .invalid_operator
    
.valid_op:
    ; Skip spaces and get second number
    call skip_spaces
    lodsb
    cmp al, 0          ; Check if string ended before second number
    je .incomplete_expression
    dec si             ; Move back for parsing
    
    call parse_decimal
    jc .invalid_input2  ; If carry flag set, invalid number
    mov ecx, eax       ; Second number in ECX
    mov ebp, edx       ; High 32 bits in EBP if needed
    pop eax            ; First number back in EAX
    pop edx            ; High 32 bits in EDX if needed
    
    ; Perform operation based on operator
    cmp bl, '+'
    je .add_nums
    cmp bl, '-'
    je .sub_nums
    cmp bl, '*'
    je .mul_nums
    cmp bl, '/'
    je .div_nums
    
    ; Should never reach here due to earlier validation
    jmp .invalid_operator
    
.show_help:
    mov si, calc_help
    call print_string
    ret

.incomplete_expression:
    mov si, calc_incomplete
    call print_string
    add sp, 8          ; Clean up stack (two 32-bit values)
    ret

.invalid_input2:
    add sp, 8          ; Clean up stack from first number
.invalid_input:
    mov si, calc_invalid_num
    call print_string
    ret

.invalid_operator:
    mov si, calc_invalid_op
    call print_string
    ret

.add_nums:
    ; 32-bit addition
    add eax, ecx
    adc edx, ebp       ; Add with carry for high bits
    jmp .print_result

.sub_nums:
    ; 32-bit subtraction
    sub eax, ecx
    sbb edx, ebp       ; Subtract with borrow for high bits
    jmp .print_result

.mul_nums:
    ; 32-bit multiplication
    push ecx
    push eax
    mul ecx            ; Multiply EAX by ECX, result in EDX:EAX
    pop ecx            ; Original EAX value
    pop ebx            ; Original ECX value 
    jmp .print_result

.div_nums:
    test ecx, ecx       ; Check for division by zero
    jz .div_zero
    push ebp           ; Save high part of divisor
    push ecx           ; Save low part of divisor
    push edx           ; Save high part of dividend
    push eax           ; Save low part of dividend
    
    xor edx, edx       ; Clear high bits for division
    div ecx            ; EDX:EAX / ECX -> EAX quotient, EDX remainder
    
    add sp, 16         ; Clean up stack
    jmp .print_result

.div_zero:
    mov si, calc_div_zero
    call print_string
    ret

.print_result:
    ; Print the sign if negative
    test eax, eax
    jns .print_positive
    push eax
    mov al, '-'
    mov ah, 0x0E
    int 0x10
    pop eax
    neg eax           ; Make positive for printing

.print_positive:
    call print_decimal32
    mov si, newline
    call print_string
    ret
    
; Parse decimal number from string to 32-bit
; Input: SI points to string
; Output: EAX = number, EDX = high bits (if needed), CF set if error
parse_decimal:
    push ebx
    push ecx
    xor eax, eax         ; Clear result
    xor edx, edx         ; Clear high bits
    xor ecx, ecx         ; Clear sign flag
    
    ; Check for minus sign
    lodsb
    cmp al, '-'
    jne .not_negative
    mov ecx, 1           ; Set sign flag
    lodsb                ; Get next character after minus
    
.not_negative:
    ; Check if first character is a digit
    cmp al, '0'
    jb .parse_error
    cmp al, '9'
    ja .parse_error
    
    ; First digit is valid, initialize our value
    sub al, '0'
    movzx ebx, al        ; EBX holds the accumulated value
    
.parse_loop:
    lodsb
    
    ; Check for end of number (space or null)
    cmp al, ' '
    je .done_parsing
    cmp al, 0
    je .done_parsing
    
    ; Validate digit
    cmp al, '0'
    jb .parse_error
    cmp al, '9'
    ja .parse_error
    
    ; Multiply accumulated value by 10 and add new digit
    sub al, '0'          ; Convert ASCII to number
    movzx edi, al        ; Save digit in EDI
    
    ; EBX = EBX * 10 + digit
    mov eax, 10
    mul ebx              ; EDX:EAX = EBX * 10
    add eax, edi         ; Add new digit to result
    adc edx, 0           ; Add carry to high 32 bits
    mov ebx, eax         ; Move back to EBX
    
    jmp .parse_loop
    
.parse_error:
    stc                  ; Set carry flag for error
    jmp .parse_exit
    
.done_parsing:
    dec si               ; Move back one character
    mov eax, ebx         ; Move result to EAX
    
    ; Apply sign if negative
    test ecx, ecx
    jz .positive
    neg eax
    
.positive:
    clc                  ; Clear carry flag for success
    
.parse_exit:
    pop ecx
    pop ebx
    ret

; Function to print a 32-bit decimal number in EAX
print_decimal32:
    push eax
    push ebx
    push ecx
    push edx
    push edi
    
    ; Check if zero
    test eax, eax
    jnz .not_zero
    
    mov al, '0'
    mov ah, 0x0E
    int 0x10
    jmp .print_done
    
.not_zero:
    mov edi, esp        ; Use stack as temporary buffer
    sub esp, 16         ; Reserve space for digits
    mov ebx, 10         ; Divisor
    xor ecx, ecx        ; Digit counter
    
.count_loop:
    xor edx, edx        ; Clear remainder
    div ebx             ; EAX/10: quotient in EAX, remainder in EDX
    add dl, '0'         ; Convert to ASCII
    dec edi             ; Move back in buffer
    mov [edi], dl       ; Store digit
    inc ecx             ; Increment digit counter
    test eax, eax       ; Check if quotient is zero
    jnz .count_loop     ; If not, continue
    
.print_loop:
    mov al, [edi]       ; Get digit from buffer
    mov ah, 0x0E        ; BIOS teletype
    int 0x10            ; Print character
    inc edi             ; Next digit
    loop .print_loop    ; Repeat for each digit
    
    add esp, 16         ; Restore stack
    
.print_done:
    pop edi
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

skip_spaces:
    lodsb
    cmp al, ' '
    je skip_spaces
    dec si
    ret

; Memory management functions
init_memory_manager:
    push es
    push di
    
    mov ax, ds
    mov es, ax
    
    mov di, HEAP_START
    mov ax, HEAP_SIZE
    stosw                   ; Store size
    mov ax, BLOCK_FREE
    stosw                   ; Store flags
    
    pop di
    pop es
    ret

; Align value in AX to MIN_BLOCK_SIZE
align_size:
    push dx
    add ax, MIN_BLOCK_SIZE - 1
    mov dx, MIN_BLOCK_SIZE
    dec dx
    not dx
    and ax, dx
    pop dx
    ret

; Find previous block
; Input: DI = current block
; Output: SI = previous block (0 if none)
find_prev_block:
    push ax
    push di
    
    mov si, HEAP_START
    cmp di, si              ; If at start, no previous block
    je .no_prev
    
.find_loop:
    mov ax, [si + BLOCK_SIZE]
    add ax, si              ; Next block address
    cmp ax, di              ; Is this the previous block?
    je .found
    mov si, ax              ; Move to next block
    jmp .find_loop
    
.no_prev:
    xor si, si
.found:
    pop di
    pop ax
    ret

; Malloc with safety checks and proper alignment
malloc:
    push bx
    push cx
    push dx
    push di
    push es

    mov ax, ds
    mov es, ax

    ; Validate size
    test bx, bx            ; Check if size is 0
    jz .malloc_failed
    
    ; Calculate total size needed
    mov ax, bx
    add ax, BLOCK_HEADER_SIZE
    jc .malloc_failed      ; Check for overflow
    
    ; Align size
    call align_size
    mov bx, ax             ; Store aligned size back in BX
    
    mov di, HEAP_START

.find_block:
    cmp di, HEAP_END
    jae .malloc_failed

    mov ax, [di + BLOCK_FLAGS]
    test ax, BLOCK_USED
    jnz .next_block

    mov cx, [di + BLOCK_SIZE]
    cmp cx, bx
    jb .next_block

    ; Found suitable block
    sub cx, bx             ; Calculate remaining size
    cmp cx, MIN_BLOCK_SIZE + BLOCK_HEADER_SIZE
    jb .use_whole_block

    ; Split block
    mov [di + BLOCK_SIZE], bx
    mov word [di + BLOCK_FLAGS], BLOCK_USED

    push di
    add di, bx
    mov [di + BLOCK_SIZE], cx
    mov word [di + BLOCK_FLAGS], BLOCK_FREE
    pop di
    jmp .malloc_done

.next_block:
    add di, [di + BLOCK_SIZE]
    jmp .find_block

.use_whole_block:
    mov word [di + BLOCK_FLAGS], BLOCK_USED

.malloc_done:
    mov ax, di
    add ax, BLOCK_HEADER_SIZE
    jmp .malloc_exit

.malloc_failed:
    xor ax, ax

.malloc_exit:
    pop es
    pop di
    pop dx
    pop cx
    pop bx
    ret

; Free with validation and coalescing
free:
    push bx
    push cx
    push dx
    push di
    push es
    push si

    mov ax, ds
    mov es, ax

    ; Validate pointer
    cmp bx, HEAP_START + BLOCK_HEADER_SIZE
    jb .free_invalid
    cmp bx, HEAP_END
    jae .free_invalid
    
    ; Convert to block header
    sub bx, BLOCK_HEADER_SIZE
    mov di, bx

    ; Validate block alignment
    mov ax, di
    sub ax, HEAP_START
    mov dx, MIN_BLOCK_SIZE
    div dl
    test ah, ah
    jnz .free_invalid

    ; Check if block is already free
    mov ax, [di + BLOCK_FLAGS]
    test ax, BLOCK_USED
    jz .free_invalid

    ; Mark block as free
    mov word [di + BLOCK_FLAGS], BLOCK_FREE

    ; Try to merge with previous block
    call find_prev_block
    test si, si
    jz .check_next         ; No previous block
    
    mov ax, [si + BLOCK_FLAGS]
    test ax, BLOCK_USED
    jnz .check_next
    
    ; Merge with previous
    mov ax, [di + BLOCK_SIZE]
    add [si + BLOCK_SIZE], ax
    mov di, si             ; Update current block pointer

.check_next:
    ; Try to merge with next block
    mov cx, [di + BLOCK_SIZE]
    mov si, di
    add si, cx            ; Point to next block

    cmp si, HEAP_END
    jae .free_done

    mov ax, [si + BLOCK_FLAGS]
    test ax, BLOCK_USED
    jnz .free_done

    ; Merge with next block
    mov ax, [si + BLOCK_SIZE]
    add [di + BLOCK_SIZE], ax

.free_done:
    mov ax, 1              ; Success
    jmp .free_exit

.free_invalid:
    xor ax, ax            ; Failure

.free_exit:
    pop si
    pop es
    pop di
    pop dx
    pop cx
    pop bx
    ret

; Memory defragmentation
; Compacts free memory blocks by moving used blocks to the beginning of the heap
; and consolidating all free space at the end
defragment_memory:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds
    
    mov ax, ds
    mov es, ax
    
    ; First pass: mark all blocks as not moved
    mov di, HEAP_START
.clear_marks:
    mov ax, [di + BLOCK_FLAGS]
    and ax, ~0x4000        ; Clear the "moved" flag (bit 14)
    mov [di + BLOCK_FLAGS], ax
    
    add di, [di + BLOCK_SIZE]
    cmp di, HEAP_END
    jb .clear_marks
    
    ; Second pass: defragment
    mov di, HEAP_START     ; Current position to scan
    mov si, HEAP_START     ; Target position for next used block
    
.scan_loop:
    cmp di, HEAP_END
    jae .done_scanning
    
    mov ax, [di + BLOCK_FLAGS]
    test ax, BLOCK_USED
    jz .skip_free_block    ; Skip free blocks
    
    ; Found used block, check if it needs to be moved
    cmp di, si
    je .no_move_needed     ; Block is already in right place
    
    ; Move the block data
    push di
    push si
    
    ; Copy header
    mov ax, [di + BLOCK_SIZE]
    mov [si + BLOCK_SIZE], ax
    mov ax, [di + BLOCK_FLAGS]
    or ax, 0x4000          ; Mark as moved
    mov [si + BLOCK_FLAGS], ax
    
    ; Copy user data
    add di, BLOCK_HEADER_SIZE
    add si, BLOCK_HEADER_SIZE
    mov cx, [di - BLOCK_HEADER_SIZE + BLOCK_SIZE]
    sub cx, BLOCK_HEADER_SIZE
    
    ; Copy data in 2-byte words
    shr cx, 1
    rep movsw
    
    ; Handle odd byte if needed
    jnc .copy_done
    movsb
    
.copy_done:
    pop si                 ; Restore target pointer
    pop di                 ; Restore source pointer
    
    ; Update any pointers to this block (stored in last_alloc)
    mov bx, di
    add bx, BLOCK_HEADER_SIZE
    cmp [last_alloc], bx
    jne .no_pointer_update
    
    ; Update the pointer to new location
    mov bx, si
    add bx, BLOCK_HEADER_SIZE
    mov [last_alloc], bx
    
.no_pointer_update:
    
    ; Move to next position
    mov bx, [si + BLOCK_SIZE]
    add si, bx             ; Advance target position
    
.no_move_needed:
    mov cx, [di + BLOCK_SIZE]
    add di, cx             ; Move to next block
    jmp .scan_loop
    
.skip_free_block:
    add di, [di + BLOCK_SIZE]
    jmp .scan_loop
    
.done_scanning:
    ; Create one large free block at the end
    cmp si, HEAP_END
    jae .all_used          ; No free space left
    
    ; Set up final free block
    ; Calculate size of final free block
    mov ax, HEAP_END
    sub ax, si
    mov [si + BLOCK_SIZE], ax

    mov word [si + BLOCK_FLAGS], BLOCK_FREE
    
.all_used:
    ; Update statistics
    mov ax, [defrag_count]
    inc ax
    mov [defrag_count], ax
    
    mov si, defrag_complete_msg
    call print_string
    
    pop ds
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; Show memory information command handler
show_memory_info:
    push ax
    push bx
    push cx
    push dx
    push di
    push si
    push es

    mov ax, ds
    mov es, ax             ; Set ES=DS for memory operations

    mov si, meminfo_header
    call print_string

    mov di, HEAP_START
    
    ; Statistics counters
    mov word [total_blocks], 0
    mov word [used_blocks], 0
    mov word [free_blocks], 0
    mov word [total_memory], 0
    mov word [used_memory], 0
    mov word [free_memory], 0
    mov word [largest_free], 0

.print_block:
    cmp di, HEAP_START + HEAP_SIZE
    jae .done_blocks

    ; Update counters
    inc word [total_blocks]
    
    mov bx, [di + BLOCK_SIZE]
    add [total_memory], bx
    
    mov ax, [di + BLOCK_FLAGS]
    test ax, BLOCK_USED
    jz .count_free
    
    ; Used block
    inc word [used_blocks]
    add [used_memory], bx
    jmp .print_block_info
    
.count_free:
    ; Free block
    inc word [free_blocks]
    add [free_memory], bx
    
    ; Check if largest free block
    cmp bx, [largest_free]
    jbe .print_block_info
    mov [largest_free], bx

.print_block_info:
    ; Print block address
    mov ax, di
    call print_hex

    ; Print spaces after address
    mov cx, 9              ; Number of spaces after address
.addr_space:
    mov si, space
    call print_string
    loop .addr_space

    ; Print block size
    mov ax, [di + BLOCK_SIZE]
    call print_hex

    ; Print spaces after size
    mov cx, 7              ; Number of spaces before status
.size_space:
    mov si, space
    call print_string
    loop .size_space

    ; Print block status
    mov ax, [di + BLOCK_FLAGS]
    test ax, BLOCK_USED
    jz .print_free
    mov si, used_msg
    jmp .print_status
.print_free:
    mov si, free_msg
.print_status:
    call print_string
    mov si, newline
    call print_string

    add di, [di + BLOCK_SIZE]
    jmp .print_block

.done_blocks:
    ; Print statistics
    mov si, meminfo_separator
    call print_string
    
    mov si, total_blocks_msg
    call print_string
    mov ax, [total_blocks]
    call print_decimal
    mov si, newline
    call print_string
    
    mov si, used_blocks_msg
    call print_string
    mov ax, [used_blocks]
    call print_decimal
    mov si, newline
    call print_string
    
    mov si, free_blocks_msg
    call print_string
    mov ax, [free_blocks]
    call print_decimal
    mov si, newline
    call print_string
    
    mov si, total_memory_msg
    call print_string
    mov ax, [total_memory]
    call print_decimal
    mov si, bytes_msg
    call print_string
    mov si, newline
    call print_string
    
    mov si, used_memory_msg
    call print_string
    mov ax, [used_memory]
    call print_decimal
    mov si, bytes_msg
    call print_string
    mov si, newline
    call print_string
    
    mov si, free_memory_msg
    call print_string
    mov ax, [free_memory]
    call print_decimal
    mov si, bytes_msg
    call print_string
    mov si, newline
    call print_string
    
    mov si, largest_free_msg
    call print_string
    mov ax, [largest_free]
    call print_decimal
    mov si, bytes_msg
    call print_string
    mov si, newline
    call print_string
    
    ; Removed fragmentation percentage calculation and display
    
    ; Display defragmentation count
    mov si, defrag_stats_msg
    call print_string
    mov ax, [defrag_count]
    call print_decimal
    mov si, newline
    call print_string

.done_info:
    pop es
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; Print decimal number in AX
print_decimal:
    push ax
    push bx
    push cx
    push dx
    
    mov bx, 10
    xor cx, cx
    
    ; Handle zero specially
    test ax, ax
    jnz .convert
    
    mov ah, 0Eh
    mov al, '0'
    int 10h
    jmp .done_decimal
    
.convert:
    ; Convert to decimal digits
    xor dx, dx
    div bx
    push dx                ; Store remainder
    inc cx
    test ax, ax
    jnz .convert
    
.print_digits:
    pop ax
    add al, '0'            ; Convert to ASCII
    mov ah, 0Eh
    int 10h
    loop .print_digits
    
.done_decimal:
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; Test malloc command handler
malloc_test:
    mov bx, 32             ; Try to allocate 32 bytes
    call malloc
    mov [last_alloc], ax   ; Store allocation result
    
    mov si, malloc_success
    call print_string
    
    mov ax, [last_alloc]   ; Restore allocation result for printing
    call print_hex
    
    mov si, newline
    call print_string
    ret

; Test free command handler
free_test:
    mov bx, [last_alloc]   ; Get the last allocated address
    test bx, bx            ; Check if address is 0 (nothing allocated)
    jz .nothing_to_free
    
    call free
    
    ; Only clear the stored address if free was successful
    test ax, ax            ; Check if free succeeded
    jz .free_failed
    
    mov word [last_alloc], 0  ; Clear stored address
    mov si, free_success
    jmp .print_result
    
.nothing_to_free:
    mov si, nothing_to_free_msg
    jmp .print_result

.free_failed:
    mov si, free_failed_msg

.print_result:
    call print_string
    ret

memory_test:
    ; Get base memory size using INT 12h
    clc                     ; Clear carry flag
    int 12h                 ; Call BIOS memory size function
    jc .memory_error        ; Jump if error (carry flag set)
    
    ; Print base memory
    push ax                 ; Save memory size
    mov si, memtest_base_msg
    call print_string
    pop ax                  ; Restore memory size
    
    ; Convert to decimal and print
    call print_decimal
    mov si, kb_msg
    call print_string
    
    ; Get extended memory size using INT 15h, AH=88h
    mov ah, 88h
    clc                     ; Clear carry flag
    int 15h                 ; Call extended memory size function
    jc .memory_error        ; Jump if error
    
    ; Print extended memory
    push ax                 ; Save memory size
    mov si, memtest_ext_msg
    call print_string
    pop ax                  ; Restore memory size
    
    ; Convert to decimal and print
    call print_decimal
    mov si, kb_msg
    call print_string
    ret

.memory_error:
    mov si, memory_error_msg
    call print_string
    ret

; Print hexadecimal number
print_hex:
    push bx
    push cx
    mov bx, ax
    mov cx, 4
.hex_loop:
    rol bx, 4
    mov ax, bx
    and ax, 0x0F
    add al, '0'
    cmp al, '9'
    jle .print_digit
    add al, 7
.print_digit:
    mov ah, 0x0E
    int 0x10
    loop .hex_loop
    pop cx
    pop bx
    ret

screen_saver:
    ; Disable cursor
    mov ah, 0x01
    mov cx, 0x2607        ; Set bits 8-15 to hide cursor
    int 0x10

    call disable_status_bar

    ; Clear the screen first
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10

    ; Initial position and direction
    mov byte [ball_x], 40      ; Starting X position
    mov byte [ball_y], 12      ; Starting Y position
    mov byte [dx_dir], 1       ; X direction (1 = right, -1 = left)
    mov byte [dy_dir], 1       ; Y direction (1 = down, -1 = up)

.screen_loop:
    ; Check for keypress
    mov ah, 0x01
    int 0x16
    jnz .exit_saver

    ; Clear old ball position
    mov ah, 0x02
    mov bh, 0
    mov dh, [ball_y]
    mov dl, [ball_x]
    int 0x10

    mov ah, 0x0E
    mov al, ' '
    int 0x10

    ; Update X position
    mov al, [ball_x]
    mov bl, [dx_dir]
    add al, bl
    mov [ball_x], al

    ; Check X boundaries
    cmp al, 0
    jle .reverse_dx
    cmp al, 79
    jge .reverse_dx
    jmp .update_y

.reverse_dx:
    neg byte [dx_dir]
    mov al, [ball_x]
    mov bl, [dx_dir]
    add al, bl
    mov [ball_x], al

.update_y:
    ; Update Y position
    mov al, [ball_y]
    mov bl, [dy_dir]
    add al, bl
    mov [ball_y], al

    ; Check Y boundaries
    cmp al, 0
    jle .reverse_dy
    cmp al, 24
    jge .reverse_dy
    jmp .draw_ball

.reverse_dy:
    neg byte [dy_dir]
    mov al, [ball_y]
    mov bl, [dy_dir]
    add al, bl
    mov [ball_y], al

.draw_ball:
    ; Draw new ball position
    mov ah, 0x02
    mov bh, 0
    mov dh, [ball_y]
    mov dl, [ball_x]
    int 0x10

    mov ah, 0x0E
    mov al, 'O'
    int 0x10

    ; Shorter delay loop
    mov cx, 0x03FF
.delay:
    push cx
    mov cx, 0x03FF
.inner_delay:
    loop .inner_delay
    pop cx
    loop .delay

    jmp .screen_loop

.exit_saver:
    ; Clear any pending keypress
    mov ah, 0x00
    int 0x16

    ; Clear screen before returning
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10

    ; Re-enable cursor
    mov ah, 0x01
    mov cx, 0x0607        ; Normal cursor
    int 0x10
    
    ; Reset cursor position
    mov ah, 0x02
    mov bh, 0
    mov dx, 0
    int 0x10

    call enable_status_bar

    mov si, newline
    mov si, newline
    call print_string    
    
    ret

edit_command:
    mov word [buffer_pos], 0
    
.show_menu:
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    mov ah, 0x02
    mov bh, 0
    mov dx, 0
    int 0x10
    mov si, editor_menu
    call print_string
    
.get_menu_choice:
    xor ah, ah
    int 0x16
    cmp al, '1'
    je .create_new
    cmp al, '2'
    je .exit_editor
    jmp .get_menu_choice
    
.create_new:
    call text_editor
    jmp .show_menu

.exit_editor:
    mov si, newline
    mov si, newline
    call print_string
    ret
    
text_editor:
    call disable_status_bar

    mov word [cursor_x], 0
    mov word [cursor_y], 0
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    
.start_editing:
    mov ah, 0x02
    mov bh, 0
    mov dh, [cursor_y]
    mov dl, [cursor_x]
    int 0x10
    
.edit_loop:
    mov ah, 0x02
    mov bh, 0
    mov dh, [cursor_y]
    mov dl, [cursor_x]
    int 0x10
    mov ah, 0x00
    int 0x16
    cmp ah, 0x01
    je .exit_editor
    cmp ah, 0x0E
    je .handle_backspace
    cmp ah, 0x1C
    je .handle_enter
    cmp al, 32
    jb .edit_loop
    cmp al, 126
    ja .edit_loop
    mov di, [buffer_pos]
    cmp di, MAX_BUFFER_SIZE - 2
    jae .edit_loop
    mov [text_buffer + di], al
    inc word [buffer_pos]
    mov ah, 0x0E
    int 0x10
    inc word [cursor_x]
    cmp word [cursor_x], 80
    jb .edit_loop
    mov word [cursor_x], 0
    inc word [cursor_y]
    cmp word [cursor_y], 25
    jb .edit_loop
    dec word [cursor_y]
    mov ah, 0x06
    mov al, 1
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    jmp .edit_loop
    
.handle_backspace:
    cmp word [buffer_pos], 0
    je .edit_loop
    cmp word [cursor_x], 0
    jne .same_line_backspace
    cmp word [cursor_y], 0
    je .edit_loop
    dec word [cursor_y]
    mov word [cursor_x], 79
    jmp .do_backspace
    
.same_line_backspace:
    dec word [cursor_x]
    
.do_backspace:
    dec word [buffer_pos]
    mov ah, 0x02
    mov bh, 0
    mov dh, [cursor_y]
    mov dl, [cursor_x]
    int 0x10
    mov ah, 0x0E
    mov al, ' '
    int 0x10
    mov ah, 0x02
    mov bh, 0
    mov dh, [cursor_y]
    mov dl, [cursor_x]
    int 0x10
    jmp .edit_loop
    
.handle_enter:
    mov di, [buffer_pos]
    mov byte [text_buffer + di], 0x0D
    inc di
    mov byte [text_buffer + di], 0x0A
    add word [buffer_pos], 2
    mov word [cursor_x], 0
    inc word [cursor_y]
    cmp word [cursor_y], 25
    jb .no_scroll
    dec word [cursor_y]
    mov ah, 0x06
    mov al, 1
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    
.no_scroll:
    mov ah, 0x0E
    mov al, 0x0D
    int 0x10
    mov al, 0x0A
    int 0x10
    jmp .edit_loop
    
.exit_editor:
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    mov ah, 0x02
    mov bh, 0
    mov dx, 0
    int 0x10
    call enable_status_bar
    ret

snake_game:
    ; Save registers
    pusha
    
    call disable_status_bar
    
    ; Initialize game
    call init_snake_game
    
    ; Main game loop
.game_loop:
    mov ah, 0x01
    int 0x16
    jz .no_input
    mov ah, 0x00
    int 0x16
    cmp al, 27          ; ESC key
    je .exit_game
    cmp ah, 0x48        ; Up arrow
    je .check_up
    cmp ah, 0x50        ; Down arrow
    je .check_down
    cmp ah, 0x4B        ; Left arrow
    je .check_left
    cmp ah, 0x4D        ; Right arrow
    je .check_right
    jmp .no_input

.check_up:
    cmp word [snake_direction], DOWN    ; Can't go up if going down
    je .no_input
    mov word [snake_direction], UP
    jmp .no_input

.check_down:
    cmp word [snake_direction], UP      ; Can't go down if going up
    je .no_input
    mov word [snake_direction], DOWN
    jmp .no_input

.check_left:
    cmp word [snake_direction], RIGHT   ; Can't go left if going right
    je .no_input
    mov word [snake_direction], LEFT
    jmp .no_input

.check_right:
    cmp word [snake_direction], LEFT    ; Can't go right if going left
    je .no_input
    mov word [snake_direction], RIGHT
    
.no_input:
    ; Move snake
    call move_snake
    
    ; Check collision
    call check_collision
    cmp ax, 1
    je .game_over
    
    ; Draw game
    call draw_game
    
    ; Delay
    mov cx, [game_speed]
.delay:
    push cx
    mov cx, 0x0FFF
.inner_delay:
    loop .inner_delay
    pop cx
    loop .delay
    
    jmp .game_loop

.game_over:
    call show_game_over
    
.exit_game:
    ; Restore screen
    mov ax, 0x0003
    int 0x10
    
    call enable_status_bar
    
    mov si, newline
    mov si, newline
    call print_string
    
    ; Restore registers
    popa
    ret

init_snake_game:
    ; Clear screen
    mov ax, 0x0003
    int 0x10
    
    ; Hide cursor
    mov ah, 0x01
    mov cx, 0x2607
    int 0x10
    
    ; Initialize variables
    mov word [snake_length], 4
    mov word [snake_direction], 0
    mov word [score], 0
    mov word [game_speed], INITIAL_SPEED
    
    ; Initialize snake position (middle of screen)
    mov cx, 4
    mov di, 0
    mov ax, GAME_WIDTH / 2
.init_snake:
    mov [snake_x + di], ax
    mov word [snake_y + di], GAME_HEIGHT / 2
    dec ax
    add di, 2
    loop .init_snake
    
    ; Place first food
    call place_food
    
    ; Draw initial screen
    call draw_game
    ret

move_snake:
    ; Move body
    mov cx, [snake_length]
    dec cx
    mov di, cx
    shl di, 1
.move_body:
    mov ax, [snake_x + di - 2]
    mov [snake_x + di], ax
    mov ax, [snake_y + di - 2]
    mov [snake_y + di], ax
    sub di, 2
    loop .move_body
    
    ; Move head based on direction
    mov ax, [snake_x]
    mov bx, [snake_y]
    
    cmp word [snake_direction], 0
    je .move_right
    cmp word [snake_direction], 1
    je .move_down
    cmp word [snake_direction], 2
    je .move_left
    ; Must be up
    dec bx
    jmp .finish_move
    
.move_right:
    inc ax
    jmp .finish_move
.move_down:
    inc bx
    jmp .finish_move
.move_left:
    dec ax

.finish_move:
    mov [snake_x], ax
    mov [snake_y], bx
    ret

check_collision:
    ; Get head position
    mov ax, [snake_x]
    mov bx, [snake_y]
    
    ; Check wall collision
    cmp ax, 0
    jl .collision
    cmp ax, GAME_WIDTH
    jge .collision
    cmp bx, 0
    jl .collision
    cmp bx, GAME_HEIGHT
    jge .collision
    
    ; Check self collision
    mov cx, [snake_length]
    dec cx
    mov di, 2
.check_self:
    mov dx, [snake_x + di]
    cmp ax, dx
    jne .next_segment
    mov dx, [snake_y + di]
    cmp bx, dx
    je .collision
.next_segment:
    add di, 2
    loop .check_self
    
    ; Check food collision
    mov dx, [food_x]
    cmp ax, dx
    jne .no_collision
    mov dx, [food_y]
    cmp bx, dx
    jne .no_collision
    
    ; Eat food
    inc word [snake_length]
    inc word [score]
    ; Speed up game slightly
    mov ax, [game_speed]
    mov bx, 95
    mul bx
    mov bx, 100
    div bx
    mov [game_speed], ax
    call place_food
    
.no_collision:
    xor ax, ax
    ret
    
.collision:
    mov ax, 1
    ret

place_food:
    ; Get system time for random seed
    mov ah, 0x00
    int 0x1A
    mov ax, dx
    
    ; Generate random x position
    xor dx, dx
    mov bx, GAME_WIDTH - 1
    div bx
    mov [food_x], dx
    
    ; Generate random y position
    mov ax, cx
    xor dx, dx
    mov bx, GAME_HEIGHT - 1
    div bx
    mov [food_y], dx
    ret

draw_game:
    ; Clear screen
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    
    ; Draw border
    mov ah, 0x02
    xor bh, bh
    mov dx, 0
    int 0x10
    
    ; Draw score
    mov ah, 0x0E
    mov al, 'S'
    int 0x10
    mov al, 'c'
    int 0x10
    mov al, 'o'
    int 0x10
    mov al, 'r'
    int 0x10
    mov al, 'e'
    int 0x10
    mov al, ':'
    int 0x10
    mov al, ' '
    int 0x10
    
    mov ax, [score]
    call print_number
    
    ; Draw snake
    mov cx, [snake_length]
    xor di, di
.draw_snake:
    mov ah, 0x02
    mov bh, 0
    mov dl, [snake_x + di]
    mov dh, [snake_y + di]
    int 0x10
    
    mov ah, 0x0E
    mov al, '*'
    int 0x10
    
    add di, 2
    loop .draw_snake
    
    ; Draw food
    mov ah, 0x02
    mov bh, 0
    mov dl, [food_x]
    mov dh, [food_y]
    int 0x10
    
    mov ah, 0x0E
    mov al, '@'
    int 0x10
    ret

show_game_over:
    ; Clear screen
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    
    ; Position cursor
    mov ah, 0x02
    xor bh, bh
    mov dx, 0x0A0A
    int 0x10
    
    ; Print game over message
    mov si, game_over_msg
    call print_string
    
    mov ax, [score]
    call print_number
    
    mov ah, 0x02
    mov dl, 0x0A
    int 0x21
    
    mov si, press_key_msg
    call print_string
    
    ; Wait for keypress
    mov ah, 0x00
    int 0x16
    ret

print_number:
    push ax
    push bx
    push cx
    push dx
    
    mov bx, 10
    xor cx, cx
    
.divide:
    xor dx, dx
    div bx
    push dx
    inc cx
    test ax, ax
    jnz .divide
    
.print_digits:
    pop dx
    add dl, '0'
    mov ah, 0x0E
    mov al, dl
    int 0x10
    loop .print_digits
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret

show_credits:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    
    ; Clear screen
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    
    ; Move cursor to top
    mov ah, 0x02
    mov bh, 0
    mov dx, 0
    int 0x10
    
    ; Display header
    mov si, credits_header
    call print_string
    
    ; Display credits with delay
    mov bx, credit_ptrs      ; Start with first credit pointer
    
.next_credit:
    mov si, [bx]             ; Load pointer to current credit string
    test si, si              ; Check if we've reached the end (0)
    jz .end_credits          ; If zero, we're done
    
    call print_string        ; Print the current credit
    
    ; Delay for approximately 1 second
    call delay_1sec
    
    add bx, 2                ; Move to next pointer (2 bytes per pointer)
    jmp .next_credit         ; Continue with next credit
    
.end_credits:
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
    
delay_1sec:
    push ax
    push bx
    push cx
    push dx
    
    ; Get current clock tick
    mov ah, 0
    int 0x1A
    mov bx, dx
    
    ; Wait for approximately 36-37 ticks (about 2 seconds)
.wait_loop:
    mov ah, 0
    int 0x1A
    sub dx, bx
    cmp dx, 36
    jb .wait_loop
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret

timer_utility:
    mov si, input_buffer
    add si, 6  ; Skip "TIMER "
    call parse_decimal
    jc .timer_error
    test ax, ax
    jz .timer_error
    mov cx, ax
.countdown:
    push cx
    mov si, countdown_msg
    call print_string
    pop ax
    push ax
    call print_decimal
    mov si, seconds_msg
    call print_string
    mov ah, 0x86
    mov cx, 0x0F
    mov dx, 0x4240  ; Approximately 1 second delay
    int 0x15
    pop cx
    loop .countdown
    
    mov si, alarm_msg
    call print_string
    
    ; Use startup sound-style beeps instead of ASCII bell
    mov cx, 3
.beep_loop:
    push cx
    
    ; Use D6 for alarm (higher pitch for attention)
    mov bx, 1521  ; D6 (784 Hz)
    mov cx, 0x0500  ; Longer duration for alarm
    call play_startup_note_with_duration
    
    ; Add a delay between beeps
    mov cx, 0x1000
.between_beeps:
    loop .between_beeps
    
    pop cx
    loop .beep_loop
    
    ret
    
.timer_error:
    mov si, timer_usage
    call print_string
    ret

pong_game:
    pusha
    call init_pong
    call disable_status_bar
.game_loop:
    mov ah, 0x01       ; Check for keypress
    int 0x16
    jz .no_input
    mov ah, 0x00
    int 0x16
    
    cmp al, 27         ; ESC to exit
    je .exit_game
    
    cmp al, 'w'        ; Player 1 up
    je .p1_up
    cmp al, 'W'
    je .p1_up
    cmp al, 's'        ; Player 1 down
    je .p1_down
    cmp al, 'S'
    je .p1_down
    
    cmp ah, 0x48       ; Player 2 up (up arrow)
    je .p2_up
    cmp ah, 0x50       ; Player 2 down (down arrow)
    je .p2_down
    
    cmp al, '+'        ; Increase speed
    je .speed_up
    cmp al, '-'        ; Decrease speed
    je .speed_down
    
    jmp .no_input

.p1_up:
    cmp word [p1_pos], 1
    jle .no_input
    dec word [p1_pos]
    jmp .no_input

.p1_down:
    mov ax, [p1_pos]
    add ax, PADDLE_HEIGHT
    cmp ax, COURT_HEIGHT
    jge .no_input
    inc word [p1_pos]
    jmp .no_input

.p2_up:
    cmp word [p2_pos], 1
    jle .no_input
    dec word [p2_pos]
    jmp .no_input

.p2_down:
    mov ax, [p2_pos]
    add ax, PADDLE_HEIGHT
    cmp ax, COURT_HEIGHT
    jge .no_input
    inc word [p2_pos]
    jmp .no_input

.speed_up:
    cmp word [pong_speed], 500
    jle .no_input
    sub word [pong_speed], 500
    jmp .no_input

.speed_down:
    add word [pong_speed], 500
    jmp .no_input

.no_input:
    call update_ball
    call check_collisions
    call draw_pong
    mov cx, [pong_speed]
.delay:
    push cx
    mov cx, 0x0FFF
.inner_delay:
    loop .inner_delay
    pop cx
    loop .delay
    jmp .game_loop

.exit_game:
    mov ax, 0x0003
    int 0x10
    call enable_status_bar
    mov si, newline
    mov si, newline
    call print_string
    popa
    ret

init_pong:
    mov ax, 0x0003     ; Clear screen
    int 0x10
    mov ah, 0x01       ; Hide cursor
    mov cx, 0x2607
    int 0x10
    
    ; Initialize game state
    mov word [ball_pos_x], COURT_WIDTH / 2
    mov word [ball_pos_y], COURT_HEIGHT / 2
    mov word [ball_vel_x], 1
    mov word [ball_vel_y], 1
    mov word [p1_pos], COURT_HEIGHT / 2 - 1
    mov word [p2_pos], COURT_HEIGHT / 2 - 1
    mov word [p1_score], 0
    mov word [p2_score], 0
    mov word [pong_speed], DEFAULT_SPEED
    
    ; Display instructions
    mov si, pong_msg
    call print_string
    ret

update_ball:
    ; Update X position
    mov ax, [ball_pos_x]
    add ax, [ball_vel_x]
    mov [ball_pos_x], ax
    
    ; Update Y position
    mov ax, [ball_pos_y]
    add ax, [ball_vel_y]
    mov [ball_pos_y], ax
    ret

check_collisions:
    ; Check top/bottom walls
    mov ax, [ball_pos_y]
    cmp ax, 1
    jle .bounce_y
    cmp ax, COURT_HEIGHT - 1
    jge .bounce_y
    jmp .check_paddles

.bounce_y:
    neg word [ball_vel_y]
    mov ax, [ball_pos_y]
    cmp ax, 1
    jg .check_paddles
    mov word [ball_pos_y], 1
    jmp .check_paddles
    
.check_paddles:
    ; Check left paddle (Player 1)
    mov ax, [ball_pos_x]
    cmp ax, P1_X + 1
    jne .check_p2
    mov bx, [ball_pos_y]
    mov cx, [p1_pos]
    cmp bx, cx
    jl .check_p2
    add cx, PADDLE_HEIGHT
    cmp bx, cx
    jg .check_p2
    neg word [ball_vel_x]
    
.check_p2:
    ; Check right paddle (Player 2)
    mov ax, [ball_pos_x]
    cmp ax, P2_X - 1
    jne .check_scoring
    mov bx, [ball_pos_y]
    mov cx, [p2_pos]
    cmp bx, cx
    jl .check_scoring
    add cx, PADDLE_HEIGHT
    cmp bx, cx
    jg .check_scoring
    neg word [ball_vel_x]
    
.check_scoring:
    ; Check if ball passed paddles
    mov ax, [ball_pos_x]
    cmp ax, 1
    jle .p2_scores
    cmp ax, COURT_WIDTH - 1
    jge .p1_scores
    ret
    
.p1_scores:
    inc word [p1_score]
    jmp .reset_ball
    
.p2_scores:
    inc word [p2_score]
    
.reset_ball:
    mov word [ball_pos_x], COURT_WIDTH / 2
    mov word [ball_pos_y], COURT_HEIGHT / 2
    neg word [ball_vel_x]
    ret

draw_pong:
    ; Clear screen
    mov ax, 0x0700
    mov bh, 0x07
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    
    ; Draw scores
    mov ah, 0x02
    mov bh, 0
    mov dx, 0
    int 0x10
    
    mov si, p1_score_msg
    call print_string
    mov ax, [p1_score]
    call print_decimal
    
    mov ah, 0x02
    mov bh, 0
    mov dx, 0x0040
    int 0x10
    
    mov si, p2_score_msg
    call print_string
    mov ax, [p2_score]
    call print_decimal
    
    ; Draw speed
    mov ah, 0x02
    mov bh, 0
    mov dx, 0x0020
    int 0x10
    
    mov si, speed_msg
    call print_string
    mov ax, [pong_speed]
    call print_decimal
    
    ; Draw Player 1 paddle
    mov cx, PADDLE_HEIGHT
    mov dx, [p1_pos]
.draw_p1:
    mov ah, 0x02
    mov bh, 0
    push dx
    mov dh, dl
    mov dl, P1_X
    int 0x10
    pop dx
    
    mov ah, 0x0E
    mov al, '|'
    int 0x10
    
    inc dx
    loop .draw_p1
    
    ; Draw Player 2 paddle
    mov cx, PADDLE_HEIGHT
    mov dx, [p2_pos]
.draw_p2:
    mov ah, 0x02
    mov bh, 0
    push dx
    mov dh, dl
    mov dl, P2_X
    int 0x10
    pop dx
    
    mov ah, 0x0E
    mov al, '|'
    int 0x10
    
    inc dx
    loop .draw_p2
    
    ; Draw ball
    mov ah, 0x02
    mov bh, 0
    mov dl, [ball_pos_x]
    mov dh, [ball_pos_y]
    int 0x10
    
    mov ah, 0x0E
    mov al, 'O'
    int 0x10
    
    ret

tictactoe_game:
    ; Initialize the game board with empty spaces
    mov di, ttt_board
    mov cx, 9
    mov al, ' '
    rep stosb
    
    ; Print the welcome message
    mov si, ttt_welcome
    call print_string
    
    ; Print the initial board
    call print_ttt_board
    
ttt_game_loop:
    ; Player's turn
    mov si, ttt_player_prompt
    call print_string
    
    ; Read player input
    mov di, input_buffer
    call read_line
    
    mov si, newline
    call print_string
    
    ; Convert input to board position (1-9)
    mov si, input_buffer
    lodsb
    
    ; Check if valid input (1-9)
    cmp al, '1'
    jb ttt_invalid_move
    cmp al, '9'
    ja ttt_invalid_move
    
    ; Convert ASCII to position index (0-8)
    sub al, '1'
    movzx bx, al
    
    ; Check if position is already taken
    cmp byte [ttt_board + bx], ' '
    jne ttt_invalid_move
    
    ; Place player's mark
    mov byte [ttt_board + bx], 'X'
    
    ; Print updated board
    call print_ttt_board
    
    ; Check for player win
    call check_ttt_win
    cmp al, 'X'
    je ttt_player_wins
    
    ; Check for draw
    call check_ttt_draw
    cmp al, 1
    je ttt_game_draw
    
    ; Computer's turn
    ; Print computer's turn message
    mov si, ttt_computer_turn_msg
    call print_string
    
    ; Generate random number to determine AI strategy
    call generate_random_0_99
    
    ; 70% chance of smart play, 30% chance of random play
    cmp ax, 70
    jae .random_move
    
    ; SMART AI STRATEGY (but only used 70% of the time)
    ; 1. Check if computer can win in next move
    call find_winning_move
    cmp bx, -1
    jne ttt_make_move
    
    ; Random chance (50%) to miss blocking the player
    call generate_random_0_99
    cmp ax, 50
    jae .skip_blocking
    
    ; 2. Block player's winning move
    call find_blocking_move
    cmp bx, -1
    jne ttt_make_move
    
.skip_blocking:
    ; Another random check (70% chance of strategic play)
    call generate_random_0_99
    cmp ax, 70
    jae .random_move
    
    ; 3. Take center if available
    cmp byte [ttt_board + 4], ' '
    jne .try_corners
    mov bx, 4
    jmp ttt_make_move
    
.try_corners:
    ; 4. Take corners if available
    mov di, ttt_corners
    mov cx, 4
.check_corners:
    movzx bx, byte [di]
    cmp byte [ttt_board + bx], ' '
    je ttt_make_move
    inc di
    loop .check_corners
    
.random_move:
    ; Find a random empty space
    mov cx, 5      ; Try up to 5 times to find a random empty space
.find_random:
    call generate_random_0_8  ; Get a position between 0-8
    mov bx, ax
    
    ; Check if it's empty
    cmp byte [ttt_board + bx], ' '
    je ttt_make_move
    
    dec cx
    jnz .find_random
    
    ; If we failed to find a random move after 5 tries, 
    ; fall back to taking any available move
    xor bx, bx
    mov cx, 9
.find_any_move:
    cmp byte [ttt_board + bx], ' '
    je ttt_make_move
    inc bx
    loop .find_any_move
    jmp ttt_game_draw  ; If no moves left, it's a draw

; Helper function to generate a random number from 0-99
; Returns: AX = random number 0-99
generate_random_0_99:
    push ebx
    push ecx
    push edx
    
    ; Load current state into eax (32-bit)
    mov eax, [prng_state]
    
    ; Xorshift algorithm:
    ; x ^= x << 13
    mov ebx, eax
    shl ebx, 13
    xor eax, ebx
    
    ; x ^= x >> 17
    mov ebx, eax
    shr ebx, 17
    xor eax, ebx
    
    ; x ^= x << 5
    mov ebx, eax
    shl ebx, 5
    xor eax, ebx
    
    ; Save updated state
    mov [prng_state], eax
    
    ; Generate a number between 0-99
    xor edx, edx
    mov ebx, 100
    div ebx      ; eax / 100, remainder in edx
    
    mov ax, dx   ; Move remainder to ax for return value
    
    pop edx
    pop ecx
    pop ebx
    ret

; Helper function to generate a random number from 0-8
; Returns: AX = random number 0-8
generate_random_0_8:
    push ebx
    push ecx
    push edx
    
    ; Load current state into eax (32-bit)
    mov eax, [prng_state]
    
    ; Xorshift algorithm:
    ; x ^= x << 13
    mov ebx, eax
    shl ebx, 13
    xor eax, ebx
    
    ; x ^= x >> 17
    mov ebx, eax
    shr ebx, 17
    xor eax, ebx
    
    ; x ^= x << 5
    mov ebx, eax
    shl ebx, 5
    xor eax, ebx
    
    ; Save updated state
    mov [prng_state], eax
    
    ; Generate a number between 0-8
    xor edx, edx
    mov ebx, 9
    div ebx      ; eax / 9, remainder in edx
    
    mov ax, dx   ; Move remainder to ax for return value
    
    pop edx
    pop ecx
    pop ebx
    ret
    
ttt_make_move:
    mov byte [ttt_board + bx], 'O'
    
    ; Print updated board
    call print_ttt_board
    
    ; Check for computer win
    call check_ttt_win
    cmp al, 'O'
    je ttt_computer_wins
    
    ; Check for draw
    call check_ttt_draw
    cmp al, 1
    je ttt_game_draw
    
    jmp ttt_game_loop
    
ttt_invalid_move:
    mov si, ttt_invalid_msg
    call print_string
    jmp ttt_game_loop
    
ttt_player_wins:
    mov si, ttt_player_win_msg
    call print_string
    ret
    
ttt_computer_wins:
    mov si, ttt_computer_win_msg
    call print_string
    ret
    
ttt_game_draw:
    mov si, ttt_draw_msg
    call print_string
    ret
    
; Find a winning move for O
; Returns: BX = winning position or -1 if none found
find_winning_move:
    push cx
    push dx
    
    ; Try each empty position
    xor bx, bx
    mov cx, 9
.check_pos:
    cmp byte [ttt_board + bx], ' '
    jne .next_pos
    
    ; Temporarily place O and check for win
    mov byte [ttt_board + bx], 'O'
    
    ; Check for win
    call check_ttt_win
    cmp al, 'O'
    je .found_winning_move
    
    ; Undo move
    mov byte [ttt_board + bx], ' '
    
.next_pos:
    inc bx
    loop .check_pos
    
    ; No winning move found
    mov bx, -1
    jmp .done
    
.found_winning_move:
    ; Undo move
    mov byte [ttt_board + bx], ' '
    
.done:
    pop dx
    pop cx
    ret
    
; Find a move to block X from winning
; Returns: BX = blocking position or -1 if none needed
find_blocking_move:
    push cx
    push dx
    
    ; Try each empty position
    xor bx, bx
    mov cx, 9
.check_pos:
    cmp byte [ttt_board + bx], ' '
    jne .next_pos
    
    ; Temporarily place X and check for win
    mov byte [ttt_board + bx], 'X'
    
    ; Check for win
    call check_ttt_win
    cmp al, 'X'
    je .found_blocking_move
    
    ; Undo move
    mov byte [ttt_board + bx], ' '
    
.next_pos:
    inc bx
    loop .check_pos
    
    ; No blocking move needed
    mov bx, -1
    jmp .done
    
.found_blocking_move:
    ; Undo move
    mov byte [ttt_board + bx], ' '
    
.done:
    pop dx
    pop cx
    ret
    
; Print the Tic-Tac-Toe board
print_ttt_board:
    push ax
    push bx
    
    mov si, newline
    call print_string
    
    ; Print board rows with position hints
    mov bx, 0
    call print_ttt_row
    
    mov si, ttt_row_div
    call print_string
    
    mov bx, 3
    call print_ttt_row
    
    mov si, ttt_row_div
    call print_string
    
    mov bx, 6
    call print_ttt_row
    
    mov si, newline
    call print_string
    
    pop bx
    pop ax
    ret
    
; Print a row of the Tic-Tac-Toe board
; BX = starting index of the row
print_ttt_row:
    push ax
    
    mov al, ' '
    mov ah, 0x0E
    int 0x10
    
    ; First cell
    mov al, [ttt_board + bx]
    cmp al, ' '
    jne .print_first
    add bl, '1'
    mov al, bl
    sub bl, '1'
.print_first:
    int 0x10
    
    mov al, ' '
    int 0x10
    
    mov al, '|'
    int 0x10
    
    mov al, ' '
    int 0x10
    
    ; Second cell
    mov al, [ttt_board + bx + 1]
    cmp al, ' '
    jne .print_second
    add bl, '2'
    mov al, bl
    sub bl, '2'
.print_second:
    int 0x10
    
    mov al, ' '
    int 0x10
    
    mov al, '|'
    int 0x10
    
    mov al, ' '
    int 0x10
    
    ; Third cell
    mov al, [ttt_board + bx + 2]
    cmp al, ' '
    jne .print_third
    add bl, '3'
    mov al, bl
    sub bl, '3'
.print_third:
    int 0x10
    
    mov al, ' '
    int 0x10
    
    mov si, newline
    call print_string
    
    pop ax
    ret
    
; Check for a win condition
; Returns: AL = X if X wins, O if O wins, 0 if no winner
check_ttt_win:
    push bx
    push cx
    push dx
    push di
    
    ; Check rows
    mov di, 0
    mov cx, 3
check_rows:
    mov al, [ttt_board + di]
    cmp al, ' '
    je next_row
    
    cmp al, [ttt_board + di + 1]
    jne next_row
    
    cmp al, [ttt_board + di + 2]
    jne next_row
    
    ; Found winner in row
    jmp check_win_done
    
next_row:
    add di, 3
    loop check_rows
    
    ; Check columns
    mov di, 0
    mov cx, 3
check_cols:
    mov al, [ttt_board + di]
    cmp al, ' '
    je next_col
    
    cmp al, [ttt_board + di + 3]
    jne next_col
    
    cmp al, [ttt_board + di + 6]
    jne next_col
    
    ; Found winner in column
    jmp check_win_done
    
next_col:
    inc di
    loop check_cols
    
    ; Check diagonal top-left to bottom-right
    mov al, [ttt_board + 0]
    cmp al, ' '
    je check_other_diag
    
    cmp al, [ttt_board + 4]
    jne check_other_diag
    
    cmp al, [ttt_board + 8]
    jne check_other_diag
    
    ; Found winner in diagonal
    jmp check_win_done
    
check_other_diag:
    ; Check diagonal top-right to bottom-left
    mov al, [ttt_board + 2]
    cmp al, ' '
    je no_winner
    
    cmp al, [ttt_board + 4]
    jne no_winner
    
    cmp al, [ttt_board + 6]
    jne no_winner
    
    ; Found winner in diagonal
    jmp check_win_done
    
no_winner:
    ; No winner found
    mov al, 0
    
check_win_done:
    pop di
    pop dx
    pop cx
    pop bx
    ret
    
; Check for a draw condition
; Returns: AL = 1 if board is full (draw), 0 otherwise
check_ttt_draw:
    push bx
    push cx
    
    mov bx, 0
    mov cx, 9
    
check_draw_loop:
    cmp byte [ttt_board + bx], ' '
    je not_draw
    inc bx
    loop check_draw_loop
    
    ; Board is full, it's a draw
    mov al, 1
    jmp check_draw_done
    
not_draw:
    mov al, 0
    
check_draw_done:
    pop cx
    pop bx
    ret


; Print a null-terminated string
print_string:
    .next_char:
        lodsb
        cmp al, 0
        je .done
        mov ah, 0x0E
        int 0x10
        jmp .next_char
    .done:
        ret

; Compare two strings (case-insensitive)
strcmp:
    .compare:
        lodsb
        mov ah, [di]
        cmp al, 'a'
        jl .skip_lower
        cmp al, 'z'
        jg .skip_lower
        sub al, 0x20
    .skip_lower:
        cmp ah, 'a'
        jl .skip_upper
        cmp ah, 'z'
        jg .skip_upper
        sub ah, 0x20
    .skip_upper:
        cmp al, ah
        jne .not_equal
        cmp al, 0
        je .equal
        inc di
        jmp .compare
    .not_equal:
        mov ax, 0
        ret
    .equal:
        mov ax, 1
        ret

; New prefix comparison function for commands like ECHO
strcmp_prefix:
    .compare:
        lodsb                   ; Load character from SI into AL
        mov ah, [di]           ; Load character from DI into AH
        
        ; Convert to uppercase for case-insensitive comparison
        cmp al, 'a'
        jl .skip_lower1
        cmp al, 'z'
        jg .skip_lower1
        sub al, 0x20
    .skip_lower1:
        cmp ah, 'a'
        jl .skip_upper1
        cmp ah, 'z'
        jg .skip_upper1
        sub ah, 0x20
    .skip_upper1:
        cmp ah, 0              ; If we've reached the end of command string
        je .check_space
        
        cmp al, ah
        jne .not_equal
        
        inc di
        jmp .compare
        
    .check_space:
        cmp al, ' '            ; Check if next character is exactly one space
        je .prefix_match
        cmp al, 0              ; Or if it's the end of string
        je .prefix_match
        
    .not_equal:
        mov ax, 0
        ret
        
    .prefix_match:
        mov ax, 1
        ret

; Print unknown command message
print_unknown_message:
    mov ah, 0x0E
    mov al, 0x27
    int 0x10
    .print_command:
        lodsb
        cmp al, 0
        je .done_unknown
        int 0x10
        jmp .print_command
    .done_unknown:
    mov al, 0x27
    int 0x10

    mov si, not_recognized_msg
    call print_string
    ret

print_unknown_help:
    mov ah, 0x0E
    mov al, 0x27
    int 0x10
    .print_help_command:
        lodsb
        cmp al, 0
        je .done_unknown_help
        int 0x10
        jmp .print_help_command
    .done_unknown_help:
        mov al, 0x27
        int 0x10
        mov si, help_not_found
        call print_string
        ret

; Function to play a note with specified duration
play_startup_note_with_duration:
    ; Set frequency
    mov al, 0xB6
    out 0x43, al
    mov al, bl
    out 0x42, al
    mov al, bh
    out 0x42, al
    
    ; Turn speaker on
    in al, 0x61
    or al, 0x03
    out 0x61, al
    
    ; Hold note for duration
    push cx
.note_duration:
    mov dx, 0x500
.note_delay:
    dec dx
    jnz .note_delay
    loop .note_duration
    pop cx
    
    ; Turn speaker off
    in al, 0x61
    and al, 0xFC
    out 0x61, al
    
    ; Pause between notes
    mov cx, 0x0075
.pause_loop:
    loop .pause_loop
    
    ret

display_boot_screen:
    push ax
    push bx
    push cx
    push dx
    push si
    
    ; Set boot screen state to active
    mov byte [boot_screen_state], BOOT_SCREEN_ACTIVE
    
    ; Clear screen with blue background 
    mov ax, 0x0600
    mov bh, 0x17    ; Blue background (1), light gray text (7)
    mov cx, 0x0000  ; Upper left corner
    mov dx, 0x184F  ; Lower right corner (24, 79)
    int 0x10
    
    ; Center the logo
    mov ah, 0x02
    mov bh, 0
    mov dx, 0x0400
    int 0x10
    
    ; Display logo
    mov si, boot_logo
    call print_string
    
    ; Center the version text
    mov ah, 0x02
    mov bh, 0
    mov dx, 0x0B14
    int 0x10
    
    ; Display version
    mov si, boot_version
    call print_string
    
    ; Position for loading text
    mov ah, 0x02
    mov bh, 0
    mov dx, 0x0E14
    int 0x10
    
    ; Display loading text
    mov si, boot_loading
    call print_string
    
    ; Position for progress bar
    mov ah, 0x02
    mov bh, 0
    mov dh, 16
    mov dl, 15
    int 0x10
    
    ; Display progress bar start
    mov si, progress_bar_start
    call print_string
    
    ; Display empty progress bar
    mov cx, PROGRESS_BAR_WIDTH
.empty_bar:
    mov si, empty_char
    call print_string
    loop .empty_bar
    
    ; Display progress bar end
    mov si, progress_bar_end
    call print_string
    
    ; Position for copyright
    mov ah, 0x02
    mov bh, 0
    mov dx, 0x1712
    int 0x10
    
    ; Display copyright
    mov si, boot_copyright
    call print_string
    
    ; Start playing startup sound (this will animate the progress bar)
    call play_startup_sound
    
    ; Clear the screen after boot animation completes
    mov ax, 0x0700
    mov bh, 0x07    ; Reset to normal colors (black background, light gray text)
    mov cx, 0x0000  ; Upper left corner
    mov dx, 0x184F  ; Lower right corner
    int 0x10
    
    ; Set boot screen state to inactive
    mov byte [boot_screen_state], BOOT_SCREEN_INACTIVE
    
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

play_startup_sound:
    push ax
    push bx
    push cx
    push dx
    
    ; Reset progress counter
    mov byte [progress_counter], 0
    
    ; Each note will update the progress bar by PROGRESS_BAR_WIDTH/8 characters
    
    mov bx, 2559  ; B4
    mov cx, 0x1000
    call play_note
    
    mov bx, 2031  ; F5
    mov cx, 0x0250
    call play_note
    
    mov bx, 2031  ; F5
    mov cx, 0x0250
    call play_note
    
    mov bx, 1521  ; D6
    mov cx, 0x0325
    call play_note
  
    mov bx, 1521  ; D6
    mov cx, 0x1000
    call play_note  
    
    mov bx, 2559  ; B4
    mov cx, 0x1000
    call play_note    
    
    mov bx, 2031  ; F5
    mov cx, 0x0250
    call play_note    
    
    mov bx, 1521  ; D6
    mov cx, 0x0250
    call play_note
    
    ; Ensure progress bar is complete at the end
    call complete_progress_bar
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; Function to play a note and update progress bar
play_note:
    push ax
    push dx
    push si
    
    ; Set frequency
    mov al, 0xB6
    out 0x43, al
    mov al, bl
    out 0x42, al
    mov al, bh
    out 0x42, al
    
    ; Turn speaker on
    in al, 0x61
    or al, 0x03
    out 0x61, al
    
    ; Update progress bar for this note
    call update_progress_bar
    
    ; Hold note for duration
    push cx
.note_duration:
    mov dx, 0x1500
.note_delay:
    dec dx
    jnz .note_delay
    loop .note_duration
    pop cx
    
    ; Turn speaker off
    in al, 0x61
    and al, 0xFC
    out 0x61, al
    
    ; Pause between notes
    mov cx, 0x0500
.pause_loop:
    loop .pause_loop
    
    pop si
    pop dx
    pop ax
    ret

; Function to update the progress bar
update_progress_bar:
    push ax
    push bx
    push cx
    push dx
    
    ; Position cursor at the beginning of the progress bar
    mov ah, 0x02
    mov bh, 0
    mov dh, 16      ; Row 16
    mov dl, 16      ; Column 16 (just after the '[')
    int 0x10
    
    ; Calculate how many steps to add (each note adds about PROGRESS_BAR_WIDTH/8 steps)
    mov ax, PROGRESS_BAR_WIDTH
    mov bl, 8       ; 8 notes total
    div bl          ; AL = PROGRESS_BAR_WIDTH/8
    
    ; Add steps based on current progress counter
    add [progress_counter], al
    
    ; Make sure we don't exceed the total width
    mov al, [progress_counter]
    cmp al, PROGRESS_BAR_WIDTH
    jbe .valid_progress
    mov al, PROGRESS_BAR_WIDTH
    mov [progress_counter], al
    
.valid_progress:
    ; Draw the progress bar
    xor cx, cx
    mov cl, [progress_counter]
    
    ; Skip if no progress to draw
    test cl, cl
    jz .skip_draw
    
.draw_progress:
    mov si, progress_char
    call print_string
    loop .draw_progress
    
.skip_draw:
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; Function to ensure progress bar is complete at the end
complete_progress_bar:
    push ax
    push bx
    push cx
    push dx
    push si
    
    ; Position cursor at the beginning of the progress bar
    mov ah, 0x02
    mov bh, 0
    mov dh, 16      ; Row 16
    mov dl, 16      ; Column 16 (just after the '[')
    int 0x10
    
    ; Draw the full progress bar
    mov cx, PROGRESS_BAR_WIDTH
.draw_full:
    mov si, progress_char
    call print_string
    loop .draw_full
    
    ; Small delay to show completed bar
    mov cx, 0x7FFF
.delay_loop:
    loop .delay_loop
    
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

timer_values:
    dw 2711  ; A4 (440 Hz)
    dw 2559  ; B4 (466 Hz)
    dw 2415  ; C5 (494 Hz)
    dw 2280  ; D5 (523 Hz)
    dw 2152  ; E5 (554 Hz)
    dw 2031  ; F5 (587 Hz)
    dw 1917  ; G5 (622 Hz)
    dw 1809  ; A5 (659 Hz)
    dw 1708  ; B5 (698 Hz)
    dw 1612  ; C6 (740 Hz)
    dw 1521  ; D6 (784 Hz)
    
morse_patterns:
    ; Letters (A-Z)
    db ".-",0         ; A
    db "-...",0       ; B
    db "-.-.",0       ; C
    db "-..",0        ; D
    db ".",0          ; E
    db "..-.",0       ; F 
    db "--.",0        ; G
    db "....",0       ; H
    db "..",0         ; I
    db ".---",0       ; J
    db "-.-",0        ; K
    db ".-..",0       ; L
    db "--",0         ; M
    db "-.",0         ; N
    db "---",0        ; O
    db ".--.",0       ; P
    db "--.-",0       ; Q
    db ".-.",0        ; R
    db "...",0        ; S
    db "-",0          ; T
    db "..-",0        ; U
    db "...-",0       ; V
    db ".--",0        ; W
    db "-..-",0       ; X
    db "-.--",0       ; Y
    db "--..",0       ; Z
    
    ; Numbers (0-9)
    db "-----",0      ; 0
    db ".----",0      ; 1
    db "..---",0      ; 2
    db "...--",0      ; 3
    db "....-",0      ; 4
    db ".....",0      ; 5
    db "-....",0      ; 6
    db "--...",0      ; 7
    db "---..",0      ; 8
    db "----.",0      ; 9

scale_notes:
    dw 2711   ; A4
    dw 2415   ; C5
    dw 2280   ; D5
    dw 2031   ; F5
    dw 1917   ; G5
    dw 1809   ; A5
    dw 1612   ; C6

rhythm_patterns:
    db 0x04   ; Short note
    db 0x08   ; Medium note
    db 0x0C   ; Longer note
    db 0x10   ; Longest note

ttt_win_patterns:
    db 0, 1, 2  ; Rows
    db 3, 4, 5
    db 6, 7, 8
    db 0, 3, 6  ; Columns
    db 1, 4, 7
    db 2, 5, 8
    db 0, 4, 8  ; Diagonals
    db 2, 4, 6

; Data Section
welcome_msg db 0x0D, 0x0A, "ExploreOS Kernel Version 0.45 [0.45.0028]", 0x0D, 0x0A
            db "Type HELP for a list of available commands", 0x0D, 0x0A
            db "Report bugs or suggest commands at: youtu.be/MUdJa0pffqs", 0x0D, 0x0A, 0x0D, 0x0A, 0
license_msg  db "ExploreOS  Copyright (C) 2025  ExploreSoft", 0x0D, 0x0A
             db "This program comes with NO WARRANTY and is free software", 0x0D, 0x0A
             db "under the GPL-3.0 license conditions.", 0x0D, 0x0A, 0
prompt_msg  db "> ", 0
not_recognized_msg db " is not recognized as an internal command.", 0x0D, 0x0A, 0
newline     db 0x0D, 0x0A, 0
halt_msg    db "System halted.", 0x0D, 0x0A, 0

; Command strings
clear_cmd   db "CLEAR",    0
ver_cmd     db "VER",      0
rand_cmd    db "RAND",     0
restart_cmd db "RESTART",  0
help_cmd    db "HELP",     0
echo_cmd    db "ECHO",     0
halt_cmd    db "HALT",     0
meminfo_cmd db "MEMINFO",  0
malloc_cmd  db "MALLOC",   0
free_cmd    db "FREE",     0
battery_cmd db "BATTERY",  0
date_cmd    db "DATE",     0
time_cmd    db "TIME",     0
beep_cmd    db "BEEP",     0
memtest_cmd db "MEMTEST",  0
bios_cmd    db "BIOS",     0
screen_cmd  db "SCREEN",   0
calc_cmd    db "CALC",     0
edit_cmd    db "EDIT",     0
snake_cmd   db "SNAKE",    0 
ngg_cmd     db "NGG",      0
piano_cmd   db "PIANO",    0
paint_cmd   db "PAINT",    0
timer_cmd   db "TIMER",    0
pong_cmd    db "PONG",     0
morse_cmd   db "MORSE",    0
boot_cmd    db "BOOT",     0
history_cmd db "HISTORY",  0
defrag_cmd  db "DEFRAG",   0
hexdump_cmd db "HEXDUMP",  0
music_cmd   db "GENMSC",   0
ttt_cmd     db "TTT",      0
license_cmd db "LICENSE",  0

; Additional messages
date_error_msg db "Error reading date from RTC", 0x0D, 0x0A, 0
time_error_msg db "Error reading time from RTC", 0x0D, 0x0A, 0
ver_short   db "ExploreOS 0.45", 0
ver_msg     db "ExploreOS Version 0.45 [0.45.0028]", 0x0D, 0x0A, 0
boot_logo db "               ______           __               _____       ______ ", 0x0D, 0x0A
          db "              / ____/  ______  / /___  ________ / ___/____  / __/ /_", 0x0D, 0x0A
          db "             / __/ | |/_/ __ \/ / __ \/ ___/ _ \\__ \/ __ \/ /_/ __/", 0x0D, 0x0A
          db "            / /____>  </ /_/ / / /_/ / /  /  __/__/ / /_/ / __/ /_  ", 0x0D, 0x0A
          db "           /_____/_/|_/ .___/_/\____/_/   \___/____/\____/_/  \__/  ", 0x0D, 0x0A
          db "                     /_/                                            ", 0x0D, 0x0A, 0x0D, 0x0A, 0
boot_version db "        ExploreSoft ExploreOS", 0x0D, 0x0A
             db "                                Version 0.45", 0x0D, 0x0A, 0
boot_loading db "         Loading ExploreOS...", 0x0D, 0x0A, 0
progress_bar_start db "[", 0
progress_bar_end db "]", 0
progress_char db "#", 0
empty_char db " ", 0
boot_copyright db "ExploreSoft (C) 2024-2025 ExploreOS Project", 0x0D, 0x0A, 0
battery_msg db "Battery level: ", 0
percent_msg db "%", 0x0D, 0x0A, 0
no_battery_msg db "No battery detected or APM not supported", 0x0D, 0x0A, 0
unknown_battery_msg db "Battery level unknown", 0x0D, 0x0A, 0
nothing_to_free_msg db "No memory block to free", 0x0D, 0x0A, 0
free_failed_msg    db "Failed to free memory", 0x0D, 0x0A, 0
meminfo_separator db "--------------------------------", 0x0D, 0x0A, 0
total_blocks_msg  db "Total blocks: ", 0
used_blocks_msg   db "Used blocks: ", 0
free_blocks_msg   db "Free blocks: ", 0
total_memory_msg  db "Total memory: ", 0
used_memory_msg   db "Used memory: ", 0
free_memory_msg   db "Free memory: ", 0
largest_free_msg  db "Largest free block: ", 0
bytes_msg         db " bytes", 0
meminfo_header   db " Address     Size      Status", 0x0D, 0x0A
                 db "---------   ------    --------", 0x0D, 0x0A, 0
used_msg         db "Used", 0
free_msg         db "Free", 0
space            db " ", 0
malloc_success   db "Memory allocated at: ", 0
free_success     db "Memory freed successfully", 0x0D, 0x0A, 0
defrag_complete_msg db "Memory defragmentation complete", 0x0D, 0x0A, 0
defrag_stats_msg    db "Total defragmentations performed: ", 0
memtest_base_msg     db "Base Memory: ", 0
memtest_ext_msg      db "Extended Memory: ", 0
kb_msg              db " KB", 0x0D, 0x0A, 0
memory_error_msg    db "Error detecting memory", 0x0D, 0x0A, 0
bios_msg        db "BIOS Date: ", 0
credits_header db 0x0D, 0x0A, "                    ***** ExploreOS 0.45 PRODUCT TEAM *****", 0x0D, 0x0A, 0x0D, 0x0A, 0
credit1 db "Windows Explorer - Lead Developer", 0x0D, 0x0A, 0
credit2 db "Windows Explorer - Operating Systems Engineer", 0x0D, 0x0A, 0
credit3 db "Windows Explorer - Memory Management Specialist", 0x0D, 0x0A, 0
credit4 db "Windows Explorer - Kernel Designer", 0x0D, 0x0A, 0
credit5 db "Windows Explorer - UI/Command Interface", 0x0D, 0x0A, 0
credit6 db "heri             - BEEP Command", 0x0D, 0x0A, 0
credit7 db "Windows Explorer - Testing & Quality Assurance", 0x0D, 0x0A, 0
credit8 db "Thank you for using ExploreOS!", 0x0D, 0x0A, 0
calc_help    db "Calculator usage: number operator number", 0x0D, 0x0A
             db "Operators: + - * /", 0x0D, 0x0A,   0
calc_incomplete  db "Incomplete expression. Usage: CALC number operator number", 0x0D, 0x0A, 0
calc_invalid_op  db "Invalid operator. Valid operators: + - * /", 0x0D, 0x0A, 0
calc_invalid_num db "Invalid number or command format. Please make sure you left 1 or more spaces between the operator and the numbers.", 0x0D, 0x0A, 0
calc_spacing_error db "Invalid format. Please put spaces before and after the operator.", 0x0D, 0x0A, 0
calc_div_zero    db "Division by zero", 0x0D, 0x0A, 0   
editor_menu      db 0x0D, 0x0A, 0x0D, 0x0A, "Text Editor Menu:", 0x0D, 0x0A
                 db "1. New text document", 0x0D, 0x0A
                 db "2. Exit", 0x0D, 0x0A
                 db 0x0D, 0x0A
                 db "Select an option: ", 0
history_header db "Command history: (Latest to earliest)", 0x0D, 0x0A, 0
no_history_msg db "No commands in history.", 0x0D, 0x0A, 0
dot_space      db ". ", 0                 
game_over_msg db "Game Over! Score: ", 0
press_key_msg db "           Press any key to exit...", 0x0D, 0x0A, 0
ngg_welcome db 0x0D, 0x0A, 0x0D, 0x0A, "Number Guessing Game!", 0x0D, 0x0A
            db "I'm thinking of a number between 1 and 100.", 0x0D, 0x0A
            db "Can you guess it?", 0x0D, 0x0A, 0
ngg_prompt  db "Enter your guess (1-100): ", 0
ngg_high    db "Too high! Try again.", 0x0D, 0x0A, 0
ngg_low     db "Too low! Try again.", 0x0D, 0x0A, 0
ngg_correct db "Congratulations! You got it in ", 0
ngg_guesses db " guesses!", 0x0D, 0x0A, 0
ngg_invalid db "Invalid input. Please enter a number between 1 and 100.", 0x0D, 0x0A, 0
ngg_play_again db "Play again? (Y/N): ", 0
piano_msg   db "Piano Mode - Press A-K keys to play notes, ESC to exit", 0x0D, 0x0A
            db "Note: Using PC speaker - sound quality is limited to square waves", 0x0D, 0x0A, 0
paint_x      dw 40    ; Current X position
paint_y      dw 12    ; Current Y position
paint_char   db '*'   ; Current drawing character
paint_color  db 0x07  ; Current color (white on black)
paint_help   db "ES-Paint Controls:", 0x0D, 0x0A
             db "Arrow keys - Move cursor", 0x0D, 0x0A
             db "Space     - Toggle drawing", 0x0D, 0x0A
             db "1-9       - Change colors", 0x0D, 0x0A
             db "S         - Change symbol", 0x0D, 0x0A
             db "C         - Clear screen", 0x0D, 0x0A
             db "ESC       - Exit", 0x0D, 0x0A, 0
paint_symbols db "* +#@$%&=.", 0
timer_usage    db "Usage: TIMER seconds", 0x0D, 0x0A, 0
countdown_msg  db "Time remaining: ", 0
seconds_msg   db " seconds", 0x0D, 0x0A, 0
alarm_msg     db "Time's up!", 0x0D, 0x0A, 0
pong_msg    db "Pong - Player 1: W/S, Player 2: Up/Down, +/- Speed, ESC to exit", 0x0D, 0x0A, 0
p1_score_msg db "Player 1: ", 0
p2_score_msg db "Player 2: ", 0
speed_msg    db "Speed: ", 0
easter_egg1 db "AND NOW, THE MOMENT YOU'VE ALL BEEN WAITING FOR,", 0
easter_egg2 db "WE PROUDLY PRESENT FOR YOUR VIEWING PLEASURE:", 0
easter_egg3 db "THE EXPLORESOFT EXPLOREOS 0.45 PRODUCT TEAM!", 0
boot_usage_msg      db "Usage: BOOT [A|B|C|D|ROM]", 0x0D, 0x0A, 0
boot_error_msg      db "Error: Unable to boot from specified drive", 0x0D, 0x0A, 0
boot_rom_error_msg  db "Error: ROM boot failed", 0x0D, 0x0A, 0
boot_rom_msg        db "Booting from ROM...", 0x0D, 0x0A, 0
boot_drive_msg      db "Booting from drive ", 0
boot_dots_msg       db "...", 0x0D, 0x0A, 0
invalid_hex_msg db "Invalid hexadecimal address", 0x0D, 0x0A, 0
too_many_digits_msg db "Error: Address must be exactly 4 hexadecimal digits (XXXX)", 0x0D, 0x0A, 0
invalid_length_msg db "Error: Address must be exactly 4 hexadecimal digits (XXXX)", 0x0D, 0x0A, 0
hexdump_usage_msg db "Usage: HEXDUMP XXXX (where XXXX is a hexadecimal address)", 0x0D, 0x0A, 0
not_aligned_msg db "Error: Address's last digit must be 0", 0x0D, 0x0A, 0
colon_space db ": ", 0
separator db " | ", 0
music_generated_msg db "Music generated.", 0x0D, 0x0A, 0
ttt_welcome      db "Welcome to Tic-Tac-Toe!", 0x0D, 0x0A
                 db "You are X, computer is O", 0x0D, 0x0A
                 db "Enter position (1-9):", 0x0D, 0x0A, 0
ttt_player_prompt db "Your move (1-9): ", 0
ttt_computer_turn db "Computer's turn...", 0x0D, 0x0A, 0
ttt_row_div      db "---+---+---", 0x0D, 0x0A, 0
ttt_invalid_msg  db "Invalid move! Try again.", 0x0D, 0x0A, 0
ttt_player_win_msg  db "You win! Congratulations!", 0x0D, 0x0A, 0
ttt_computer_win_msg db "Computer wins! Better luck next time.", 0x0D, 0x0A, 0
ttt_draw_msg     db "It's a draw! Game over.", 0x0D, 0x0A, 0
ttt_computer_turn_msg db "Computer thinking...", 0x0D, 0x0A, 0
help_msg    db "Available Commands:", 0x0D, 0x0A
            db "System: CLEAR VER RESTART HALT BOOT", 0x0D, 0x0A
            db "Info:   BIOS DATE TIME BATTERY MEMTEST MEMINFO HISTORY HELP LICENSE", 0x0D, 0x0A
            db "Memory: MALLOC FREE DEFRAG HEXDUMP", 0x0D, 0x0A
            db "Tools:  ECHO BEEP CALC EDIT SCREEN RAND PAINT TIMER MORSE GENMSC", 0x0D, 0x0A
            db "Games:  SNAKE NGG PIANO PONG TTT", 0x0D, 0x0A
            db 0x0D, 0x0A
            db "Type 'HELP command' for detailed information about a specific command", 0x0D, 0x0A, 0
help_not_found db " - No help available for this command or command doesn't exist.", 0x0D, 0x0A, 0
help_ver     db "VER - Displays the current version of ExploreOS", 0x0D, 0x0A
             db "Usage: VER", 0x0D, 0x0A, 0
help_help    db "HELP - Lists the available commands or shows the usages and descriptions of the specified command.", 0x0D, 0x0A
             db "Usage: HELP command", 0x0D, 0x0A, 0
help_clear   db "CLEAR - Clears the screen", 0x0D, 0x0A
             db "Usage: CLEAR", 0x0D, 0x0A, 0
help_restart db "RESTART - Reboots the system", 0x0D, 0x0A
             db "Usage: RESTART", 0x0D, 0x0A, 0
help_rand    db "RAND - Generates a random number from 0 to 9", 0x0D, 0x0A
             db "Usage: RAND", 0x0D, 0x0A, 0
help_halt    db "HALT - Stops the system", 0x0D, 0x0A
             db "Usage: HALT", 0x0D, 0x0A, 0
help_echo    db "ECHO - Displays a message", 0x0D, 0x0A
             db "Usage: ECHO message", 0x0D, 0x0A, 0
help_beep    db "BEEP - Plays a short beep sound", 0x0D, 0x0A
             db "Usage: BEEP", 0x0D, 0x0A, 0
help_bios    db "BIOS - Shows BIOS date information", 0x0D, 0x0A
             db "Usage: BIOS", 0x0D, 0x0A, 0
help_date    db "DATE - Displays current date from RTC", 0x0D, 0x0A
             db "Usage: DATE", 0x0D, 0x0A, 0
help_time    db "TIME - Displays current time from RTC", 0x0D, 0x0A
             db "Usage: TIME", 0x0D, 0x0A, 0
help_battery db "BATTERY - Shows battery level if available", 0x0D, 0x0A
             db "Usage: BATTERY", 0x0D, 0x0A, 0
help_memtest db "MEMTEST - Displays available base & extended memory (Can detect up to 65535KB)", 0x0D, 0x0A
             db "Usage: MEMTEST", 0x0D, 0x0A, 0
help_meminfo db "MEMINFO - Shows memory allocation information", 0x0D, 0x0A
             db "Usage: MEMINFO", 0x0D, 0x0A, 0
help_malloc  db "MALLOC - Allocates a block of memory", 0x0D, 0x0A
             db "Usage: MALLOC", 0x0D, 0x0A, 0
help_free    db "FREE - Frees previously allocated memory", 0x0D, 0x0A
             db "Usage: FREE", 0x0D, 0x0A, 0
help_calc    db "CALC - Calculate a basic math problem (Can calculate up to 2147483647)", 0x0D, 0x0A
             db "Usage: CALC number operator number", 0x0D, 0x0A
             db "Operators: + - * /", 0x0D, 0x0A, 0
help_edit    db "EDIT - Opens a text editor", 0x0D, 0x0A
             db "Usage: EDIT", 0x0D, 0x0A
             db "Controls: ESC to exit, Backspace to delete and Enter to make a new line", 0x0D, 0x0A, 0
help_screen  db "SCREEN - Plays a bouncing ball screen saver", 0x0D, 0x0A
             db "Usage: SCREEN", 0x0D, 0x0A
             db "Controls: Any key to exit", 0x0D, 0x0A, 0
help_snake   db "SNAKE - Opens a snake game", 0x0D, 0x0A
             db "Usage: SNAKE", 0x0D, 0x0A
             db "Controls: Arrow keys to move, ESC to exit", 0x0D, 0x0A, 0
help_ngg     db "NGG - Opens a number guessing game", 0x0D, 0x0A
             db "Usage: NGG", 0x0D, 0x0A
help_piano   db "PIANO - Opens a piano app", 0x0D, 0x0A
             db "Usage: PIANO", 0x0D, 0x0A
             db "Controls: A-K keys for notes, ESC to exit", 0x0D, 0x0A, 0
help_paint   db "PAINT - Opens a paint app", 0x0D, 0x0A
             db "Usage: PAINT", 0x0D, 0x0A
             db "Controls: 1-9 to select colors, C to clear, S to change symbol, Space to toggle drawing, Arrow keys to move the cursor, ESC to exit", 0x0D, 0x0A, 0
help_timer   db "TIMER - Displays a countdown timer", 0x0D, 0x0A
             db "Usage: TIMER sec", 0x0D, 0x0A, 0
help_pong    db "PONG - Play a 2 player pong game", 0x0D, 0x0A
             db "Usage: PONG", 0x0D, 0x0A
             db "Controls: P1 (W/S), P2 (UP/DOWN)", 0x0D, 0x0A, 0
help_morse   db "MORSE - Converts text to morse code audio", 0x0D, 0x0A
             db "Usage: MORSE text", 0x0D, 0x0A, 0
help_boot    db "BOOT - Boots from a specified drive", 0x0D, 0x0A
             db "Usage: BOOT [A|B|C|D|ROM]", 0x0D, 0x0A, 0
help_history db "HISTORY - Shows command history", 0x0D, 0x0A
             db "Usage: HISTORY", 0x0D, 0x0A, 0
help_defrag  db "DEFRAG - Defragments the memory", 0x0D, 0x0A
             db "Usage: DEFRAG", 0x0D, 0x0A, 0
help_hexdump db "HEXDUMP - Displays a memory dump", 0x0D, 0x0A
             db "Usage: HEXDUMP XXXX", 0x0D, 0x0A, 0
help_genmsc  db "GENMSC - Generates a random music (it can generate a nearly infinite amount of musics)", 0x0D, 0x0A
             db "Usage: GENMSC", 0x0D, 0x0A, 0
help_ttt     db "TTT - Opens a game of tic-tac-toe", 0x0D, 0x0A
             db "Usage: TTT", 0x0D, 0x0A, 0
help_license db "LICENSE - Displays copyright and license information", 0x0D, 0x0A
             db "Usage: LICENSE", 0x0D, 0x0A, 0

; Variables
prng_state dd 0x12345678   ; Seed for random number generation
input_buffer times 77 db 0 ; Buffer for user input
last_alloc dw 0   ; Store last allocated memory address
total_blocks dw 0
used_blocks  dw 0
free_blocks  dw 0
total_memory dw 0
used_memory  dw 0
free_memory  dw 0
largest_free dw 0
ball_x  db 40    ; Ball X position
ball_y  db 12    ; Ball Y position
dx_dir  db 1     ; X direction
dy_dir  db 1     ; Y direction
SNAKE_LENGTH_MAX equ 100
GAME_WIDTH equ 80
GAME_HEIGHT equ 25
INITIAL_SPEED equ 1500
RIGHT equ 0
DOWN  equ 1
LEFT  equ 2
UP    equ 3
paint_mode   db 0     ; 0 = move only, 1 = drawing
easter_egg_step db 0 
credits_end     db 0
credit_ptrs dw credit1, credit2, credit3, credit4, credit5, credit6, credit7, credit8, 0
defrag_count dw 0
total_notes db 0
current_note db 0
boot_screen_state    db BOOT_SCREEN_INACTIVE
progress_counter     db 0
timer_counter     dw 0
old_timer_vector  dd 0        ; Original timer interrupt (4 bytes: offset + segment)
status_buffer     times 81 db 0  ; Buffer for status bar (80 chars + null)
saved_cursor_row  db 0
saved_cursor_col  db 0
status_bar_enabled db 1
ttt_board times 9 db 0  ; Game board (3x3)
ttt_corners db 0, 2, 6, 8   ; Corner positions

section .bss:
cursor_x         resw 1
cursor_y         resw 1
buffer_pos       resw 1
text_buffer      resb MAX_BUFFER_SIZE
scroll_offset    resw 1      ; Track scroll position
total_lines      resw 1      ; Track total number of lines
snake_x times SNAKE_LENGTH_MAX dw 0
snake_y times SNAKE_LENGTH_MAX dw 0
snake_length dw 0
snake_direction dw 0  ; 0=right, 1=down, 2=left, 3=up
food_x dw 0
food_y dw 0
score dw 0
game_speed dw 0
ngg_target      resw 1    ; Target number to guess
ngg_num_guesses resw 1    ; Number of guesses made
history_buffer   resb HISTORY_SIZE * HISTORY_CMD_SIZE  ; Circular buffer for commands
history_count    resw 1    ; Number of commands in history
history_position resw 1    ; Current position in history while navigating
history_index    resw 1    ; Index for adding new commands
current_input    resb 76  ; Store current input when navigating history
ball_pos_x   resw 1    ; Ball X position
ball_pos_y   resw 1    ; Ball Y position
ball_vel_x   resw 1    ; Ball X velocity
ball_vel_y   resw 1    ; Ball Y velocity
p1_pos       resw 1    ; Player 1 paddle position
p2_pos       resw 1    ; Player 2 paddle position
p1_score     resw 1    ; Player 1 score
p2_score     resw 1    ; Player 2 score
pong_speed   resw 1    ; Game speed (lower = faster)
