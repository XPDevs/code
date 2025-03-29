#!/bin/bash

# Prompt the user to choose a directory
echo "Please enter the path of the directory where you want to create the folders:"
read directory

# Check if the directory exists
if [ -d "$directory" ]; then
  # Create the core directory structure first
  echo "Creating core directory structure..."
mkdir -p "$directory/core/end/halt" "$directory/core/end/reboot" \
           "$directory/core/startup/memtest" "$directory/core/startup" "$directory/core/version"

  # Now create the isolinux folder
  echo "Creating isolinux folder..."
  mkdir -p "$directory/isolinux"  # Ensure isolinux folder exists

  # List of raw URLs to download from the GitHub repository
  files=(
    "https://raw.githubusercontent.com/XPDevs/code/main/core/isolinux/isolinux.bin"
    "https://raw.githubusercontent.com/XPDevs/code/main/core/isolinux/ldlinux.c32"
  )

  # Download the files using wget, without creating extra directories
  echo "Downloading isolinux files..."
  for file in "${files[@]}"; do
    wget -N "$file" -P "$directory/isolinux"
  done

  # Navigate to the isolinux folder and create isolinux.cfg
  echo "Creating isolinux.cfg..."
  cd "$directory/isolinux" || exit  # Change directory to isolinux

  # Create the isolinux.cfg file with the specified content
  cat > isolinux.cfg <<EOF
DEFAULT kernel
DISPLAY /core/startup/core.msg

LABEL kernel
    KERNEL /core/core.bin
    INITRD /core/vmlinuz

LABEL mc
    PROMPT 1
    F1 /core/startup/core.msg
    F2 /core/startup/f2

LABEL reboot
    MENU LABEL Reboot
    kernel /core/end/reboot/reboot.bin

LABEL halt
    MENU LABEL halt
    kernel /core/end/halt/halt.bin

LABEL memtest
    MENU LABEL memtest
    kernel /core/startup/memtest/mem.efi

LABEL version-info
    MENU LABEL version-info
    kernel /core/version/version.bin

EOF
sleep 2

cd ..
cd core/startup/memtest
wget -N "https://github.com/XPDevs/code/blob/main/core/core/startup/memtest/mem.efi"
cd ..
# make core.msg
  # Create core.msg with the specified content
  echo "Creating core.msg..."

echo " Core boot manager 19.2 (C) James Turner 2009-2025

   ( '>')
  /) DC (\   Core is distributed with ABSOLUTELY NO WARRANTY.
 (/-_--_-\)    https://xpdevs.github.io/Doors-Operating-System
                       
Press <Enter> to begin or F2 to view commands." >> core.msg

sleep 2 

echo "Creating f2..."

echo " Core boot manager 19.2 (C) James Turner 2009-2025

At boot prompt enter one of the options:
Press F1 to return 

 halt           This will halt the computer
 reboot         This will reboot the computer
 memtest        This will start testing your computers memory
 version-info   This will display the operating systems version infomation" >> f2 

cd ..
cd end
cd halt

echo "Creating halt.asm..."

echo "
[org 0x7C00]        ; Bootloader starts at memory address 0x7C00
jmp start            ; Jump to start label to begin execution

start:
    ; Do any initialization here if necessary (optional)

    ; Trigger a system halt by using INT 0x10 (video services)
    mov ah, 0x00     ; Function 0x00 is to set video mode
    int 0x10         ; BIOS interrupt for video services, acts as a placeholder

    ; To effectively halt, we will enter an infinite loop
halt:
    jmp halt         ; Infinite loop to "halt" the system

TIMES 510 - (\$ - \$$) db 0   ; Fill the remaining space with zeros to make the total size 512 bytes

dw 0xAA55                   ; Bootloader signature" >> halt.asm

echo "Compiling halt.asm"
nasm -f bin halt.asm -o halt.bin
sleep 2

cd ..
cd reboot

echo "Creating reboot.asm..."

echo "
[org 0x7C00]        ; Bootloader starts at memory address 0x7C00
jmp start            ; Jump to start label to begin execution

start:
    ; Do any initialization here if necessary (optional)

    ; Trigger a software reset by calling INT 0x19
    mov ah, 0x00     ; Function 0x19 is system reset
    int 0x19         ; Call BIOS interrupt for reset

TIMES 510 - (\$ - \$$) db 0   ; Fill the remaining space with zeros to make the total size 512 bytes
dw 0xAA55                   ; Bootloader signature" >> reboot.asm

echo "Compiling reboot.asm"
nasm -f bin reboot.asm -o reboot.bin
sleep 2

cd ..
cd ..
cd version

echo "Creating version.asm"

echo "
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
Version:    db 'Version: 1.3.0', 0x0D, 0x0A, 0
BuildDate:  db 'Build Date: 22/03/2025', 0x0D, 0x0A, 0
Architecture: db 'Architecture: x86', 0x0D, 0x0A, 0
Prompt:     db 'Press <Enter> to return to boot menu...', 0x0D, 0x0A, 0

; Bootloader signature: 0xAA55
TIMES 510 - (\$ - \$$) db 0   ; Fill the remaining space with zeros to make the total size 512 bytes
dw 0xAA55                   ; Boot signature (0xAA55)" >> version.asm

echo "Compiling version.asm"
nasm -f bin version.asm -o version.bin
sleep 2

cd ..

echo "Creating core.asm"

echo "
[bits 16]
[org 0x7c00]

; ==== Initial Setup ====
mov ax, 0x0003         ; Clear screen (mode 03h)
int 0x10

; Blue background
mov ah, 0x0b
mov bh, 0x00
mov bl, 0x01
int 0x10

; Loading message
mov si, msg_loading
call print_string
call new_line

; Simulate error (loader.txt not found)
mov ax, 0    
cmp ax, 0 ; 0 = not present, 1 = present
je display_error

; If all is good, display fake loader.txt contents
mov si, msg_loader_txt
call print_string
call new_line
hlt

; ==== Error Display ====
display_error:
    ; Red background
    mov ah, 0x0b
    mov bh, 0x00
    mov bl, 0x04
    int 0x10

    ; Error messages
    mov si, msg_error_1
    call print_string
    call new_line

    mov si, msg_error_2
    call print_string
    call new_line

    mov si, msg_error_3
    call print_string
    call new_line
    call new_line

    ; Prompt to restart
    mov si, msg_restart
    call print_string

.wait_key:
    xor ah, ah
    int 0x16
    cmp al, 'Y'
    je restart
    cmp al, 'y'
    je restart
    cmp al, 'N'
    je shutdown
    cmp al, 'n'
    je shutdown
    jmp .wait_key

; ==== Restart Path ====
restart:
    mov si, msg_restarting
    call print_string
    call delay

    ; Reset screen and registers
    mov ax, 0x0003
    int 0x10

    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx
    xor si, si
    xor di, di
    xor bp, bp
    xor sp, sp

    int 0x19       ; BIOS bootstrap (reboots)

; ==== Shutdown Path ====
shutdown:
    mov si, msg_shutdown
    call print_string
    call delay
    jmp $

; ==== Subroutines ====
print_string:
    lodsb
    or al, al
    jz .done
    mov ah, 0x0e
    int 0x10
    jmp print_string
.done:
    ret

new_line:
    mov ah, 0x0e
    mov al, 0x0D
    int 0x10
    mov al, 0x0A
    int 0x10
    ret

delay:         ; Simple delay using loop
    mov cx, 0xffff
.delay_loop:
    loop .delay_loop
    ret

; ==== Messages ====
msg_loading     db 'Core is loading the GUI...', 0
msg_loader_txt  db 'Contents of loader.txt: Simulated file content.', 0

msg_error_1     db 'Oops! Something went wrong while starting the system.', 0
msg_error_2     db 'Error Code: 0x01 - Could not find the GUI file.', 0
msg_error_3     db 'Please restart your computer and try again.', 0
msg_restart     db 'Would you like to restart the computer? (Y/N): ', 0

msg_restarting  db 0x0D, 0x0A, 'Restarting the system...', 0
msg_shutdown    db 0x0D, 0x0A, 'Shutting down the system...', 0

; ==== Boot Signature ====
TIMES 510 - (\$ - \$$) db 0
dw 0xaa55" >> core.asm

echo "Compiling core.asm"
nasm -f bin core.asm -o core.bin
sleep 2

echo "Creating iso tool..."

cd ..

echo "#!/bin/bash

# Function to display error messages
show_error() {
    echo '[ERROR] \$1'
}

# Function to display informational messages
show_message() {
    echo '[INFO] \$1'
}

# Function to display loading bar
loading_bar() {
    echo -n '['
    for i in \$(seq 1 20); do
        echo -n '='
        sleep 0.2
    done
    echo ']'
}

# Function to create the bootable ISO
create_iso() {
    USER_DESKTOP=\$(xdg-user-dir DESKTOP)

    if [ -z \"\$USER_DESKTOP\" ]; then
        show_error 'Unable to find the Desktop directory.'
        return 1
    fi

    # Delete all ISO files on the Desktop
    echo 'Deleting all ISO files on the Desktop...'
    find \"\$USER_DESKTOP\" -type f -name \"*.iso\" -exec rm -f {} \\;
    if [ \$? -eq 0 ]; then
        show_message 'All ISO files on the Desktop have been deleted.'
    else
        show_error 'Failed to delete ISO files on the Desktop.'
        return 1
    fi

    # Ask the user for the ISO name
    OUTPUT_ISO_NAME='Core'
    OUTPUT_ISO=\"\$OUTPUT_ISO_NAME.iso\"

    if [ ! -f 'isolinux/isolinux.bin' ]; then
        show_error 'isolinux/isolinux.bin not found.'
        return 1
    fi

    # Create the ISO image
    show_message 'Creating bootable ISO...'
    loading_bar  # Display loading bar during ISO creation

    genisoimage -o \"\$OUTPUT_ISO\" \
        -b isolinux/isolinux.bin \
        -c isolinux/boot.cat \
        -no-emul-boot \
        -boot-load-size 4 \
        -boot-info-table \
        -R -J -V 'BootableISO' .

    if [ \$? -eq 0 ]; then
        show_message 'Bootable ISO created successfully: \$OUTPUT_ISO'
    else
        show_error 'Failed to create ISO.'
        return 1
    fi

    cp \"\$OUTPUT_ISO\" \"\$USER_DESKTOP\"
    show_message 'ISO copied to the Desktop: \$USER_DESKTOP/\$OUTPUT_ISO'

    rm \"\$OUTPUT_ISO\"

    # Using Zenity for the Yes/No dialog
    if zenity --question --text='Do you want to run the ISO in QEMU?'; then
        if ! command -v qemu-system-x86_64 &> /dev/null; then
            show_error 'QEMU is not installed. Please install QEMU and try again.'
            return 1
        fi
        echo 'QEMU has started the emulation'
        qemu-system-x86_64 -cdrom \"\$USER_DESKTOP/\$OUTPUT_ISO\" -boot d
    else
        show_message 'ISO creation complete. You can manually run the ISO from the Desktop.'
    fi
}

# Main execution of the script
echo 'Starting ISO creation process...'
create_iso" >> iso

echo "Making iso tool executable..."
chmod +x iso
fi 
