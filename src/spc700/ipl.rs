//! Contains the IPL ROM (Initial Program Load) that is mapped into the upper 64 Bytes of the
//! SPC700's address space. The IPL ROM will perform some basic setup and then wait for the main
//! CPU to transfer a program, which is then executed.

pub static IPL_ROM: [u8; 64] = [
    // NOTE: mov operands are `dest, source`

    // Set up stack pointer at $01ef
    0xcd, 0xef,         // FFC0  mov x, #$ef
    0xbd,               // FFC2  mov sp, x
    // Fill memory at $00-$ef with $00 (also sets all registers to 0)
    0xe8, 0x00,         // FFC3  mov a, #$00
    0xc6,               // FFC5  mov (x), a       :fill_zero_page
    0x1d,               // FFC6  dec x
    0xd0, 0xfc,         // FFC7  bne $ffc5        -> fill_zero_page

    // Write 0xaabb to ports 0 and 1 (registers $f4 and $f5)
    // This is the "ready" signal. The main CPU will wait until it sees it.
    0x8f, 0xaa, 0xf4,   // FFC9  mov $f4, #$aa
    0x8f, 0xbb, 0xf5,   // FFCC  mov $f5, #$bb

    // Wait until $cc is written to port 0 (reg $f4)
    0x78, 0xcc, 0xf4,   // FFCF  cmp $f4, #$cc    :wait_start
    0xd0, 0xfb,         // FFD2  bne $ffcf        -> wait_start

    0x2f, 0x19,         // FFD4  bra $ffef        -> start
    //---------------

    // Wait until a non-zero value is in reg 0
    0xeb, 0xf4,         // FFD6  mov y, $f4       :recv
    0xd0, 0xfc,         // FFD8  bne $ffd6        -> recv

    0x7e, 0xf4,         // FFDA  cmp y, $f4       :loop
    0xd0, 0x0b,         // FFDC  bne $ffe9        -> lbl1
    0xe4, 0xf5,         // FFDE  mov a, $f5
    0xcb, 0xf4,         // FFE0  mov $f4, y
    0xd7, 0x00,         // FFE2  mov [$00]+y, a
    0xfc,               // FFE4  inc y
    0xd0, 0xf3,         // FFE5  bne $ffda        -> loop
    0xab, 0x01,         // FFE7  inc $01
    0x10, 0xef,         // FFE9  bpl $ffda        :lbl1   -> loop
    0x7e, 0xf4,         // FFEB  cmp y, $f4
    0x10, 0xeb,         // FFED  bpl $ffda        -> loop

    // Load reg 2 ($f6) into y and reg 3 ($f7) into a
    0xba, 0xf6,         // FFEF  movw ya, $f6     :start
    // Write Y and A in memory at $00 and $01
    0xda, 0x00,         // FFF1  movw $00, ya
    // Load reg 0 ($f4) into y, reg 1 ($f5) into a
    0xba, 0xf4,         // FFF3  movw ya, $f4
    // Write reg 1's value to reg 0
    0xc4, 0xf4,         // FFF5  mov $f4, a
    0xdd,               // FFF7  mov a, y
    0x5d,               // FFF8  mov x, a
    // If reg 1's value is not 0, start the transmission loop
    0xd0, 0xdb,         // FFF9  bne $ffd6 (-37)  -> recv

    // We're done, jump to the address we just received and stored in $0000 (x is 0 here)
    0x1f, 0x00, 0x00,   // FFFB  jmp [$0000+x]

    // reset vector is at 0xfffe and points to the start of the IPL ROM: 0xffc0
    0xc0, 0xff,
];
