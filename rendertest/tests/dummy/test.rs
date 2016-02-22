Test {
    frames: 8,
    description: "dummy test producing a black screen, tests test harness sanity",
    preparation: &[],
    data_file: Some("data.hex"),
    code: &assemble6502! {{
        // All code starts at 0x8000 in bank 0, since that's the first mapped ROM page. We need to
        // tell the assembler, since it needs to offset all jumps by this.
        start: CODE_ADDRESS,
        code: {
            lda #DATA_ADDRESS_BANK
            ldx #DATA_ADDRESS_LO
            ldy #DATA_ADDRESS_HI
        end:
            jmp end
        }
    }},
}
