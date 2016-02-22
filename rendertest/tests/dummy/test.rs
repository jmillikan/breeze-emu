Test {
    frames: 42,
    description: "dummy test producing a black screen, tests test harness sanity",
    preparation: &[],
    data_file: Some("data.hex"),
    code: &assemble6502! {
        lda #DATA_ADDRESS_BANK
        ldx #DATA_ADDRESS_LO
        ldy #DATA_ADDRESS_HI
    end:
        jmp end
    },
}
