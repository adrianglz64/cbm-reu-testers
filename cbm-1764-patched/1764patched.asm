                    processor 6502
                    
                    include "c64_io_sym.asm"

zp_dma_data         = $50
zp_ptr              = $51
zp_fill_ptr         = $53
zp_test_num         = $55
zp_hexw_lo_lonyb    = $56
zp_hexw_lo_hinyb    = $57
zp_hexw_hi_lonyb    = $58
zp_hexw_hi_hinyb    = $59
zp_fill_end         = $5a
zp_dma_failed_byte  = $5c
hex_lo_char         = $5d
hex2_lo_char        = $5e
hex_hi_char         = $5f
hex2_hi_char        = $60
zp_hexb_lonyb       = $61
zp_hexb_hinyb       = $62
zp_reu_bank_tmp     = $63
zp_data_table_offs  = $64
zp_screen_line_num  = $65
zp_reu_bank         = $66
zp_test1_failed     = $67
zp_test2_failed     = $68
zp_test3_4_failed   = $69
zp_test7_failed     = $6b
zp_test8_failed     = $6c
zp_test9_failed     = $6d
zp_testa_failed     = $6e
reu_addr_tmp_lo     = $6f
reu_addr_tmp_hi     = $70
zp_reu_fail_lo      = $71
zp_reu_fail_hi      = $72
zp_xfer_count       = $73
zp_temp_int         = $76
zp_fill_byte_index  = $78
                    
                    org $8000

start               sei
                    lda #$06
                    sta d021_vBackgCol0
                    jsr init_color_mem
                    jsr init_screen_mem
                    ldx #$00
L800e               lda txt_exp_ram_test,x
                    sta $0400,x
                    inx
                    cpx #$28
                    bne L800e
                    ldx #$00
L801b               lda txt_top_header,x
                    sta $0450,x
                    inx
                    cpx #$28
                    bne L801b
                    ldy #$00
                    lda #$20
                    ; point all sprites to $0800
init_sprite_ptrs    sta $07f8,y
                    iny
                    cpy #$08
                    bne init_sprite_ptrs
                    lda #$ff
                    ldx #$3e
init_sprite_data    sta $0800,x
                    dex
                    bpl init_sprite_data
                    ; init sprite colors, expansion, etc
                    lda #$00
                    sta d017_vSprExpandY
                    sta d01d_vSprExpandX
                    sta d01b_vSprPriority
                    lda #$ff
                    sta d025_vSprMCMCol0
                    lda #$00
                    sta d026_vSprMCMCol1
                    lda #$00
                    sta d027_vSpr0Col
                    lda #$01
                    sta d028_vSpr1Col
                    lda #$02
                    sta d029_vSpr2Col
                    lda #$03
                    sta d02a_vSpr3Col
                    lda #$05
                    sta d02b_vSpr4Col
                    lda #$07
                    sta d02c_vSpr5Col
                    lda #$08
                    sta d02d_vSpr6Col
                    lda #$09
                    sta d02e_vSpr7Col
                    ; set all sprites at Y coord $e0
                    lda #$e0
                    sta d001_vSprite0Y
                    sta d003_vSprite1Y
                    sta d005_vSprite2Y
                    sta d007_vSprite3Y
                    sta d009_vSprite4Y
                    sta d00b_vSprite5Y
                    sta d00d_vSprite6Y
                    sta d00f_vSprite7Y
                    ; manually set sprite x coords starting at x = $26, incrementing $26 each time
                    lda #$26
                    sta d000_vSprite0X
                    lda #$4c
                    sta d002_vSprite1X
                    lda #$72
                    sta d004_vSprite2X
                    lda #$98
                    sta d006_vSprite3X
                    lda #$be
                    sta d008_vSprite4X
                    lda #$e4
                    sta d00a_vSprite5X
                    lda #$0a
                    sta d00c_vSprite6X
                    lda #$30
                    sta d00e_vSprite7X
                    lda #$c0
                    sta d010_vSpriteXMSB
                    lda #$ff
                    sta d015_vSprEnable
                    jmp L80c8
                    
                    ; Test 1: fill each 64k reu bank with $00, $55, $aa, $ff, using dma compare to verify each operation
                    ; ==================================================================================================
L80c8               lda #$01
                    sta zp_test_num
                    lda #$05
                    sta zp_screen_line_num
                    ldx #$00
                    stx zp_test1_failed
                    ldy #$00
                    sty zp_reu_bank
L80d8               sty reu_ram_bank_num
                    stx zp_data_table_offs
                    sty zp_reu_bank_tmp
                    lda dma_data_table,x
                    sta zp_dma_data
                    ; c64 source addr $0050
                    ldx #<zp_dma_data
                    ldy #>zp_dma_data
                    jsr set_c64_addr
                    ; reu dest addr $0000
                    ldx #$00
                    ldy #$00
                    jsr set_reu_addr
                    ; 64k transfer length
                    ldx #$00
                    ldy #$00
                    jsr set_reu_xfer_len
                    ldx #$00
                    ; fixed c64 address
                    ldy #$80
                    ; fill one entire reu bank with byte at $0050
                    lda #$90
                    jsr do_reu_xfer
                    ; check transfer completed bit.  Not necessary
L8102               lda reu_status_reg
                    and #$40
                    beq L8102
                    ldy zp_reu_bank_tmp
                    sty reu_ram_bank_num
                    ldx #$00
                    ldy #$00
                    jsr set_reu_addr
                    ldx #$00
                    ldy #$00
                    jsr set_reu_xfer_len
                    ldx #$00
                    ; fixed c64 address
                    ldy #$80
                    ; use dma compare to check reu bank
                    lda #$93
                    jsr do_reu_xfer
L8125               lda reu_status_reg
                    tay
                    and #$20
                    bne L8135
                    tya
                    and #$40
                    beq L8125
                    jmp L813d
                    
                    ; compare mismatch
L8135               inc zp_test1_failed
                    ; test failed.  This jsr does not return
                    jsr display_test_fail
                    jmp L8140
                    
                    ; bank tested ok, advance to next bank
L813d               jsr display_test_pass
L8140               ldy zp_reu_bank_tmp
                    ldx zp_data_table_offs
                    cpy #$03
                    beq test1_bank_done
                    inc zp_reu_bank
                    lda #$11
                    sta zp_xfer_count
                    inc zp_screen_line_num
                    iny
                    jmp L80d8
                    
                    ; set up for next test byte
test1_bank_done     ldx zp_data_table_offs
                    inx
                    ldy #$00
                    sty zp_reu_bank
                    cpx #$04
                    beq start_test2
                    lda #$05
                    sta zp_screen_line_num
                    jmp L80d8
                    
                    ; Test 2: fill $4000-7fff with $55 $aa, then use that to fill all reu banks and verify them with dma compare
                    ; ==========================================================================================================
start_test2         lda #$02
                    sta zp_test_num
                    lda #$01
                    sta zp_dma_data
                    lda #$05
                    sta zp_screen_line_num
                    ldx #$00
                    ldy #$00
                    stx zp_test2_failed
                    sty zp_reu_bank
                    stx zp_data_table_offs
                    sty zp_reu_bank_tmp
                    sty reu_ram_bank_num

                    ; fill 4000-7fff with 55aa
                    lda #$00
                    sta zp_ptr
                    tay
                    lda #$40
                    sta zp_ptr+1
                    tax
mem_init_55aa       lda #$55
                    sta (zp_ptr),y
                    iny
                    lda #$aa
                    sta (zp_ptr),y
                    iny
                    bne mem_init_55aa
                    
mem_init_next_page  inc zp_ptr+1
                    dex
                    bne mem_init_55aa
                    
                    ; fill all 8 reu banks (256k) with 55aa pattern
mem_init_done       ldx #$00
                    ldy #$00
                    jsr set_reu_addr
L81b4               ldx #$00
                    ldy #$40
                    jsr set_c64_addr
                    ldx #$00
                    ldy #$40
                    jsr set_reu_xfer_len
                    ldx #$00
                    ldy #$00
                    lda #$90
                    ; copy $4000-$7fff to reu (16k of 55aa)
                    jsr do_reu_xfer
L81cb               lda reu_status_reg
                    and #$40
                    beq L81cb
                    inc zp_dma_data
                    lda zp_dma_data
                    ; count to $11.  16K * 16 transfers = 256k
                    cmp zp_xfer_count
                    bne L81b4
                    
                    ; compare reu memory 16k at a time with c64 memory at $4000
                    lda #$01
                    sta zp_dma_data
                    lda #$00
                    sta reu_ram_bank_num
                    ldx #$00
                    ldy #$00
                    jsr set_reu_addr
L81ea               ldx #$00
                    ldy #$40
                    jsr set_c64_addr
                    ldx #$00
                    ldy #$40
                    jsr set_reu_xfer_len
                    ldx #$00
                    ldy #$00
                    lda #$93
                    ; do dma compare
                    jsr do_reu_xfer
L8201               lda reu_status_reg
                    tay
                    and #$20
                    bne test2_mismatch
                    tya
                    and #$40
                    beq L8201
                    jsr display_test_pass
                    jmp L8219
                    
test2_mismatch      inc zp_test2_failed
                    jsr display_test_fail
L8219               lda reu_ram_bank_num
                    and #$07
                    cmp zp_reu_bank
                    beq L8228
                    inc zp_reu_bank
                    inc zp_reu_bank_tmp
                    inc zp_screen_line_num
L8228               inc zp_dma_data
                    lda zp_dma_data
                    cmp zp_xfer_count
                    bne L81ea
                    jmp L8233
                    
                    ; Test 3/4: transfer from reu to c64, check for memory corruption, and verify transferred data
                    ; ==========================================================================================================
L8233               lda #$03
                    sta zp_test_num
                    sta zp_fill_byte_index
                    lda #$00
                    sta zp_test3_4_failed
                    ; test 4 starts here with data 00
L823d               lda #$01
                    sta zp_dma_data
                    lda #$05
                    sta zp_screen_line_num
                    ldy #$00
                    ldx #$00
                    sty zp_reu_bank_tmp
                    stx zp_data_table_offs
                    sty zp_reu_bank
                    sty reu_ram_bank_num
                    
_test_3_4_next_bank
                    ; fill memory areas in table with fill byte ($ff in test 3, $00 in test 4)
                    ldx zp_fill_byte_index
                    lda dma_data_table,x
                    sta zp_temp_int

                    lda #$00
                    sta zp_fill_ptr
                    tax
L8252               
                    ldy mem_fill_start_tbl,x
                    lda mem_fill_end_tbl,x
                    sta zp_fill_end
                    inx
                    lda mem_fill_start_tbl,x
                    sta zp_fill_ptr+1
                    lda mem_fill_end_tbl,x
                    sta zp_fill_end+1
                    inx
                    
_fill_next_page     lda zp_fill_ptr+1
                    cmp zp_fill_end+1
                    beq fill_last_part
                    
                    lda zp_temp_int
_fill_first         sta (zp_fill_ptr),y
                    iny
                    bne _fill_first
                    inc zp_fill_ptr+1
                    bne _fill_next_page

fill_last_part
                    lda zp_temp_int
_fill_last          sta (zp_fill_ptr),y
                    cpy zp_fill_end
                    beq _fill_area_done
                    iny
                    bne _fill_last
                    
                    
_fill_area_done     ; repeat until 5 memory areas are filled (10 bytes of pointers)
                    cpx #$0a
                    bne L8252
                    
                    ; transfer $2000 bytes from current REU bank to $1000 and $6000
                    ldx #$00
                    stx zp_data_table_offs
                    ldx #$00
                    ldy #$10
                    jsr set_c64_addr
L82a4               ldx #$00
                    ldy #$00
                    jsr set_reu_addr
                    ldx #$00
                    ldy #$20
                    jsr set_reu_xfer_len
                    ldx #$00
                    ldy #$00
                    lda #$91
                    jsr do_reu_xfer
L82bb               lda reu_status_reg
                    and #$40
                    beq L82bb
                    inc zp_data_table_offs
                    ldx zp_data_table_offs
                    cpx #$02
                    beq compare_prefilled
                    ldx #$00
                    ldy #$60
                    jsr set_c64_addr
                    jmp L82a4
                    
                    ; check previously filled areas to verify no bytes have been corrupted outside of the dma transfer ranges
compare_prefilled
                    lda #$00
                    sta zp_fill_ptr
                    tax

_compare_next_area
                    ldy mem_check_start_tbl,x
                    lda mem_check_end_tbl,x
                    sta zp_fill_end
                    inx
                    lda mem_check_start_tbl,x
                    sta zp_fill_ptr+1
                    lda mem_check_end_tbl,x
                    sta zp_fill_end+1
                    inx
                    
_compare_next_page  lda zp_fill_ptr+1
                    cmp zp_fill_end+1
                    beq compare_last_part
                    
                    lda zp_temp_int
_compare_first      cmp (zp_fill_ptr),y
                    bne _test_3_4_failed
                    iny
                    bne _compare_first
                    inc zp_fill_ptr+1
                    bne _compare_next_page

compare_last_part
                    lda zp_temp_int
_compare_last       cmp (zp_fill_ptr),y
                    bne _test_3_4_failed
                    cpy zp_fill_end
                    beq _compare_area_done
                    iny
                    bne _compare_last

_compare_area_done        
                    ; repeat for all 6 areas (12 bytes of pointers)
                    cpx #$0c
                    bne _compare_next_area
                    beq compare_dma_data

_test_3_4_failed
                    sty zp_fill_ptr
                    inc zp_test3_4_failed
                    jmp display_test_fail

compare_dma_data
                    ; check dma transfer data.  $1000-$2fff and $6000-7fff should be filled with $55 $aa.  Check $1000-$2fff first
                    lda #$00
                    sta zp_fill_ptr
                    lda #$10
                    sta zp_fill_ptr + 1
                    ldy #$00
_compare_dma_next   ldx #$20
_compare_dma        lda (zp_fill_ptr),y
                    cmp #$55
                    bne _test_3_4_failed
                    iny
                    lda (zp_fill_ptr),y
                    cmp #$aa
                    bne _test_3_4_failed
                    iny
                    bne _compare_dma
                    inc zp_fill_ptr+1
                    dex
                    bne _compare_dma
                    
                    lda zp_fill_ptr+1
                    cmp #$80
                    beq compare_done
                    lda #$60
                    sta zp_fill_ptr+1
                    bne _compare_dma_next
                    
compare_done
                    jsr display_test_pass
                    ; advance to next reu bank
L83d8               lda reu_ram_bank_num
                    and #$07
                    clc
                    adc #$01
                    sta reu_ram_bank_num
                    sta zp_reu_bank
                    sta zp_reu_bank_tmp
                    adc #$05
                    sta zp_screen_line_num
                    ldx #$00
                    stx zp_data_table_offs
                    ; increment by 4 (4 x 16K = 64K)
                    inc zp_dma_data
                    lda zp_dma_data
                    clc
                    adc #$03
                    sta zp_dma_data
                    ; loop until all 4 banks tested (256K)
                    cmp zp_xfer_count
                    beq L83ff
                    jmp _test_3_4_next_bank
                    
L83ff               lda zp_fill_byte_index
                    cmp #$00
                    beq start_test_7
                    lda #$00
                    sta zp_fill_byte_index
                    lda #$04
                    ; set up for test 4
                    sta zp_test_num
                    jmp L823d
                    
                    ; Test 7: register autoload test.  Transfer $1000 bytes from c64 to reu and check registers were restored properly.
                    ; =================================================================================================================
start_test_7        lda #$07
                    sta zp_test_num
                    lda #$00
                    sta zp_test7_failed
                    lda #$10
                    sta zp_screen_line_num
                    ldy #$08
                    sty zp_reu_bank_tmp
                    ldx #$00
                    ldy #$00
                    stx zp_data_table_offs
                    sty zp_reu_bank
                    sty reu_ram_bank_num
                    ldx #$00
                    ; display bottom test header
L842d               lda txt_bottom_header,x
                    sta $0608,x
                    inx
                    cpx #$28
                    bne L842d
                    ; source c64 addr $0000
                    ldx #$00
                    ldy #$00
                    jsr set_c64_addr
                    ; dest reu addr $0000
                    ldx #$00
                    ldy #$00
                    jsr set_reu_addr
                    ; length $1000 bytes
                    ldx #$00
                    ldy #$10
                    jsr set_reu_xfer_len
                    ; transfer to reu with register autoload
                    ldx #$00
                    ldy #$00
                    lda #$b0
                    jsr do_reu_xfer
L8456               lda reu_status_reg
                    and #$40
                    beq L8456
                    nop
                    ; check that address/bank registers were reloaded to $00, and length to $1000
                    lda reu_c64_addr_lo
                    bne L8488
                    lda reu_c64_addr_hi
                    bne L8488
                    lda reu_ram_addr_lo
                    bne L8488
                    lda reu_ram_addr_hi
                    bne L8488
                    lda reu_xfer_len_lo
                    bne L8488
                    lda reu_xfer_len_hi
                    cmp #$10
                    bne L8488
                    lda reu_ram_bank_num
                    and #$07
                    bne L8488
                    jmp L8496
                    
L8488               inc zp_test7_failed
                    jsr print_fail_txt
                    jsr print_test_num
                    jsr print_test7_addr
                    jsr wait_key
L8496               jsr print_pass_txt
                    jsr print_test_num
                    jsr print_test7_addr
                    jmp start_test_8
                    
print_test7_addr    ldy reu_ram_addr_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_ram_addr_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$6a
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    ; print reu address
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    ldy reu_c64_addr_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_c64_addr_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$70
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    ; print c64 address
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    ldy reu_xfer_len_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_xfer_len_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$76
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    ; print dma length
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    lda reu_ram_bank_num
                    and #$07
                    tay
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    lda #$7b
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    ; print bank
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    rts
                    
                    ; Test 8: fixed reu address test.  Fill c64 memory with a single byte from the reu
                    ; =================================================================================================================
start_test_8        lda #$08
                    sta zp_test_num
                    lda #$00
                    sta zp_test8_failed
                    lda #$09
                    sta zp_reu_bank_tmp
                    lda #$11
                    sta zp_screen_line_num
                    ldx #$00
                    ldy #$00
                    stx zp_data_table_offs
                    sty zp_reu_bank
                    sty reu_ram_bank_num
                    ldx #$00
                    ldy #$30
                    jsr set_c64_addr
                    ldx #$00
                    ldy #$00
                    jsr set_reu_addr
                    ldx #$00
                    ldy #$04
                    jsr set_reu_xfer_len
                    ldx #$00
                    ldy #$40
                    lda #$b1
                    ; fill $3000-$33ff with reu byte at $0 0000
                    jsr do_reu_xfer
L858f               lda reu_status_reg
                    and #$40
                    beq L858f
                    lda $3000
                    sta zp_fill_byte_index
                    ldy #$00
L859d               lda $3000,y
                    cmp zp_fill_byte_index
                    bne test_8_mismatch
                    lda $3100,y
                    cmp zp_fill_byte_index
                    bne test_8_mismatch
                    lda $3200,y
                    cmp zp_fill_byte_index
                    bne test_8_mismatch
                    lda $3300,y
                    cmp zp_fill_byte_index
                    bne test_8_mismatch
                    iny
                    cpy #$00
                    bne L859d
                    jmp test_8_passed
                    
test_8_mismatch     inc zp_test8_failed
                    jsr print_fail_txt
                    jsr print_test_num
                    jsr print_test_8_addr
                    jsr wait_key
test_8_passed       jsr print_pass_txt
                    jsr print_test_num
                    jsr print_test_8_addr
                    jmp start_test_9
                    
print_test_8_addr   ldy reu_ram_addr_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_ram_addr_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$92
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    ; print reu address
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    ldy reu_c64_addr_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_c64_addr_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$98
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    ; print c64 address
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    ldy reu_xfer_len_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_xfer_len_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$9e
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    ; print dma length
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    lda reu_ram_bank_num
                    and #$07
                    tay
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    lda #$a3
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    ; print bank
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    rts
                    
                    ; Test 9: $ff00 decode test.  Test succeeds if transfer doesn't happen.
                    ; =================================================================================================================
start_test_9        lda #$09
                    sta zp_test_num
                    lda #$00
                    sta zp_test9_failed
                    lda #$0a
                    sta zp_reu_bank_tmp
                    lda #$12
                    sta zp_screen_line_num
                    ldx #$00
                    ldy #$00
                    stx zp_data_table_offs
                    sty zp_reu_bank
                    sty reu_ram_bank_num
                    ldx #$00
                    ldy #$30
                    jsr set_c64_addr
                    ldx #$00
                    ldy #$00
                    jsr set_reu_addr
                    ldx #$00
                    ldy #$04
                    jsr set_reu_xfer_len
                    ldx #$00
                    ldy #$40
                    lda #$a1
                    jsr do_reu_xfer
                    lda #$00
                    sta zp_dma_data
L86cc               inc zp_dma_data
                    ldy zp_dma_data
                    cpy #$00
                    beq L86f5
                    lda reu_status_reg
                    and #$40
                    beq L86cc
                    inc zp_test9_failed
                    jsr print_fail_txt
                    jsr print_test_num
                    jsr print_test_9_addr
                    jsr wait_key
                    ; Execution never reaches this point.  Starting transfer by writing to $ff00 is never actually tested
                    lda #$3e
                    sta $ff00
L86ee               lda reu_status_reg
                    and #$40
                    beq L86ee
L86f5               jsr print_pass_txt
                    jsr print_test_num
                    jsr print_test_9_addr
                    jmp start_test_a
                    
print_test_9_addr   ldy reu_ram_addr_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_ram_addr_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$ba
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    ldy reu_c64_addr_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_c64_addr_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$c0
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    ldy reu_xfer_len_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_xfer_len_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$c6
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    lda reu_ram_bank_num
                    and #$07
                    tay
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    lda #$cb
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    rts
                    
                    ; test a: test interrupt flag.  Does not check if an actual interrupt occurs.
                    ; =================================================================================================================
start_test_a        lda #$0a
                    sta zp_test_num
                    lda #$0b
                    sta zp_reu_bank_tmp
                    lda #$00
                    sta zp_testa_failed
                    lda #$13
                    sta zp_screen_line_num
                    ldx #$00
                    stx zp_data_table_offs
                    ldy #$00
                    sty zp_reu_bank
                    jsr set_c64_addr
                    jsr set_reu_addr
                    ldy #$01
                    jsr set_reu_xfer_len
                    lda reu_status_reg
                    ; enable interrupts on transfer complete and verify error
                    ldx #$e0
                    ldy #$00
                    lda #$90
                    jsr do_reu_xfer
                    lda #$00
                    sta zp_ptr
                    ; delay.  Not needed
L87e8               inc zp_ptr
                    ldx zp_ptr
                    cpx #$ff
                    beq L8803
                    ; test interrupt flag in reu status register.  Test considered passed if flag is set
                    lda reu_status_reg
                    and #$80
                    beq L87e8
                    jsr print_pass_txt
                    jsr print_test_num
                    jsr S8814
                    jmp L8811
                    
L8803               inc zp_testa_failed
                    jsr print_fail_txt
                    jsr print_test_num
                    jsr S8814
                    jsr wait_key
L8811               jmp print_test_passed
                    
S8814               ldy reu_ram_addr_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_ram_addr_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$e2
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    ldy reu_c64_addr_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_c64_addr_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$e8
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    ldy reu_xfer_len_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy reu_xfer_len_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    lda #$ee
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    lda zp_hexw_hi_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    lda reu_ram_bank_num
                    and #$07
                    tay
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    lda #$f3
                    sta zp_fill_ptr
                    lda #$06
                    sta zp_fill_ptr + 1
                    ldy #$00
                    lda zp_hexw_lo_hinyb
                    sta (zp_fill_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_fill_ptr),y
                    rts
                    
                    ; done testing
print_test_passed   nop
                    ldx #$00
L88cb               lda txt_pass_ram_exp,x
                    sta $0798,x
                    inx
                    cpx #$28
                    bne L88cb
                    
                    lda #$00
                    tax
                    tay
delay1              inx
                    bne delay1
                    iny
                    bne delay1
                    jmp start
                    
wait_key            nop
                    jmp wait_key_restart
                    
L88dd               rts
                    
display_test_pass   jsr print_pass_txt
                    jsr print_bank_num
                    jsr print_test_num
                    jsr print_data_out
                    rts
                    
display_test_fail   jsr print_fail_txt
                    jsr print_bank_num
                    jsr print_test_num
                    jsr print_data_out
                    jsr get_print_data_in
                    jsr print_reu_addr
                    jmp wait_key
                    
print_reu_addr      lda zp_test_num
                    cmp #$03
                    beq L8931
                    cmp #$04
                    beq L8931
                    cmp #$05
                    beq L8931
                    cmp #$06
                    beq L8931
                    ldx reu_ram_addr_lo
                    ldy reu_ram_addr_hi
                    jsr decrement_int_yx
                    ldy zp_reu_fail_lo
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy zp_reu_fail_hi
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
                    jmp L8943
                    
L8931               ldy zp_fill_ptr
                    jsr yreg_to_hex
                    sty zp_hexw_lo_lonyb
                    stx zp_hexw_lo_hinyb
                    ldy zp_fill_ptr + 1
                    jsr yreg_to_hex
                    sty zp_hexw_hi_lonyb
                    stx zp_hexw_hi_hinyb
L8943               lda zp_reu_bank_tmp
                    asl
                    tay
                    lda reu_addr_scrn_addr,y
                    sta zp_ptr
                    iny
                    lda reu_addr_scrn_addr,y
                    sta zp_ptr + 1
                    lda zp_hexw_hi_hinyb
                    ldy #$00
                    sta (zp_ptr),y
                    iny
                    lda zp_hexw_hi_lonyb
                    sta (zp_ptr),y
                    iny
                    lda zp_hexw_lo_hinyb
                    sta (zp_ptr),y
                    iny
                    lda zp_hexw_lo_lonyb
                    sta (zp_ptr),y
                    rts
                    
init_screen_mem     ldx #$00
_set_line_addr      lda screen_line_table,x
                    sta zp_ptr
                    inx
                    lda screen_line_table,x
                    sta zp_ptr + 1
                    ldy #$00
_clear_line         lda #$20
                    sta (zp_ptr),y
                    iny
                    cpy #$28
                    bne _clear_line
                    inx
                    cpx #$32
                    bne _set_line_addr
                    rts
                    
init_color_mem      lda #$03
                    ldx #$00
_do_color           sta $d800,x
                    sta $d900,x
                    sta $da00,x
                    sta $db00,x
                    inx
                    bne _do_color
                    rts
                    
yreg_to_hex         tya
                    tax
                    and #$0f
                    tay
                    txa
                    and #$f0
                    lsr
                    lsr
                    lsr
                    lsr
                    tax
                    cpx #$0a
                    bcs L89b3
                    txa
                    clc
                    adc #$30
                    tax
                    jmp L89b8
                    
L89b3               txa
                    sec
                    sbc #$09
                    tax
L89b8               cpy #$0a
                    bcs L89c4
                    tya
                    clc
                    adc #$30
                    tay
                    jmp L89c9
                    
L89c4               tya
                    sec
                    sbc #$09
                    tay
L89c9               rts
                    
print_test_num      lda zp_reu_bank_tmp
                    asl
                    tay
                    lda test_screen_addr,y
                    sta zp_ptr
                    iny
                    lda test_screen_addr,y
                    sta zp_ptr + 1
                    ldx zp_test_num
                    lda txt_digit_chars,x
                    ldy #$00
                    sta (zp_ptr),y
                    rts
                    
print_data_out      lda zp_test_num
                    cmp #$02
                    beq _data_out_word
                    cmp #$03
                    beq _data_out_word
                    cmp #$04
                    beq _data_out_word
                    cmp #$05
                    beq _data_out_word
                    cmp #$06
                    beq _data_out_word
                    ; print single data byte
                    ldx zp_data_table_offs
                    lda dma_data_table,x
                    tay
                    jsr yreg_to_hex
                    sty hex_lo_char
                    stx hex_hi_char
                    lda zp_reu_bank_tmp
                    asl
                    tay
                    lda single_data_addr,y
                    sta zp_ptr
                    iny
                    lda single_data_addr,y
                    sta zp_ptr + 1
                    ldy #$00
                    lda hex_hi_char
                    sta (zp_ptr),y
                    iny
                    lda hex_lo_char
                    sta (zp_ptr),y
                    rts
                    
                    ; print two data bytes
_data_out_word      ldx #$01
                    lda dma_data_table,x
                    tay
                    jsr yreg_to_hex
                    sty hex_lo_char
                    stx hex_hi_char
                    ldx #$02
                    lda dma_data_table,x
                    tay
                    jsr yreg_to_hex
                    sty hex2_lo_char
                    stx hex2_hi_char
                    lda zp_reu_bank_tmp
                    asl
                    tay
                    lda double_data_addr,y
                    sta zp_ptr
                    iny
                    lda double_data_addr,y
                    sta zp_ptr + 1
                    ldy #$00
                    lda hex_hi_char
                    sta (zp_ptr),y
                    iny
                    lda hex_lo_char
                    sta (zp_ptr),y
                    iny
                    lda hex2_hi_char
                    sta (zp_ptr),y
                    iny
                    lda hex2_lo_char
                    sta (zp_ptr),y
                    rts
                    
get_print_data_in   lda zp_test_num
                    cmp #$03
                    beq L8aaf
                    cmp #$04
                    beq L8aaf
                    cmp #$05
                    beq L8aaf
                    cmp #$06
                    beq L8aaf
                    ; get dma compare mismatch end address
                    ldx reu_ram_addr_lo
                    stx reu_addr_tmp_lo
                    ldy reu_ram_addr_hi
                    sty reu_addr_tmp_hi
                    ; subtract 1
                    jsr decrement_int_yx
                    lda zp_reu_fail_lo
                    ; load dma registers to get failed byte
                    sta reu_ram_addr_lo
                    lda zp_reu_fail_hi
                    sta reu_ram_addr_hi
                    ldx #$01
                    ldy #$00
                    jsr set_reu_xfer_len
                    lda zp_reu_bank_tmp
                    sta reu_ram_bank_num
                    ldx #$5c
                    ldy #$00
                    jsr set_c64_addr
                    ldx #$00
                    ldy #$c0
                    lda #$91
                    ; dma failed byte to $5c
                    jsr do_reu_xfer
L8aa5               lda reu_status_reg
                    and #$40
                    beq L8aa5
                    jmp L8ab6
                    
L8aaf               ldy #$00
                    lda (zp_fill_ptr),y
                    jmp L8ab8
                    
L8ab6               lda zp_dma_failed_byte
L8ab8               tay
                    jsr yreg_to_hex
                    sty zp_hexb_lonyb
                    stx zp_hexb_hinyb
                    lda zp_reu_bank_tmp
                    asl
                    tay
                    lda data_in_scrn_addr,y
                    sta zp_ptr
                    iny
                    lda data_in_scrn_addr,y
                    sta zp_ptr + 1
                    ldy #$00
                    lda zp_hexb_hinyb
                    sta (zp_ptr),y
                    iny
                    lda zp_hexb_lonyb
                    sta (zp_ptr),y
                    ; restore reu address.  broken!
                    lda reu_addr_tmp_lo
                    sta reu_ram_addr_lo
                    lda reu_addr_tmp_hi
                    ; should be sta $df05
                    sta reu_addr_tmp_hi
                    rts
                    
print_fail_txt      lda zp_screen_line_num
                    asl
                    tay
                    lda screen_line_table,y
                    sta zp_ptr
                    iny
                    lda screen_line_table,y
                    sta zp_ptr + 1
                    ldx #$00
                    ldy #$00
L8af7               lda txt_fail,x
                    sta (zp_ptr),y
                    inx
                    iny
                    cpy #$28
                    bne L8af7
                    rts
                    
print_pass_txt      lda zp_screen_line_num
                    asl
                    tay
                    lda screen_line_table,y
                    sta zp_ptr
                    iny
                    lda screen_line_table,y
                    sta zp_ptr + 1
                    ldx #$00
                    ldy #$00
L8b16               lda txt_pass_test,x
                    sta (zp_ptr),y
                    inx
                    iny
                    cpy #$28
                    bne L8b16
                    rts
                    
print_bank_num      lda zp_reu_bank
                    asl
                    tay
                    lda reu_bank_scrn_addr,y
                    sta zp_ptr
                    iny
                    lda reu_bank_scrn_addr,y
                    sta zp_ptr + 1
                    ldy zp_reu_bank_tmp
                    lda txt_digit_chars,y
                    ldy #$00
                    sta (zp_ptr),y
                    rts
                    
set_c64_addr        stx reu_c64_addr_lo
                    sty reu_c64_addr_hi
                    rts
                    
set_reu_addr        stx reu_ram_addr_lo
                    sty reu_ram_addr_hi
                    rts
                    
set_reu_xfer_len    stx reu_xfer_len_lo
                    sty reu_xfer_len_hi
                    rts
                    
do_reu_xfer         stx reu_int_mask_reg
                    sty reu_addr_ctrl_reg
                    sta reu_command_reg
                    rts
                    
decrement_int_yx    sec
                    txa
                    sbc #$01
                    sta zp_reu_fail_lo
                    tya
                    sbc #$00
                    sta zp_reu_fail_hi
                    rts
                    
increment_int_yx    clc
                    txa
                    adc #$01
                    sta zp_temp_int
                    tya
                    adc #$00
                    sta zp_temp_int + 1
                    rts

wait_key_restart    lda $dc01
                    bpl L8f0c
                    cmp #$ff
                    beq wait_key_restart
                    jmp start
                    
L8f0c               jmp ($fffc)
                    
dma_data_table      dc.b $00,$55,$aa,$ff
txt_digit_chars     dc.b $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$01
test_screen_addr    dc.w $04a2,$04ca,$04f2,$051a,$0542,$056a,$0592,$05ba,$065a,$0682,$06aa,$06d2,$06fa,$0722
reu_bank_scrn_addr  dc.w $04be,$04e6,$050e,$0536,$055e,$0586,$05ae,$05d6
                    dc.w $0676,$069e,$06c6,$06ee,$0716,$073e
data_in_scrn_addr   dc.w $04b9,$04e1,$0509,$0531,$0559,$0581,$05a9,$05d1,$0671,$0699,$06c1,$06e9,$0711,$0739
single_data_addr    dc.w $04b3,$04db,$0503,$052b,$0553,$057b,$05a3,$05cb,$066b,$0693,$06bb,$06e3,$070b,$0733
double_data_addr    dc.w $04b2,$04da,$0502,$052a,$0552,$057a,$05a2,$05ca,$066a,$0692,$06ba,$06e2,$070a,$066a
                    dc.w $0692,$06ba,$06e2,$070a,$0732
reu_addr_scrn_addr  dc.w $04c1,$04e9,$0511,$0539,$0561,$0589,$05b1,$05d9,$0679,$06a1,$06c9,$06f1,$0719,$0741
screen_line_table   dc.w $0400,$0400,$0428,$0450,$0478,$04a0,$04c8,$04f0,$0518,$0540,$0568,$0590,$05b8
                    dc.w $05e0,$0608,$0630,$0658,$0680,$06a8,$06d0,$06f8,$0720,$0748,$0770,$0798,$07c0

mem_fill_start_tbl  dc.w $0002,$0080,$0200,$0800,$c000,$e000
mem_fill_end_tbl    dc.w $004f,$0136,$03ff,$7fff,$cfff,$feff
mem_check_start_tbl dc.w $0002,$0080,$0200,$0800,$3000,$c000,$e000
mem_check_end_tbl   dc.w $004f,$0136,$03ff,$0fff,$5fff,$cfff,$feff

txt_exp_ram_test    dc.b $20,$20,$20,$20,$20,$20,$20,$20,$32,$35,$36,$0b,$20,$05,$18,$10,$01,$0e,$13,$09,$0f,$0e,$20,$12,$01,$0d,$20,$14,$05,$13,$14,$20,$20,$20,$20,$20,$20,$20,$20,$20
txt_top_header      dc.b $20,$14,$05,$13,$14,$20,$20,$10,$01,$13,$13,$2f,$06,$01,$09,$0c,$20,$20,$04,$0f,$15,$14,$20,$20,$04,$09,$0e,$20,$20,$02,$0e,$0b,$2f,$01,$04,$04,$12,$20,$20,$20
txt_pass_test       dc.b $20,$20,$31,$20,$20,$20,$20,$20,$20,$10,$01,$13,$13,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
txt_fail            dc.b $20,$20,$31,$20,$20,$20,$20,$20,$20,$06,$01,$09,$0c,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
txt_bottom_header   dc.b $20,$14,$05,$13,$14,$20,$20,$10,$01,$13,$13,$2f,$06,$01,$09,$0c,$20,$20,$12,$01,$04,$04,$20,$20,$03,$01,$04,$04,$20,$20,$02,$14,$0c,$08,$20,$02,$0b,$20,$20,$20
txt_pass_ram_exp    dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$10,$01,$13,$13,$20,$12,$01,$0d,$20,$05,$18,$10,$01,$0e,$13,$09,$0f,$0e,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

