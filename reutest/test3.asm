                processor 6502

; zero page locations
zp_ptr          = $fb
dst_ptr         = $fd
zp_tmp          = $02

; constants
dma_step        = $10

; kernal routines
kern_getin      = $ffe4
kern_plot       = $fff0

                org $0801

                ; basic stub
                .byte $0b,$08,$e9,$07
                .byte $9e,$32,$30,$36
                .byte $31,$00,$00,$00

start
                lda #$0e
                sta $d020
                lda #$06
                sta $d021
                jsr init_altscreen
                jsr set_text_mode
                jsr init_textout
                ldy #$00
                jsr textout

                ; set pointer for pokehex
                lda #$a0
                sta dst_ptr
                lda #$04
                sta dst_ptr+1

                ; fill 4000-7fff with 55aa
                sei
                lda #$00
                sta zp_ptr
                tay
                lda #$40
                sta zp_ptr+1
                tax
mem_init_55aa   lda #$55
                sta (zp_ptr),y
                iny
                lda #$aa
                sta (zp_ptr),y
                iny
                bne mem_init_55aa
mem_init_next   inc zp_ptr+1
                dex
                bne mem_init_55aa

                ; fill all 8 reu banks (256k) with 55aa pattern
                ;-----------------------------------------------
mem_init_done   
                lda #$10
                sta zp_tmp

                jsr init_reu_addr_tbl
                jsr set_reu_addr

do_next_xfer    ldx #$00
                ldy #$40
                jsr set_c64_addr
                ldx #$00
                ldy #$40
                jsr set_xfer_len

                ; copy $4000-$7fff to reu (16k of 55aa)
                lda #$90
                sta $df01

                dec zp_tmp
                bne do_next_xfer

                lda fill_byte
                sta zp_tmp
                jsr clear_mem

                lda #$00
                sta pass_num
                sta pass_num+1

restart_test
                jsr print_pass_num

                lda #$20
                sta block_count
                jsr init_reu_addr_tbl
                jsr set_reu_addr
                lda #$01
                sta $df07
                lda #$00
                sta $df08

do_next_block
                jsr print_reu_addr
                lda #$00
                sta zp_ptr
                sta $df02
                lda #$20
                sta zp_ptr+1
                sta $df03
                ldy #$00
                ldx fill_byte

                ; transfer one byte at a time from reu to c64 and verify
.next_page
.next_word      lda #$91
                sta $df01
                lda (zp_ptr),y
                cmp #$55
                bne do_error
                txa
                sta (zp_ptr),y
                iny

                lda #$91
                sta $df01
                lda (zp_ptr),y
                cmp #$aa
                bne do_error
                txa
                sta (zp_ptr),y
                iny

                bne .next_word
                ;jsr clear_block
                ;cpy #$00
                ;bne .next_xfer
                inc zp_ptr+1
                lda zp_ptr+1
                cmp #$40
                bne .next_page

                lda $dc01
                lsr
                bcs .check_2
                jsr set_text_mode
                jmp .check_block
.check_2        and #$04
                bne .check_block
                jsr set_bitmap_mode

.check_block    dec block_count
                bne do_next_block
                
                inc pass_num
                bne .skip_pass_hi
                inc pass_num+1
.skip_pass_hi    jmp restart_test

                cli
                rts

do_error
                sty zp_ptr
                lda $de00               ; pulse IO1 to trigger analyzer
                lda #$02
                sta $d020
                jsr save_fail_page
                ; print fail addr
                ldy #$5c
                lda zp_ptr+1
                jsr pokehex
                lda zp_ptr
                jsr pokehex
                ; print =
                lda #$3d
                sta (dst_ptr),y
                iny
                sty zp_tmp
                ; print fail byte
                ldy #$00
                lda (zp_ptr),y
                ldy zp_tmp
                jsr pokehex
                cli
                lda zp_ptr
                pha
                lda zp_ptr+1
                pha
                jsr init_textout
                ldy #prompttxt-msgtxt
                jsr textout
                pla
                sta zp_ptr+1
                pla
                sta zp_ptr
do_refresh      jsr display_page
wait_key        jsr kern_getin
                beq wait_key
                cmp #$03
                beq exit
                cmp #$5f
                beq exit
                cmp #$20
                beq restart
                cmp #$85
                beq do_page_up
                cmp #$86
                beq do_page_down
                cmp #$88
                beq do_refresh
                cmp #$31
                bne check_2
                jsr set_text_mode
                jmp wait_key
check_2         cmp #$32
                bne check_3
                jsr set_bitmap_mode
                jmp wait_key
check_3         cmp #$33
                bne wait_key
                ldy #$00
                lda (zp_ptr),y
                eor #$ff
                sta (zp_ptr),y
                jmp wait_key
restart         jmp start

exit            
                ldx #$14
                ldy #$00
                clc
                jsr kern_plot
                rts
                
do_page_up
                dec reu_dst_addr+1
                lda reu_dst_addr+1
                cmp #$ff
                bne skip_dec_hi
                dec reu_dst_addr+2
skip_dec_hi     jmp do_refresh

do_page_down
                inc reu_dst_addr+1
                bne skip_inc_hi
                inc reu_dst_addr+2
skip_inc_hi     jmp do_refresh



;----------------------------------------------------------
set_text_mode   lda #$1b
                sta $d011
                lda #$14
                sta $d018
                rts

set_bitmap_mode lda #$3b
                sta $d011
                lda #$38
                sta $d018
                rts


;----------------------------------------------------------
set_c64_addr
                ; set c64 address
                stx $df02
                sty $df03
                rts


;----------------------------------------------------------
set_reu_addr
                ; set reu address
                subroutine
                ldx #$02
.set_addr       lda reu_dst_addr,x
                sta $df04,x
                dex
                bpl .set_addr
                rts


;----------------------------------------------------------
save_fail_page
                subroutine
                lda $df04
                sec
                sbc #$10
                and #$00
                sta reu_dst_addr
                lda $df05
                sbc #$00
                sta reu_dst_addr+1
                lda $df06
                sbc #$00
                and #$03
                sta reu_dst_addr+2
                rts


;----------------------------------------------------------
set_xfer_len
                ; set transfer length
                stx $df07
                sty $df08
                rts


;----------------------------------------------------------
clear_mem
                subroutine
                ldx #$00
                stx zp_ptr
                ldx #$10
                stx zp_ptr+1
                ldx #$90
                ldy #$00
.clear          sta (zp_ptr),y
                iny
                bne .clear
                inc zp_ptr+1
                dex
                bne .clear
                rts


; ;----------------------------------------------------------
; clear_dst
;                 subroutine
;                 ldx #$00
;                 stx zp_ptr
;                 ldx #$10
;                 stx zp_ptr+1
;                 ldy #$00
; .clear          sta (zp_ptr),y
;                 iny
;                 bne .clear
;                 inc zp_ptr+1
;                 ldx zp_ptr+1
;                 cpx #$80
;                 beq .done
;                 cpx #$30
;                 bne .clear
;                 ldx #$60
;                 stx zp_ptr+1
;                 bne .clear
; .done           rts


;----------------------------------------------------------
; clear_block
;                 subroutine
;                 lda fill_byte
;                 ldx #$10
;                 ldy zp_tmp
; .clear          sta (zp_ptr),y
;                 iny
;                 dex
;                 bne .clear
;                 rts


;----------------------------------------------------------
init_reu_addr_tbl
                subroutine
                ; init reu address table
                lda #$00
                ldx #$02
.init_tbl       sta reu_dst_addr,x
                dex
                bpl .init_tbl
                rts


;----------------------------------------------------------
init_altscreen
                subroutine
                ldy #$00
                lda #$20
.clrscr         sta $0c00,y
                sta $0d00,y
                sta $0e00,y
                sta $0f00,y
                iny
                bne .clrscr
                rts


;----------------------------------------------------------
init_textout
                subroutine
                lda #<msgtxt
                sta zp_ptr
                lda #>msgtxt
                sta zp_ptr+1
                rts


;----------------------------------------------------------
textout
         lda (zp_ptr),y
         beq done
         jsr $ffd2
         iny
         bne textout
done     rts


;----------------------------------------------------------
msgtxt
                .byte $93, $9a
                .byte "CBM EXPANSION RAM TEST"
                .byte $0d, $0d
                .byte "TEST #3"
                .byte $0d, $0d
                .byte "PASS      :"
                .byte $0d
                .byte "REU  ADDR :"
                .byte $0d
                .byte "FAIL ADDR :"
                .byte $0d
                .byte $00

prompttxt
                .byte $0d
                .byte "PRESS SPACE TO RESTART TEST"
                .byte $0d
                .byte "F1/F3 TO DISPLAY PREV/NEXT PAGE"
                .byte $0d
                .byte "F7 TO REFRESH.  STOP TO EXIT."
                .byte $00


print_reu_addr
;---------------------------------------
                subroutine
                ldy #$34
                lda $df06
                and #$07
                jsr pokehex
                lda $df05
                jsr pokehex
                lda $df04
                jsr pokehex
                rts


print_pass_num
;---------------------------------------
                subroutine
                ldy #$0c
                lda pass_num+1
                jsr pokehex
                lda pass_num
                jsr pokehex
                rts


;---------------------------------------
pokehex
                subroutine
                pha
                lsr
                lsr
                lsr
                lsr
                jsr .hexchar
                sta (dst_ptr),y
                pla
                and #$0f
.hexchar        ora #$30
                cmp #$3a
                bcc .skiphex
                sbc #$39
.skiphex        sta (dst_ptr),y
                iny
                rts


;---------------------------------------
display_page
                subroutine
                dec $d020
                lda #$e0
                sta $df02
                lda #$05
                sta $df03
                jsr set_reu_addr
                jsr print_reu_addr
                ldx #$00
                stx $df07
                inx
                stx $df08
                ; copy one page from REU to screen
                lda #$91
                sta $df01
                inc $d020
                rts


;----------------------------------------------------------
reu_dst_addr
                .byte $00,$00,$00

block_count     .byte $00
pass_num        .byte $00,$00
fill_byte       .byte $ff
