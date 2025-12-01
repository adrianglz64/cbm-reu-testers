                processor 6502

; zero page locations
zp_ptr          = $fb
dst_ptr         = $fd
zp_tmp          = $02

; constants
dma_step        = $10

; kernal routines
kern_getin      = $ffe4


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

                lda #$00
                sta pass_num
                sta pass_num+1

restart_test
                jsr print_pass_num

                lda #$ff
                jsr clear_mem

                lda #$10
                sta block_count
                jsr set_reu_addr
                lda #$00
                sta $df08

do_next_block
                jsr print_reu_addr
                lda #$ff
                jsr clear_dst
                lda #$00
                sta zp_ptr
                sta $df02
                lda #$10
                sta zp_ptr+1
.next_half      sta $df03
                ldy #$00

                ; copy from reu to c64 (16 bytes of 55aa)
.next_xfer      ldx #dma_step
                stx $df07
                lda #$91
                sta $df01
.next_word      lda (zp_ptr),y
                cmp #$55
                bne do_error
                iny
                dex
                lda (zp_ptr),y
                cmp #$aa
                bne do_error
                iny
                dex
                bne .next_word
                cpy #$00
                bne .next_xfer
                inc zp_ptr+1
                lda zp_ptr+1
                cmp #$80
                beq .block_done
                cmp #$30
                bne .next_xfer
                ;beq do_error
                lda #$60
                sta zp_ptr+1
                bne .next_half

.block_done
                dec block_count
                bne do_next_block
                
                inc pass_num
                bne skip_pass_hi
                inc pass_num+1
skip_pass_hi    jmp restart_test

                cli
                rts

do_error
                sty zp_ptr
                lda $de00               ; pulse IO1 to trigger analyzer
                lda #$02
                sta $d020
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
                jsr init_textout
                ldy #prompttxt-msgtxt
                jsr textout
wait_key        jsr kern_getin
                beq wait_key
                cmp #$20
                bne exit
                jmp start
exit            rts
                

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


;----------------------------------------------------------
clear_dst
                subroutine
                ldx #$00
                stx zp_ptr
                ldx #$10
                stx zp_ptr+1
                ldy #$00
.clear          sta (zp_ptr),y
                iny
                bne .clear
                inc zp_ptr+1
                ldx zp_ptr+1
                cpx #$80
                beq .done
                cpx #$30
                bne .clear
                ldx #$60
                stx zp_ptr+1
                bne .clear
.done           rts


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
                .byte "ANY OTHER KEY TO EXIT"
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


;----------------------------------------------------------
reu_dst_addr
                .byte $00,$00,$00

block_count     .byte $00
pass_num        .byte $00,$00
