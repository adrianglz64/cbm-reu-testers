
; Test program to check REU DMA timing before and after the VICII takes over
; the bus.  Transfers data from the REU to the c64 during sprite data fetch.

                processor 6502

startlinev = $60
spritexv = 96
spriteyv = 37
;spriteyv = 106

dmadelay = $52
dmaop    = $53
spritex  = $59
spritey  = $5b
startline = $60
rasterln = $61
prevkey  = $62
shift    = $63
doupdate = $64
repdelay = $65
storea   = $66
minbranch       = $67
maxbranch       = $68

delay1v  = $18
delay2v  = $02
cycledelayv     = $3a

tptr     = $fb
rastcnt  = $fd
storex   = $fe
npflag   = $14

destaddr = $d006

reuaddr  = $00
reubank  = $00

;dmacount = $0c
maxdmalen       = $09

         org $0801

         .byte $0b,$08,$e6,$07
         .byte $9e,$32,$30,$36
         .byte $31,$00,$00,$00

start
         sei
         jsr detectnp
         jsr initscreen
         jsr spriteinit
         sei
         
         tsx
         stx storex

         jsr reuzpinit
         jsr updatedmalen

         lda #<nmi
         sta $fffa
         sta $fffc
         lda #>nmi
         sta $fffb
         sta $fffd

         lda #$7f
         sta $dc0d
         lda $dc0d

         ; set up key scan
         lda #$ff
         sta prevkey
         lda #delay1v
         sta repdelay

         lda #$35
         sta $01

         jsr setup_raster_timer

         lda #<raster1
         sta $fffe
         lda #>raster1
         sta $ffff


;          lda #$1b
;          sta $d011

; rwait    bit $d011
;          bpl rwait

          jsr datatoreu
          lda #$20
          sta $df02
          lda #$d0
          sta $df03
;          lda #startlinev
;          sta startline
;          jsr rastreset

                ldx #$ff
                stx minbranch
                inx
                stx maxbranch

                lda #$30
                sta $d012

         lda #$01
         sta $d01a
         sta $d019

         cli
endless
         inc $0334
         dec $0334
         ;inc $0334
         jmp endless

         cli
         rts


raster1
                sta storea
                lda dmadelay
                sbc $dd04
                sta .adjdelay+1
.adjdelay       bpl .dodelay

.dodelay        cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c5
                nop

                ldx #$b1
                stx $df01
                ;stx $d020
                ldx #$0e
                stx $d020

                sta $0424
;                 cmp minbranch
;                 bcs .skipmin
;                 sta minbranch
;                 sta $0426
; .skipmin        cmp maxbranch
;                 bcc .skipmax
;                 sta maxbranch
;                 sta $0427
; .skipmax

                lda dmadelay
                sbc $dd04
                sta .adjdelay2+1
.adjdelay2      bpl .dodelay2

.dodelay2       cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c9
                cmp #$c5
                nop

                ldx #$b1
                stx $df01
                sta $0425

                lda #$01
                sta $d019

                jsr keyscan
                jsr updatescreen
                jsr updatedmalen

                lda storea
                rti


;---------------------------------------
keyscan
         lda #$00
         sta shift
         sta doupdate
         lda #%10111111
         sta $dc00
         lda $dc01
         and #%00010000
         bne noshift1
         dec shift
noshift1 lda #%11111101
         sta $dc00
         lda $dc01
         and #%10000000
         bne noshift2
         dec shift
noshift2 lda #%11111110
         sta $dc00
         
         lda $dc01
         cmp #$ff
         bne repkey
         sta prevkey
         lda #delay1v
         sta repdelay
         beq repkey
.kdone   jmp keydone

repkey   cmp prevkey
         sta prevkey
         bne diffkey
         dec repdelay
         bne .kdone
         ldx #delay2v
         .byte $2c
diffkey  ldx #delay1v
         stx repdelay

keychks  tax

         ; cursor down
         and #%10000000
         bne chkcrsrt
         lda spritey
         jsr incdec
         sta spritey
         inc doupdate

chkcrsrt
         txa
         ; cursor right
         and #%00000100
         bne checkf1
         ldy #$00
         bit shift
         bpl adjxpos
         dey
         tya
         .byte $2c
adjxpos  lda #$01
         clc
         adc spritex
         sta spritex
         tya
         adc spritex+1
         sta spritex+1
         inc doupdate
         
checkf1  
         txa
         and #%00010000
         bne checkf3
         bit shift
         bpl decdelay
         lda $52
         cmp #$ff
         beq keydone
         inc $52
         bne delaydone
decdelay lda $52
         cmp #$00
         beq keydone
         dec $52
delaydone
         inc doupdate

checkf3
         txa
         and #%00100000
         bne checkf5
         bit shift
         bmi declen
         lda $57
         cmp #$09
         beq keydone
         inc $57
         bne lendone
declen   lda $57
         cmp #$01
         beq keydone
         dec $57
lendone  inc doupdate

checkf5
         txa
         and #%01000000
         bne keydone
         lda dmaop
         bit shift
         bmi decop
         cmp #$03
         beq keydone
         inc dmaop
         bne opdone
decop    cmp #$00
         beq keydone
         dec dmaop
opdone   inc doupdate

keydone
        ;  lda doupdate
        ;  beq scandone
        ;  jsr setpritexy
scandone
         rts


incdec
         clc
         bit shift
         bmi incr
         adc #$01
         .byte $2c
incr     adc #$ff
         rts
         

;---------------------------------------
updatescreen
         subroutine
         lda #$50
         sta tptr
         lda #$04
         sta tptr+1
         ldy #$12
         lda $52
         jsr pokehex
         lda $57
         ldy #$3a
         jsr pokehex
         lda dmaop
         asl
         asl
         asl
         clc
         adc #msgdmaop-msgtxt
         tax
         ldy #$61
         jsr pokestr
         
        ;  lda startline
        ;  ldy #$62
        ;  jsr printhex
;          lda #$18
;          sta tptr
;          lda #$05
;          sta tptr+1
;          ldx #$00
; nextbyte ldy #$05
;          lda $d000,x
;          jsr printhex
;          lda tptr
;          clc
;          adc #$28
;          sta tptr
;          bcc skiphi
;          inc tptr+1
; skiphi   inx
;          cpx #$10
;          bne nextbyte
         rts


;---------------------------------------
updatedmalen
         subroutine
         lda $57
         sta $df07
         sec
         lda #maxdmalen
         sbc $57
         sta $df04
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
         sta (tptr),y
         pla
         and #$0f
.hexchar  ora #$30
         cmp #$3a
         bcc .skiphex
         sbc #$39
.skiphex  sta (tptr),y
         iny
         rts


;---------------------------------------
pokestr
                subroutine
.pokemore       lda msgtxt,x
                beq .pokedone
                cmp #$40
                bcc .dontfix
                eor #$40
.dontfix        sta (tptr),y
                inx
                iny
                bne .pokemore
.pokedone       rts


;---------------------------------------
; rastreset
;          lda startline
;          sta $d012
;          sta rasterln

;          lda #dmacount
;          sta rastcnt

;          ;jsr reuzpinit
;          rts


;---------------------------------------
reuzpinit
         subroutine
         lda #cycledelayv
         sta dmadelay
         lda #$01
         sta dmaop
        ;  lda #<destaddr
        ;  sta $52
        ;  lda #>destaddr
        ;  sta $53
        ;  lda #<reuaddr
        ;  sta $54
        ;  lda #>reuaddr
        ;  sta $55
        ;  lda #reubank
        ;  sta $56
          lda #<maxdmalen
          sta $57
          lda #>maxdmalen
          sta $58
         rts


;---------------------------------------
initcolor
         ldy #$00
         lda #$01
clrcolor sta $d800,y
         sta $d900,y
         sta $da00,y
         sta $db00,y
         iny
         bne clrcolor
         rts


;---------------------------------------
initscreen
         subroutine
         lda #$00
         sta $d021
         lda #$93
         jsr $ffd2
         lda #<msgtxt
         sta tptr
         lda #>msgtxt
         sta tptr+1

         ldy #msgpal-msgtxt
         bit npflag
         bvc .donpmsg
         ldy #msgntsc-msgtxt
.donpmsg  jsr textout

         ldy #$00
         jsr textout

;          ldy #$0f
;          lda #$30
;          sta tptr
; .printaddr
;          lda #$44
;          jsr $ffd2
;          lda #$30
;          jsr $ffd2
;          jsr $ffd2
;          lda tptr
;          jsr $ffd2
;          lda #$0d
;          jsr $ffd2
;          inc tptr
;          lda tptr
;          cmp #$3a
;          bne .skiphex
;          lda #$41
;          sta tptr 
; .skiphex  dey
;          bpl .printaddr

         jsr initcolor
         rts

textout
         lda (tptr),y
         beq done
         jsr $ffd2
         iny
         bne textout
done     rts

msgtxt
         .byte $0d,$11
         .byte "(F1/F2) DELAY  : $"
         .byte $0d
         .byte "(F3/F4) DMA LEN: $"
         .byte $0d
         .byte "(F5/F6) DMA OP : "
         .byte $0d
         ;.byte "(CRSR)  SPRITE X/Y"
         ;.byte $0d
         .byte $11,$00
         ;.byte $11,$11,$11,$11,$a3
         ;.byte $13,$00

msgpal
         .byte "PAL"
         .byte $00
msgntsc
         .byte "NTSC"
         .byte $00

msgdmaop
                .byte "64->REU"
                .byte $00
                .byte "REU->64"
                .byte $00
                .byte "64<>REU"
                .byte $00
                .byte "64==REU"
                .byte $00

;---------------------------------------
spriteinit
         subroutine
         lda #spriteyv
         sta spritey
         ;sta buffer
         lda #spritexv
         sta spritex
         lda #$00
         sta spritex+1
         jsr setpritexy
         lda #$0f
         sta $d015
         lda #$00
         sta $d017
         lda #$44
         ldy #$00
.sprdat   sta $3e00,y
         sta $3f00,y
         iny
         bne .sprdat
         lda #$bc                       ; set the vic idle read byte to something we
         sta $3fff                      ; can easily identify with the logic analyzer
         lda #$f8
         clc
.sprptr   sta $07f8,y
         iny
         adc #$01
         bcc .sprptr
         sta tptr
         lda #$3e
         sta tptr+1
         sei
         lda #$33
         sta $01
         ldx #$00
.nextchr  ldy #$28
.chardat  lda $d180,x
         sta (tptr),y
         iny
         iny
         iny
         inx
         txa
         and #$07
         bne .chardat
         lda tptr
         clc
         adc #$40
         sta tptr
         bcc .skipptr
         inc tptr+1
.skipptr  cpx #$40
         bne .nextchr

         lda #$37
         sta $01
         cli
         rts


;---------------------------------------
setpritexy
         subroutine
         lda spritey
         ldy #$00
.ypos     sta $d001,y
         iny
         iny
         cpy #$10
         bne .ypos
         lda #$00
         sta tptr
         sta tptr+1
         jsr setspritex
         rts


;---------------------------------------
setspritex
         subroutine
         lda spritex+1
         sta tptr
         ldy #$00
         lda spritex
.xpos     sta $d000,y
         tax
         lda tptr
         lsr
         ror tptr+1
         txa
         clc
         adc #$18
         bcc .skiphi
         inc tptr
.skiphi   iny
         iny
         cpy #$10
         bne .xpos
         lda tptr+1
         sta $d010
         rts


;---------------------------------------
datatoreu
         lda #<buffer
         sta $df02
         lda #>buffer
         sta $df03
         lda #<reuaddr
         sta $df04
         lda #>reuaddr
         sta $df05
         lda #reubank
         sta $df06

         lda #<maxdmalen
         sta $df07
         lda #>maxdmalen
         sta $df08

         lda #$00
         sta $df0a
         lda #$b0
         sta $df01

         lda #$80
         sta $df0a
         rts


;---------------------------------------
detectnp
         subroutine
         bit $d011
         bmi detectnp
.wait2    bit $d011
         bpl .wait2
         lda #$40
         sta npflag
.wait3    bit $d011
         bpl .npdone
         lda $d012
         cmp #$0a
         bcc .wait3
         lda #$00
         sta npflag
.npdone   rts


; Set up timer A on CIA B to count the number of cycles remaining on the
; current scanline.  Wish the VICII could just tell us.
;------------------------------------------------------------------------
setup_raster_timer
                subroutine
                lda #$9b
                sta $d011
.again          sei
                lda #$00
.rwait1         cmp $d012
                bne .rwait1
                bit $d011
                bpl .rwait1
.rwait2         cmp $d012
                beq .rwait2
                lda #$02
                sta $d012
                lda #$01
                sta $d019
                sta $d01a
                lda #<.tmpirq
                sta $fffe
                lda #>.tmpirq
                sta $ffff
                cli
                inc $d020
                dec $d020
                lsr $02
                tsx
                nop
                nop
                nop
                nop
                nop
                nop
                nop
                nop
                nop
                nop
                nop
                nop
                nop
                ;jmp .again
                jmp *

.tmpirq         ; jitter reduced to one cycle of jitter at this point
                inc $d020
                dec $d020
                lda #$01
                sta $d019
                bit npflag
                bvs .ntsc1
.ntsc1          bvs .ntsc2
.ntsc2
                txs                     ; 2
                bit $02                 ; 3
                lsr $02                 ; 5
                lsr $02                 ; 5
                lsr $02                 ; 5
                lsr $02                 ; 5

                lda $d012
                cmp #$02
                beq .delay1
.delay1         ; fully stable here.  Now we can set up the timer
                sta $0422
                
                bit npflag
                bvc .palval
                lda #64                 ; count down from 64 on ntsc
                bvs .ntscval
.palval         lda #62                 ; count down from 62 on pal
.ntscval        sta $dd04
                lda #$00
                sta $dd05
                bvc .pal1
.pal1           bvc .pal2
.pal2           lda $ff00               ; 4

                bit $02
                lsr $02
                lda #$01
                sta $d020
                lda #$0e
                sta $d020
                nop
                lda #$11                ; lda #$13 to generate a pulse on PB6
                sta $dd0e               ; start timer A right at the end of the scanline

;DEBUG           = 1

            IFNCONST DEBUG
                sei
                lda #$1b
                sta $d011
                lda #$00
                sta $d01a
                rts
            ELSE
                lsr $02
                lsr $02                 ; 10
                lsr $02
                lsr $02                 ; 20
                lsr $02
                lsr $02                 ; 30
                lsr $02
                lsr $02                 ; 40
                lsr $02
                lsr $02                 ; 50
                lsr $02
                lsr $02                 ; 60
                ;nop
                bit $02
                lda $dd04
                sta $0401
                ;nop
                jmp .again
            ENDIF



;---------------------------------------
nmi
         ;inc $d020
         lda #$37
         sta $01

         lda #$00
         sta $d015

         lda #$81
         sta $dc0d
         ldx #$00
         stx $d01a
         inx
         stx $d019
         ldx storex
         txs
         cli
         rts


;---------------------------------------
buffer
         .byte $02,$13,$24,$35
         .byte $46,$57,$68,$79,$8e
