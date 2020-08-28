.proc tmp12
  cmp     #'0'
  bcc    skip
  cmp     #':'

  .byt $90,$35
skip
  stx   $0f
  tax
.endproc
  
  .byt $30,$2e
   sta $c1
   pla
   tax
   pla
   pha
   cpx #$0e
	.byt $d0,$04
  
  .byt $c9,$c9,$f0,$09,$e0,$8a,$d0,$18,$c9
	.byt $ca,$d0,$14,$24,$18,$6e,$fc,$04,$a0,$ff,$c8,$b1,$e9,$f0,$11,$c9
	.byt $3a,$f0,$0d,$c9,$d4,$d0,$f3,$8a,$48,$a5,$c1,$a6,$0f,$4c,$b9,$ec
	.byt $68,$20,$e9,$04,$20,$67,$04,$0e,$fc,$04,$b0,$03,$4c,$c1,$c8,$6e
	.byt $52,$02,$60,$20,$77,$04,$b1,$16,$4c,$77,$04,$a9,$45,$a0,$d8,$d0
	.byt $0a,$a9,$8e,$a0,$f8,$d0,$04,$a9,$ae,$a0,$d3,$8d,$f0,$04,$8c,$f1
	.byt $04,$20,$77,$04,$20,$ef,$04,$08
  
  pha
  sei

.ifdef SEDORIC_SD  
; This is for bank switching
  lda     $321
LD47D  
  eor     #$06                  ; Atmos bank
  sta     V2DRA                           
  pla                                     
  plp                                     
  rts
  
  nop                           ; keep these nops to align page 4 and keep compat 
  nop                           ; keep these nops to align page 4
  nop                           ; keep these nops to align page 4
.else  
  lda     $04fb
  eor     #$02
  sta     $04fb
  sta     $314
  pla
  plp
  rts
.endif  
  .byt $2c,$0d,$03,$50,$0f,$48,$a9,$04
	.byt $2d,$6a,$02,$f0,$03,$ee,$74,$02,$68,$4c,$22,$ee,$68,$68,$85,$f2
	.byt $68,$aa,$a9,$36,$a0,$d1,$d0,$c3,$20,$f2,$04,$68,$40,$8d,$14,$03
	.byt $6c,$fc,$ff,$18,$20,$77,$04,$48,$a9,$04,$48,$a9,$a8,$48,$08,$b0
	.byt $03,$4c,$44,$02,$20,$b8,$f8,$a9,$17,$a0,$ec,$20,$6b,$04,$4c,$71
	.byt $c4,$a9,$04,$48,$a9,$f1,$48,$8a,$48,$98,$48,$20,$f2,$04,$4c,$06
	.byt $d3,$ea,$ea,$ea,$ea,$ea,$ea,$ea,$ea,$4c,$87,$04,$4c,$71,$04,$4c
	.byt $00,$00,$4c,$77,$04,$4c,$b3,$04,$4c,$b4,$04,$84,$00,$00,$00,$00

