////////////////////////////////////////////////////////////////////////////////
//
// SPACE INVADERS
//
// G400 to start
//
//  press 1 to move left
//  press 2 to move right
//  press 8 to restart after a game has finished
//
// Keypad	any key on bottom keypad row moves left
//		and key on second from bottom keypad row moves right
//
////////////////////////////////////////////////////////////////////////////////

// Tanbug Defines
ICHAR		equ	$0001
//
VDUSTT	equ	$0200
VDUFST	equ	$0220
VDUMID	equ	$0300
LINBOT	equ	$03E0
//
SGRAPH	equ	$BFF0		// Read to set GRAPHIC mode
KBWRIT	equ	$BFF2
STEXT		equ	$BFF3		// Write to set TEXT mode
KBREAD	equ	$BFF3
POLLKB	equ	$FDFA
OUTPCR	equ	$FE73
OPCHR		equ	$FE75

////////////////////////////////////////////////////////////////////////////////

		org	$30		// Variables
score		equ	$38		// 3 bytes?
GunPos	equ	$49		// Position of gun on bottom line


		org	$0400

start		jmp	Lbl_0A30

Lbl_0403	ldy	#$00
		beq	Lbl_0409	// Unconditional branch

Lbl_0407	ldy	#$01
Lbl_0409	ldx	$40
		lda	$80,X
		lsr	A		// Get blocks column 0 or 1
		php	
		txa	
		lsr	A		// Divide by 16
		lsr	A
		lsr	A
		lsr	A
		tax			// Point to ship type 0-3	
		plp	
		bcc	Lbl_0422	// Col 0
		lda	Ships2L,X	// Col 1
		sta	$41
		lda	Ships2R,X
		bne	Lbl_042A
		
Lbl_0422	lda	Ships1L,X
		sta	$41
		lda	Ships1R,X
Lbl_042A	sta	$42
		lda	#$00
		sta	$43
		sta	$44
		ldx	$40
		lda	$C0,X
		and	#$03
		tax	
		beq	Lbl_044E
Lbl_043B	lsr	$41
		ror	$43
		lsr	$41
		ror	$43
		lsr	$42
		ror	$44
		lsr	$42
		ror	$44
		dex	
		bne	Lbl_043B
Lbl_044E	ldx	$40
		lda	$80,X
		lsr	A
		pha	
		lda	$C0,X
		lsr	A
		lsr	A
		tax	
		lda	$18,X
		sta	$45
		lda	$28,X
		sta	$46
		inx	
		lda	$18,X
		sta	$47
		lda	$28,X
		sta	$48
		pla	
		dey	
		bmi	Lbl_0491
		tay			// Erase ship from screen	
		lda	$41
		eor	#$FF
		and	($45),Y
		sta	($45),Y
		lda	$43
		eor	#$FF
		and	($47),Y
		sta	($47),Y
		iny	
		lda	$42
		eor	#$FF
		and	($45),Y
		sta	($45),Y
		lda	$44
		eor	#$FF
		and	($47),Y
		sta	($47),Y
		rts	

Lbl_0491	pha	
		ldx	$40
		lda	$52
		sec	
		sbc	$C0,X
		cmp	#$02
		bcs	Lbl_04A6
		lda	$51
		sec	
		sbc	$80,X
		cmp	#$03
		bcc	Lbl_04C9
Lbl_04A6	pla	
		tay			// Draw ship to screen	
		lda	$41
		ora	($45),Y
		sta	($45),Y
		lda	$43
		ora	($47),Y
		sta	($47),Y
		iny	
		lda	$42
		ora	($45),Y
		sta	($45),Y
		lda	$44
		ora	($47),Y
		sta	($47),Y
		lda	$C0,X
		bne	Lbl_04C8
		jmp	Lbl_0A05	// "Invasion Complete"

Lbl_04C8	rts	

Lbl_04C9	lda	#$00
		jsr	Lbl_0867
		sta	$C0,X
		tax	
		pla	
		jsr	Lbl_063A
		rts	

// Erase gun from screen
DspGun	lda	GunPos
		lsr	A		// Multiply by 2
		tax			// X = position on bottom line
		lda	#$00
		rol	A
		tay			// Y will be 0 or 1, index to shape
		lda	GunShapeL,Y
		eor	#$FF
		and	LINBOT,X
		sta	LINBOT,X	// Left part of gun to screen
		inx	
		lda	GunShapeR,Y
		eor	#$FF
		and	LINBOT,X
		sta	LINBOT,X	// Right part of gun to screen
		rts	

Lbl_04F6	lda	#10
		sta	GunPos
Lbl_04FA	lda	GunPos
		lsr	A
		tax	
		lda	#$00
		rol	A
		tay	
		lda	GunShapeL,Y
		and	LINBOT,X
		bne	Lbl_0525	// Gun already there?
		lda	GunShapeR,Y
		and	LINBOT+1,X
		bne	Lbl_0525
		lda	GunShapeL,Y
		ora	LINBOT,X
		sta	LINBOT,X	// Write gun to screen
		lda	GunShapeR,Y
		eor	LINBOT+1,X
		sta	LINBOT+1,X
		rts	

Lbl_0525	ldx	#5
Lbl_0527	ldy	#0
		lda	$59,X
		beq	Lbl_0533
Lbl_052D	dex	
		bpl	Lbl_0527
		jmp	Lbl_0A05	// Invasion complete

Lbl_0533	lda	GunPos
		lsr	A
		bcc	Lbl_0539
		iny	
Lbl_0539	sec	
		sbc	$5F,X
		bne	Lbl_054D
		lda	$65,X
		and	GunShapeL,Y
		beq	Lbl_052D
Lbl_0545	lda	#$FF
		sta	$59,X
		jsr	Lbl_078B
		rts	

Lbl_054D	cmp	#$FF
		bne	Lbl_052D
		lda	$65,X
		and	GunShapeR,Y
		bne	Lbl_0545
		beq	Lbl_052D

Lbl_055A	jsr	GraphClr	// Clear screen
		lda	#$00
		sta	$40
		lda	$4A
		sta	$4B
Lbl_0565	lda	#$08
		sta	$4C
Lbl_0569	ldx	$40
		lda	$4B
		sta	$C0,X
		lda	$4C
		sta	$80,X
		jsr	Lbl_0403
		inc	$40
		lda	$4C
		clc	
		adc	#6
		sta	$4C
		cmp	#$37
		bcc	Lbl_0569
		lda	$4B
		sec	
		sbc	#4
		sta	$4B
		lda	$40
		cmp	#$40
		bne	Lbl_0565
		jsr	Lbl_04F6
		lda	$4A
		cmp	#$25
		bcc	Lbl_05AB
		
		ldx	#$1B
Lbl_059B	ldy	#6
Lbl_059D	lda	BldgShape,Y
		sta	$03C0,X	// Draw bldgs to screen
		dex	
		bmi	Lbl_05AB
		dey	
		bpl	Lbl_059D
		bmi	Lbl_059B
Lbl_05AB	rts	

// Clear screen, mid-left down, mid-right up
GraphClr	lda	SGRAPH	// 05AC
		ldx	#$FF
		ldy	#$00
GraphClr1	adc	#$01
		nop	
		bne	GraphClr1	// Delay to see screen clear
		sta	VDUSTT,X
		sta	VDUMID,Y
		dex	
		iny	
		bne	GraphClr1
		rts	

Lbl_05C3	lda	$6E
		beq	Lbl_05C9
		dec	$6E
Lbl_05C9	inc	$40
Lbl_05CB	ldx	$40
		cpx	#$40
		beq	Lbl_0602
		lda	$C0,X
		beq	Lbl_05C9
		jsr	Lbl_0407
		ldx	$40
		inc	$80,X
		lda	$4D
		bne	Lbl_05E4
		dec	$80,X
		dec	$80,X
Lbl_05E4	lda	$4E
		beq	Lbl_05EA
		dec	$C0,X
Lbl_05EA	lda	$C0,X
		cmp	#$01
		bne	Lbl_05FB
		ldy	GunPos
		iny	
		tya	
		sec	
		sbc	$80,X
		cmp	#$03
		bcc	Lbl_05FF
Lbl_05FB	jsr	Lbl_0403
		rts	

Lbl_05FF	jmp	Lbl_0A05

Lbl_0602	lda	#$00
		sta	$4E
		sta	$40
		ldx	#$E0
		lda	$4D
		bne	Lbl_061F
Lbl_060E	lda	VDUMID,X
		ora	VDUSTT,X
		bne	Lbl_0630
		txa	
		sec	
		sbc	#$20
		tax	
		bcs	Lbl_060E
		bcc	Lbl_05CB
Lbl_061F	lda	$031F,X
		ora	$021F,X
		bne	Lbl_0630
		txa	
		sec	
		sbc	#$20
		tax	
		bcs	Lbl_061F
		bcc	Lbl_05CB
		
Lbl_0630	lda	$4D
		eor	#$01
		sta	$4D
		inc	$4E
		bne	Lbl_05CB
Lbl_063A	ldy	$55
		txa	
		bmi	Lbl_0674
		lda	($53),Y
		and	$56
		bne	Lbl_064A
Lbl_0645	lda	#$00
		sta	$52
		rts	

Lbl_064A	lda	$56
		eor	#$FF
		and	($53),Y
		sta	($53),Y
		txa	
		beq	Lbl_0645
		lda	#$00
		inc	$52
		lsr	$56
		ror	A
		lsr	$56
		ror	A
		beq	Lbl_0674
		sta	$56
		lda	$52
		cmp	#$38
		bcs	Lbl_0645
		lda	$53
		sec	
		sbc	#$20
		sta	$53
		bcs	Lbl_0674
		dec	$54
Lbl_0674	lda	$56
		and	($53),Y
		bne	Lbl_0681
		lda	$56
		ora	($53),Y
		sta	($53),Y
		rts	

Lbl_0681	ldx	$52
		cpx	#$34
		bcc	Lbl_068A
		jmp	Lbl_091A

Lbl_068A	ldx	#$3F
Lbl_068C	lda	$C0,X
		beq	Lbl_06A4
		lda	$52
		sec	
		sbc	$C0,X
		nop	
		nop	
		cmp	#$02
		bcs	Lbl_06A4
		lda	$51
		sec	
		sbc	$80,X
		cmp	#$03
		bcc	Lbl_06AB
Lbl_06A4	dex	
		bpl	Lbl_068C
		ldx	#$00
		beq	Lbl_064A
Lbl_06AB	lda	$40
		pha	
		stx	$40
		jsr	Lbl_0407
		ldx	$40
		jsr	Lbl_0867
		lda	#$00
		sta	$C0,X
		pla	
		sta	$40
		jmp	Lbl_0645

// Shapes for spaceships etc
// Four ships, Left/Right for pos 1, then L/R for one block right
Ships1L	fcb	$60,$70,$B0,$90
Ships1R	fcb	$40,$50,$10,$10

Ships2L	fcb	$80,$A0,$20,$20
Ships2R	fcb	$90,$B0,$70,$60

// Gun shape, left (pos1 and 1 block right), right (ditto)
GunShapeL	fcb	$E0,$80
GunShapeR	fcb	$40,$D0

BldgShape	fcb	$00		// Loaded from 059D
Lbl_06D7	fcb	$00,$00		
Lbl_06D9	fcb	$00,$FE	
		fcb	$FF,$FD
		
Lbl_06DD	ldy	#$05
Lbl_06DF	lda	$0059,Y
		bmi	Lbl_06E8
		dey	
		bpl	Lbl_06DF
		rts	

Lbl_06E8	lda	$C0,X
		sec	
		sbc	#$01
		pha	
		lsr	A
		lsr	A
		sta	$0059,Y
		lda	$80,X
		clc	
		adc	#$01
		lsr	A
		ora	#$80
		sta	$005F,Y
		pla	
		and	#$03
		rol	A
		tax	
		lda	Lbl_070A,X
		sta	$0065,Y
		rts	

Lbl_070A	fcb	$40,$80,$10,$20	// Loaded from 0703
		fcb	$04,$08,$01,$02

Lbl_0712	ldx	#$05
Lbl_0714	lda	$59,X
		bpl	Lbl_071C
Lbl_0718	dex	
		bpl	Lbl_0714
		rts	

Lbl_071C	ldy	$59,X
		lda	$18,Y
		sta	$6B
		lda	$0028,Y
		sta	$6C
		lda	$5F,X
		php	
		and	#$7F
		sta	$5F,X
		tay	
		lda	($6B),Y
Lbl_0732	plp	
		bmi	Lbl_075E
		and	$65,X
		bne	Lbl_073F
Lbl_0739	lda	#$FF
		sta	$59,X
		bmi	Lbl_0718
Lbl_073F	lda	($6B),Y
		eor	$65,X
		sta	($6B),Y
		lda	#$00
		asl	$65,X
		rol	A
		asl	$65,X
		rol	A
		beq	Lbl_075E
		sta	$65,X
		dec	$59,X
		lda	$6B
		clc	
		adc	#$20
		sta	$6B
		bcc	Lbl_075E
		inc	$6C
Lbl_075E	lda	$6C
		cmp	#$04
		beq	Lbl_0739
		lda	($6B),Y
		and	$65,X
		bne	Lbl_0772
		lda	($6B),Y
		ora	$65,X
		sta	($6B),Y
		bne	Lbl_0718
Lbl_0772	lda	$59,X
		bne	Lbl_0782
		lda	$65,X
		and	#$F0
		beq	Lbl_0782
		jsr	Lbl_078B
		jmp	Lbl_0739

Lbl_0782	lda	$65,X
		eor	($6B),Y
		sta	($6B),Y
		jmp	Lbl_0718

Lbl_078B	txa	
		pha	
		tya	
		pha	
		jsr	DspGun
		lda	GunPos
		lsr	A
		tax	
		lda	#$00
		rol	A
		tay	
Lbl_079A	lda	LINBOT,X
		pha	
		lda	Lbl_07F9,Y
		sta	LINBOT,X
		inx	
		iny	
		iny	
		cpy	#$06
		bcc	Lbl_079A
		txa	
		pha	
		lda	#$05
		sta	$6B
Lbl_07B1	ldx	#$FF
Lbl_07B3	txa	
		tay	
		lda	$6F
		and	#$FD
		sta	$BC04
		eor	#$10
		sta	$6F
Lbl_07C0	dey	
		bne	Lbl_07C0
		dex	
		bne	Lbl_07B3
		dec	$6B
		bne	Lbl_07B1
		lda	#$01
		jsr	Lbl_0801
		lda	$6F
		and	#$EF
		sta	$6F
		pla	
		tax	
		pla	
		sta	$03DF,X
		pla	
		sta	$03DE,X
		pla	
		sta	$03DD,X
		jsr	Lbl_04F6
		sed	
		sec	
		lda	$3C
		sbc	#$01
		sta	$3C
		bcs	Lbl_07F3
		jmp	Lbl_0A05

Lbl_07F3	cld	
		pla	
		tay	
		pla	
		tax	
		rts	

Lbl_07F9	fcb	$24,$08	// Loaded from 079E
		fcb	$60,$90
		fcb	$04,$18

// Some sort of complex delay loop
Lbl_07FF	lda	#$0F
Lbl_0801	sta	$6B
Lbl_0803	ldx	#$FF
Lbl_0805	dey	
		bne	Lbl_0805
		jsr	Lbl_0BF6	// 6522 timing??
		asl	A
		sta	$7E
		and	#$02
		bne	Lbl_0819
		dex	
		bne	Lbl_0805
		dec	$6B
		bne	Lbl_0803
Lbl_0819	rts

//
// Write text to screen where data is formatted:
//  00, 01 = LO/HI bytes of screen address
//  02..   = Text, terminated by $00
//
Lbl_081A	sta	STEXT		// Set TEXT mode
Lbl_081D	lda	Lbl_0961,X
		sta	$70		// Write screen addr LO
		inx	
		lda	Lbl_0961,X
		sta	$71		// Write screen addr HI
		ldy	#$FF
Lbl_082A	iny
		inx
		lda	Lbl_0961,X	// And write string to screen
		beq	Lbl_0835	// until $00 byte
		sta	($70),Y
		bne	Lbl_082A
Lbl_0835	lda	SGRAPH		// Set GRAPHIC mode
		rts

Lbl_0839	ldx	#$00
Lbl_083B	ldy	Lbl_085F,X
		sta	STEXT
		lda	$38,X
		pha	
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		clc	
		adc	#$30
		sta	VDUFST,Y
		iny	
		pla	
		and	#$0F
		adc	#$30
		sta	VDUFST,Y
		iny	
		inx	
		cpx	#$08
		bne	Lbl_083B
		beq	Lbl_0835

Lbl_085F	fcb	$08,$06,$04,$0C		// Loaded from 083B
		fcb	$10,$18,$16,$14

Lbl_0867	pha	
		txa	
		pha	
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		tax	
		sed	
		lda	$52
		cmp	#$34
		bcs	Lbl_087E
		clc	
		lda	Lbl_08B1,X
		adc	$38
		sta	$38
Lbl_087E	lda	$39
		pha	
		adc	#$00
		sta	$39
		lda	$3A
		adc	#$00
		sta	$3A
		pla	
		eor	$39
		and	#$E0
		beq	Lbl_089A
		lda	$3C
		cmp	#$04		// Bottom of screen?
		beq	Lbl_089A
		inc	$3C

Lbl_089A	lda	$52
		cmp	#$34
		bcs	Lbl_08AC
		jsr	$1050		// NOT GOT THIS BIT
		sbc	#$01
		sta	$3B
		bne	Lbl_08AC
		jmp	Lbl_0A7D

Lbl_08AC	cld	
		pla	
		tax	
		pla	
		rts	

// Scores?
Lbl_08B1	fcb	$50,$40,$30,$20		// Loaded from 0877

Lbl_08B5	lda	$78
		lsr	A
		tax	
		lda	#$00
		rol	A
		tay	
		cpx	#$0E
		bcc	Lbl_0929
		cpx	#$33
		bcs	Lbl_0929
Lbl_08C5	cpx	#$12
		bcc	Lbl_0908
		cpx	#$2E
		bcs	Lbl_0908
		lda	Lbl_0934,Y
		eor	#$FF
		and	$0230,X
Lbl_08D5	sta	$0230,X
		sty	$6B
		lda	$7D
		beq	Lbl_08EA
		iny	
		tya	
		lsr	A
		bcs	Lbl_08F1
		dey	
		dey	
Lbl_08E5	dey	
		dey	
		jmp	Lbl_08F1

Lbl_08EA	iny	
		tya	
		lsr	A
		bcc	Lbl_08E5
		iny	
		iny	
Lbl_08F1	cpy	#$0A
Lbl_08F3	bcs	Lbl_0906
		lda	Lbl_0934,Y
		and	$0230,X
		bne	Lbl_091A
		lda	Lbl_0934,Y
		ora	$0230,X
		sta	$0230,X
Lbl_0906	ldy	$6B
Lbl_0908	iny	
		iny	
		inx	
		cpy	#$0A
		bcc	Lbl_08C5
		inc	$78
		lda	$7D
		bne	Lbl_0919
		dec	$78
		dec	$78
Lbl_0919	rts	

Lbl_091A	jsr	Lbl_0867
		lda	#$00
		sta	$52
		ldx	#$1F
Lbl_0923	sta	$0240,X
		dex	
		bpl	Lbl_0923
Lbl_0929	lda	#$00
		sta	$78
		lda	$6F
		and	#$F7
		sta	$6F
		rts	

Lbl_0934	fcb	$00,$00,$B8,$20		// Loaded from 08CD, 08F5, 08FD
		fcb	$3F,$7E,$74,$BD
		fcb	$00,$10
	
Lbl_093E	dec	$7A
		bne	Lbl_0960
		dec	$7B
		bne	Lbl_0960
		lda	#$05
		sta	$7B
		lda	#$1C
		ldx	$4D
		stx	$7D
		bne	Lbl_0954
		lda	#$63
Lbl_0954	sta	$78
		lda	#$1E
		sta	$79
		lda	$6F
		ora	#$08
		sta	$6F
Lbl_0960	rts	

// Text data
Lbl_0961	fdb	$0248		// Loaded from 081D,0823,082C, 09F1,09FC
		fcc	'INVASION COMPLETE'
		fcb	$0
Lbl_0975	fdb	$0389
		fcc	'PRESS PLAY KEY'
		fcb	$0
Lbl_0986	fdb	$0249
// Start screen texts
		fcc	'SPACE INVASION'
		fcb	$0
Lbl_0997	fdb	$0291
		fcc	'100'
		fcb	$0
Lbl_099D	fdb	$02D1
		fcc	'50'
		fcb	$0
Lbl_09A2	fdb	$0311
		fcc	'40'
		fcb	$0
Lbl_09A7	fdb	$0351
		fcc	'30'
		fcb	$0
Lbl_09AC	fdb	$0391
		fcc	'20'
		fcb	$0
Lbl_09B1	fdb	$03C3
		fcc	'CT,ETI,(C)TANGERINE 1980'
		fcb	$0
		fcb	$0		// Terminator

// Spaceship data
Lbl_09CD	fdb	$028B
		fcb	$B8,$3F,$74,$0
Lbl_09D3	fdb	$02CC
		fcb	$18,$10,$0
Lbl_09D8	fdb	$030C
		fcb	$1C,$14,$0
Lbl_09DD	fdb	$034C
		fcb	$2C,$04,$0
Lbl_09E2	fdb	$038C
		fcb	$24,$04,$0
		fcb	$0		// Terminator
 	
Lbl_09E8	jsr	GraphClr	// Clear screen?
		ldx	#$25
Lbl_09ED	jsr	Lbl_081A	// SPACE INVASION text
		inx	
		lda	Lbl_0961,X
		bne	Lbl_09ED	// Print all strings to extra $00

// Display spaceships, note DspData in GRAPHIC mode on exit

		ldx	#$6C		// $0961,X = $09CD
Lbl_09F8	jsr	Lbl_081D	// Display spaceship chars
		inx	
		lda	Lbl_0961,X
		bne	Lbl_09F8
		jsr	Lbl_07FF	// Some sort of delay?
		rts	

Lbl_0A05	ldx	#$00
		cld	
		jsr	Lbl_081A	// Write INVASION COMPLETE to screen
		ldx	#$10
		lda	#$0F
Lbl_0A0F	sta	$0268,X
		dex	
		bpl	Lbl_0A0F
		jsr	Lbl_07FF	// Delay?
		ldx	#$02
Lbl_0A1A	lda	$3D,X
		cmp	$38,X
		bcc	Lbl_0A25
		bne	Lbl_0A4D
		dex	
		bpl	Lbl_0A1A
Lbl_0A25	ldx	#$02
Lbl_0A27	lda	$38,X
		sta	$3D,X
		dex	
		bpl	Lbl_0A27
		bmi	Lbl_0A4D

Lbl_0A30	cld			// Here from start
		lda	#$00
		ldx	#$CF
ClrVar	sta	$30,X		// Clear all variables $30-$FF
		dex	
		bne	ClrVar
		ldx	#15
		lda	#$00		// Generate line indexes from $200
		ldy	#$02
Lbl_0A40	sta	$18,X
		sty	$28,X
		clc	
		adc	#32		// Chars per line
		bcc	Lbl_0A4A
		iny	
Lbl_0A4A	dex	
		bpl	Lbl_0A40
		
Lbl_0A4D	ldx	#$FF
		txs	
		jsr	Lbl_09E8	// Display initial screen
		sei	
		lda	#$30
		sta	$4A
		jsr	Lbl_055A
		BRK
		ldx	#$14
		jsr	Lbl_081A
		lda	#$00
		sta	$39
		sta	$3A
		sta	$38
		lda	#$03
		sta	$3C
		lda	#$01
		sta	$BC04
		jsr	Lbl_0839
		jsr	Lbl_07FF
		jsr	Lbl_0BF6
		lsr	A
		bcc	Lbl_0A4D
Lbl_0A7D	lda	#$02
		jsr	Lbl_0801		// Delay?
		ldx	#$FF
		cld	
		txs	
		lda	#$64
		sta	$3B
		lda	#$05
		sta	$7B
		sta	$6F
		ldy	#$01
		sty	$6D
		sty	$4F
		sty	$50
		dey	
		sty	$4D
		sty	$4E
		sty	$57
		sty	$52
		sty	$78
		sty	$58
		dey	
		ldx	#$05
Lbl_0AA8	sty	$59,X
		dex	
		bpl	Lbl_0AA8
		jsr	Lbl_055A
		lda	#$FF
		sta	$40
		dec	$4A
		dec	$4A
Lbl_0AB8	lda	#$FF
		eor	$7E
		tay	
Lbl_0ABD	dey	
		bne	Lbl_0ABD
		dec	$50
		bne	Lbl_0AF7
		jsr	Lbl_05C3
		lda	#$08
		sta	$50
		jsr	Lbl_093E
		lda	$6E
		bne	Lbl_0AF7
		lda	$4E
		bne	Lbl_0AF7
		lda	$40
		and	#$07
		clc	
		adc	#$38
Lbl_0ADD	tax	
		lda	$C0,X
		bne	Lbl_0AE8
		txa	
		sec	
		sbc	#$08
		bcs	Lbl_0ADD
Lbl_0AE8	jsr	Lbl_06DD
		ldx	$7A
		lda	Lbl_0A30,X		// 0A30 = program?!?
		and	#$7F
		adc	$3B
		lsr	A
		sta	$6E
Lbl_0AF7	dec	$4F
		bne	Lbl_0B2C
		inc	$4F
		jsr	Lbl_0BF6
		and	#$04
		beq	Lbl_0B18
		ldx	GunPos
		cpx	#2		// Gun at left most pos?
		beq	Lbl_0B2C
		jsr	DspGun
		dec	GunPos
Lbl_0B0F	jsr	Lbl_04FA
		lda	#$10
		sta	$4F
		bne	Lbl_0B2C
Lbl_0B18	jsr	Lbl_0BF6
		and	#$02
		beq	Lbl_0B2C
		ldx	GunPos
		cpx	#58		// Gun at right most pos?
		beq	Lbl_0B2C
		jsr	DspGun
		inc	GunPos
		bne	Lbl_0B0F
Lbl_0B2C	ldx	#$01
		lda	$52
		bne	Lbl_0B71
		jsr	Lbl_0BF6
		and	#$08
		bne	Lbl_0B41
		dec	$58
		bpl	Lbl_0B88
		inc	$58
		beq	Lbl_0B88
Lbl_0B41	lda	$58
		bne	Lbl_0B88
		jsr	Lbl_093E
		lda	#$03
		sta	$54
		lda	#$E0
		sta	$53
		ldx	GunPos
		inx	
		stx	$51
		txa	
		lsr	A
		sta	$55
		lda	$6F
		and	#$FB
		sta	$6F
		lda	#$04
		bcc	Lbl_0B64
		asl	A
Lbl_0B64	sta	$56
		lda	#$02
		sta	$52
		ldx	#$00
		stx	$57
		dex	
		bmi	Lbl_0B7B
Lbl_0B71	lda	$6F
		ora	#$04
		sta	$6F
		lda	$57
		bne	Lbl_0B86
Lbl_0B7B	jsr	Lbl_063A
		lda	#$06
		sta	$57
		lda	#$02
		sta	$58
Lbl_0B86	dec	$57
Lbl_0B88	jsr	Lbl_0BF6
		and	#$10
		beq	Lbl_0BA7
Lbl_0B8F	ldy	#$FF
		lda	#$05
		sta	KBWRIT
Lbl_0B96	jsr	Lbl_0BF6
		and	#$10
		bne	Lbl_0B8F
		dey	
		bne	Lbl_0B96
Lbl_0BA0	jsr	Lbl_0BF6
		and	#$0F
		beq	Lbl_0BA0
Lbl_0BA7	dec	$6D
		bne	Lbl_0BB6
		jsr	Lbl_0712
		lda	$3B
		lsr	A
		lsr	A
		adc	#$0A
		sta	$6D
Lbl_0BB6	jsr	Lbl_0839
		lda	$6F
		sta	$BC04
		lda	#$02
		cmp	$50
		bne	Lbl_0BE3
		dec	$7C
		bne	Lbl_0BE3
		ldx	#$04
		ldy	#$02
		lda	$6F
		and	#$02
		beq	Lbl_0BDC
		dey	
		dey	
		lda	$6F
		eor	#$02
		sta	$6F
		ldx	$3B
Lbl_0BDC	stx	$7C
		tya	
		ora	$6F
		sta	$6F
Lbl_0BE3	lda	$78
		beq	Lbl_0BF2
		dec	$79
		bne	Lbl_0BF2
		lda	#$1E
		sta	$79
		jsr	Lbl_08B5
Lbl_0BF2	jmp	Lbl_0AB8

		fcb	$0F		// Invalid opcode $0F ($0BF5)	

Lbl_0BF6	php	
		lda	#$00
		sta	Lbl_0C3D
		lda	$BFC1
		and	#$10
		bne	Lbl_0C0B
		lda	#$04
		ora	Lbl_0C3D
		sta	Lbl_0C3D
Lbl_0C0B	lda	$BFC1
		and	#$40
		bne	Lbl_0C1A
		lda	#$02
		ora	Lbl_0C3D
		sta	Lbl_0C3D
Lbl_0C1A	lda	$BFC1
		and	#$08
		bne	Lbl_0C29
		lda	#$01
		ora	Lbl_0C3D
		sta	Lbl_0C3D
Lbl_0C29	lda	$BFC1
		and	#$04
		bne	Lbl_0C38
		lda	#$08
		ora	Lbl_0C3D
		sta	Lbl_0C3D
Lbl_0C38	plp	
		lda	Lbl_0C3D
		rts	

Lbl_0C3D	brk			// Referenced by various above
