/////////////////////////////////////////////////////////////////////////////////
//
// AIR ATTACK
//
// Copyright David A Brown, 1980
//
// Press any key to drop bombs. Flatten the buildings before you descend and
// crash into them. Land the plane on the ground to win.
//
// G420 to start -keyboard version	(G450 to start -keypad version)
//
/////////////////////////////////////////////////////////////////////////////////

USEKB		EQU	1		// Set 1 for ASCII keyboard, set 0 for keypad
DELAY		EQU	$20		// Reduced from $60

ICHAR		EQU	$0001
SGRAPH		EQU	$BFF0
STEXT		EQU	$BFF3		// Write to set text mode
KBREAD		EQU	$BFF3
POLLKB		EQU	$FDFA
OUTPCR		EQU	$FE73

		ORG	$40		// Zero page variables
		
HISCORL	RMB	1		// High score
HISCORH	RMB	1
SCOREL	RMB	1		// Running score
SCOREH	RMB	1
BOMFAL	RMB	1		//
BOMLOCL	RMB	1		//
BOMLOCH	RMB	1
SKYHIT	RMB	1		//
LOCATL	RMB	1		//
LOCATH	RMB	1
SCRAPEL	RMB	1		//
SCRAPEH	RMB	1


// Start of code

#if USEKB
		ORG	$0410
		
clkbuf		lda	ICHAR
		pha
		lda	#00
		sta	ICHAR
		pla
		rts
		
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		
kreset		lda	#00
		sta	ICHAR
		jmp	reset
		
lbl0427		sta	STEXT
		lda	#$20
		sta	$03E0
		rts		
#endif

		ORG	$044C

// Aircraft shape

plane		FCB	$FD, $F0, $F0, $D0

// Reset variables

reset		lda	#$00
		sta	HISCORL
		sta	HISCORH
newgam		sta	SCOREL
		sta	SCOREH
newpic		ldx	#$05
clear		sta	SCOREH,X
		dex
		bne	clear
		lda	#$02
		sta	LOCATH
		lda	#$a0
		sta	SCRAPEL
		lda	#$03
		sta	SCRAPEH

// Set up display	
	
clrpic		ldy	#$10
clrlin		jsr	OUTPCR
		dey
		bne	clrlin
		lda	SGRAPH
		ldy	#00
nexsky		ldx	table,Y
skyput		lda	#$ff
		sta	(SCRAPEL),Y
		dex
		beq	next
		sec
		lda	SCRAPEL
		sbc	#$20
		sta	SCRAPEL
		lda	SCRAPEH
		sbc	#00
		sta	SCRAPEH
		clc
		jmp	skyput
		
next		iny
		cpy	#$20
		beq	botrow
		lda	#03
		sta	SCRAPEH
		lda	#$a0
		sta	SCRAPEL
		jmp	nexsky
		
// Display landing strip

botrow		ldx	#$20
		lda	#$19
pribot		sta	$03bf,X
		dex
		bne	pribot
		
// Print out highscore

#if USEKB
lbl04AF		jsr	lbl0427
#else
lbl04AF		sta	STEXT
#endif
		lda	HISCORL
		jsr	ascsym
		stx	$03E5
		sty	$03E6
		lda	HISCORH
		jsr	ascsym
		stx	$03E7
		sty	$03E8
		jmp	manlop
		
// X = high digit, Y = low digit, in ascii

ascsym		tay
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		ora	#$30
		tax
		tya
		and	#$0f
		ora	#$30
		tay
		rts
		
// Print out score

manlop		lda	SCOREL
		jsr	ascsym
		stx	$03F5
		sty	$03F6
		lda	SCOREH
		jsr	ascsym
		stx	$03F7
		sty	$03F8
		lda	BOMFAL
		bne	movbom
#if USEKB
		jsr	clkbuf
#else
		lda	KBREAD
#endif
		beq	movpla
		sta	BOMFAL
		lda	LOCATL
		sta	BOMLOCL
		lda	LOCATH
		sta	BOMLOCH
		lda	#05
		sta	SKYHIT
		jmp	movpla
		
// Move bomb and check to see if anything hit

movbom		clc
		lda	SGRAPH
		lda	#00
		tax
		sta	(BOMLOCL,X)
		nop
		lda	BOMLOCL
		adc	#$20
		sta	BOMLOCL
		lda	BOMLOCH
		adc	#00
		sta	BOMLOCH
		lda	(BOMLOCL,X)
		cmp	#$19		// checker pattern
		bne	knock
stopbm		txa
		sta	BOMFAL
		jmp	movpla
		
knock		cmp	#$ff		// block pattern
		bne	bomon
		sed
		lda	SCOREH
		adc	#01
		sta	SCOREH
		lda	SCOREL
		adc	#00
		sta	SCOREL
		dec	SKYHIT
		beq	stopbm
bomon		lda	#$C0		// was $45, half block
		sta	(BOMLOCL,X)
		nop
		nop
		nop
		nop

// Move plane

movpla		lda	SGRAPH
		lda	#00
		tax
		sta	(LOCATL,X)
		cld
		inc	LOCATL
		lda	LOCATL
		bne	pribar
		inc	LOCATH
pribar		ldy	#0

// Put plane on screen

birdon		lda	plane,Y
		sta	(LOCATL),Y
		iny
		cpy	#04
		bne	birdon

// Check if it hit anything

		lda	(LOCATL),Y
		cmp	#$19		// checker pattern
		bne	theend
		lda	#00
		jmp	piclnk
		
theend		cmp	#$ff		// block pattern
		beq	finish
		ldx	#DELAY	// was $65, reduced to $60, and then $10
dloop		dey
		bne	dloop
		dex
		bne	dloop
		jmp	lbl04AF	// was "manlop"
		
// Print out 'game over'

finish		sta	STEXT
		ldy	#00
gamovr		lda	messag,Y
		sta	$026B,Y
		iny
		cpy	#09
		bne	gamovr

// Check for new high score

		sed
		sec
		lda	HISCORH
		sbc	SCOREH
		lda	HISCORL
		sbc	SCOREL
		bcs	notnew
		lda	SCOREL
		sta	HISCORL
		lda	SCOREH
		sta	HISCORH

// Wait for a key to be pressed

#if USEKB
notnew		jsr	POLLKB
		nop
		nop
#else
notnew		lda	KBREAD
		beq	notnew
#endif
		sta	STEXT
		lda	#00
		jmp	newgam
		
// Reset variables for new picture

piclnk		sta	STEXT
		lda	#00
		jmp	newpic

// Game data

messag		fcc	'GAME OVER'

table		fcb	06,06,06,01,01,05,02,02
		fcb	01,04,03,02,06,02,04,01
		fcb	03,03,01,05,05,02,03,02
		fcb	01,02,05,02,01,04,04,01
		
		END