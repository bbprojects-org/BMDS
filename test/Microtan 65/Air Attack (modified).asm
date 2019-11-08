/////////////////////////////////////////////////////////////////////////////////
//
// AIR ATTACK
//
// Copyright David A Brown, 1980
//
// Press any key to drop bombs. Flatten the buildings before you descend and
// crash into them. Land the plane on the ground to win.
//
// Minor mods by RCB, Aug 2005
//
// G400 to start
// Set flag USEKB to 1 for ASCII keyboard, to 0 for keypad version
//
/////////////////////////////////////////////////////////////////////////////////
USEKB		EQU	1
/////////////////////////////////////////////////////////////////////////////////

DELAY		EQU	$40		// Delay value between airplane moves

ICHAR		EQU	$0001
SGRAPH		EQU	$BFF0		// Read to set GRAPHIC mode
STEXT		EQU	$BFF3		// Write to set TEXT mode
KBREAD		EQU	$BFF3
POLLKB		EQU	$FDFA
OUTPCR		EQU	$FE73

SPACE		EQU	$20

		ORG	$40		// Zero page variables
		
HI_SCORE	RMB	2		// Highest score so far
SCORE		RMB	2		// Running score
BOMBING		RMB	1		// Is bomb falling?
BOMBLOC		RMB	2		// Bomb location
SKYHIT		RMB	1
LOCAT		RMB	2
SCRAPER		RMB	2


// Start of code

		org	$0400

#if USEKB
kstart		lda	#0		// Set keyboard char to NULL
		sta	ICHAR
#endif

// Reset variables

start		lda	#0
		sta	HI_SCORE
		sta	HI_SCORE+1
newgame	sta	SCORE
		sta	SCORE+1
newpic		ldx	#5
clear		sta	SCORE+1,X
		dex
		bne	clear
		lda	#2
		sta	LOCAT+1
		lda	#$A0
		sta	SCRAPER
		lda	#$03
		sta	SCRAPER+1

// Set up display

clrpic		jsr	clrscr		// Clear screen
		ldy	#0
title1		lda	title,Y		// Write title
		sta	$03E1,Y
		iny
		cpy	#10
		bne	title1
		lda	SGRAPH		// Set GRAPHIC mode
		ldy	#0
nexsky		ldx	table,Y		// Draw skyscrapers
skyput		lda	#$FF
		sta	(SCRAPER),Y
		dex			// height in XREG
		beq	next
		sec
		lda	SCRAPER
		sbc	#32
		sta	SCRAPER
		lda	SCRAPER+1
		sbc	#0
		sta	SCRAPER+1
		clc
		jmp	skyput
		
next		iny
		cpy	#32		// x32 "towers"
		beq	botrow
		lda	#$03
		sta	SCRAPER+1
		lda	#$A0
		sta	SCRAPER
		jmp	nexsky
		
// Display landing strip

botrow		ldx	#32		// Number of columns
		lda	#$19		// Checker pattern
bot1		sta	$03bf,X
		dex
		bne	bot1
		
// Print out highscore, LHS on bottom line

#if USEKB
prthi		sta	STEXT		// Set TEXT mode
		lda	#SPACE
		sta	$03E0
#else
prthi		sta	STEXT
#endif
		lda	HI_SCORE
		jsr	ascsym
		stx	$03F4
		sty	$03F5
		lda	HI_SCORE+1
		jsr	ascsym
		stx	$03F6
		sty	$03F7
		
// Print out score, RHS on bottom line

mainloop	lda	SCORE
		jsr	ascsym
		stx	$03FB
		sty	$03FC
		lda	SCORE+1
		jsr	ascsym
		stx	$03FD
		sty	$03FE
		lda	BOMBING		// Is bomb falling?
		bne	movbomb

#if USEKB
clkbuf		lda	ICHAR		// Key pressed?
		pha
		lda	#0
		sta	ICHAR
		pla
#else
		lda	KBREAD
#endif
		beq	movplane
		sta	BOMBING		// If so note bomb falling
		lda	LOCAT
		sta	BOMBLOC
		lda	LOCAT+1
		sta	BOMBLOC+1
		lda	#5		// Max amount of bldg to destroy
		sta	SKYHIT
		jmp	movplane
		
// Move bomb and check to see if anything hit

movbomb	clc
		lda	SGRAPH		// Set GRAPHIC mode
		lda	#0
		tax
		sta	(BOMBLOC,X)
		lda	BOMBLOC
		adc	#32
		sta	BOMBLOC
		lda	BOMBLOC+1
		adc	#0
		sta	BOMBLOC+1
		lda	(BOMBLOC,X)
		cmp	#$19		// Checker pattern?
		bne	hit
stopbomb	txa
		sta	BOMBING		// Clear bombing flag
		jmp	movplane
		
hit		cmp	#$FF		// Block pattern?
		bne	bombon
		sed
		lda	SCORE+1
		adc	#1
		sta	SCORE+1
		lda	SCORE
		adc	#0
		sta	SCORE
		dec	SKYHIT
		beq	stopbomb
bombon		lda	#$C0		// Half block
		sta	(BOMBLOC,X)

// Move plane

movplane	lda	SGRAPH		// Set GRAPHIC mode
		lda	#0
		tax
		sta	(LOCAT,X)	// Clear old plane position
		cld
		inc	LOCAT
		lda	LOCAT
		bne	pribar
		inc	LOCAT+1
pribar		ldy	#0

// Put plane on screen

birdon		lda	plane,Y
		sta	(LOCAT),Y	// Write new plane position
		iny
		cpy	#4
		bne	birdon

// Check if it hit anything

		lda	(LOCAT),Y
		cmp	#$19		// Checker pattern, landed?
		bne	theend
		sta	STEXT		// Set TEXT mode
		jmp	newpic
		
theend		cmp	#$FF		// Block pattern = skyscraper
		beq	finish
		ldx	#DELAY		// Delay before next iteration
delay1		dey
		bne	delay1
		dex
		bne	delay1
		jmp	prthi		// Go back to print scores ...
		
// Game Over

finish		sta	STEXT		// Set TEXT mode
		ldy	#0
msg1		lda	message1,Y	// "Game over"
		sta	$026B,Y
		iny
		cpy	#9
		bne	msg1
#if USEKB		
		ldy	#0
msg2		lda	message2,Y	// "Press RETURN to start again"
		sta	$02A2,Y
		iny
		cpy	#27
		bne	msg2
#endif

// Check for new high score, if so then set it

		sed
		sec
		lda	HI_SCORE+1
		sbc	SCORE+1
		lda	HI_SCORE
		sbc	SCORE
		bcs	waitkey
		lda	SCORE
		sta	HI_SCORE
		lda	SCORE+1
		sta	HI_SCORE+1

// Wait for a key to be pressed

#if USEKB
waitkey		jsr	POLLKB
		lda	ICHAR
		cmp	#13		// RETURN to start new game
		bne	waitkey
#else
waitkey		lda	KBREAD
		beq	waitkey
#endif
		sta	STEXT		// Set TEXT mode
		lda	#0
		jmp	newgame
		
// Clear screen

clrscr		ldy	#0
		lda	#SPACE
clr1		sta	$200,Y		// Write spaces in two blocks
		sta	$300,Y
		iny
		bne	clr1
		rts

// Convert ACC to ASCII in X and Y registers

ascsym		tay
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		ora	#$30
		tax
		tya
		and	#$0F
		ora	#$30
		tay
		rts
		
// Game data

plane		fcb	$FD, $F0, $F0, $D0  // Plane shape

table		fcb	06,06,06,01,01,05,02,02
		fcb	01,04,03,02,06,02,04,01
		fcb	03,03,01,05,05,02,03,02
		fcb	01,02,05,02,01,04,04,01
		
title		fcc	'Air Attack'
message1	fcc	'GAME OVER'
#if USEKB
message2	fcc	'Press RETURN to start again'
#endif

		end