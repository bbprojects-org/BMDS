//=============================================================================
//
// MICROTAN 65 TANBUG MONITOR
//
//
// If TANBUG_V2 is set to 0, then this will assemble the simple TANBUG V1 (1K),
// essentially the second half of this code from line 780 onwards.
//
// If it is set to 1, then will give TANBUG V2 (2K) expanded version which
// includes translate (assembly) and dis-assembly routines. Refer to
// Microtan65 manual with Tanbug V2
//
//=============================================================================

TANBUG_V2     EQU    1                 // 0 = Tanbug V1, 1 = Tanbug V2

//
// DEFINE ZERO PAGE VARIABLES
//

              ORG   $0, RAM

PSTAT         RMB   1                  // Printer status V2
ICHAR         RMB   1                  // ASCII character
OCHAR         RMB   1                  // Temp char store
VDUIND        RMB   1                  // Display index
INTFS         RMB   3                  // Fast interrupt link
NMIJP         RMB   3                  // NMI link
ICURS         RMB   2                  // Cursor index
RUNIND        RMB   1                  // Zero if in user mode
SINGLE        RMB   1                  // Nonzero if single instruction mode
PROCED        RMB   1                  // Proceed count (single instr to execute)
SIMCOM        RMB   1                  // Keypad (simple) / ASCII (complex) keyboard
INTSL         RMB   3                  // Slow interrupt link
HXPKL         RMB   1                  // Hexpack store
HXPKH         RMB   1

// Pseudo registers

PCLBCK        RMB   1                  // PCL
PCHBCK        RMB   1                  // PCH
PSWBCK        RMB   1                  // PSW
SPBCK         RMB   1                  // SP
XBCK          RMB   1                  // IX
YBCK          RMB   1                  // IY
ABCK          RMB   1                  // A

// Temporary stores

MODADL        RMB   1
MODADH        RMB   1
COPL          RMB   1
COPH          RMB   1

// Breakpoint status table and code store

BPTLO         RMB   1
BPTHI         RMB   15
BPTCOD        RMB   16

//
// DECLARE IMPORTANT ROUTINES / VARIABLES
//

// Error exit

INPERR        EQU   $F7F7

// Stack base

STKBSE        EQU   $100

// Display scroll labels

VDUSTT        EQU   $200
VDUFST        EQU   $220
VDUMID        EQU   $300
VDUTOP        EQU   $320
LINBOT        EQU   $3E0

// Serial interface

SERDT         EQU   $BFD0              // Serial data
SERST         EQU   $BFD1              // Status
SERCM         EQU   $BFD2              // Command
SERCN         EQU   $BFD3              // Control

// Printer VIA defs, 2nd 6522

PRIBD         EQU   $BFE0              // Printer status data
PRIAD         EQU   $BFE1              // Printer data port A
PRIBE         EQU   $BFE2              // And EN
PRIAE         EQU   $BFE3              // Data enable
PRPCR         EQU   $BFEC              // PCR
PRIFR         EQU   $BFED

// I/O Ports

SGRAPH        EQU   $BFF0              // Read sets graphics on
KBINCL        EQU   $BFF0              // Write to reset keyboard interrupt
SNMI          EQU   $BFF1              // Write to set Delayed NMI
KBWRIT        EQU   $BFF2
KBREAD        EQU   $BFF3              // Read keyboard port
STEXT         EQU   $BFF3              // Write to set graphics off (text mode)

// BASIC entry points

BASRT         EQU   $E2ED              // Basic cold start
BSWARM        EQU   $C34B              // Basic warm start
STKINI        EQU   $C558              // Basic reinit subroutine


#if TANBUG_V2

// Start of TANBUG V2 code block. 1K extra so starts at $F800

              ORG   $F800, CODE

//
// Subroutine jump table. Located at start of TANBUG V2 ROM. Jump addresses
// will not change with future TANBUG versions
//

PRPUP         JMP   OINT               // Jump table to utility routines
OUTPAR        JMP   PARPR              // Parallel print output
OUTSER        JMP   SERPNT             // Serial printer output
OUTSCR        JMP   SCREEN             // Screen output
OUTRET        LDA   #13                // Same as OUTPCR, output CR
OUTALL        JMP   OPCHR              // Same as $FE73, output CH
JMNRW         JMP   MMANG              // Memory management, non-increment
JMNRWI        JMP   MMINC              // Memory management, auto-increment
JHXPCK        JMP   HEXPCK             // Hex pack
JHXPNT        JMP   HEXPNT             // Hex print
JPLKB         JMP   POLLKB             // Poll keyboard
RETMS         LDX   #$FF               // Reset stack
              TXS
RETMON        JMP   MONTOR             // User return to monitor
JCURSN        JMP   CURSON             // Cursor on
JCURSF        JMP   CURSOF             // Cursor off
JWASKB        JMP   WASKB              // User input to mono

SERN          FCB   >SERNOX
              FCB   <SERNOX

//
// This table sets zero page after a reset
//

SETUP         JMP   KBINT
              JMP   NMNT
              FDB   LINBOT             // ICURS setting
              FCB   1                  // RUNIND
              FCB   0                  // SINGLE
              FCB   0                  // PROCED
              FCB   0                  // SIMCOM
              FCB   $40                // Slow Interrupt (RTI instruction)

//
// PNSUP - SETUP PRINTER MODE
//
// After init, screen output on, both printers off
// Also saves BASIC zero page if in BASIC when reset hit
//

PNSUP         BIT   PSTAT              // BASIC warm start set?
              BPL   PNSUP1             // No, then skip
              LDX   #$4F               // Else put $10-$5F on stack
PNSUP2        LDA   INTSL,X
              STA   STKBSE,X
              DEX                           
              BPL   PNSUP2
PNSUP1        LDA   #0                 // Clear status of printers
              STA   PSTAT
              LDA   #$8B               // Setup serial option
              STA   SERCM
              LDA   #$FF
              STA   SERCN
              LDA   #$8B
              CMP   SERCM              // If cannot read, not there, so skip
              BNE   NORSP
              LDA   #$93               // Else finish setup
              STA   SERCN
              LDA   SERST              // Is a keyboard connected?
              AND   #$60
              BNE   NORSP              // No, skip
              LDA   #$89               // Else enable serial interface
              STA   SERCM
              LDA   #$21               // Turn serial printer and input on
              STA   PSTAT

NORSP         LDX   #$FF               // Set stack. X must be $FF on exit
              TXS                           
              JMP   OLSAR1             // Back to old TANBUG

//
// In TANBUG V2, OPCHR jumps here to determine what type of output
// A jump is used to maintain compatibility with TANBUG V1
// OCHAR holds char char to be output
// X,Y must be saved
//

OPTYPE        STA   OCHAR              // Save character
              TXA                           
              PHA                           
              TYA                           
              PHA

// Look for control codes

              LDA   SERN               // Put SERNOF address on stack
              PHA                           
              LDA   SERN+1
              PHA                           
              LDX   OCHAR              // Get character in X
              LDY   PSTAT              // Is device control DC1 set?
              TYA                           
              AND   #$04
              BNE   LAB1               // Yes, go deal
              CPX   #$11               // Else is this DC code?
              BNE   DVCN9              // No, normal ACN
              TYA                      // Else set flag
              ORA   #$04
              STA   PSTAT              // to say DC
              RTS                      // and jump to SERNOF from stack

LAB1          TYA
              AND   #$FB
              STA   PSTAT
              CPX   #$06               // Is it a legal device code?
              BCS   DVCN9              // No, normal output
              LDA   HIDC,X             // Else get service address
              PHA                           
              LDA   LODC,X
              PHA                           
              RTS                      // and jump to it via RTS

// Get here if normal print

DVCN9         LDA   PSTAT              // Get print status
              ASL   A
              ASL   A
              BCS   NOVDU              // Skip if screen disabled
              PHA                      // Else save PSTS shifted
              TXA                      // Get the character in Acc
              CMP   #10                // No action if LF
              BEQ   NOVDU1
              JSR   SCREEN             // Else print ($FE79)
NOVDU1        PLA                      // Recover status
NOVDU         LDX   OCHAR              // Was symbol delete?
              CPX   #$7F
              BNE   NPDL               // No
              LDX   #$2F               // Else change to slash
              STX   OCHAR

NPDL          ASL   A                  // Is serial on?
              BCC   NOSRN              // No, skip
              PHA                      // Else save print status
              CPX   #13                // Is character CR?
              BNE   NOSRN1             // No, skip
              AND   #$10               // LF only on serial?
              BNE   NOSRN2             // Yes, skip
NOSRN1        JSR   SERPNT             // Print it (Lbl_F99B)
NOSRN2        PLA                      // Recover status
NOSRN         ASL   A                  // Parallel print on?
              BCC   NOPAN              // No, skip
              PHA                           
              LDX   OCHAR
              CPX   #13                // Char = CR?
              BNE   NOPAN1             // No, skip
              AND   #$20               // Don't print unless special bit set
              BEQ   NOPAN2
NOPAN1        JSR   PARPR              // Parallel print subroutine (Lbl_F9B2)
NOPAN2        PLA
NOPAN         ASL   A                  // Ext printer?
              BCC   NEXPR              // No, skip
              JSR   EXTPNT             // Else print
NEXPR         LDX   OCHAR
              CPX   #13                // Was character CR?
              BNE   SERNOX             // No, skip
              LDA   #10                // Else give a LF
              JSR   OPTYPE             // by a recursive call
SERNOX        RTS                      // Return to SERNOF

// SERNOF must follow this

SERNOF        PLA
              TAY                           
              PLA                           
              TAX                           
              RTS                           

//
// External User Print subroutine
// Goes via INTSL. User must return via RTS
//

EXTPNT        JMP   (INTSL+1)

//
// Jump table for DC commands
//

HIDC          FCB   >SERON             // Hi bytes
              FCB   >SEROFF
              FCB   >PARINI
              FCB   >PAROFF
              FCB   >SCRON
              FCB   >SCROF

LODC          FCB   <SERON-1           // Lo bytes
              FCB   <SEROFF-1
              FCB   <PARINI-1
              FCB   <PAROFF-1
              FCB   <SCRON-1
              FCB   <SCROF-1

//
// CONCHK is entered from the keyboard interrupt routine in TANBUG V1
// It checks for control keys
// ACC previously saved. Must not use X or Y without saving
//

CONCHK        STA   KBINCL             // Clear keyboard interrupt
              CMP   #$10               // Is it Ctrl-P?
              BNE   CTRLS              // No, try Ctrl-S
              JSR   PARCH              // Else set parallel printer
              JMP   SPRN

CTRLS         CMP   #$13               // Is it Ctrl-S?
              BNE   TRSPPP             // No
              JSR   SCRCH              // Else change status of screen supp
              JMP   SPRN

TRSPPP        CMP   #$16               // Ctrl-V, V24?
              BNE   NOCTRL             // No, skip
              JSR   SERCH
SPRN          LDA   #$00
              STA   ICHAR
NOCTRL        RTS

// PARCH - changes state of Parallel Printer

PARCH         LDA   PSTAT
              AND   #$10               // Printer on?
              BEQ   PARINI             // No, turn it on

PAROFF        LDA   PSTAT              // Else, turn it off
              AND   #$EF
PARCH1        STA   PSTAT
              RTS                           

// PARINI - inits Parallel Printer and turns on PARINI

PARINI        LDA   #0                 // Use data enable to see if present
              STA   PRIAE
              CMP   PRIAE              // If cannot read, not there
              BNE   OINT1              // So ignore
              LDA   PSTAT              // Set the on bit in Print Status
              ORA   #$10
              STA   PSTAT

// OINT - inits printer, doesn't affect stat

OINT          LDA   #$FF               // Set ports
              STA   PRIAE
              LDA   #$FE
              STA   PRIBD              // Init low, rest high
              LDA   #9                 // Set outputs
              STA   PRIBE
              LDA   #$0F               // CA2 high, CB1 pos trig
              STA   PRIAD
              JSR   STBPA              // Cause strobe
              LDA   #$FF               // Remove it
              STA   PRIBD
OINT1         RTS

// SCRCH - change state of screen suppress

SCRCH         LDA   PSTAT              // Is screen suppressed?
              AND   #$40
              BEQ   SCROF              // No, suppress
SCRON         LDA   PSTAT
              AND   #$BF               // Else, unsuppress
SCRON1        STA   PSTAT
              RTS                           

SCROF         LDA   PSTAT              // Inhibit it here
              ORA   #$40
              BNE   SCRON1

// SERCH - change serial printer status

SERCH         LDA   PSTAT
              AND   #$20               // Printer on?
              BEQ   SERON              // No, turn on
SEROFF        LDA   PSTAT
              AND   #$DF               // Turn off
SEROF1        STA   PSTAT
              RTS                           

SERON         LDA   SERCM              // If reads as $FF, not there
              CMP   #$FF               // so ignore
              BEQ   SEROFF
              LDA   PSTAT
              ORA   #$20
              BNE   SEROF1             // Turn on

//
// SERPNT - serial printer subroutine
// Registers are not saved
//

SERPNT        LDX   #0                 // Delay count
SERPN2        LDA   SERST
              AND   #$10               // Xmit buffer empty?
              BNE   OPSER              // Yes, go output data
SERPN1        DEY                      // Else delay
              BNE   SERPN1
              DEX                           
              BNE   SERPN2
              BEQ   PRERR              // Give time out if failure

OPSER         LDA   OCHAR              // Get data
              STA   SERDT              // Write it
              RTS                           

//
// PARPR - parallel print output. Assumes printer setup
// Entered from TANBUG as a result of OPCHR
// Acc not saved, Char in OCHAR
//

PARPR         LDA   PRIBD
              AND   #$04               // Error (low)?
              BEQ   PRERR              // Yes, go error
              LDX   #$FF               // Else, delay count
PABSY         TYA
              LDY   #$0F
DY            DEY
              BPL   DY
              TAY                           
              DEY                      // Outside loop
              BNE   PABSY1             // To give a time out
              DEX                           
              BEQ   PRERR              // If not ok an 10 sec
PABSY1        LDA   PRIBD
              AND   #$02               // Printer busy?
              BNE   PABSY              // Yes, wait
              LDA   PRIFR
              AND   #$02               // Ack received?
              BEQ   PABSY              // No, wait
              LDA   OCHAR
              STA   PRIAD              // Else, write data

// INT is cleared, straight to strobe out

STBPA         LDA   #%0000.1101        // Strobe low
              STA   PRPCR
STBPA1        SBC   #$01               // Wait a moment
              BNE   STBPA1
              LDA   #%0000.1111        // Strobe high
              STA   PRPCR
              RTS                           

// PRERR - printer error

PRERR         LDA   PSTAT
              AND   #$8F               // Printer off
              STA   PSTAT
              LDX   #$0C               // Error message
PRERR1        LDA   PREMS,X
              JSR   OPCHR
              DEX                           
              BPL   PRERR1
              RTS                           

PREMS         FCB   13                 // CR
              FCC   'RORRE TNIRP'      // 'PRINT ERROR' backwards
              FCB   13                 // CR

//
// CURDL - Moves the cursor back one on DEL
//         On entry Y holds position on line. Cursor will also move up
//

CURDL         DEY                      // Decrement its position
              BPL   CURDL1             // Return if no Start Of Line
              LDA   ICURS              // Else up one line
              SEC                           
              SBC   #32
              STA   ICURS
              LDA   ICURS+1
              SBC   #$00
              STA   ICURS+1
              LDY   #31                // Y to end of line
              CMP   #1                 // Of top of screen?
              BNE   CURDL1             // No, return
              JSR   CLSC               // Else, clear screen
CURDL1        RTS

//
// CLSC - Clears the screen and resets the pointers
//

CLSC          LDY   #0
              LDA   #32
CLSC1         STA   VDUSTT,Y
              STA   VDUMID,Y
              INY                           
              BNE   CLSC1
              TYA                           
              STA   ICURS
              LDA   #2
              STA   ICURS+1
              RTS                           

//
// SCROLL - Scrolls the screen up one and resets params for bottom line
//

SCROLL        LDX   #0
LOWBLK        LDA   VDUFST,X
              STA   VDUSTT,X
              INX                           
              BNE   LOWBLK
HIBLK         LDA   VDUTOP,X
              STA   VDUMID,X
              INX                           
              CPX   #$E0
              BNE   HIBLK
              STX   ICURS
              LDA   #3
              STA   ICURS+1
              LDA   #$20
              TAY                           
MORSP         DEY
              STA   (ICURS),Y
              BNE   MORSP
              RTS                           

//
// STNGC - compare a command string, starting at line start, with a
//         stored string. On exit condition code EQ is set if there
//         was a match
//         On enter X must be set to offset address of string to be compared
//

STNGC         LDY   #0                 // Start of VDU line
STNGC2        LDA   COMST,X
              BNE   STNGC3             // If not terminating 0 keep going
STNGC1        RTS

STNGC3        CMP   (ICURS),Y          // Compare with input
              BNE   STNGC1             // If no match, return
              INX                           
              INY                           
              BNE    STNGC2            // Try next character

// Strings for comparison

COM1          FCB   'BAS', 0           // Basic cold start
COM2          FCB   'WAR', 0           // Basic warm start

COMST         EQU   COM1
BASIC         EQU   COM1-COMST
WARM          EQU   COM2-COMST

//
// TRCMDS - Is entered via a jump when TANBUG V1 instruction parse fails.
//          Jumped to from ERRQ. It looks for 3 letter commands
//          Note it is tried before XBUG commands. If you try to enter
//          BASIC and it's not there then you crash!!
//

TRCMDS        LDX   #BASIC             // 'BAS' command string
              JSR   STNGC              // Equality?
              BNE   TRCMD1             // No, skip
              ASL   PSTAT
              SEC                      // Else set in BASIC flag
              ROR   PSTAT
              JMP   BASRT              // And start BASIC

TRCMD1        LDX   #WARM              // Try 'WAR' command
              JSR   STNGC
              BNE   TRCMD2             // No match, skip
              LDX   #$4F               // Else recover BASIC zero page
TRCMD3        LDA   STKBSE,X
              STA   INTSL,X
              DEX                           
              BPL   TRCMD3
              ASL   PSTAT
              SEC                      // Set BASIC warm start bit
              ROR   PSTAT
              JSR   STKINI             // Initialise BASIC parameters
              JMP   BSWARM             // Go to warm start

// Following ends up at RETERR (No XBUG) or $F7F7 (XBUG)

TRCMD2        JMP   INPERR             // ($F7F7)

//
// TRYSR1 - Is executed from the Interrupt handler KBINT. In addition to
//          looking at the Microtan keyboard port, it looks at the serial
//          input port, if on, and obtains data.
//          The negative bit in PSW is set and character in Acc if keyboard
//          or TTY. Using the Micron keyboard turns off serial TTY input
//

TRYSRI        LDA   PSTAT
              AND   #$01               // Allowing serial input?
              BEQ   TRYSR1
              LDA   SERST              // Serial interrupt?
              BPL   TRYSR1             // No, go try keyboard
              PHP                      // Else, save status
              LDA   SERDT              // and data
              PHA                           
              LDA   SERCM
              PHA                           
              ORA   #$02               // Clear interrupt
              STA   SERCM
              PLA                           
              STA   SERCM              // Re-enable interrupt
TRYSR3        PLA                      // Recover character
              PLP                      // And negative bit set
TRYSR2        RTS

TRYSR1        LDA   KBREAD             // Try Microtan keyboard
              BPL   TRYSR2             // No, exit with PSW positive
              PHP                      // Else, save values
              PHA                           
              LDA   PSTAT
              AND   #$01               // Is serial input enabled?
              BEQ   TRYSR3             // No, return
              LDA   #0                 // Else, disable
              STA   PSTAT
              LDA   #$8B               // And disable interrupts
              STA   SERCM
              BNE   TRYSR3             // Unconditional to return

//
// PADINI - Is called from startup sequence and disables the keypad if
//          a TTY input is enabled
//

PADINI        STA   KBINCL             // Clear keyboard input
              LDA   PSTAT
              AND   #$01               // Is serial input enabled?
              BEQ   PADIN1             // No, skip
              LDA   #0                 // Else, disable keypad
              STA   SIMCOM
PADIN1        RTS

//
// MMANG - Memory management subroutine
//

MEMSEG        EQU   $40                // Read and write segments
MEMDAW        EQU   $41                // Data to be written
MEMDAR        EQU   $42                // Data read
MEMLO         EQU   $43                // Lo byte address
MEMHI         EQU   $44                // Hi byte address
MNGST         EQU   $FFFF

// None of these are corrupt except by use of input in BASIC
// or your own code

MMANG         PHA                      // Save Acc and Y
              TYA                           
              PHA                           
              LDA   MEMSEG             // Get mapping info
              STA   MNGST              // Set status
              LDY   #0
              PHA                      // Save management setting
              AND   #$F0
              BEQ   MMANG1
              LDA   (MEMLO),Y          // Get data
              STA   MEMDAR             // and store
MMANG1        PLA
              AND   #$0F               // Is a write required?
              BEQ   MMANG2             // No, skip
              LDA   MEMDAW             // Else, write
              STA   (MEMLO),Y
MMANG2        STY   MNGST              // Reset page 0
              PLA                           
              TAY                           
              PLA                           
              RTS                           

//
// MMINC - Calls the above, then increments memory address
//

MMINC         JSR   MMANG
              INC   MEMLO
              BNE   MMINC1
              INC   MEMHI
MMINC1        RTS

//
// CURSON / CURSOF - subroutines to turn cursor on/off at current position
//

CURSON        PHA                      // Turn on
              TYA                           
              PHA                           
              LDA   #$FF
CURSN1        LDY   VDUIND
              STA   (ICURS),Y
              PLA                           
              TAY                           
              PLA                           
              RTS                           

CURSOF        PHA                      // Turn off
              TYA                           
              PHA                           
              LDA   #$20
              BNE   CURSN1

//
// OTBUG - Old TANBUG V1
// All location addresses after this point must be preserved in
// future versions
//

// NOT SURE WHAT THIS LOT IS... NOT IN LISTING, BUT IS IN ROM?

              JSR   $D5F5
              JMP   ($0033)

              LDA   #$FF
              STA   $BFC2
              STA   $BFC3
              RTS                           

              PHA                           
              TYA                           
              PHA                           
              LDY   #$03
              STY   $BFC0
              LDA   $80
              STA   $BFC1
              LDY   #$00
              STY   $BFC0
              LDA   $81
              STA   $BFC1
              LDY   #$02
              STY   $BFC0
              LDY   #$00
              STY   $BFC0
              PLA                           
              TAY                           
              PLA                           
              RTS                           

              PHA                           
              TXA                           
              PHA                           
              LDA   #$20
              LDX   #$1F
Lbl_FB65      STA   LINBOT,X
              DEX                           
              BPL   Lbl_FB65
              PLA                           
              TAX                           
              PLA                           
              RTS                           

SPARE         RMB   145, $FF           // Output $FF gap fillers in Code

// If all went well for TANBUG V2 expanded code, the next address should
// be $FC00 for TANBUG V1 element

#else

// Start of code for TANBUG V1 is $FC00

              ORG   $FC00, CODE

#endif

//=============================================================================
//
// TANBUG (V1)
//
// The TANBUG monitor program is located in 1K bytes of read only memory
// (ROM) at the top of the 6502 microprocessor 64K byte address space
//
// TANBUG will only operate in the memory map of the microtan system, it is
// not a general purpose 6502 software and has been specifically written for
// Microtan
//
// Locations $F7F7, $F7F8 and $F7F9 are reserved for a jump to an expansion
// monitor ROM which is positioned on the expansion board (see V2 above)
//
// Locations $200 - $3FF are the visual display memory. TANBUG writes to these
// locations whenever a command is typed to the monitor
//
// Locations $100 - $1FF are used as the stack by the microprocessor
//
//=============================================================================

// TANBUG starts here on reset

#if TANBUG_V2
OLSAR         JMP   PNSUP              // Goto printer setup, returns to $FC03
#else
OLSAR         LDX   #$FF
              TXS                      // Set stack pointer to top of the stack
#endif

OLSAR1        INX
              STX   PSWBCK             // Initialise stored PSW
              JSR   BPTCLR             // Clear breakpoints
              STA   STEXT              // Set text mode

// Use table in ROM to initialise parameters (note order of table must
// correspond with the order of INTFS1 to ICURSH in RAM definitions)

#if TANBUG_V2
              LDX   #12
SETUP1        LDA   SETUP,X
#else
              LDX   #14
SETUP1        LDA   SETUP,X            // SETUP holds parameter table
#endif

              STA   INTFS,X            // Store in zero page RAM
              DEX
              BPL   SETUP1

// Determine keyboard type and set flag, note IX = $FF

              INX
TSFIV         STX   KBWRIT             // Clear keyboard write latch
              STA   KBINCL             // Clear keyboard interrupt flag
              DEX
              STX   KBWRIT             // Write to keyboard lines
              INX                      // Reset IX
              LDA   KBREAD             // Read it back
              BPL   KPCPLX             // If plus not set - alphanumeric
              INC   SIMCOM             // If set - must be keypad

#if TANBUG_V2
KPCPLX        JSR   PADINI             // Disable keypad if TTY is enabled
#else
KPCPLX        STA   KBINCL             // Clear keyboard interrupt
#endif

TBMS          LDA   HDR,X              // Display TANBUG message
              BEQ   MONTOR             // Output chars until a 0
              JSR   OPCHR
              INX
              BNE   TBMS


//
// MAIN LOOP - monitor user input and act accordingly
//

MONTOR        CLD                      // Set binary mode
              CLI
              JSR   POLLKB             // Look at keyboard
              LDA   ICHAR              // Get char
              CMP   #$21               // Less than a space - term
              BMI   MONCH1             // Else output char
ISTERM        JSR   OPCHR
              JMP   MONTOR

MONCH1        JSR   MONEN2             // Call string process
RC1           LDA   #13                // Set up CR
              BNE   ISTERM             // Unconditional branch loop

MONEN2        LDY   #0
              LDA   (ICURS),Y          // Pick up command
              TAX
              INY
              LDA   (ICURS),Y          // Peek at next char
              BPL   MULTI              // If not -ve (cursor) must be parameter
              LDA   #0                 // Else set A to zero


//
// S - SINGLE INSTRUCTION MODE COMMAND
//
// This command switches single instruction mode ON.
// Single instruction mode is a very powerful debugging aid. When set
// TANBUG executes the user program one instruction at a time, re-entering
// the monitor between each instruction and printing out the status of all
// of the microprocessor's internal registers as they were after the last
// instruction executed in the user program.
// The S command is used in conjunction with the proceed command P and the
// normal mode command N.
//

TRYS          CPX   #$53               // Was it S?
              BNE   TRYN               // No, skip
              STX   SINGLE             // Else, set single step mode
              RTS


//
// N - NORMAL MODE COMMAND
//
// The N command is the complement of the S command and is used to cancel
// the S command so that the microprocessor executes the user program in
// the normal manner without returning to the monitor between each instruction.
// Reset automatically sets the normal mode of operation.
//

TRYN          CPX   #$4E               // Command N?
              BNE   TRYP
              STA   SINGLE             // Clear single instruction mode (ACC=0)
              RTS


//
// P - PROCEED COMMAND (with no argument)
//
// This command is used to execute the next instruction when in single
// instruction mode (after the program has been started with a G command).
// The pseudo registers are reloaded and the next instruction executed.
// After execution, the microprocessor status is displayed as follows:
//	ADDR PSW SP IX IY ACC
// With no parameters entered, only the next instruction is executed
//

TRYP          CPX   #$50               // Command P with no arg?
              BNE   TRYR
              STA   PROCED             // Clear P count (A=0)
              BEQ   PROC1              // Uncond. branch proc


//
// R - REGISTER MODIFY / EXAMINE COMMAND
//
// This command is used to view / set the pseudo registers prior to running
// a program with the G command. It effectively executes a M15 command
// to examine the pseudo registers in locations $15 to $1B
//

TRYR          CPX   #$52               // Command R?
              BNE   TRYB
              STA   MODADH             // Note A=0
              LDA   #$15               // Set pseudo reg
              STA   MODADL
              JMP   REOPEN             // Jump mod memory


//
// B - BREAKPOINT COMMAND (no parameters)
//
// A breakpoint is a complementary debugging aid to single instruction mode.
// Instead of stepping singly through all instructions in a program, the
// breakpoint facility allows the user to specify the address at which he
// requires the monitor to be re-entered from his/her program. A breakpoint
// can be set just previous to where the fault is suspected to exist and the
// program started with the G command. Normal execution occurs until the
// breakpoint is reached, then the monitor is re-entered with the same status
// print-out as for single instruction mode. Any monitor commands can then be
// used and the program continued.
//
// With no parameters, the B command removes all breakpoints
//

TRYB          CPX   #$42               // Command B?
              BNE   ERRQ               // No - then error
              JSR   BPTCLR             // Else clear breakpoints
              RTS


// With no TANEX $FFF7 will respond to $F7F7
// With TANEX, this monitor can be expanded since $FFF7 jumps back to here

#if TANBUG_V2
ERRQ          JMP   TRCMDS             // Try TANBUG2 commands
#else
ERRQ          JMP   INPERR
#endif

RETERR        LDA   #$3F
              JSR   OPCHR              // Display question mark
              RTS


// If we get here command expects parameters too

MULTI         DEY
              TXA                      // Save cmd on stack
              PHA
              JSR   HEXPCK             // Pack its argument
              BNE   MOREY              // Any more data?
              PLA                      // Restore command
              BVC   ERRQ               // Error if no arg


//
// G - GO COMMAND
//
// Parameter is the address of the first instruction to run
// When executed the cursor disappears, and is restored on completion
// The program counter is initialised to the parameter address, and the
// stack pointer to $1FF (top of stack). The other registers: A, X, Y, PSW
// are loaded from the pseudo registers in the zero page ($15 - $1B)
//

              CMP   #$47               // Is it a G?
              BNE   TRYPL              // No - skip
              LDX   #0
              STX   PROCED             // Clear proceed count
              DEX                      // Set IX to $FF
GOEND         TXS                      // Reload it
              LDA   HXPKH              // Push PC high
              PHA
              LDA   HXPKL              // Push PC low
              PHA
              LDA   PSWBCK
              PHA
              DEC   RUNIND             // Clear run flag
              LDA   #$20
              LDY   VDUIND
              STA   (ICURS),Y          // Obliterate cursor
              LDX   XBCK
              LDY   YBCK
SRET          LDA   ABCK               // Set users Acc
              STA   SNMI               // Set NMI for next
              RTI                      // Goto user prog


//
// P - PROCEED COMMAND (with argument)
//
// This command is used to execute the next instruction when in single
// instruction mode (after the program has been started with a G command).
// The pseudo registers are reloaded and the next instruction executed.
// After execution, the microprocessor status is displayed as follows:
//	ADDR PSW SP IX IY ACC
// If a parameter less than or equal to $FF is entered, then this number
// of instructions are executed before returning to the monitor
//

TRYPL         CMP   #$50               // Command P with arg?
              BNE   TRYM
              LDA   HXPKL              // Set P count
              STA   PROCED
PROC1         LDA   PCHBCK             // Restore users PC
PNOARG        STA   HXPKH
              LDA   PCLBCK
              STA   HXPKL
              LDX   SPBCK              // Set IX to users SP
              JMP   GOEND              // Then back to user


//
// M - MODIFY/EXAMINE MEMORY COMMAND
//
// Parameter is the memory address to be examined / modified
// TANBUG shows memory address and location, and awaits user input:
// - any terminator, location not altered and closes command
// - value followed by terminator, modifies location with value, if terminator is
//	CR, closes command
//	SPACE, shows same location
//	LF, shows next memory location
//	ESC, shows previous memory location
//

TRYM          CMP   #$4D               // Command M address?
              BNE   ERRQ               // No - error
EQPT          LDA   #$2C
              JSR   OPCHR              // Output a comma
              LDY   #0
              LDA   (HXPKL),Y          // Pick up value
              JSR   HEXPNT             // and display it
              PLA                      // Pop stack return
              PLA
              LDA   #$2C               // Load comma
              JMP   ISTERM             // and back to monitor


// If here then there is a second parameter

MOREY         CPX   #$2C               // Was term a comma?
              BEQ   GETPT2             // Yes - continue
LINKPH        PLA                      // Else pull command
LINKR         JMP   ERRQ               // and give error

GETPT2        LDA   HXPKL              // No comma - store previous
              STA   MODADL             // in MODADL & MODADH
              LDA   HXPKH
              STA   MODADH
              JSR   HEXPCK             // Pack next value
              BNE   MOREY1             // Not cursor - more yet
              PLA                      // Else pull command
              BVC   LINKR              // No argument? - error


//
// L - LIST COMMAND (see LISTIT routine below)
//

              CMP   #$4C               // Command L?
              BEQ   LISTIT             // Yes - list it


//
// O - OFFSET COMMAND
//
// The offset command is a program writing aid. It calculates branch offsets
// for the user for incorporation as arguments in branch instructions.
// Parameters are the address of the branch opcode, and the address of the
// destination. The result is the value to be used as the argument for the
// branch.
// Note that the maximum branch is $7F forwards, or backwards
//

              CMP   #$4F               // Command O?
              BNE   TRYBPT
              LDA   HXPKL              // Get branch dest
              SEC
              SBC   #2                 // Adjust for branch code
              BCS   NOTOPO
              DEC   HXPKH
NOTOPO        SEC
              SBC   MODADL             // Subtract source
              TAX                      // Hold result in IX
              LDA   HXPKH              // Subtract high byte
              SBC   MODADH
              TAY                      // Store in IY
              TXA                      // Get low byte
              BMI   RNGNG              // If -ve branch
              TYA                      // If +ve look at high
              BNE   LINKR              // Not 0 - then error
              BEQ   PNTITO             // If ok continue

RNGNG         INY                      // If -ve, then high is $FF
              BNE   LINKR
PNTITO        LDA   #$3D
              JSR   OPCHR              // Ok - display equals sign
              TXA
              JSR   HEXPNT             // ... and the value
              RTS


//
// B - BREAKPOINT COMMAND (with parameters)
//
// As described above.
// Parameters are the address of any opcode instruction, and the number of the
// breakpoint to be set (0 to 7)
//

TRYBPT        CMP   #$42               // Command B?
LINK1         BNE   LINKR              // No - error
              LDA   HXPKL              // Maximum breakpoint code is 7
              BMI   LINKR
              CMP   #8                 // If greater - then error
              BPL   LINKR
              ASL   A                  // Double A
              TAX                      // Set IX for indexed addressing
              LDA   MODADL             // Store breakpoint address
              STA   BPTLO,X
              LDA   MODADH
              STA   BPTHI,X
JPRTRN        RTS


//
// L - LIST MEMORY COMMAND
//
// Parameters are start memory address, and number of lines of 8 consecutive
// memory locations to show
// If 0 lines are requested, 256 lines will be shown
// TANBUG pauses between each line to permit the user to read the output (V1)
//

LISTIT        JSR   OUTPCR             // Output carriage return
NXLI          LDY   #0
              LDA   MODADH             // Display address
              JSR   HEXPNT             // High byte ...
              LDA   MODADL
NXLIST        JSR   HEXPNT             // Output low byte
              LDA   #$20
              JSR   OPCHR              // Output a space
              LDA   (MODADL),Y         // Display memory locations
              INY
              CPY   #9                 // For eight memory locations
              BMI   NXLIST
              DEC   HXPKL              // Decrement line count
              BEQ   JPRTRN             // O? Return via CR (note requesting 0 lines gives 256)

DELX1         NOP                      // Padding replacing old delay routine
              NOP
              NOP
              NOP
              NOP
              NOP

              LDA   MODADL             // Now adjust the address, for next 8 locations
              CLC
              ADC   #8
              STA   MODADL
              BCC   LISTIT             // If Carry set
              INC   MODADH             // then increment high byte
              JMP   LISTIT


// If we get here there  is a third parameter

MOREY1        CPX   #$2C               // Comma?
              BEQ   TERMOK
ERJUM2        JMP   LINKPH             // No - then error

TERMOK        LDA   HXPKL              // Else store parameter
              STA   COPL
              LDA   HXPKH
              STA   COPH
              JSR   HEXPCK             // then pack new para
              BNE   ERJUM2             // error not term
              PLA


//
// M - MEMORY MODIFY / EXAMINE (modify location)
//
// If user has entered a value to be entered into the momeory location
// then this routine processes it
//

              CMP   #$4D               // Command M?
              BEQ   MEM100             // Yes - modify memory


//
// C - COPY COMMAND
//
// The copy command allows copying of the contents of one block of memory to
// another. The parameters are start address source, end address source and
// start address destination. These are copied to MODADL, COPL, and HXPKL
// respectively.
//

              CMP   #$43               // Command C?
              BNE   LINK1              // No - then error
              BVC   LINK1              // Argument? Error if not
              LDY   #0
NXCOP         LDA   (MODADL),Y         // Copy from source start address
              STA   (HXPKL),Y          // to destination
              LDA   COPH               // Check if we've reached end address
              CMP   MODADH
              BNE   ICMCOP
              LDA   COPL
              CMP   MODADL
              BEQ   ENDLS
ICMCOP        INC   MODADL             // No, then increment source
              BNE   NOHIH1
              INC   MODADH
NOHIH1        INC   HXPKL              // and increment destination
              BNE   NXCOP
              INC   HXPKH
              BNE   NXCOP              // Destination can not roll over top of memory
              RTS


// Modify memory location (M command above)

MEM100        LDX   ICHAR              // Get input character in IX
              CPX   #$20               // Was it a space?
              BEQ   REOPEN             // Yes - reopen
              BVC   NOENT              // Branch if nothing
              LDA   HXPKL              // Else enter data
              LDY   #0
              STA   (MODADL),Y

NOENT         CPX   #$0A               // Was LF typed?
              BEQ   WASLF              // Yes - then process it

              CPX   #$1B               // Was it ESC?
              BNE   ENDLS              // No - then return

ESCIT         DEC   MODADL             // Decrement memory modify address
              LDA   MODADL
              CMP   #$FF
              BNE   REOPEN
              DEC   MODADH
              JMP   REOPEN

WASLF         INC   MODADL             // Increment memory address
              BNE   REOPEN
              INC   MODADH             // and high byte, if required

REOPEN        JSR   OUTPCR             // Output carriage return
              LDA   #$4D
              JSR   OPCHR              // Display 'M'
              LDA   MODADH             // and address
              JSR   HEXPNT
              LDA   MODADL
              JSR   HEXPNT
              JMP   MONEN2             // Check next command


//
// POLL KEYBOARD
//
// Gets a character from either the simple keypad or ASCII keyboard. Key
// value is stored in ICHAR ($01)
//

POLLKB        LDA   #0
              PHA                      // Push 0 - shift indicator
PLKB1         STA   ICHAR              // Set ICHAR to zero
              CMP   SIMCOM             // Check if keypad
              BNE   SIMPLE

// ASCII keyboard routine

WAIT1         CMP   ICHAR              // Else wait for interrupt
              BEQ   WAIT1
PLKEND        PLA                      // Pop indicator
ENDLS         RTS

// Simple keypad routine

SIMPLE        LDA   #$0F
              STA   KBWRIT             // Enable all keyboard lines
              LDA   KBREAD             // Look at keyboard lines
              BNE   SIMPLE             // Key down? - wait till up

              LDX   #$40               // Debounce it
DEBOUN        DEY
              BNE   DEBOUN
              DEX
              BNE   DEBOUN

PLK1          LDY   #$FF               // Now poll the keypad properly
              PLA                      // Peek at shift indicator
              PHA
              BEQ   NOSHIF             // If shift set modify IY
              LDY   #$13
NOSHIF        LDX   #8                 // Set IX - keyboard drive
PLK2          STX   KBWRIT             // Drive keyboard lines
              LDA   KBREAD             // Get result
              BNE   ACHAR              // Not 0 - a char - so skip
              INY                      // Else adjust IY
              INY
              INY
              INY
              INY
              TXA
              LSR   A                  // Shift IX right
              TAX
              BEQ   PLK1               // If zero repeat
              BNE   PLK2               // Else next line

// If we get here a key has been pressed

ACHAR         INY
              LSR   A                  // Which key of 5?
              BCC   ACHAR              // C set? That's the key
              LDA   CHRTBL,Y           // Get ASCII equivalent
              STA   ICHAR              // And put in ICHAR
              BNE   PLKEND             // If zero - shift
              PLA                      // Pull shift
              EOR   #$FF               // Change shift state
              PHA                      // Push shift
              JMP   SIMPLE             // ... and continue

// Character look up table for ASCII equivalent

CHRTBL        FCB   $33, $37, $42, $46, $3F, $32, $36, $41
              FCB   $45, $0D, $31, $35, $39, $44, $0A, $30
              FCB   $34, $38, $43, $00, $2C, $52, $4C, $4E
              FCB   $3F, $32, $43, $49, $53, $20, $31, $4F
              FCB   $1B, $47, $7F, $30, $34, $50, $4D, $00


//
// OUTPUT A CARRIAGE RETURN
//
// This subroutine causes the display to scroll up one line by outputting
// a carriage return to the screen. It also reinstates the cursor when a
// user program is run with the G command
// ACC is corrupted, IX and IY are preserved
//

OUTPCR        LDA   #13                // Output a CR

//
// OUTPUT A CHARACTER
//
// Displays the character in the ACC on the screen
// ACC is corrupted, IX and IY are preserved
//

#if TANBUG_V2

OPCHR         JMP   OPTYPE             // Determine type of output and process
              NOP

// SCREEN - now the subroutine which outputs to the screen. Character in OCHAR

SCREEN        STA   OCHAR
              LDY   VDUIND             // Get cursor position
              LDA   #$20
              STA   (ICURS),Y          // and erase cursor
              LDX   OCHAR              // Get char in IX
              CPX   #12
              BNE   TRYDEL
SCREN3        JSR   CLSC               // Clear screen
DICURS        LDA   #$FF
              STA   (ICURS),Y          // Restore cursor
              STY   VDUIND             // and save VDU index
              RTS

TRYDEL        CPX   #$7F               // Is it delete?
              BNE   TRYCR              // No, try CR
              JSR   CURDL              // Else, delete
              BPL   DICURS             // Unconditional

TRYCR         CPX   #13                // Is it a CR?
              BEQ   NXLINE
              TXA                      // No, then output character
              STA   (ICURS),Y
              INY                      // Increment vdu index
              CPY   #32                // End of line?
              BNE   DICURS             // No, so output cursor
NXLINE        LDA   #32                // Else, add 32 (chars) to line pointer
              CLC
              ADC   ICURS
              STA   ICURS
              LDA   ICURS+1
              ADC   #0
              STA   ICURS+1
              LDY   #0                 // Reset Y
              CMP   #4                 // Off end of screen?
              BNE   DICURS             // No, cursor up
              JSR   SCROLL             // Else, scroll
              BEQ   DICURS             // Unconditional

              FCB   $FA                // Padding?
              FCB   $FA
              FCB   $FA
              FCB   $FA
              FCB   $FA

#else

OPCHR         STA   OCHAR              // Save the character
              TXA                      // Save IX and IY
              PHA
              TYA
              PHA
              LDY   VDUIND             // Get cursor position
              LDA   #$20
              STA   (ICURS),Y          // and erase cursor
              LDX   OCHAR              // Get char in IX
              CPX   #$7F               // Is it delete?
              BNE   TRYCR
              DEY                      // Decrement vdu index
              BPL   DODEL
ZERCUR        LDY   #0                 // If negative set zero
DODEL         LDA   #$FF
              STA   (ICURS),Y          // Display cursor
              STY   VDUIND             // and save index
              PLA                      // Restore registers and exit
              TAY
              PLA
              TAX
              RTS

TRYCR         CPX   #13                // Is it a CR?
              BEQ   DOCR
              TXA                      // No, then output character
              STA   (ICURS),Y
              INY                      // Increment vdu index
              CPY   #32                // End of line?
              BMI   DODEL              // No - then tidy up and exit
DOCR          LDX   #0                 // Scroll line
LOWBLK_V1     LDA   VDUFST,X           // Do in two blocks
              STA   VDUSTT,X
              INX
              BNE   LOWBLK_V1
HIBLK_V1      LDA   VDUTOP,X
              STA   VDUMID,X
              INX
              CPX   #$E0
              BNE   HIBLK_V1

              LDA   #$20
              TAY
MORSP_V1      DEY                      // Fill line with spaces
              STA   (ICURS),Y
              BNE   MORSP_V1
              BEQ   ZERCUR             // When done, tidy up

#endif


//
// KEYBOARD INTERRUPT
//
// Note this is entered via jump instruction stored in RAM location INTFS1
// so that user can access interrupts quickly.
// A reset will always initiate INTFS1
//

KBINT         PHA                      // Save accumulator
              CLD                      // Set binary mode
              TXA                      // Save IX
              PHA
              TSX                      // Get SP in IX
              INX                      // Point IX to old PSW
              INX
              INX
              LDA   STKBSE,X           // and get the PSW
              AND   #$10               // Was it a break?
              BNE   BRKP               // Yes - then process it
              PLA                      // Else restore IX
              TAX

#if TANBUG_V2
              JSR   TRYSRI             // Read keyboard / serial input
#else
              LDA   KBREAD             // Read keyboard
#endif

              BMI   WASKB              // If -ve, then was keyboard
USER          PLA                      // Else restore A
              JMP   INTSL              // and check the slow interrupt
                                       // Note that INTSL1 normally holds an RTI,
                                       // unless the user has modified it

WASKB         AND   #$7F               // Mask top bit
              STA   ICHAR              // and store in ICHAR

#if TANBUG_V2
              JSR   CONCHK             // Check for control characters
#else
              STA   KBINCL             // Clear keyboard interrupt flip-flop
#endif

              CMP   #$1B               // Was key ESC?
              BNE   USER               // No - then normal return
              LDA   RUNIND             // Else - check if user program running?
              BNE   USER               // No - then normal return
              STA   PROCED             // Else clear proceed count
              BEQ   ACTBP              // and unconditionally branch to break


//
// BREAK PROCESSING
//
// Note the user should not set a break at own break or in this interrupt routine
// else crashes. Adjust PC to return to instr must subtract 3
//

BRKP          INX                      // address PC L
              SEC                      // set C
              LDA   STKBSE,X           // get PCL
              SBC   #2                 // subtract 3
              STA   STKBSE,X           // put it back
              BCS   NOROLL             // C set? NOHI byte
              INX                      // else address PCH
              DEC   STKBSE,X           // dec PCH
NOROLL        PLA                      // pull IX
              TAX                      // restore it
ACTBP         PLA                      // pull accumulator
              STA   ABCK               // back it up
              JSR   BPTREM             // restore user code
NOROL         JMP   NMNT1              // service break


//
// DISPLAY HEX VALUES
//
// Takes a hex value stored in accumulator and displays as two hex characters
// Registers ACC and IX are corrupted
//

HEXPNT        PHA                      // Save value of char
              LDX   #1
              LSR   A                  // Get top part by multiple shifts
              LSR   A
              LSR   A
              LSR   A
PNT2          CLC
              ADC   #$30               // Add hex 30
              CMP   #$3A               // More than 9?
              BMI   PNT1               // No - then display it
              CLC
              ADC   #7                 // Adjust again
PNT1          JSR   OPCHR              // and display it
              DEX
              BPL   MOR1               // -ve? - end else low bit
              RTS

MOR1          PLA                      // Recover character
              AND   #$0F               // Clear unwanted bits
              BPL   PNT2               // and branch unconditionally


//
// PACK HEX CHARACTERS
//
// This subroutine reads hex characters from the bottom line of the display
// and packs them up into two eight bit binary values.
// On exit, caused by any non-hex character, HXPKL and HXPKH contain a two
// byte number.
// Registers are not saved. On exit IY points to the last character. The zero flag
// is clear if the terminating character was the cursor, else set. The overflow
// flag is set if this subroutine encountered some hex data.
//

HEXPCK        LDA   #0                 // Clear accumulator
              PHA                      // Push as PSW
              STA   HXPKL              // Clear parameters
              STA   HXPKH
NXHX          INY
              LDA   (ICURS),Y          // Get character from display
              TAX                      // Save in IX
              SEC
              SBC   #$30               // Subtract hex 30 to give 0 to 9
              BMI   ENDTS
HX1           CMP   #$0A               // Is it 0 to 9?
              BMI   HX2                // Yes - then pack it
              SEC                      // Else adjust by hex 11
              SBC   #$11
              BMI   ENDTS              // Goto carry setup
HX3           CLC                      // Else add 9
              ADC   #$0A
              CMP   #$10               // If more than 15, then exit
              BMI   HX2
ENDTS         PLP                      // Deal with V flag
              CPX   #$FF               // Check if cursor
              RTS

// Note character is in IX on exit, return does not affect zero flag

HX2           LDX   #4
HX5           ASL   HXPKH              // Shift result left 4 bits
              ASL   HXPKL
              BCC   HX4
              INC   HXPKH
HX4           DEX                      // Until IX zero (4 times)
              BNE   HX5
              CLC
              ADC   HXPKL              // Add in character
              STA   HXPKL
              PLA                      // Get pseudo PSW
              LDA   #$40               // Set V bit
              PHA                      // and push it
              BNE   NXHX               // Next ...


//
// NON-MASKABLE INTERRUPT
//
// This routine handles single instruction mode
//

NMNT          STA   ABCK               // Save accumulator
              CLD                      // Set binary mode
              LDA   SINGLE             // Test single instruction mode
              BNE   NMNT1
              TXA                      // If not save IX
              PHA
              JSR   BPSET              // Set breakpoints
              PLA                      // Restore IX
              TAX
              LDA   ABCK               // Restore accumulator
              RTI

NMNT1         LDA   PROCED             // Get proceed count
              BEQ   ZERBCK             // If zero, then break
              DEC   PROCED             // else decrement count
              JMP   SRET               // RTS via single test

ZERBCK        INC   RUNIND             // Set not running
              PLA                      // Pop breakpoint PSW to Acc
              STA   PSWBCK             // and back it up
              PLA
              STA   PCLBCK             // Ditto PCL
              PLA
              STA   PCHBCK             // Ditto PCH
              STX   XBCK               // Save IX
              STY   YBCK               // and IY
              TSX
              STX   SPBCK              // and operators SP
PSEUD         JSR   OUTPCR
              LDA   PCHBCK
              JSR   HEXPNT             // Display PC
              LDA   PCLBCK
              JSR   HEXPNT
              LDY   #0
PSNX          LDA   #$20               // Display two spaces
              PHA
              JSR   OPCHR
              PLA
              JSR   OPCHR
              LDA   PSWBCK,Y           // Then display all registers
              JSR   HEXPNT
              INY
              CPY   #5
              BMI   PSNX
              JMP   RC1


//
// CLEAR BREAKPOINTS
//
// Sets all breakpoint addresses to zero
//

BPTCLR        LDX   #$1F               // Set count for 8 breakpoints
              LDA   #0
BPTCL1        STA   BPTLO,X            // Set to zero
              DEX
              BPL   BPTCL1             // For all locations
              RTS


//
// REMOVE BREAKPOINTS
//
// Writes back the user's original code
//

BPTREM        TXA
              PHA
              LDX   #$0E               // Set count for 8 breakpoints

BPTRM1                                 //
#if TANBUG_V2
              LDA   BPTLO,X
              ORA   BPTHI,X
              BEQ   BPTRM2
#endif

              LDA   BPTCOD,X           // Load saved old instruction
              STA   (BPTLO,X)          // Write to program memory
BPTRM2        DEX
              DEX
              BPL   BPTRM1             // For all locations
              PLA
              TAX
              RTS


//
// SET BREAKPOINTS
//
// Examines entered breakpoint addresses, stores the present instruction
// and writes BRK opcode to program memory
//

BPSET         LDX   #$0E               // Set count for 8 breakpoints

BPS1                                   //
#if TANBUG_V2
              LDA   BPTHI,X
              ORA   BPTLO,X
              BEQ   BPS2
#endif

              LDA   (BPTLO,X)          // Get user's instruction
              STA   BPTCOD,X           // Store it
              LDA   #0
              STA   (BPTLO,X)          // Set BRK instruction...
BPS2          DEX
              DEX
              BPL   BPS1               // Repeat until done
              RTS


#if TANBUG_V2
// Setup table for V2 in low ROM at $F831
#else
//
// SETUP TABLE
//
// Copy in ROM of the initial settings for zero page variables
//

SETUP         JMP   KBINT
              JMP   NMNT
              FDB   LINBOT             // ICURS setting
              FCB   1                  // RUNIND
              FCB   0                  // SINGLE
              FCB   0                  // PROCED
              FCB   0                  // SIMCOM
              FCB   $40                // Slow Interrupt (RTI instruction)
#endif


// Header message

HDR           FCB   13                 // CR
              FCC   'TANBUG'
              FCB   13, 0              // CR, end of line terminator

#if TANBUG_V2
              FCB   $FA                // Padding
              FCB   $FA
              FCB   $FA
#else
              FCB   0, 0               // Padding
#endif


//
// RESET / INTERRUPT VECTORS
//
// These vectors must appear at the top of the ROM space
//

              JMP   RETERR             // $FFF7, also mapped to $F7F7 if no TANEX
              FDB   NMIJP              // Non Maskable Interrupt vector
              FDB   OLSAR              // Reset vector
              FDB   INTFS              // Interrupt vector

              END

