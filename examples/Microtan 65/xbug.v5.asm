//=============================================================================
//
// MICROTAN 65 - XBUG V5
//
//=============================================================================

//
// Note: some versions of XBUG may be slightly different from this listing
//       due to version 5 mods being patched rather than assembled
//

// DEFINE ZERO PAGE VARIABLES

ICHAR         EQU   $01                // ASCII character
OCHAR         EQU   $02                // Temp char store
VDUIND        EQU   $03                // Display index
ICURS         EQU   $0A                // Cursor index
RUNIND        EQU   $0C
HXPKL         EQU   $13                // Hexpack store
HXPKH         EQU   $14
MODADL        EQU   $1C
MODADH        EQU   $1D
COPL          EQU   $1E
COPH          EQU   $1F

CSUM          EQU   $13                // Same as HXPKL
ACMDL         EQU   $31
ACMDH         EQU   $33
IPMODL        EQU   $35
DSPCNT        EQU   $37
NOPAUS        EQU   $39
FSTSLO        EQU   $50
CHAR          EQU   $51
NAME          EQU   $52

// XBUG FIX DEFINITIONS

TYFG          EQU   $31                // Same as ACMDL

// DEFINE OTHER CONSTANTS

LINBOT        EQU   $03E0

// DEFINE ROUTINES USED

RC1           EQU   $FC4B
RETERR        EQU   $FC89
POLLKB        EQU   $FDFA
OUTPCR        EQU   $FE73
OPCHAR        EQU   $FE75
HEXPNT        EQU   $FF0B
HEXPCK        EQU   $FF28


// Start of XBUG code block

              ORG   $F000, CODE

//
// Subroutine jump table. Located at start of XBUG ROM. Jump addresses
// will not change with future XBUG versions. Allows BASIC to remain
// unmodified even if changes to XBUG occur
//

JINITC        JMP   INITCS
JAPND         JMP   APND
JFNAME        JMP   FNAME
JOP32M        JMP   OP32M
JRCFN         JMP   RCFN
JRECOR        JMP   RECORD
JINDCM        JMP   INDCMP
JOBLCU        JMP   OBLCUR
JRDWD         JMP   RDWD
JRERD         JMP   RERD
JRCDBL        JMP   RCDBLK
JRDDBL        JMP   RDDBLK
JSPERR        JMP   SPERR

//
// PROMPT
//
// Used in XBUG sub programs to replace the cursor with a ! to distinguish
// from TANBUG
//

PROMPT        LDA   #$21
              LDY   VDUIND
              STA   (ICURS),Y
              RTS

//
// HXRDAS
//
// Reads a string $HHHH. On exit overflow is clear if there are any errors
// at all - first char not $, no data, or more than four hex chars.
// On exit, X contains the next illegal encountered char, Y points to the
// char before the $. HXPCKL,H contained the packed value
//

HXRDAS        INY                      // Point to char
              LDA   (ICURS),Y
              TAX
              CPX   #$24               // Should be a $
              BNE   TRYPRI
              TYA                      // Get cursor index
              ADC   #$05               // Add 5
              STA   OCHAR              // And store for length check
              JSR   HEXPCK             // Pack the arg (via TANBUG routine)
              BVC   HXRDER             // Error if none
              CPY   OCHAR              // Check length, too many digits?
              BCC   HXRDOK             // No, OK, exit overflow clear
HXRDER        CLV                      // Else, error exit overflow clear
HXRDOK        RTS

TRYPRI        CPX   #$27               // Is it a prime?
              BEQ   PRIME              // Yes, go do
              CPX   #$60               // Else is it inverse prime?
              BNE   HXRDER             // No, error
PRIME         INY                      // Else, get ASCII char
              LDA   (ICURS),Y
              STA   HXPKL
              LDA   #$00
              STA   HXPKH              // and store it
              INY
              LDA   (ICURS),Y
              TAX                      // Get next char in X
              BIT   TRYPRI             // Set V flag for OK
              RTS

//
// MODBIT
//
// Reads command mode type from X and sets ACMDL,H in a corresponding
// bit pattern. COPH is set with the opcode offset
//

MODBIT        LDA   OFSTBL,X           // Get opcode offset
              STA   COPH               // and save in COPH
              LDA   #0                 // Initialise mode flag words
              STA   ACMDH
              SEC
MDSET         ROL   A                  // Double value
              ROL   ACMDH              // Set appropriate bit
              DEX
              BPL   MDSET
              STA   ACMDL              // Store the lo part
              RTS

//
// PRTSTR
//
// Prints the hex instruction parts, stores the same and increments PC
// stored in MODADL
//

PRTSTR        LDY   #0
              STA   (MODADL),Y         // Store the number
              CMP   (MODADL),Y         // Did it go in?
              PHA
              BEQ   WENTIN             // Yes, ok, else tell output
              LDA   DSPCNT             // Disable the men check
              BNE   WENTIN             // if we are disassembling
              JSR   OUTPCR
              JSR   MERR
              JSR   OUTPCR
WENTIN        INC   MODADL             // Increment the address
              BNE   NOHM
              INC   MODADH
NOHM          PLA
              JSR   PRBITE             // Print the number
              RTS

//
// CHKARG
//
// On entry X contains the mnemonic table index, ACMDL,H contain input mode.
// On exit, if any illegality is detected Carry is clear. It is set if OK.
// On exit, A holds the opcode, X index as input, ACMDH,L as input, MODADL,H
//  the PC, HXPKH the required argument
//

CHKARG        LDY   #$FF               // Use Y to index legality table
              TXA
NXLGL         INY
              CMP   LEGTBL,Y           // Access the words
              BCS   NXLGL              // which says what's legal

// Y Now contains the index to legal words
// X still holds cmd and opcode index

              LDA   ACMDH              // Hi part mode legal?
              AND   LEGH,Y
              BNE   NM4                // Yes, continue
              LDA   ACMDL              // Else, try lo
              AND   LEGL,Y
              BEQ   EROUT              // If illegal, error

// Ok the modes legal - we can get opcode
// X holds opcode index. A holds the mode of entered instruction
// First perform frigs for the instructions which don't fit into the
// general pattern. Y indexes legal table

MODLEG        CPY   #1                 // Type 1 instruction?
              BNE   NM4                // No, skip
              CMP   #4                 // Mode 2 input?
              BEQ   NM3                // Yes go frig the opcode
              CMP   #$80               // Mode 7 input?
              BNE   NM4                // No, skip else
              LDA   #0                 // Frig type 1 mode 7 to be mode 9
              STA   ACMDL
              LDA   #2
              STA   ACMDH
              LDA   #24                // and set dec opcode offset
              BNE   NORMP1             // Unconditional

NM3           LDA   #8                 // Else frig mode 2 to add 8
NORMP1        STA   COPH               // Instead of 0 for M2
NORMOP        LDA   OPCOD,X            // Get opcode from table
              CLC                      // Add the offset for the mode
              ADC   COPH
              CPY   #5                 // Is it a LDX instruction?
              BNE   PCOPJ              // No, skip
              CMP   #$BA               // Else, is it opcode $BA
              BNE   PCOPJ              // No, skip
              LDA   #$BE               // But if so, frig it
PCOPJ         SEC                      // Clear error flag
              RTS

NM4           CPX   #46                // Dec. further frigs, BR,JMP,JSR?
              BCC   NORMOP             // No, normal procedure
              LDA   ACMDH              // Mode = 8?
              CMP   #1
              BEQ   NORMOP             // Yes, normal
              LDA   #$20               // Else, set mode 5
              STA   ACMDL
              LDA   #0                 // and instruction offset to 0
              CPX   #$30               // Was it branch?
              BCC   NORMP1             // No, normal almost
              LDA   #8                 // Else set mode 3
              STA   ACMDL
              LDA   HXPKL              // Calculate branch offset
              SEC
              SBC   #2                 // Sub the instruction offset
              BCS   NODB
              DEC   HXPKH
NODB          SEC                      // Sub the source address
              SBC   MODADL
              PHA                      // Temp save the result
              LDA   HXPKH              // Hi part
              SBC   MODADH
              TAY                      // Save this in Y
              PLA                      // Recover lo byte
              BPL   PL                 // If 0 or plus branch
              INY                      // Else inc hi part
PL            CPY   #0                 // Offset too big?
              BEQ   OFOK               // No, store it
EROUT         CLC                      // Error, set error flag
              RTS

OFOK          STA   HXPKL              // Store it
              // Hi part immaterial
              LDA   OPCOD,X            // Get the opcode in A
              SEC                      // Clear the errors
              RTS

//
// PNTINS
//
// Prints address, opcode, args
// $3FC-$3FE on bottom line of screen at RHS
// On entry, cursor is assumed to be at line start
// ACMDH,L contains instruction mode, A the opcode
//

PNTINS        PHA                      // Save opcode
              JSR   PADDR
              INC   VDUIND             // Skip a space
              PLA                      // Get opcode again
              STA   $03FC              // Show ASCII equivalent on screen
              JSR   PRTSTR             // Print and store it
              LDA   ACMDL
              AND   #3                 // Mode 0 or 1?
              BNE   ASRT               // Yes, go tidy up
              LDA   HXPKL              // Else, store ind print HXPKL
              STA   $03FD              // Show ASCII equivalent on screen
              JSR   PRTSTR
              LDA   ACMDL
              AND   #$60               // Mode 5, 6, 8, 9?
              BNE   THIRMD             // Yes, third byte of print
              LDA   ACMDH
              AND   #3
              BEQ   ASRT               // Else, tidy up
THIRMD        LDA   HXPKH
              STA   $03FE              // Show ASCII equivalent on screen
              JSR   PRTSTR
ASRT          RTS

//
// OFSTBL
//
// Contains numeric offsets to be added to basic opcode sets for
// each mode
//

OFSTBL        FCB   0, 8, 0, 4, 20, 12, 28, 20, 32, 24, 16, 0

//
// LEGTBL
//
// The legality table works as follows:
// LEGTBL contains an index to the chaacter table FRSCH etc
// LEGH and LEGH contain bit patterns indicating which modes are legal for
// the group of instructions with an index less than that addressing FRSCH
//

LEGTBL        FCB   25, 32, 36, 38, 40, 41, 42, 43, 44, 45, 46, 47, 56

// LEG / LEGL

LEGH          FCB   %0000.0000
              FCB   %0000.1110
              FCB   %0000.0000
              FCB   %0000.0000
              FCB   %0000.0000
              FCB   %0000.0010
              FCB   %0000.0000
              FCB   %0000.1110
              FCB   %0000.0000
              FCB   %0000.0000
              FCB   %0000.0000
              FCB   %0000.0001
              FCB   %0000.0000

LEGL          FCB   %0000.0001
              FCB   %1111.1100
              FCB   %0111.1010
              FCB   %0010.1100
              FCB   %0111.1000
              FCB   %1010.1100
              FCB   %0111.1100
              FCB   %0111.1000
              FCB   %1010.1000
              FCB   %0011.1000
              FCB   %0010.1000
              FCB   %0010.1000
              FCB   %0010.1000

//
// FRSCH, SECCH, THIRCH
//
// Instruction mnemonic tables and opcode lookup. Tables are first, second
// and third letters of the command and base opcode. Note order is important,
// and ties in with LEGTBL too
//

              //     0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF01234567
FRSCH         FCB   'BCCCCDDIINPPPPRRSSSTTTTTTAACELOSALRRCCIDLLSSSBJJBBBBBBBB', 0
SECCH         FCB   'RLLLLEENNOHHLLTTEEEAAYSXXDNMODRBSSOOPPNEDDTTTIMSCCEMNPVV'
THIRCH        FCB   'KCDIVXYXYPAPAPISCDIXYAXASCDPRAACLRLRXYCCXYAXYTPRCSQIELCS'

// OPCODE - opcodes table

OPCOD         FCB   $00, $18, $D8, $58, $B8, $CA, $88, $E8
              FCB   $C8, $EA, $48, $08, $68, $28, $40, $60
              FCB   $38, $F8, $78, $AA, $A8, $98, $BA, $8A
              FCB   $9A, $61, $21, $C1, $41, $A1, $01, $E1
              FCB   $02, $42, $22, $62, $E0, $C0, $E2, $C2
              FCB   $A2, $A0, $81, $82, $80, $20, $4C, $20
              FCB   $90, $B0, $F0, $30, $D0, $10, $50, $70

//
// PRDEX, PRTBL
//
// PRDEX has pointers to command argument text strings
// PRTBL holds the text strings
//

PRDEX         FCB   0, 1, 3, 6, 8, 12, 14, 18, 22, 26, 30, 36

PRTBL         FCB   '',      0
              FCB   'A',     0
              FCB   '#$',    0
              FCB   '$',     0
              FCB   '$,X',   0
              FCB   '$',     0
              FCB   '$,X',   0
              FCB   '$,Y',   0
              FCB   '($)',   0
              FCB   '$,Y',   0
              FCB   '($),Y', 0
              FCB   '($,X)', 0

//
// LINE BY LINE ASSEMBLER MAIN PROGRAM
//

DISPLP        JSR   OPCHAR             // Output character
PMPIT         JSR   PROMPT             // Change prompt
PLKKIT        JSR   POLLKB             // Await a character
              LDA   ICHAR
              CMP   #$1B               // Was it ESC?
              BNE   NOESC              // No, skip
              RTS                      // Else, return to monitor

NOESC         CMP   #13                // Was it CR?
              BEQ   PRCODE             // If so, process
              CMP   #$7F               // Was it DEL?
              BNE   UPARTS             // No, branch
              LDY   #$0C
              CPY   VDUIND             // No DEL if cursor base
              BEQ   PLKKIT
              BNE   DISPLP

UPARTS        CMP   #$5E               // Was it up arrow?
              BNE   NOARW              // No, skip
              DEC   MODADL             // Else decrement PC
              LDA   #$FF
              CMP   MODADL
              BNE   LINENT
              DEC   MODADH
              JMP   LINENT

NOARW         CMP   #10                // Was it LF?
              BNE   DISPLP             // No, print it and look for next
              INC   MODADL             // Else, increment PC
              BNE   LINENT
              INC   MODADH
              BNE   LINENT             // Print PC then next command

PRCODE        LDY   #0                 // Clear mem err inhibit flag
              STY   DSPCNT
              LDY   #12                // Set input pointer
              LDA   (ICURS),Y          // Get first on line
              CMP   #';'               // Was it a ';', comment?
              BEQ   LINENT             // Yes, ignore line
              CMP   #$2A               // Was it program counter command '*'
              BNE   NASTY              // No, skip
              INY                      // Else, next character
              LDA   (ICURS),Y
              CMP   #'='               // Must be '='
              BNE   ASERR              // No, so error
              JSR   HXRDAS             // Else, read hex argument
              BVC   ASERR              // Error if illegal argument
              CPX   #'!'               // Error if cursor '!' not next?
              BNE   ASERR
              JSR   HXTOMO             // Transfer HXPCK to MODAL

LINENT        JSR   OUTPCR             // Print the PC
              JSR   PADDR
              LDA   #$20               // Print a space
              JSR   OPCHAR
              LDY   #$00
              LDA   (MODADL),Y         // Get locations contents
              JSR   HEXPNT             // and print it
              JSR   OBLCUR
              LDY   #12
              STY   VDUIND
              BNE   PMPIT

NASTY         DEY                      // Look for a direct number entry
              JSR   HXRDAS             // Is it one?
              LDY   #12                // Reset Y
              BVC   NAST               // No, try mnemonic
              CPX   #'!'               // Else check cursor follows
              BNE   ASERR
              LDA   #1                 // Else set mode 0
              STA   ACMDL
              LDA   HXPKL              // Get number in Acc
              JMP   PCOPC              // and print it

ASERR         JSR   RETERR             // Error print
              JMP   LINENT

// If we get here, try a mnemonic

NAST          STY   COPL               // Store the mnemonic addr
              INY
              INY
              INY                      // Skip the mnemonic chars

//
// Y now points to the character immediately after the supposed command.
// The following code looks at the argument to see if it is valid, and
// assigns a mode number as follows:
// 0  = Imp
// 1  = Acc
// 2  = Imm
// 3  = ZP
// 4  = ZP,X
// 5  = Abs
// 6  = Abs,X
// 7  = ZP,Y
// 8  = Jmp indirect
// 9  = Abs,Y
// 10 = (Ind),Y
// 11 = (Ind),X
// All others, though they may be set, are illegal
//
// The following code compares the input string with the arg text table to
// see if the command is legally formatted and get its mode
//

              LDX   #$00               // Set mode 0
              LDA   (ICURS),Y          // Pick up next character
              CMP   #$20               // Is it a space?
              BNE   CURSTS             // No, try cursor
              INY                      // Else increment pointer
              STY   COPH               // and save it
NXFLSR        INX                      // Increment mode
              CPX   #12                // Error if X > 11, no mode found
              BEQ   ASJP
              TXA
              PHA                      // Save mode
              LDA   PRDEX,X            // Get string address idx for this mode
              TAX
NXBTSR        LDA   PRTBL,X            // Get next string offset
              CMP   #'$'               // Do we expect a number?
              BNE   NHXINP             // No, skip
              DEY                      // Else, adjust input pointer
              TXA
              PHA                      // Save X
              JSR   HXRDAS             // Get number
              PLA
              TAX                      // Recover X
              BVC   NXIPSR             // If error, try next compare str
DONXIP        INX                      // Else OK, next this string
              BNE   NXBTSR             // Unconditional

NHXINP        CMP   #0                 // If not numeric, is it string end?
              BNE   NOENDP             // No, skip
              LDA   (ICURS),Y
              CMP   #'!'               // Is it cursor?
              BEQ   FRIGCD             // Yes, go to next section

NXIPSR        PLA                      // Else, recover mode as this one
              TAX                      // is wrong
              LDY   COPH               // Recover input pointer
              BNE   NXFLSR             // Unconditional, try next

NOENDP        CMP   (ICURS),Y          // Here we compare characters
              BNE   NXIPSR             // No match, try next string
              INY                      // If yes, increment input pointer
              BNE   DONXIP             // Uncond, continue

FRIGCD        PLA                      // Found it, recover mode
              TAX
              CPX   #1                 // Mode 1?
              BEQ   CURSTS             // Yes, no ZP check
              CPX   #8                 // Ditto if JMP indirect
              BEQ   CURSTS
              CPX   #2                 // Mode 2? If so check arg is max 256
              BNE   ZCHK               // Else, do ZP check
              LDA   HXPKH
              BEQ   ZCHK
ASJP          JMP   ASERR


ZCHK          LDA   HXPKH              // If no ZPAGE, add 2 to mode
              BEQ   CURSTS
              INX
              INX
CURSTS        LDA   (ICURS),Y          // Look for cursor here
              CMP   #'!'
              BNE   ASJP               // If not, error
              CPX   #12                // Also check for illegal mode
              BPL   ASJP

// Point back to first file

              JSR   MODBIT            // Set mode bits in ACMDH,L

// Now search cmd table for equiv

              LDX   #$FF               // Use X to address table index
NXCOM         LDY   COPL               // Get where mnem stored
              INX
              LDA   FRSCH,X            // Compare first character
              BEQ   ASJP               // If zero table end, no equiv error
              CMP   (ICURS),Y          // Else, compare with input char
              BNE   NXCOM              // Next if not equal
              INY                      // Next char
              LDA   SECCH,X
              CMP   (ICURS),Y
              BNE   NXCOM
              INY
              LDA   THIRCH,X
              CMP   (ICURS),Y
              BNE   NXCOM

// If we drop through, matched. X is index, max val 55

              JSR   CHKARG            // Check the argument
              BCC   ASJP              // Error if carry set

//
// PCOPC
//
// Here we know instruction and mode are legal. Argument set right.
// Acc holds opcode. So put them into men and print
//

PCOPC         PHA                      // Save opcode
              JSR   OBLCUR
              LDY   #0                 // Point to line start
              STY   VDUIND
              PLA
              JSR   PNTINS             // Print instr string and opcode

ASTDY         JMP   LINENT

//
// DISENT
//
// Disassembly entry point. Address in HXPKL,H
//

DISERY
DISENT        LDA   #15                // Set the page on/off flag
              STA   NOPAUS
DISAS         LDA   #15                // And page line count
              STA   DSPCNT
NXINST        JSR   OBLCUR
              LDA   NOPAUS             // ID paging disabled
              BEQ   NXINS1             // Then skip
              DEC   DSPCNT             // Else, see if page full
              BNE   NXINS1             // No, skip
              JSR   OUTPCR
              JSR   PROMPT
              JSR   POLLKB             // If so, await the user
              LDA   ICHAR
              CMP   #$1B               // Is input character ESC?
              BNE   NDSES              // No, skip
              RTS

NDSES         CMP   #10                // Is it LF?
              BNE   DISAS              // No, reset count and next page
              LDA   #0                 // Else, disable page mode
              BEQ   DISERY+2           // Unconditional

NXINS1        LDX   #55                // Highest instruction index
TRYALL        LDA   #12                // Highest instruction mode
              STA   IPMODL             // Store it
NXIPMD        DEC   IPMODL             // Decrement the mode
              BPL   TRTHS              // If pos, try it
              DEX                      // Else, no more modes. Dec instr index
              BPL   TRYALL             // Do this for all instr
              INC   IPMODL             // If never found, set up
              BEQ   ILLOP              // to print error
TRTHS         TXA                      // Save opcode index
              PHA
              LDX   IPMODL             // Get mode type
              JSR   MODBIT             // Setup bit flags
              PLA
              TAX                      // Restore instruction code
              LDA   MODADL             // Set arg = PC to avoid branch error
              STA   HXPKL
              LDA   MODADH
              STA   HXPKH
              JSR   CHKARG             // Is arg legal?
              BCC   NXIPMD             // No, next mode
              LDY   #0                 // Pick up store contents
              CMP   (MODADL),Y         // Does it = generated code?
              CLC
              BNE   NXIPMD             // If not, try next opcode

// Else we've found it

ILLOP         TXA                      // Save opcode index
              PHA
              LDY   #0
              LDA   (MODADL),Y         // Push opcode
              PHA
              INY
              LDA   (MODADL),Y         // Pick up next two memory bytes
              STA   HXPKL              // in HXPK
              INY
              LDA   (MODADL),Y
              STA   HXPKH
              JSR   OUTPCR             // Give CR
              PLA                      // Recover opcode
              JSR   PNTINS             // Print addr and opcode
              JSR   OBLCUR             // Obliterate cursor
              LDA   #12                // Set print format
              STA   VDUIND             // for TANBUG
              PLA
              TAX
              BPL   NODISR             // Skip if legal to unpack
              JSR   RETERR             // Else, print question mark
NXINJP        JMP   NXINST             // and do next instr

// Else OK, print the mnemonic

NODISR        LDA   FRSCH,X            // Print the three characters
              JSR   OPCHAR
              LDA   SECCH,X
              JSR   OPCHAR
              LDA   THIRCH,X
              JSR   OPCHAR             // Then a space
              LDA   #$20
              JSR   OPCHAR

// Now print the leading part of the arg

              LDY   IPMODL             // Get mode in Y
              LDA   PRDEX,Y            // Get index into text strings
              TAY
NXTRAI        LDA   PRTBL,Y            // Get text char
              BNE   NOTLLL             // If zero, this line all done
              JMP   NXINST

NOTLLL        PHA                      // Else, print character
              JSR   OPCHAR
              PLA
              CMP   #'$'               // Was it dollar?
              BNE   NODOL              // No, skip

// Else do the numeric part

              CPX   #$30               // Was it a branch
              BCC   NOBR               // No, skip the next bit

// Else calculate the absolute address to be printed from HXPKL (offset)
// and MODADL (branch opcode + 2)

              LDA   HXPKL              // Get offset
              PHA
              LDX   MODADH             // Hi byte in X
              CLC
              ADC   MODADL             // Add PC
              STA   HXPKL              // Store result
              PLA                      // Test branch sign
              BMI   NEGO               // Skip if neg offset
              TXA
              ADC   #0                 // Else add carry
              BCC   NOCLHH             // Go store
NEGO          TXA
              SBC   #0                 // Neg, sub carry
              BCS   NOCLHH

NOBR          LDX   HXPKH              // Get hi part arg
              LDA   ACMDL              // If modes 5,6,8,9
              AND   #$60               // Don't clea HXPKH, else ZPAGE
              BNE   NOCLH1
              LDA   ACMDH
              AND   #3
              BNE   NOCLH1
              LDX   #0                 // If ZPAGE type, zero the hi bit
NOCLH1        TXA                      // Get in Acc
NOCLHH        JSR   HEXPNT             // Print address
              LDA   HXPKL
              JSR   HEXPNT

// Now, finally, print the arg ending

NODOL         INY                      // Inc index
              BNE   NXTRAI             // Next print string

//
// CASSETTE SUBROUTINES
//

BASE          EQU   $BFC0

IOORB         EQU   $BFC0
IODDRB        EQU   $BFC2
IOT1CL        EQU   $BFC4
IOT1LL        EQU   $BFC6
IOT1LH        EQU   $BFC7
T2CH          EQU   $BFC9
IOACR         EQU   $BFCB
IOPCR         EQU   $BFCC
IOIFR         EQU   $BFCD
IOIER         EQU   $BFCE

//
// INITCS
//

INITCS        SEI                      // Disable keyboard
              LDY   #6                 // Y = table end
NXINIT        LDX   IN6522,Y           // Index table address
              LDA   OPDTA,Y            // Required data
              STA   BASE,X             // Send it
              DEY
              BPL   NXINIT
              RTS

IN6522        FCB   $05, $04, $0B, $02, $0C, $08, $0E

OPDTA         FCB   $01, $38, $C0, $FF, $40, $0C, $7F

//
// ENDCY
//

ENDCY         LDA   IOT1CL             // Clear timeout bit
WAITCY        BIT   IOIFR              // Wait for it to set
              BVC   WAITCY
              RTS

//
// OP32
//

OP32          LDY   #$20               // A while of 2400 to settle record
OP32M         JSR   OPNON
              DEY
              BNE   OP32M
              RTS

//
// OPNON
//

OPNON         JSR   ENDCY              // Wait for cycles to end
              DEX
              BNE   OPNON
              RTS

//
// XOP
//

XOP           LDA   #$38               // 2400 time code
              LDX   #1
              BCS   OPFS               // Logic 1 - skip
              ASL   A                  // Else, set for 0
              INX
OPFS          STA   IOT1LL             // Prepare latches
              STX   IOT1LH
              BPL   ENDCY              // Unconditional to wait 'till effective

//
// OPBIT
//

OPBIT         PHA
              PHP
              LDA   FSTSLO             // Fast/Slow?
              BNE   CUTS0              // Skip to CUTS
              SEC                      // Else, output 1/2 cyc
              JSR   XOP
              PLP
              JSR   XOP                // Then data bit
              PLA
              RTS

CUTS0         JSR   XOP                // Output one data cycle
              LDX   #$0F
              PLP                      // Set no reqd,X
              BCS   REST
              LDX   #7
REST          JSR   OPNON              // Transmit the rest
              PLA
              RTS

//
// RECORD
//

RECORD        STA   CHAR
              TXA
              PHA
              TYA
              PHA
              JSR   ENDCY
              CLC
              LDY   #9                 // Set up 8 bits
              LDA   #0                 // Acc = parity
              BEQ   SRTBT              // Output 0 start bit 1st

NXBIT         LSR   CHAR               // Output the char bit by bit
              PHP
              ADC   #0                 // Update parity
              PLP
SRTBT         JSR   OPBIT
              DEY                      // 8 times
              BNE   NXBIT
              EOR   #1                 // Even parity
              LSR   A
              LDY   #4                 // Output parity and...
RPT           JSR   OPBIT              // two stop bits
              SEC
              DEY
              BNE   RPT
              PLA
              TAY
              PLA
              TAX
              RTS

//
// XTIM
//

XTIM          PHA
              LDA   IOORB              // Clear edge detector
XTIM1         LDA   IOIFR              // Wait for edge
              AND   #8
              BEQ   XTIM1
              LDA   T2CH               // Get time
              PHA
              LDA   #$FF               // Reset timer
              STA   T2CH
              PLA
              CMP   #$FC               // Setup carry
              PLA
              RTS

//
// RDBIT
//

RDBIT         JSR   XTIM               // Get bit
BITEND        LDX   FSTSLO             // Fast/slow?
              BEQ   EXRD               // Fast, exit
              PHA
              JSR   XTIM
              LDX   #2                 // CURS, read xtra bits
              BCC   NOUG
              LDX   #6
NOUG          LDA   #0                 // Use a best of N transfers
NOUG1         JSR   XTIM
              ADC   #0
              DEX
              BNE   NOUG1
              CMP   #4                 // Set up carry
              PLA
EXRD          RTS

//
// RDWD
//

RDWD          TYA
              PHA
              TXA
              PHA
              JSR   XTIM               // Dummy read
LOOK0         JSR   XTIM               // Look for 0
              BCS   LOOK0
              JSR   BITEND             // Format
              BCS   RDEND              // CUTS frame error
              LDA   #0                 // Parity
              LDY   #8                 // Bit count
NXRDW         JSR   RDBIT              // Read
              PHP
              ROR   CHAR               // Store bit
              PLP
              ADC   #0                 // Update parity
              DEY                      // Do 8 bits
              BNE   NXRDW
              JSR   RDBIT              // Get parity
              ADC   #0                 // Add in
              LSR   A                  // Set carry as parity
RDEND         PLA
              TAX
              PLA
              TAY
              LDA   CHAR
              RTS

//
// BURST
//

BURST         LDY   #$0C               // Count of $70
BRS1          JSR   XTIM               // Get bit
              BCC   BURST              // Start again if 1
              DEX
              BNE   BRS1               // $70 consecutive 1s
              DEY
              BNE   BRS1
ENL           RTS

//
// FURTHER CASSETTE FILE AND NAME HANDLING SUBROUTINES
//

//
// CROBL
//

CROBL         JSR   OUTPCR
OBLCUR        LDY   VDUIND             // Obliterate cursor
              LDA   #$20
              STA   (ICURS),Y
              RTS

//
// FNAME
//
// Packs up an entered FileName to loc $52 onwards. Carry set on exit if
// name was too long
//

FNAME         LDA   #0                 // Terminating 0
              LDX   #8                 // Initialise to spaces
INITB         STA   NAME,X
              LDA   #$20
              DEX
              BPL   INITB
NXFC          INY                      // Get chars from screen
              LDA   (ICURS),Y
              CMP   #$2D               // Check legal char
              BCC   FTDY               // and finish when illegal occurs
              CMP   #$3A
              BCC   FCH
              CMP   #$41
              BCC   FTDY
              CMP   #$5B
              BCS   FTDY
FCH           INX                      // Store if legal
              STA   NAME,X
              CPX   #8                 // Buffer full?
              BNE   NXFC               // No, next
              SEC                      // Else, error, set carry
              RTS

FTDY          CLC
              RTS

//
// APNDA
//

APNDA         LDA   #$41               // Set Acc as appendix

APND          STA   NAME+7             // Overwrite with appendix
              LDA   #$2E               // Dot
              STA   NAME+6
              JSR   OUTPCR             // Output CR
              LDX   #0                 // Print name
NPT           LDA   NAME,X
              BEQ   OBIT
              JSR   OPCHAR
              INX
              BNE   NPT
OBIT          JSR   OBLCUR             // Obliterate cursor
              RTS

//
// RCFN
//
// Record the FileName
//

RCFN          LDX   #0
NEXX          LDA   NAME,X
              JSR   RECORD
              INX
              CPX   #8
              BNE   NEXX
              RTS

//
// INDCMP
//

INDCMP        LDA   MODADL
              CMP   COPL
              BNE   INEND
              LDA   MODADH
              CMP   COPH
              BEQ   RTN
INEND         INC   MODADL
              BNE   RTN
              INC   MODADH
RTN           RTS

//
// GENERR
//

GENERR        JSR   RETERR
GENRET        LDX   #$FF               // Set stack
              TXS
CLST          LDA   #0                 // Close tape
              STA   IOACR
              JMP   RC1                // To monitor loop

//
// Tape Error Prints
//

FERR          LDY   #'F'
              BNE   SPERR

MERR          LDY   #'M'
              BNE   SPERR

PERR          LDY   #'P'
              BNE   SPERR

CERR          LDY   #'C'

SPERR         LDA   #$20               // Output space
              JSR   OPCHAR
              TYA                      // and error character
              JSR   OPCHAR

PADDR         LDA   MODADH             // Print address
              JSR   HEXPNT
              LDA   MODADL

PRBITE        JSR   HEXPNT
              JSR   OBLCUR
              RTS

//
// HXTOMO
//
// Transfers HXPKL,H contents to MODADL,H
//

HXTOMO        LDA   HXPKL
              STA   MODADL
              LDA   HXPKH
              STA   MODADH
              RTS

// XBUG CODE FIX

// Temporary frig to rectify X1 record bug

XBUG          LDA   #0
              STA   TYFG               // Zero type flag
              LDY   #1                 // Zero string index
              STY   RUNIND
              LDA   (ICURS),Y          // One, letter?
              CMP   #$FF
              BNE   STNG               // No, string
              DEY                      // Else, C or F
              LDA   (ICURS),Y
              CMP   #$43               // CUTS?
              BNE   NOTCUT
              STA   FSTSLO             // Yes, set slow
              RTS

NOTCUT        CMP   #$46               // Fast?
              BNE   STNG1
              STY   FSTSLO             // Set fast
              RTS

STNG          DEY
STNG1         LDA   (ICURS),Y          // Get cmd char
              SEC                      // Sub ASCII 'D'
              SBC   #$44
              CMP   #3                 // D, E or F
              BCC   CSCM               // Yes, continue
              ADC   #$43               // Else revert to ASCII, carry set
              CMP   #'A'
              BNE   TRYI               // Temp X1 to X2 conversion frig
              INC   TYFG
              LDA   #2                 // Reset fetch cmd
              CLC                      // Normalise carry
              BCC   CSCM               // Go to CS software
TRYI          PHA                      // Push the letter
              JSR   HEXPCK             // Pack an argument
              BVC   EJ                 // Question mark, if error
              JSR   HXTOMO             // Transfer to MODADL,H
              CPX   #$FF               // Cursor next?
              BNE   EJ                 // No, error
              PLA                      // Pick up cmd letter
              CMP   #'I'               // Is it I for instr disassembly?
              BNE   NOTI               // No, skip
              JMP   DISERY             // Yes, go and disassemble

NOTI          CMP   #'T'               // Is it T for translate?
              BNE   EJ                 // No, bomb out
              JMP   LINENT             // Yes, go translate

// RTS from these 2 returns to the bug

EJ            JMP   GENERR

CSCM          SBC   #0                 // Adjust cmd
              PHA                      // Save cmd
              BPL   RDCD               // Go to read
              JSR   HEXPCK             // Dump SA
              BVC   EJ                 // Error if no arg
              CPX   #$2C               // Error if no number
              BNE   EJ
              JSR   HXTOMO             // Transfer
              JSR   HEXPCK             // Dump FA
              BVC   EJ                 // Sim err checks
              LDA   HXPKL
              STA   COPL
              LDA   HXPKH
              STA   COPH
              DEY                      // Adj Y
RDCD          INY                      // Read entry
              LDA   (ICURS),Y
              CMP   #','               // Look for comma
              BNE   EJ
              JSR   FNAME              // Pack filename
              BCS   EJ                 // Error if too many chars
              CMP   #$FF               // Cursor next
              BNE   EJ                 // Error if not
              LDA   #0                 // Ensure remote control on
              STA   IOORB
              JSR   APNDA              // Pack and print filename
              JSR   INITCS             // Init CS
              PLA                      // Read if E or F
              BPL   RDCDOE
              JSR   OP32               // Output leader
              JMP   PATCH1             // PATCH, NOT PART OF LISTING

RCDBLK        LDX   #3                 // Record dump SA and FA
RCST          LDA   MODADL,X
              JSR   RECORD
              DEX
              BPL   RCST
              JSR   OPNON
              LDY   #0                 // Clear index
              STY   CSUM               // Clear checksum
OPNXRC        LDA   (MODADL),Y         // Output a byte
              PHA
              JSR   RECORD
              PLA
              CLC
              ADC   CSUM               // Update checksum
              STA   CSUM
              JSR   INDCMP             // Increment index and compare
              BNE   OPNXRC

// Output checksum a few times

              LDX   #$10
NXTZR         LDA   CSUM
              JSR   RECORD
              DEX
              BNE   NXTZR
              RTS

//
// RERD
//
// Looks for starting burst, reads filename to VDU, reads file status to
// MODADL, etc, compares the name read to that entered and exits if OK
// Else, it looks for next. Interrupts are enabled to allow a non-reset exit
//

              FDB   $F622              // EXTRA TO XBUG5 LISTING. ISOLATED?

RERD          JSR   CROBL              // CR
              CLI                      // Allow interrupts
              JSR   BURST              // Look for file burst
              LDX   #8                 // Read filename to VDU
NXRVD         JSR   RDWD
              BCS   OKFNN              // Parity?
PFERR         JSR   FERR
              JMP   RERD

OKFNN         JSR   OPCHAR
              JSR   OBLCUR
              DEX
              BNE   NXRVD
              LDX   #3                 // SA and FA
NSTAT         JSR   RDWD
              BCC   PFERR
              STA   MODADL,X

// Temp X1 format frig

              LDA   TYFG
              BEQ   NORMLR
              DEX
              BNE   NSTAT
              BEQ   CRAOP
// End frig

NORMLR        DEX
              BPL   NSTAT
              INX
CRAOP         STX   CSUM               // Clear checksum
              LDY   #$20
              JSR   SPERR              // Print SA
              LDX   #7                 // Compare filenames
NXCOMP        LDA   LINBOT,X
              CMP   NAME,X
              BNE   RERD               // Skip if no match
              DEX
              BPL   NXCOMP             // Exit here if a match
              SEI                      // Disable interrupts
              RTS

RDCDOE        PHA                      // Push the cmd mnem
              JSR   RERD               // Read the name and status
              JMP   Lbl_F7B0    // WHERE IS THIS ???

NXDAIP        LDY   #0
              JSR   RDWD               // Get data
              PHA
              BCS   READOK             // Parity?
              JSR   PERR
READOK        PLA                      // Cmd E,F?
              TAX
              CLC
              ADC   CSUM
              STA   CSUM
              PLA
              PHA
              BEQ   ECMD               // E, don't put in mem
              TXA
              STA   (MODADL),Y         // F, store it
ECMD          TXA
              CMP   (MODADL),Y         // Did it go in?
              BEQ   MEMOK
              JSR   MERR               // No, then error
MEMOK         JSR   INDCMP
              BNE   NXDAIP             // Next
              JSR   RDWD               // Get checksum
              CMP   CSUM               // Check it
              BEQ   NCSME
              JSR   CERR
NCSME         PLA                      // Unless done, TANBUG ret
              RTS

// END XBUG V5 LISTING. FOLLOWING APPEARS TO BE PATCHES & MISC PAD BYTES?

              FCB   $F6                // ???
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD  
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD

Lbl_F7B0      PLA
              JSR   RDDBLK
              JMP   GENRET

RDDBLK        PHA
              JSR   CROBL
              JMP   NXDAIP

              FCB   $DD                // ???
              FCB   $DA

PATCH1        JSR   RCFN               // Record file name
              JSR   RCDBLK
              JMP   GENRET

              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD

              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD

              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD

              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD

              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD

              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD

              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD

              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD
              FCB   $DD

              FCB   $FF
              FCB   $FF
              FCB   $FF
              FCB   $FF
              FCB   $FF

              FCB   $FF

              JMP   XBUG

              FCB   $FF
              FCB   $FF
              FCB   $FF
              FCB   $FF
              FCB   $FF

              FCB   $FF

              END

