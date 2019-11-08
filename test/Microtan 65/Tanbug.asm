//=============================================================================
//
// MICROTAN 65 - TANBUG
//
// The TANBUG monitor program is located in 1K bytes of read only memory
// (ROM) at the top of the 6502 microprocessor 64K byte address space.
//
// TANBUG will only operate in the memory map of the microtan system, it is
// not a general purpose 6502 software and has been specifically written for
// Microtan.
//
// Locations $F7F7, $F7F8 and $F7F9 are reserved for a jump to an expansion
// monitor ROM which is positioned on the expansion board.
// Locations $200 - $3FF are the visual display memory. TANBUG writes to these
// locations whenever a command is typed to the monitor.
// Locations $100 - $1FF are used as the stack by the microprocessor.
//
//=============================================================================

TANEX         EQU    0                 // Tanbug Expansion = False (basic board)

              ORG    $0                // Zero system page variables

NULL          RMB    1                 // Reserved for bpt use
ICHAR         RMB    1                 // ASCII character
OCHAR         RMB    1                 // Temp char store
VDUIND        RMB    1                 // Display index
INTFS1        RMB    3                 // Fast interrupt link
NMIJP         RMB    3                 // NMI link
ICURS         RMB    2                 // Cursor index
RUNIND        RMB    1                 // Zero if in user mode
SINGLE        RMB    1                 // Nonzero if single instruction mode
PROCED        RMB    1                 // Proceed count (single instr to execute)
SIMCOM        RMB    1                 // Keypad (simple) / ASCII (complex) keyboard
INTSL1        RMB    3                 // Slow interrupt link
HXPKL         RMB    1                 // Hexpack store
HXPKH         RMB    1

// Pseudo registers

PCLBCK        RMB    1                 // PCL
PCHBCK        RMB    1                 // PCH
PSWBCK        RMB    1                 // PSW
SPBCK         RMB    1                 // SP
XBCK          RMB    1                 // IX
YBCK          RMB    1                 // IY
ABCK          RMB    1                 // A

// Temporary stores

MODADL        RMB    1
MODADH        RMB    1
COPL          RMB    1
COPH          RMB    1

// Breakpoint status table and code store

BPTLO         RMB    1
BPTHI         RMB    15
BPTCOD        RMB    16

// Stack base

STKBSE        EQU    $100

// Display scroll labels

VDUSTT        EQU    $200
VDUFST        EQU    $220
VDUMID        EQU    $300
VDUTOP        EQU    $320
LINBOT        EQU    $3E0

// I/O Ports

SGRAPH        EQU    $BFF0
KBINCL        EQU    $BFF0             // Alternative to SGRAPH
SNMI          EQU    $BFF1
KBWRIT        EQU    $BFF2
KBREAD        EQU    $BFF3
STEXT         EQU    $BFF3             // Write to set text mode

INPERR        EQU    $F7F7             // Error exit


#if TANEX
              ORG    $F800

              JMP    $F951             // Jump table to utility routines
              JMP    $F9B2
              JMP    $F99B
              JMP    OPCHR1
              LDA    #13
              JMP    OPCHR
              JMP    $FAE9
              JMP    $FB0C
              JMP    HEXPCK
              JMP    HEXPNT
              JMP    POLLKB

              LDX    #$FF
              TXS
              JMP    MONTOR

              JMP    $FB16
              JMP    $FB23
              JMP    WASKB
              FDB    $FCF8

SETUP         JMP    KBINT
              JMP    NMNT
              FDB    LINBOT            // ICURS setting
              FCB    1                 // RUNIND
              FCB    0                 // SINGLE
              FCB    0                 // PROCED
              FCB    0                 // SIMCOM
              FCB    $40               // Slow Interrupt (RTI instruction)

              BIT    $0
#endif


// Start of ROM code for Microtan 65 TANBUG

              ORG    $FC00

// TANBUG starts here on reset

#if TANEX
START         JMP    $F83E
#else
START         LDX    #$FF
              TXS                      // Set stack pointer to top of the stack
#endif
              INX                      //
              STX    PSWBCK            // Clear breakpoint store as their values will be
                                       // indeterminate on power up.
              JSR    BPTCLR            // Clear breakpoints
              STA    STEXT             // Set text mode

// Use table in ROM to initialise parameters (note order of table must
// correspond with the order of INTFS1 to ICURSH in RAM definitions)

#if TANEX
              LDX    #12
SETUP1        LDA    SETUP,X
#else
              LDX    #14
SETUP1        LDA    SETUP,X           // SETUP holds parameter table
#endif
              STA    INTFS1,X          // Store in zero page RAM
              DEX
              BPL    SETUP1

// Determine keyboard type and set flag, note IX = $FF

              INX
TSFIV         STX    KBWRIT            // Clear keyboard write latch
              STA    KBINCL            // Clear keyboard interrupt flag
              DEX
              STX    KBWRIT            // Write to keyboard lines
              INX                      // Reset IX
              LDA    KBREAD            // Read it back
              BPL    KPCPLX            // If plus not set - alphanumeric
              INC    SIMCOM            // If set - must be keypad
#if TANEX
KPCPLX        JSR    $FADB
#else
KPCPLX        STA    KBINCL            // Clear keyboard interrupt
#endif
TBMS          LDA    HDR,X             // Display TANBUG message
              BEQ    MONTOR            // Output chars until a 0
              JSR    OPCHR
              INX
              BNE    TBMS


//
// MAIN LOOP - monitor user input and act accordingly
//

MONTOR        CLD                      // Set binary mode
              CLI
              JSR    POLLKB            // Look at keyboard
              LDA    ICHAR             // Get char
              CMP    #$21              // Less than a space - term
              BMI    MONCH1            // Else output char
ISTERM        JSR    OPCHR
              JMP    MONTOR

MONCH1        JSR    MONEN2            // Call string process
RC1           LDA    #$0D              // Set up CR
              BNE    ISTERM            // Uncond. branch loop

MONEN2        LDY    #0
              LDA    (ICURS),Y         // Pick up command
              TAX
              INY
              LDA    (ICURS),Y         // Peek at next char
              BPL    MULTI             // If not -ve (cursor) must be parameter
              LDA    #0                // Else set A to zero


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

TRYS          CPX    #$53              // Was it S?
              BNE    TRYN
              STX    SINGLE            // Yes - set single step mode
              RTS


//
// N - NORMAL MODE COMMAND
//
// The N command is the complement of the S command and is used to cancel
// the S command so that the microprocessor executes the user program in
// the normal manner without returning to the monitor between each instruction.
// Reset automatically sets the normal mode of operation.
//

TRYN          CPX    #$4E              // Command N?
              BNE    TRYP
              STA    SINGLE            // else clear single instruction mode (ACC=0)
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

TRYP          CPX    #$50              // Command P with no arg?
              BNE    TRYR
              STA    PROCED            // Clear P count (A=0)
              BEQ    PROC1             // Uncond. branch proc


//
// R - REGISTER MODIFY / EXAMINE COMMAND
//
// This command is used to view / set the pseudo registers prior to running
// a program with the G command. It effectively executes a M15 command
// to examine the pseudo registers in locations $15 to $1B
//

TRYR          CPX    #$52              // Command R?
              BNE    TRYB
              STA    MODADH            // Note A=0
              LDA    #$15              // Set pseudo reg
              STA    MODADL
              JMP    REOPEN            // Jump mod memory


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

TRYB          CPX    #$42              // Command B?
              BNE    ERRQ              // No - then error
              JSR    BPTCLR            // Else clear breakpoints
              RTS


// With no TANEX FFF7 will respond to F7F7
// With TANEX, this monitor can be expanded since FFF7 jumps back to here

#if TANEX
ERRQ          JMP    $FA75
#else
ERRQ          JMP    INPERR
#endif

RETERR        LDA    #$3F
              JSR    OPCHR             // Display question mark
              RTS


// If we get here command expects parameters too

MULTI         DEY   
              TXA                      // Save cmd on stack
              PHA
              JSR    HEXPCK            // Pack its argument
              BNE    MOREY             // Any more data?
              PLA                      // Restore command
              BVC    ERRQ              // Error if no arg


//
// G - GO COMMAND
//
// Parameter is the address of the first instruction to run
// When executed the cursor disappears, and is restored on completion
// The program counter is initialised to the parameter address, and the
// stack pointer to $1FF (top of stack). The other registers: A, X, Y, PSW
// are loaded from the pseudo registers in the zero page ($15 - $1B)
//

              CMP    #$47              // Is it a G?
              BNE    TRYPL             // No - skip
              LDX    #0
              STX    PROCED            // Clear proceed count
              DEX                      // Set IX to $FF
GOEND         TXS                      // Reload it
              LDA    HXPKH             // Push PC high
              PHA
              LDA    HXPKL             // Push PC low
              PHA
              LDA    PSWBCK
              PHA
              DEC    RUNIND            // Clear run flag
              LDA    #$20
              LDY    VDUIND
              STA    (ICURS),Y         // Obliterate cursor
              LDX    XBCK
              LDY    YBCK
SRET          LDA    ABCK              // Set users Acc
              STA    SNMI              // Set NMI for next
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

TRYPL         CMP    #$50              // Command P with arg?
              BNE    TRYM
              LDA    HXPKL             // Set P count
              STA    PROCED
PROC1         LDA    PCHBCK            // Restore users PC
PNOARG        STA    HXPKH
              LDA    PCLBCK
              STA    HXPKL
              LDX    SPBCK             // Set IX to users SP
              JMP    GOEND             // Then back to user


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

TRYM          CMP    #$4D              // Command M address?
              BNE    ERRQ              // No - error
EQPT          LDA    #$2C
              JSR    OPCHR             // Output a comma
              LDY    #0
              LDA    (HXPKL),Y         // Pick up value
              JSR    HEXPNT            // and display it
              PLA                      // Pop stack return
              PLA
              LDA    #$2C              // Load comma
              JMP    ISTERM            // and back to monitor


// If here then there is a second parameter

MOREY         CPX    #$2C              // Was term a comma?
              BEQ    GETPT2            // Yes - continue
LINKPH        PLA                      // Else pull command
LINKR         JMP    ERRQ              // and give error

GETPT2        LDA    HXPKL             // No comma - store previous
              STA    MODADL            // in MODADL & MODADH
              LDA    HXPKH
              STA    MODADH
              JSR    HEXPCK            // Pack next value
              BNE    MOREY1            // Not cursor - more yet
              PLA                      // Else pull command
              BVC    LINKR             // No argument? - error


//
// L - LIST COMMAND (see LISTIT routine below)
//

              CMP    #$4C              // Command L?
              BEQ    LISTIT            // Yes - list it


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

              CMP    #$4F              // Command O?
              BNE    TRYBPT
              LDA    HXPKL             // Get branch dest
              SEC
              SBC    #2                // Adjust for branch code
              BCS    NOTOPO
              DEC    HXPKH
NOTOPO        SEC   
              SBC    MODADL            // Subtract source
              TAX                      // Hold result in IX
              LDA    HXPKH             // Subtract high byte
              SBC    MODADH
              TAY                      // Store in IY
              TXA                      // Get low byte
              BMI    RNGNG             // If -ve branch
              TYA                      // If +ve look at high
              BNE    LINKR             // Not 0 - then error
              BEQ    PNTITO            // If ok continue

RNGNG         INY                      // If -ve, then high is $FF
              BNE    LINKR
PNTITO        LDA    #$3D
              JSR    OPCHR             // Ok - display equals sign
              TXA
              JSR    HEXPNT            // ... and the value
              RTS


//
// B - BREAKPOINT COMMAND (with parameters)
//
// As described above.
// Parameters are the address of any opcode instruction, and the number of the
// breakpoint to be set (0 to 7)
//

TRYBPT        CMP    #$42              // Command B?
LINK1         BNE    LINKR             // No - error
              LDA    HXPKL             // Maximum breakpoint code is 7
              BMI    LINKR
              CMP    #8                // If greater - then error
              BPL    LINKR
              ASL    A                 // Double A
              TAX                      // Set IX for indexed addressing
              LDA    MODADL            // Store breakpoint address
              STA    BPTLO,X
              LDA    MODADH
              STA    BPTHI,X
JPRTRN        RTS   


//
// L - LIST MEMORY COMMAND
//
// Parameters are start memory address, and number of lines of 8 consecutive
// memory locations to show
// If 0 lines are requested, 256 lines will be shown
// TANBUG pauses between each line to permit the user to read the output
//

LISTIT        JSR    OUTPCR            // Output carriage return
NXLI          LDY    #0
              LDA    MODADH            // Display address
              JSR    HEXPNT            // High byte ...
              LDA    MODADL
NXLIST        JSR    HEXPNT            // Output low byte
              LDA    #$20
              JSR    OPCHR             // Output a space
              LDA    (MODADL),Y        // Display memory locations
              INY
              CPY    #9                // For eight memory locations
              BMI    NXLIST
              DEC    HXPKL             // Decrement line count
              BEQ    JPRTRN            // O? Return via CR (note requesting 0 lines gives 256)

#if TANEX
DELX1         NOP   
              NOP
              NOP
              NOP
              NOP
              NOP
#else
DELX1         DEY                      // Time delay
              BNE    DELX1
              DEX
              BNE    DELX1
#endif

              LDA    MODADL            // Now adjust the address, for next 8 locations
              CLC
              ADC    #8
              STA    MODADL
              BCC    LISTIT            // If Carry set
              INC    MODADH            // then increment high byte
              JMP    LISTIT


// If we get here there  is a third parameter

MOREY1        CPX    #$2C              // Comma?
              BEQ    TERMOK
ERJUM2        JMP    LINKPH            // No - then error

TERMOK        LDA    HXPKL             // Else store parameter
              STA    COPL
              LDA    HXPKH
              STA    COPH
              JSR    HEXPCK            // then pack new para
              BNE    ERJUM2            // error not term
              PLA


//
// M - MEMORY MODIFY / EXAMINE (modify location)
//
// If user has entered a value to be entered into the momeory location
// then this routine processes it
//

              CMP    #$4D              // Command M?
              BEQ    MEM100            // Yes - modify memory


//
// C - COPY COMMAND
//
// The copy command allows copying of the contents of one block of memory to
// another. The parameters are start address source, end address source and
// start address destination. These are copied to MODADL, COPL, and HXPKL
// respectively.
//

              CMP    #$43              // Command C?
              BNE    LINK1             // No - then error
              BVC    LINK1             // Argument? Error if not
              LDY    #0
NXCOP         LDA    (MODADL),Y        // Copy from source start address
              STA    (HXPKL),Y         // to destination
              LDA    COPH              // Check if we've reached end address
              CMP    MODADH
              BNE    ICMCOP
              LDA    COPL
              CMP    MODADL
              BEQ    ENDLS
ICMCOP        INC    MODADL            // No, then increment source
              BNE    NOHIH1
              INC    MODADH
NOHIH1        INC    HXPKL             // and increment destination
              BNE    NXCOP
              INC    HXPKH
              BNE    NXCOP             // Destination can not roll over top of memory
              RTS


// Modify memory location (M command above)

MEM100        LDX    ICHAR             // Get input character in IX
              CPX    #$20              // Was it a space?
              BEQ    REOPEN            // Yes - reopen
              BVC    NOENT             // Branch if nothing
              LDA    HXPKL             // Else enter data
              LDY    #0
              STA    (MODADL),Y

NOENT         CPX    #$0A              // Was LF typed?
              BEQ    WASLF             // Yes - then process it

              CPX    #$1B              // Was it ESC?
              BNE    ENDLS             // No - then return

ESCIT         DEC    MODADL            // Decrement memory modify address
              LDA    MODADL
              CMP    #$FF
              BNE    REOPEN
              DEC    MODADH
              JMP    REOPEN

WASLF         INC    MODADL            // Increment memory address
              BNE    REOPEN
              INC    MODADH            // and high byte, if required

REOPEN        JSR    OUTPCR            // Output carriage return
              LDA    #$4D
              JSR    OPCHR             // Display 'M'
              LDA    MODADH            // and address
              JSR    HEXPNT
              LDA    MODADL
              JSR    HEXPNT
              JMP    MONEN2            // Check next command


//
// POLL KEYBOARD
//
// Gets a character from either the simple keypad or ASCII keyboard. Key
// value is stored in ICHAR ($01)
//

POLLKB        LDA    #0
              PHA                      // Push 0 - shift indicator
PLKB1         STA    ICHAR             // Set ICHAR to zero
              CMP    SIMCOM            // Check if keypad
              BNE    SIMPLE

// ASCII keyboard routine

WAIT1         CMP    ICHAR             // Else wait for interrupt
              BEQ    WAIT1
PLKEND        PLA                      // Pop indicator
ENDLS         RTS   

// Simple keypad routine

SIMPLE        LDA    #$0F
              STA    KBWRIT            // Enable all keyboard lines
              LDA    KBREAD            // Look at keyboard lines
              BNE    SIMPLE            // Key down? - wait till up

              LDX    #$40              // Debounce it
DEBOUN        DEY   
              BNE    DEBOUN
              DEX
              BNE    DEBOUN

PLK1          LDY    #$FF              // Now poll the keypad properly
              PLA                      // Peek at shift indicator
              PHA
              BEQ    NOSHIF            // If shift set modify IY
              LDY    #$13
NOSHIF        LDX    #8                // Set IX - keyboard drive
PLK2          STX    KBWRIT            // Drive keyboard lines
              LDA    KBREAD            // Get result
              BNE    ACHAR             // Not 0 - a char - so skip
              INY                      // Else adjust IY
              INY
              INY
              INY
              INY
              TXA
              LSR    A                 // Shift IX right
              TAX
              BEQ    PLK1              // If zero repeat
              BNE    PLK2              // Else next line

// If we get here a key has been pressed

ACHAR         INY   
              LSR    A                 // Which key of 5?
              BCC    ACHAR             // C set? That's the key
              LDA    CHRTBL,Y          // Get ASCII equivalent
              STA    ICHAR             // And put in ICHAR
              BNE    PLKEND            // If zero - shift
              PLA                      // Pull shift
              EOR    #$FF              // Change shift state
              PHA                      // Push shift
              JMP    SIMPLE            // ... and continue

// Character look up table for ASCII equivalent

CHRTBL        FCB    $33, $37, $42, $46, $3F, $32, $36, $41
              FCB    $45, $0D, $31, $35, $39, $44, $0A, $30
              FCB    $34, $38, $43, $00, $2C, $52, $4C, $4E
              FCB    $3F, $32, $43, $49, $53, $20, $31, $4F
              FCB    $1B, $47, $7F, $30, $34, $50, $4D, $00


//
// OUTPUT A CARRIAGE RETURN
//
// This subroutine causes the display to scroll up one line by outputting
// a carriage return to the screen. It also reinstates the cursor when a
// user program is run with the G command
// ACC is corrupted, IX and IY are preserved
//

OUTPCR        LDA    #$0D              // Output a CR

//
// OUTPUT A CHARACTER
//
// Displays the character in the ACC on the screen
// ACC is corrupted, IX and IY are preserved
//

#if TANEX
OPCHR         JMP    $F87C
              NOP
OPCHR1        STA    OCHAR
              LDY    VDUIND            // Get cursor position
              LDA    #$20
              STA    (ICURS),Y         // and erase cursor
              LDX    OCHAR             // Get char in IX
              CPX    #12
              BNE    TRYDEL
              JSR    $FA23
DODEL         LDA    #$FF
              STA    (ICURS),Y         // Display cursor
              STY    VDUIND            // and save index
              RTS

TRYDEL        CPX    #$7F              // Is it delete?
              BNE    TRYCR
              JSR    $FA09
              BPL    DODEL

TRYCR         CPX    #$0D              // Is it a CR?
              BEQ    NXLINE
              TXA                      // No, then output character
              STA    (ICURS),Y
              INY                      // Increment vdu index
              CPY    #32               // End of line?
              BNE    DODEL
NXLINE        LDA    #32
              CLC
              ADC    ICURS
              STA    ICURS
              LDA    ICURS+1
              ADC    #0
              STA    ICURS+1
              LDY    #0
              CMP    #4
              BNE    DODEL
              JSR    $FA38
              BEQ    DODEL
              NOP
              NOP
              NOP
              NOP
              NOP

#else
OPCHR         STA    OCHAR             // Save the character
              TXA                      // Save IX and IY
              PHA
              TYA
              PHA
              LDY    VDUIND            // Get cursor position
              LDA    #$20
              STA    (ICURS),Y         // and erase cursor
              LDX    OCHAR             // Get char in IX
              CPX    #$7F              // Is it delete?
              BNE    TRYCR
              DEY                      // Decrement vdu index
              BPL    DODEL
ZERCUR        LDY    #0                // If negative set zero
DODEL         LDA    #$FF
              STA    (ICURS),Y         // Display cursor
              STY    VDUIND            // and save index
              PLA                      // Restore registers and exit
              TAY
              PLA
              TAX
              RTS

TRYCR         CPX    #13               // Is it a CR?
              BEQ    DOCR
              TXA                      // No, then output character
              STA    (ICURS),Y
              INY                      // Increment vdu index
              CPY    #32               // End of line?
              BMI    DODEL             // No - then tidy up and exit
DOCR          LDX    #0                // Scroll line
LOWBLK        LDA    VDUFST,X          // Do in two blocks
              STA    VDUSTT,X
              INX
              BNE    LOWBLK
HIBLK         LDA    VDUTOP,X
              STA    VDUMID,X
              INX
              CPX    #$E0
              BNE    HIBLK

              LDA    #$20
              TAY
MORSP         DEY                      // Fill line with spaces
              STA    (ICURS),Y
              BNE    MORSP
              BEQ    ZERCUR            // When done, tidy up
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
              LDA    STKBSE,X          // and get the PSW
              AND    #$10              // Was it a break? 
              BNE    BRKP              // Yes - then process it
              PLA                      // Else restore IX
              TAX
#if TANEX
              JSR    $FAA3
#else
              LDA    KBREAD            // Read keyboard
#endif
              BMI    WASKB             // If -ve, then was keyboard
USER          PLA                      // Else restore A
              JMP    INTSL1            // and check the slow interrupt
                                       // Note that INTSL1 normally holds an RTI,
                                       // unless the user has modified it

WASKB         AND    #$7F              // Mask top bit
              STA    ICHAR             // and store in ICHAR
#if TANEX
              JSR    $F911
#else
              STA    KBINCL            // Clear keyboard interrupt flip-flop
#endif
              CMP    #$1B              // Was key ESC?
              BNE    USER              // No - then normal return
              LDA    RUNIND            // Else - check if user program running?
              BNE    USER              // No - then normal return
              STA    PROCED            // Else clear proceed count
              BEQ    ACTBP             // and unconditionally branch to break


//
// BREAK PROCESSING
//
// Note the user should not set a break at own break or in this interrupt routine
// else crashes. Adjust PC to return to instr must subtract 3
//

BRKP          INX                      // address PC L
              SEC                      // set C
              LDA    STKBSE,X          // get PCL
              SBC    #2                // subtract 3
              STA    STKBSE,X          // put it back
              BCS    NOROLL            // C set? NOHI byte
              INX                      // else address PCH
              DEC    STKBSE,X          // dec PCH
NOROLL        PLA                      // pull IX
              TAX                      // restore it
ACTBP         PLA                      // pull accumulator
              STA    ABCK              // back it up
              JSR    BPTREM            // restore user code
NOROL         JMP    NMNT1             // service break


//
// DISPLAY HEX VALUES
//
// Takes a hex value stored in accumulator and displays as two hex characters
// Registers ACC and IX are corrupted
//

HEXPNT        PHA                      // Save value of char
              LDX    #1
              LSR    A                 // Get top part by multiple shifts
              LSR    A
              LSR    A
              LSR    A
PNT2          CLC   
              ADC    #$30              // Add hex 30
              CMP    #$3A              // More than 9?
              BMI    PNT1              // No - then display it
              CLC
              ADC    #7                // Adjust again
PNT1          JSR    OPCHR             // and display it
              DEX
              BPL    MOR1              // -ve? - end else low bit
              RTS

MOR1          PLA                      // Recover character
              AND    #$0F              // Clear unwanted bits
              BPL    PNT2              // and branch unconditionally


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

HEXPCK        LDA    #0                // Clear accumulator
              PHA                      // Push as PSW
              STA    HXPKL             // Clear parameters
              STA    HXPKH
NXHX          INY   
              LDA    (ICURS),Y         // Get character from display
              TAX                      // Save in IX
              SEC
              SBC    #$30              // Subtract hex 30 to give 0 to 9
              BMI    ENDTS
HX1           CMP    #$0A              // Is it 0 to 9?
              BMI    HX2               // Yes - then pack it
              SEC                      // Else adjust by hex 11
              SBC    #$11
              BMI    ENDTS             // Goto carry setup
HX3           CLC                      // Else add 9
              ADC    #$0A
              CMP    #$10              // If more than 15, then exit
              BMI    HX2
ENDTS         PLP                      // Deal with V flag
              CPX    #$FF              // Check if cursor
              RTS

// Note character is in IX on exit, return does not affect zero flag

HX2           LDX    #4
HX5           ASL    HXPKH             // Shift result left 4 bits
              ASL    HXPKL
              BCC    HX4
              INC    HXPKH
HX4           DEX                      // Until IX zero (4 times)
              BNE    HX5
              CLC
              ADC    HXPKL             // Add in character
              STA    HXPKL
              PLA                      // Get pseudo PSW
              LDA    #$40              // Set V bit
              PHA                      // and push it
              BNE    NXHX              // Next ...


//
// NON-MASKABLE INTERRUPT
//
// This routine handles single instruction mode
//

NMNT          STA    ABCK              // Save accumulator
              CLD                      // Set binary mode
              LDA    SINGLE            // Test single instruction mode
              BNE    NMNT1
              TXA                      // If not save IX
              PHA
              JSR    BPSET             // Set breakpoints
              PLA                      // Restore IX
              TAX
              LDA    ABCK              // Restore accumulator
              RTI

NMNT1         LDA    PROCED            // Get proceed count
              BEQ    ZERBCK            // If zero, then break
              DEC    PROCED            // else decrement count
              JMP    SRET              // RTS via single test

ZERBCK        INC    RUNIND            // Set not running
              PLA                      // Pop breakpoint PSW to Acc
              STA    PSWBCK            // and back it up
              PLA
              STA    PCLBCK            // Ditto PCL
              PLA
              STA    PCHBCK            // Ditto PCH
              STX    XBCK              // Save IX
              STY    YBCK              // and IY
              TSX
              STX    SPBCK             // and operators SP
PSEUD         JSR    OUTPCR
              LDA    PCHBCK
              JSR    HEXPNT            // Display PC
              LDA    PCLBCK
              JSR    HEXPNT
              LDY    #0
PSNX          LDA    #$20              // Display two spaces
              PHA
              JSR    OPCHR
              PLA
              JSR    OPCHR
              LDA    PSWBCK,Y          // Then display all registers
              JSR    HEXPNT
              INY
              CPY    #5
              BMI    PSNX
              JMP    RC1


//
// CLEAR BREAKPOINTS
//
// Sets all breakpoint addresses to zero
//

BPTCLR        LDX    #$1F              // Set count for 8 breakpoints
              LDA    #0
BPTCL1        STA    BPTLO,X           // Set to zero
              DEX
              BPL    BPTCL1            // For all locations
              RTS


//
// REMOVE BREAKPOINTS
//
// Writes back the user's original code
//

BPTREM        TXA   
              PHA
              LDX    #$0E              // Set count for 8 breakpoints
BPTRM1                                 //
#if TANEX
              LDA    BPTLO,X
              ORA    BPTHI,X
              BEQ    BPTRM2
#endif
              LDA    BPTCOD,X          // Load saved old instruction
              STA    (BPTLO,X)         // Write to program memory
BPTRM2        DEX   
              DEX
              BPL    BPTRM1            // For all locations
              PLA
              TAX
              RTS


//
// SET BREAKPOINTS
//
// Examines entered breakpoint addresses, stores the present instruction
// and writes BRK opcode to program memory
//

BPSET         LDX    #$0E              // Set count for 8 breakpoints
BPS1                                   //
#if TANEX
              LDA    BPTHI,X
              ORA    BPTLO,X
              BEQ    BPS2
#endif
              LDA    (BPTLO,X)         // Get user's instruction
              STA    BPTCOD,X          // Store it
              LDA    #0
              STA    (BPTLO,X)         // Set BRK instruction...
BPS2          DEX   
              DEX
              BPL    BPS1              // Repeat until done
              RTS


#if TANEX
//	Setup table in low ROM

#else

//
// SETUP TABLE
//
// Copy in ROM of the initial settings for zero page variables
//

SETUP         JMP    KBINT
              JMP    NMNT
              FDB    LINBOT            // ICURS setting
              FCB    1                 // RUNIND
              FCB    0                 // SINGLE
              FCB    0                 // PROCED
              FCB    0                 // SIMCOM
              FCB    $40               // Slow Interrupt (RTI instruction)

#endif


// Header message

HDR           FCB    13                // CR
              FCC    'TANBUG'
              FCB    13, 0             // CR, end of line terminator

#if TANEX
              NOP
              NOP
              NOP
#else
              FCB    0, 0              // Padding
#endif


//
// RESET / INTERRUPT VECTORS
//
// These vectors must appear at the top of the ROM space
//

              JMP    RETERR            // $FFF7, also mapped to $F7F7
              FDB    NMIJP             // Non Maskable Interrupt vector
              FDB    START             // Reset vector
              FDB    INTFS1            // Interrupt vector

              END
