// ============================================================================
//
// Standard Definitions for Microtan 65
//
// ============================================================================

// Page zero variables

              org    $0                // Zero system page variables

NULL          rmb    1                 // Reserved for bpt use
ICHAR         rmb    1                 // Input ASCII character
OCHAR         rmb    1                 // Temp output character store
VDUIND        rmb    1                 // Display index
INTFS1        rmb    3                 // Fast interrupt vector
NMIJMP        rmb    3                 // NMI vector
ICURS         rmb    2                 // Cursor position
RUNIND        rmb    1                 // Zero if in user mode
SINGLE        rmb    1                 // Nonzero if single step mode
PROCED        rmb    1                 // Proceed count (single isteps to do)
SIMCOM        rmb    1                 // Keypad / ASCII keyboard
INTSL1        rmb    3                 // Slow interrupt vector
HXPKL         rmb    1                 // HexPck store
HXPKH         rmb    1

// Pseudo registers

PCLBCK        rmb    1                 // PCL
PCHBCK        rmb    1                 // PCH
PSWBCK        rmb    1                 // PSW
SPBCK         rmb    1                 // SP
XBCK          rmb    1                 // IX
YBCK          rmb    1                 // IY
ABCK          rmb    1                 // A

// Temporary stores

MODADL        rmb    1
MODADH        rmb    1
COPL          rmb    1
COPH          rmb    1

// Breakpoint status table and code store

BPTLO         rmb    1
BPTHI         rmb    15
BPTCOD        rmb    16

// Stack base

STKBSE        equ    $100

// Display scroll labels

VDUSTT        equ    $200
VDUFST        equ    $220
VDUMID        equ    $300
VDUTOP        equ    $320
LINBOT        equ    $3E0
                                       // Note Read / Write text here used
// I/O Ports                           // by disassembler, do not edit

SGRAPH        equ    $BFF0             // READ to set Graphics mode
KBINCL        equ    $BFF0             // WRITE to clear keyboard interrupt
SNMI          equ    $BFF1             // Soft NMI
KBWRIT        equ    $BFF2             // Strobe keypad
KBREAD        equ    $BFF3             // READ keypad
STEXT         equ    $BFF3             // WRITE to set Text mode

// Standard routines

POLLKB        equ    $FFCA             // Poll keyboard for key
OUTPCR        equ    $FFE3             // Output a CR
OPCHR         equ    $FFE5             // Output a character
HEXPNT        equ    $FF0B             // Display char in Hex
HEXPCK        equ    $FF28             // Get hex characters from screen

// Misc

SETUP         equ    $FFDF             // Setup table
HDR           equ    $FFEC             // TANBUG header
