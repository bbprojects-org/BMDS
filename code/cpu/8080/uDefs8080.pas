{ ==============================================================================

  DEFINITIONS FOR 8080 MICROPROCESSOR

  Addressing modes:

  - Implied / Inherent: The addressing mode of some instructions is implied
      by the instruction itself. For example, the RAL (rotate accumulator left)
      instruction deals only with the accumulator, and PCHL loads the program
      counter with the contents of register-pair HL

  - Register: With each instruction using register addressing, only one
      register is specified (except for the MOV instruction), although in many
      of them the accumulator is implied as second operand. Examples are CMP E,
      which compares register E with the accumulator, and DCR B, which
      decrements register B. A few instructions deal with 16 bit register-pairs:
      examples are DCX B, which decrements register-pair BC and the PUSH and
      POP instructions

  - Immediate: The immediate value can be an 8 bit value, as in ADI 10 which
      adds 10 to the accumulator, or a 16 bit value, as in LXI H,1000, which
      loads 1000 in the register-pair HL

  - Direct: Jump instructions include a 16 bit address as part of the
      instruction. The instruction SHLD 1234 stores the contents of register
      pair HL on memory locations 1234 and 1235. The high order byte is stored
      at the highest address

  - Register Indirect: Each instruction that may refer to an 8 bit register,
      may refer also to a memory location. In this case the letter M (for
      Memory) has to be used instead of a register. It indicates the memory
      location pointed to by H and L, so ADD M adds the contents of the memory
      location specified by H and L to the contents of the accumulator. The
      register-pairs BC and DE can also be used for indirect addressing, but
      only to load or store the accumulator. For example, STAX B stores the
      contents of the accumulator into the memory location addressed by
      register-pair BC

  [ Each CPU has selection of:

  - Implied/Inherent
  - Immediate
  - Direct
  - Extended : uses the next two bytes after the opcode as the address, the
      order depends on the endianess of the CPU, little endian for 8080, that
      is lower byte first
  - Indirect Register
  - Indirect Memory : uses the address following the opcode, as the pointer to
      the address of the actual data
  - Program Relative : uses the value after the opcode as an offset to
      the program counter
  - Base Relative : a memory location or register is used as the base, and
      all offsets are relative to this
  - Index : the index register(s) is added to the address field
      of the instruction to get the effective address. The index register
      can be incremented/decremented to adjust the EA
  ]

  =============================================================================}

unit uDefs8080;

{$mode objfpc}
{$H+}

interface

uses
  Classes,
  //
  uCpuTypes;

type
  TAddrMode8080 = (mNIL, mINH, mREG, mIMM, mDIR, mIND);

const
  INFO_8080: TCpuInfo =
    ( Name:                 '8080';
      Registers:            // Must be uppercase, for assembler
                            'A B C D E H L AF BC DE HL SP ' +
                            // Need flags too
                            'NZ Z NC C PO PE P M';
      WildChar:             '*';
      LittleEndian:         True;
      SupportsDisassembler: True;
      SupportsCpuTest:      True;
      TraceWidth:           540;
      RegistersHeight:      332;
      Template:             '' );

  REGISTERS_ORIG = 'A B C D E H L M BC DE HL SP PSW';

  { Values for Interrupt Number }

  NO_INTERRUPT = -1;
  RST_0        = 0 * 8;
  RST_1        = 1 * 8;
  RST_2        = 2 * 8;
  RST_3        = 3 * 8;
  RST_4        = 4 * 8;
  RST_5        = 5 * 8;
  RST_6        = 6 * 8;
  RST_7        = 7 * 8;

  { Processor condition flags bit definitions }

  FLAG_CARRY     = $01;
  FLAG_PARITY    = $04;
  FLAG_AUX_CARRY = $10;
  FLAG_INTERRUPT = $20;
  FLAG_ZERO      = $40;
  FLAG_SIGN      = $80;

  { Define column details for 8080 trace, including regisers }

  TRACE_COLS_8080: array[0..10] of TTraceColumns = (
    (Title: 'Addr'; Width: 36; Align: taLeftJustify),    // These should be
    (Title: 'Code'; Width: 60; Align: taLeftJustify),    // common to all CPU
    (Title: ' '; Width: 16; Align: taLeftJustify),       // only varying widths
    (Title: 'Mnem'; Width: 40; Align: taLeftJustify),    //
    (Title: 'Operand'; Width: 80; Align: taLeftJustify), //
    (Title: 'A'; Width: 24; Align: taLeftJustify),
    (Title: 'B.C'; Width: 48; Align: taLeftJustify),
    (Title: 'D.E'; Width: 48; Align: taLeftJustify),
    (Title: 'H.L'; Width: 48; Align: taLeftJustify),
    (Title: 'SP'; Width: 38; Align: taLeftJustify),
    (Title: 'SZ-H-P-C'; Width: 72; Align: taLeftJustify)
    );

{ =============================================================================
  This section defines the instruction set for the 8080 processor, and is used
  by the emulator, assembler and disassembler
  =============================================================================

  See 'uCpuBase' for record definition TOpcodeRawData and TRule

  M: The mnemonic associated with the instruction opcode, case is irrelevant

  A: The address mode syntax, all characters are taken literally except '*'
     which represents an operand expression. Double quotes (empty string)
     mean no argument expected

  S: Ordinal value of the addressing mode. Use type casting to access
     its proper value

  O: The opcode value in hexadecimal. Max 6 chars, each pair representing
     single byte, right most pair being placed in the lowest memory position

  N: The number of bytes in the instruction including the opcode itself

  C: The basic number of cycles for each opcode. Some opcodes use extra
     cycles depending on the operation and these are added by the relevant
     program code

  R: The action / rule to be performed for this instruction and address mode:
        NIL   Do nothing
        ZP    If '*' < 256 then use zero page opcode (next row)
        CR    Combine least significant bytes of first two args
     	      making second one relative to PC
        REL   Relative addressing (single byte offset)
     [not used for 8080]

  T: The type field has bits set according to which CPU is supported.
     All opcodes in basic instruction set have bit 0 set (1). Opcodes
     for extended processor instructions have other bits set
     - 8080 with original mnemonics  = %01 (1)
     - 8080 with Z80 style mnemonics = %10 (2)

  NOTE: Assembler looks for a match for a specific address mode against a
        list of mnemonic names, hence must list all mnemonics grouped by name,
        although order of each group of mnemonics does not matter }


  OPCODES_8080: array[0..488] of TOpcodeRawData =
  (                                                                            // 8080 Mnemonics
  // This is the undefined opcode data, note Type = 0 (and NumBytes = 0)
  ( M: '???';  A: '';       S: 00{mNIL}; O: $00; N: 0; C: 0;  R: rNIL; T: 0 ),

  {$INCLUDE 'uDefs8080_O.inc'}
  {$INCLUDE 'uDefs8080_Z.inc'}

  );

implementation


end.
