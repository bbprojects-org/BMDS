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

  { CPU Names }

  CPU_8080  = '8080';

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
     [not used for 8080]


  NOTE: Assembler looks for a match for a specific address mode against a
        list of mnemonic names, hence must list all mnemonics grouped by name,
        although order of each group of mnemonics does not matter }


  OPCODES_8080: array[0..244] of TOpcodeRawData =
  (                                                                            // 8080 Mnemonics
  // This is the undefined opcode data, note Type = 0 (and NumBytes = 0)
  ( M: '???';  A: '';       S: 00{mNIL}; O: $00; N: 0; C: 0;  R: rNIL; T: 0 ),

  // Move, load, store
  ( M: 'LD';   A: 'B,B';    S: 02{mReg}; O: $40; N: 1; C:  5; R: rNIL; T: 1 ), // MOV B,reg
  ( M: 'LD';   A: 'B,C';    S: 02{mReg}; O: $41; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'B,D';    S: 02{mReg}; O: $42; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'B,E';    S: 02{mReg}; O: $43; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'B,H';    S: 02{mReg}; O: $44; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'B,L';    S: 02{mReg}; O: $45; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'B,(HL)'; S: 02{mReg}; O: $46; N: 1; C:  7; R: rNIL; T: 1 ), // MOV B,M
  ( M: 'LD';   A: 'B,A';    S: 02{mReg}; O: $47; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'B,*';    S: 03{mIMM}; O: $06; N: 2; C:  7; R: rNIL; T: 1 ), // MVI B

  ( M: 'LD';   A: 'C,B';    S: 02{mReg}; O: $48; N: 1; C:  5; R: rNIL; T: 1 ), // MOV C,reg
  ( M: 'LD';   A: 'C,C';    S: 02{mReg}; O: $49; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'C,D';    S: 02{mReg}; O: $4A; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'C,E';    S: 02{mReg}; O: $4B; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'C,H';    S: 02{mReg}; O: $4C; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'C,L';    S: 02{mReg}; O: $4D; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'C,(HL)'; S: 02{mReg}; O: $4E; N: 1; C:  7; R: rNIL; T: 1 ), // MOV C,M
  ( M: 'LD';   A: 'C,A';    S: 02{mReg}; O: $4F; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'C,*';    S: 03{mIMM}; O: $0E; N: 2; C:  7; R: rNIL; T: 1 ), // MVI C

  ( M: 'LD';   A: 'D,B';    S: 02{mReg}; O: $50; N: 1; C:  5; R: rNIL; T: 1 ), // MOV D,reg
  ( M: 'LD';   A: 'D,C';    S: 02{mReg}; O: $51; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'D,D';    S: 02{mReg}; O: $52; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'D,E';    S: 02{mReg}; O: $53; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'D,H';    S: 02{mReg}; O: $54; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'D,L';    S: 02{mReg}; O: $55; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'D,(HL)'; S: 02{mReg}; O: $56; N: 1; C:  7; R: rNIL; T: 1 ), // MOV D,M
  ( M: 'LD';   A: 'D,A';    S: 02{mReg}; O: $57; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'D,*';    S: 03{mIMM}; O: $16; N: 2; C:  7; R: rNIL; T: 1 ), // MVI D

  ( M: 'LD';   A: 'E,B';    S: 02{mReg}; O: $58; N: 1; C:  5; R: rNIL; T: 1 ), // MOV E,reg
  ( M: 'LD';   A: 'E,C';    S: 02{mReg}; O: $59; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'E,D';    S: 02{mReg}; O: $5A; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'E,E';    S: 02{mReg}; O: $5B; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'E,H';    S: 02{mReg}; O: $5C; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'E,L';    S: 02{mReg}; O: $5D; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'E,(HL)'; S: 02{mReg}; O: $5E; N: 1; C:  7; R: rNIL; T: 1 ), // MOV E,M
  ( M: 'LD';   A: 'E,A';    S: 02{mReg}; O: $5F; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'E,*';    S: 03{mIMM}; O: $1E; N: 2; C:  7; R: rNIL; T: 1 ), // MVI E

  ( M: 'LD';   A: 'H,B';    S: 02{mReg}; O: $60; N: 1; C:  5; R: rNIL; T: 1 ), // MOV H,reg
  ( M: 'LD';   A: 'H,C';    S: 02{mReg}; O: $61; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'H,D';    S: 02{mReg}; O: $62; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'H,E';    S: 02{mReg}; O: $63; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'H,H';    S: 02{mReg}; O: $64; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'H,L';    S: 02{mReg}; O: $65; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'H,(HL)'; S: 02{mReg}; O: $66; N: 1; C:  7; R: rNIL; T: 1 ), // MOV H,M
  ( M: 'LD';   A: 'H,A';    S: 02{mReg}; O: $67; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'H,*';    S: 03{mIMM}; O: $26; N: 2; C:  7; R: rNIL; T: 1 ), // MVI H

  ( M: 'LD';   A: 'L,B';    S: 02{mReg}; O: $68; N: 1; C:  5; R: rNIL; T: 1 ), // MOV L,reg
  ( M: 'LD';   A: 'L,C';    S: 02{mReg}; O: $69; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'L,D';    S: 02{mReg}; O: $6A; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'L,E';    S: 02{mReg}; O: $6B; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'L,H';    S: 02{mReg}; O: $6C; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'L,L';    S: 02{mReg}; O: $6D; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'L,(HL)'; S: 02{mReg}; O: $6E; N: 1; C:  7; R: rNIL; T: 1 ), // MOV L,M
  ( M: 'LD';   A: 'L,A';    S: 02{mReg}; O: $6F; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'L,*';    S: 03{mIMM}; O: $2E; N: 2; C:  7; R: rNIL; T: 1 ), // MVI L

  ( M: 'LD';   A: '(HL),B'; S: 02{mReg}; O: $70; N: 1; C:  7; R: rNIL; T: 1 ), // MOV M,reg
  ( M: 'LD';   A: '(HL),C'; S: 02{mReg}; O: $71; N: 1; C:  7; R: rNIL; T: 1 ),
  ( M: 'LD';   A: '(HL),D'; S: 02{mReg}; O: $72; N: 1; C:  7; R: rNIL; T: 1 ),
  ( M: 'LD';   A: '(HL),E'; S: 02{mReg}; O: $73; N: 1; C:  7; R: rNIL; T: 1 ),
  ( M: 'LD';   A: '(HL),H'; S: 02{mReg}; O: $74; N: 1; C:  7; R: rNIL; T: 1 ),
  ( M: 'LD';   A: '(HL),L'; S: 02{mReg}; O: $75; N: 1; C:  7; R: rNIL; T: 1 ),
  ( M: 'LD';   A: '(HL),A'; S: 02{mReg}; O: $77; N: 1; C:  7; R: rNIL; T: 1 ),
  ( M: 'LD';   A: '(HL),*'; S: 03{mIMM}; O: $36; N: 2; C:  7; R: rNIL; T: 1 ), // MVI r

  ( M: 'LD';   A: 'A,B';    S: 02{mReg}; O: $78; N: 1; C:  5; R: rNIL; T: 1 ), // MOV A,reg
  ( M: 'LD';   A: 'A,C';    S: 02{mReg}; O: $79; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'A,D';    S: 02{mReg}; O: $7A; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'A,E';    S: 02{mReg}; O: $7B; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'A,H';    S: 02{mReg}; O: $7C; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'A,L';    S: 02{mReg}; O: $7D; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'A,(HL)'; S: 02{mReg}; O: $7E; N: 1; C:  7; R: rNIL; T: 1 ), // MOV A,M
  ( M: 'LD';   A: 'A,A';    S: 02{mReg}; O: $7F; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'LD';   A: 'A,(*)';  S: 04{mDir}; O: $3A; N: 3; C: 13; R: rNIL; T: 1 ), // LDA A
  ( M: 'LD';   A: 'A,*';    S: 03{mIMM}; O: $3E; N: 2; C:  7; R: rNIL; T: 1 ), // MVI A

  ( M: 'LD';   A: 'A,(BC)'; S: 02{mReg}; O: $0A; N: 1; C:  7; R: rNIL; T: 1 ), // LDAX B
  ( M: 'LD';   A: 'A,(DE)'; S: 02{mReg}; O: $1A; N: 1; C:  7; R: rNIL; T: 1 ), // LDAX D

  ( M: 'LD';   A: '(BC),A'; S: 02{mReg}; O: $02; N: 1; C:  7; R: rNIL; T: 1 ), // STAX B
  ( M: 'LD';   A: '(DE),A'; S: 02{mReg}; O: $12; N: 1; C:  7; R: rNIL; T: 1 ), // STAX D

  ( M: 'LD';   A: 'BC,*';   S: 04{mIMM}; O: $01; N: 3; C: 10; R: rNIL; T: 1 ), // LXI B
  ( M: 'LD';   A: 'DE,*';   S: 04{mIMM}; O: $11; N: 3; C: 10; R: rNIL; T: 1 ), // LXI D
  ( M: 'LD';   A: 'HL,*';   S: 04{mIMM}; O: $21; N: 3; C: 10; R: rNIL; T: 1 ), // LXI H

  ( M: 'LD';   A: '(*),HL'; S: 04{mDir}; O: $22; N: 3; C: 16; R: rNIL; T: 1 ), // SHLD
  ( M: 'LD';   A: 'HL,(*)'; S: 04{mDir}; O: $2A; N: 3; C: 16; R: rNIL; T: 1 ), // LHLD
  ( M: 'LD';   A: '(*),A';  S: 04{mDir}; O: $32; N: 3; C: 13; R: rNIL; T: 1 ), // STA

  ( M: 'LD';   A: 'SP,HL';  S: 02{mReg}; O: $F9; N: 1; C:  5; R: rNIL; T: 1 ), // SPHL
  ( M: 'LD';   A: 'SP,*';   S: 04{mIMM}; O: $31; N: 3; C: 10; R: rNIL; T: 1 ), // LXI SP

  ( M: 'EX';   A: 'DE,HL';   S: 02{mReg}; O: $EB; N: 1; C:  4; R: rNIL; T: 1 ), // XCHG
  ( M: 'EX';   A: '(SP),HL'; S: 02{mReg}; O: $E3; N: 1; C: 18; R: rNIL; T: 1 ), // XTHL

  // Stack Ops
  ( M: 'PUSH'; A: 'BC';     S: 02{mReg}; O: $C5; N: 1; C: 11; R: rNIL; T: 1 ), // PUSH B
  ( M: 'PUSH'; A: 'DE';     S: 02{mReg}; O: $D5; N: 1; C: 11; R: rNIL; T: 1 ), // PUSH D
  ( M: 'PUSH'; A: 'HL';     S: 02{mReg}; O: $E5; N: 1; C: 11; R: rNIL; T: 1 ), // PUSH H
  ( M: 'PUSH'; A: 'AF';     S: 02{mReg}; O: $F5; N: 1; C: 11; R: rNIL; T: 1 ), // PUSH PSW

  ( M: 'POP';  A: 'BC';     S: 02{mReg}; O: $C1; N: 1; C: 10; R: rNIL; T: 1 ), // POP B
  ( M: 'POP';  A: 'DE';     S: 02{mReg}; O: $D1; N: 1; C: 10; R: rNIL; T: 1 ), // POP D
  ( M: 'POP';  A: 'HL';     S: 02{mReg}; O: $E1; N: 1; C: 10; R: rNIL; T: 1 ), // POP H
  ( M: 'POP';  A: 'AF';     S: 02{mReg}; O: $F1; N: 1; C: 10; R: rNIL; T: 1 ), // POP PSW

  // Jump
  ( M: 'JP';   A: 'NZ,*';   S: 04{mDir}; O: $C2; N: 3; C: 10; R: rNIL; T: 1 ), // JNZ
  ( M: 'JP';   A: 'Z,*';    S: 04{mDir}; O: $CA; N: 3; C: 10; R: rNIL; T: 1 ), // JZ
  ( M: 'JP';   A: 'NC,*';   S: 04{mDir}; O: $D2; N: 3; C: 10; R: rNIL; T: 1 ), // JNC
  ( M: 'JP';   A: 'C,*';    S: 04{mDir}; O: $DA; N: 3; C: 10; R: rNIL; T: 1 ), // JC
  ( M: 'JP';   A: 'PO,*';   S: 04{mDir}; O: $E2; N: 3; C: 10; R: rNIL; T: 1 ), // JPO
  ( M: 'JP';   A: 'PE,*';   S: 04{mDir}; O: $EA; N: 3; C: 10; R: rNIL; T: 1 ), // JPE
  ( M: 'JP';   A: 'P,*';    S: 04{mDir}; O: $F2; N: 3; C: 10; R: rNIL; T: 1 ), // JP
  ( M: 'JP';   A: 'M,*';    S: 04{mDir}; O: $FA; N: 3; C: 10; R: rNIL; T: 1 ), // JM
  ( M: 'JP';   A: '(HL)';   S: 02{mReg}; O: $E9; N: 1; C:  5; R: rNIL; T: 1 ), // PCHL
  ( M: 'JP';   A: '*';      S: 04{mDir}; O: $C3; N: 3; C: 10; R: rNIL; T: 1 ), // JMP

  // Call
  ( M: 'CALL'; A: 'NZ,*';   S: 04{mDir}; O: $C4; N: 3; C: 11; R: rNIL; T: 1 ), // CNZ
  ( M: 'CALL'; A: 'Z,*';    S: 04{mDir}; O: $CC; N: 3; C: 11; R: rNIL; T: 1 ), // CZ
  ( M: 'CALL'; A: 'NC,*';   S: 04{mDir}; O: $D4; N: 3; C: 11; R: rNIL; T: 1 ), // CNC
  ( M: 'CALL'; A: 'C,*';    S: 04{mDir}; O: $DC; N: 3; C: 11; R: rNIL; T: 1 ), // CC
  ( M: 'CALL'; A: 'PO,*';   S: 04{mDir}; O: $E4; N: 3; C: 11; R: rNIL; T: 1 ), // CPO
  ( M: 'CALL'; A: 'PE,*';   S: 04{mDir}; O: $EC; N: 3; C: 11; R: rNIL; T: 1 ), // CPE
  ( M: 'CALL'; A: 'P,*';    S: 04{mDir}; O: $F4; N: 3; C: 11; R: rNIL; T: 1 ), // CP
  ( M: 'CALL'; A: 'M,*';    S: 04{mDir}; O: $FC; N: 3; C: 11; R: rNIL; T: 1 ), // CM
  ( M: 'CALL'; A: '*';      S: 04{mDir}; O: $CD; N: 3; C: 17; R: rNIL; T: 1 ), // CALL

  // Return
  ( M: 'RET';  A: 'NZ';     S: 01{mINH}; O: $C0; N: 1; C:  5; R: rNIL; T: 1 ), // RNZ
  ( M: 'RET';  A: 'Z';      S: 01{mINH}; O: $C8; N: 1; C:  5; R: rNIL; T: 1 ), // RZ
  ( M: 'RET';  A: 'NC';     S: 01{mINH}; O: $D0; N: 1; C:  5; R: rNIL; T: 1 ), // RNC
  ( M: 'RET';  A: 'C';      S: 01{mINH}; O: $D8; N: 1; C:  5; R: rNIL; T: 1 ), // RC
  ( M: 'RET';  A: 'PO';     S: 01{mINH}; O: $E0; N: 1; C:  5; R: rNIL; T: 1 ), // RPO
  ( M: 'RET';  A: 'PE';     S: 01{mINH}; O: $E8; N: 1; C:  5; R: rNIL; T: 1 ), // RPE
  ( M: 'RET';  A: 'P';      S: 01{mINH}; O: $F0; N: 1; C:  5; R: rNIL; T: 1 ), // RP
  ( M: 'RET';  A: 'M';      S: 01{mINH}; O: $F8; N: 1; C:  5; R: rNIL; T: 1 ), // RM
  ( M: 'RET';  A: '';       S: 01{mINH}; O: $C9; N: 1; C: 10; R: rNIL; T: 1 ), // RET

  // Restart
  ( M: 'RST';  A: '0';      S: 01{mINH}; O: $C7; N: 1; C: 11; R: rNIL; T: 1 ), // RST n
  ( M: 'RST';  A: '1';      S: 01{mINH}; O: $CF; N: 1; C: 11; R: rNIL; T: 1 ),
  ( M: 'RST';  A: '2';      S: 01{mINH}; O: $D7; N: 1; C: 11; R: rNIL; T: 1 ),
  ( M: 'RST';  A: '3';      S: 01{mINH}; O: $DF; N: 1; C: 11; R: rNIL; T: 1 ),
  ( M: 'RST';  A: '4';      S: 01{mINH}; O: $E7; N: 1; C: 11; R: rNIL; T: 1 ),
  ( M: 'RST';  A: '5';      S: 01{mINH}; O: $EF; N: 1; C: 11; R: rNIL; T: 1 ),
  ( M: 'RST';  A: '6';      S: 01{mINH}; O: $F7; N: 1; C: 11; R: rNIL; T: 1 ),
  ( M: 'RST';  A: '7';      S: 01{mINH}; O: $FF; N: 1; C: 11; R: rNIL; T: 1 ),

  // Increment and Decrement
  ( M: 'INC';  A: 'B';      S: 02{mReg}; O: $04; N: 1; C:  5; R: rNIL; T: 1 ), // INR reg
  ( M: 'INC';  A: 'C';      S: 02{mReg}; O: $0C; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'INC';  A: 'D';      S: 02{mReg}; O: $14; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'INC';  A: 'E';      S: 02{mReg}; O: $1C; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'INC';  A: 'H';      S: 02{mReg}; O: $24; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'INC';  A: 'L';      S: 02{mReg}; O: $2C; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'INC';  A: 'A';      S: 02{mReg}; O: $3C; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'INC';  A: 'BC';     S: 02{mReg}; O: $03; N: 1; C:  5; R: rNIL; T: 1 ), // INX B
  ( M: 'INC';  A: 'DE';     S: 02{mReg}; O: $13; N: 1; C:  5; R: rNIL; T: 1 ), // INX D
  ( M: 'INC';  A: 'HL';     S: 02{mReg}; O: $23; N: 1; C:  5; R: rNIL; T: 1 ), // INX H
  ( M: 'INC';  A: '(HL)';   S: 02{mReg}; O: $34; N: 1; C: 10; R: rNIL; T: 1 ), // INR M
  ( M: 'INC';  A: 'SP';     S: 02{mReg}; O: $33; N: 1; C:  5; R: rNIL; T: 1 ), // INX SP

  ( M: 'DEC';  A: 'B';      S: 02{mReg}; O: $05; N: 1; C:  5; R: rNIL; T: 1 ), // DCR reg
  ( M: 'DEC';  A: 'C';      S: 02{mReg}; O: $0D; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'DEC';  A: 'D';      S: 02{mReg}; O: $15; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'DEC';  A: 'E';      S: 02{mReg}; O: $1D; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'DEC';  A: 'H';      S: 02{mReg}; O: $25; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'DEC';  A: 'L';      S: 02{mReg}; O: $2D; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'DEC';  A: 'A';      S: 02{mReg}; O: $3D; N: 1; C:  5; R: rNIL; T: 1 ),
  ( M: 'DEC';  A: 'BC';     S: 02{mReg}; O: $0B; N: 1; C:  5; R: rNIL; T: 1 ), // DCX B
  ( M: 'DEC';  A: 'DE';     S: 02{mReg}; O: $1B; N: 1; C:  5; R: rNIL; T: 1 ), // DCX D
  ( M: 'DEC';  A: 'HL';     S: 02{mReg}; O: $2B; N: 1; C:  5; R: rNIL; T: 1 ), // DCX H
  ( M: 'DEC';  A: '(HL)';   S: 02{mReg}; O: $35; N: 1; C: 10; R: rNIL; T: 1 ), // DCR M
  ( M: 'DEC';  A: 'SP';     S: 02{mReg}; O: $3B; N: 1; C:  5; R: rNIL; T: 1 ), // DCX SP

  // Add
  ( M: 'ADD';  A: 'A,B';    S: 02{mReg}; O: $80; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADD';  A: 'A,C';    S: 02{mReg}; O: $81; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADD';  A: 'A,D';    S: 02{mReg}; O: $82; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADD';  A: 'A,E';    S: 02{mReg}; O: $83; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADD';  A: 'A,H';    S: 02{mReg}; O: $84; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADD';  A: 'A,L';    S: 02{mReg}; O: $85; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADD';  A: 'A,(HL)'; S: 02{mReg}; O: $86; N: 1; C: 7;  R: rNIL; T: 1 ), // ADD M
  ( M: 'ADD';  A: 'A,A';    S: 02{mReg}; O: $87; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADD';  A: 'A,*';    S: 03{mIMM}; O: $C6; N: 2; C: 7;  R: rNIL; T: 1 ), // ADI

  ( M: 'ADD';  A: 'HL,BC';  S: 02{mReg}; O: $09; N: 1; C: 10; R: rNIL; T: 1 ), // DAD B
  ( M: 'ADD';  A: 'HL,DE';  S: 02{mReg}; O: $19; N: 1; C: 10; R: rNIL; T: 1 ), // DAD D
  ( M: 'ADD';  A: 'HL,HL';  S: 02{mReg}; O: $29; N: 1; C: 10; R: rNIL; T: 1 ), // DAD H
  ( M: 'ADD';  A: 'HL,SP';  S: 02{mReg}; O: $39; N: 1; C: 10; R: rNIL; T: 1 ), // DAD SP

  ( M: 'ADC';  A: 'A,B';    S: 02{mReg}; O: $88; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADC';  A: 'A,C';    S: 02{mReg}; O: $89; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADC';  A: 'A,D';    S: 02{mReg}; O: $8A; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADC';  A: 'A,E';    S: 02{mReg}; O: $8B; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADC';  A: 'A,H';    S: 02{mReg}; O: $8C; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADC';  A: 'A,L';    S: 02{mReg}; O: $8D; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADC';  A: 'A,(HL)'; S: 02{mReg}; O: $8E; N: 1; C: 7;  R: rNIL; T: 1 ), // ADC M
  ( M: 'ADC';  A: 'A,A';    S: 02{mReg}; O: $8F; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'ADC';  A: 'A,*';    S: 03{mIMM}; O: $CE; N: 2; C: 7;  R: rNIL; T: 1 ), // ACI

  // Sub
  ( M: 'SUB';  A: 'B';      S: 02{mReg}; O: $90; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SUB';  A: 'C';      S: 02{mReg}; O: $91; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SUB';  A: 'D';      S: 02{mReg}; O: $92; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SUB';  A: 'E';      S: 02{mReg}; O: $93; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SUB';  A: 'H';      S: 02{mReg}; O: $94; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SUB';  A: 'L';      S: 02{mReg}; O: $95; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SUB';  A: '(HL)';   S: 02{mReg}; O: $96; N: 1; C: 7;  R: rNIL; T: 1 ), // SUB M
  ( M: 'SUB';  A: 'A';      S: 02{mReg}; O: $97; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SUB';  A: '*';      S: 03{mIMM}; O: $D6; N: 2; C: 7;  R: rNIL; T: 1 ), // SUI

  ( M: 'SBC';  A: 'A,B';    S: 02{mReg}; O: $98; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SBC';  A: 'A,C';    S: 02{mReg}; O: $99; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SBC';  A: 'A,D';    S: 02{mReg}; O: $9A; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SBC';  A: 'A,E';    S: 02{mReg}; O: $9B; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SBC';  A: 'A,H';    S: 02{mReg}; O: $9C; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SBC';  A: 'A,L';    S: 02{mReg}; O: $9D; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SBC';  A: 'A,(HL)'; S: 02{mReg}; O: $9E; N: 1; C: 7;  R: rNIL; T: 1 ), // SBB M
  ( M: 'SBC';  A: 'A,A';    S: 02{mReg}; O: $9F; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'SBC';  A: 'A,*';    S: 03{mIMM}; O: $DE; N: 2; C: 7;  R: rNIL; T: 1 ), // SBI

  // Logical
  ( M: 'AND';  A: 'B';      S: 02{mReg}; O: $A0; N: 1; C: 4;  R: rNIL; T: 1 ), // ANA reg
  ( M: 'AND';  A: 'C';      S: 02{mReg}; O: $A1; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'AND';  A: 'D';      S: 02{mReg}; O: $A2; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'AND';  A: 'E';      S: 02{mReg}; O: $A3; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'AND';  A: 'H';      S: 02{mReg}; O: $A4; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'AND';  A: 'L';      S: 02{mReg}; O: $A5; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'AND';  A: '(HL)';   S: 02{mReg}; O: $A6; N: 1; C: 7;  R: rNIL; T: 1 ), // ANA M
  ( M: 'AND';  A: 'A';      S: 02{mReg}; O: $A7; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'AND';  A: '*';      S: 03{mIMM}; O: $E6; N: 2; C: 7;  R: rNIL; T: 1 ), // ANI

  ( M: 'XOR';  A: 'B';      S: 02{mReg}; O: $A8; N: 1; C: 4;  R: rNIL; T: 1 ), // XRA reg
  ( M: 'XOR';  A: 'C';      S: 02{mReg}; O: $A9; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'XOR';  A: 'D';      S: 02{mReg}; O: $AA; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'XOR';  A: 'E';      S: 02{mReg}; O: $AB; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'XOR';  A: 'H';      S: 02{mReg}; O: $AC; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'XOR';  A: 'L';      S: 02{mReg}; O: $AD; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'XOR';  A: '(HL)';   S: 02{mReg}; O: $AE; N: 1; C: 7;  R: rNIL; T: 1 ), // XRA M
  ( M: 'XOR';  A: 'A';      S: 02{mReg}; O: $AF; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'XOR';  A: '*';      S: 03{mIMM}; O: $EE; N: 2; C: 7;  R: rNIL; T: 1 ), // XRI

  ( M: 'OR';   A: 'B';      S: 02{mReg}; O: $B0; N: 1; C: 4;  R: rNIL; T: 1 ), // ORA reg
  ( M: 'OR';   A: 'C';      S: 02{mReg}; O: $B1; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'OR';   A: 'D';      S: 02{mReg}; O: $B2; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'OR';   A: 'E';      S: 02{mReg}; O: $B3; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'OR';   A: 'H';      S: 02{mReg}; O: $B4; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'OR';   A: 'L';      S: 02{mReg}; O: $B5; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'OR';   A: '(HL)';   S: 02{mReg}; O: $B6; N: 1; C: 7;  R: rNIL; T: 1 ), // ORA M
  ( M: 'OR';   A: 'A';      S: 02{mReg}; O: $B7; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'OR';   A: '*';      S: 03{mIMM}; O: $F6; N: 2; C: 7;  R: rNIL; T: 1 ), // ORI

  ( M: 'CP';   A: 'B';      S: 02{mReg}; O: $B8; N: 1; C: 4;  R: rNIL; T: 1 ), // CMP reg
  ( M: 'CP';   A: 'C';      S: 02{mReg}; O: $B9; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'CP';   A: 'D';      S: 02{mReg}; O: $BA; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'CP';   A: 'E';      S: 02{mReg}; O: $BB; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'CP';   A: 'H';      S: 02{mReg}; O: $BC; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'CP';   A: 'L';      S: 02{mReg}; O: $BD; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'CP';   A: '(HL)';   S: 02{mReg}; O: $BE; N: 1; C: 7;  R: rNIL; T: 1 ), // CMP M
  ( M: 'CP';   A: 'A';      S: 02{mReg}; O: $BF; N: 1; C: 4;  R: rNIL; T: 1 ),
  ( M: 'CP';   A: '*';      S: 03{mIMM}; O: $FE; N: 2; C: 7;  R: rNIL; T: 1 ), // CPI

  // Rotate
  ( M: 'RLCA'; A: '';       S: 01{mINH}; O: $07; N: 1; C: 4;  R: rNIL; T: 1 ), // RLC
  ( M: 'RRCA'; A: '';       S: 01{mINH}; O: $0F; N: 1; C: 4;  R: rNIL; T: 1 ), // RRC
  ( M: 'RLA';  A: '';       S: 01{mINH}; O: $17; N: 1; C: 4;  R: rNIL; T: 1 ), // RAL
  ( M: 'RRA';  A: '';       S: 01{mINH}; O: $1F; N: 1; C: 4;  R: rNIL; T: 1 ), // RAR

  // Specials
  ( M: 'CPL';  A: '';       S: 01{mINH}; O: $2F; N: 1; C: 4;  R: rNIL; T: 1 ), // CMA
  ( M: 'SCF';  A: '';       S: 01{mINH}; O: $37; N: 1; C: 4;  R: rNIL; T: 1 ), // STC
  ( M: 'CCF';  A: '';       S: 01{mINH}; O: $3F; N: 1; C: 4;  R: rNIL; T: 1 ), // CMC
  ( M: 'DAA';  A: '';       S: 01{mINH}; O: $27; N: 1; C: 4;  R: rNIL; T: 1 ),

  // Input / Output
  ( M: 'OUT';  A: '(*),A';  S: 03{mIMM}; O: $D3; N: 2; C: 10; R: rNIL; T: 1 ),
  ( M: 'IN';   A: 'A,(*)';  S: 03{mIMM}; O: $DB; N: 2; C: 10; R: rNIL; T: 1 ),

  // Control
  ( M: 'EI';   A: '';       S: 01{mINH}; O: $FB; N: 1; C:  4; R: rNIL; T: 1 ),
  ( M: 'DI';   A: '';       S: 01{mINH}; O: $F3; N: 1; C:  4; R: rNIL; T: 1 ),
  ( M: 'NOP';  A: '';       S: 01{mINH}; O: $00; N: 1; C:  4; R: rNIL; T: 1 ),
  ( M: 'HALT'; A: '';       S: 01{mINH}; O: $76; N: 1; C: 7;  R: rNIL; T: 1 )  // HLT
);

implementation


end.
