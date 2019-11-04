{ ==============================================================================

  CPU 6502 DEFINITIONS

    Definitions for 6502 Microprocessor


  LICENSE:

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

  =============================================================================}
           
{ TODO : uDefs6502 -> Opcode value OK for low-endian system, but need to cater for other way around }

unit uDefs6502;


{$mode objfpc}
{$H+}

interface

uses
  Classes,
  //
  uCommon, uCpuBase;

type
  TAddrMode6502 = (mNIL, mIMP, mIMM, mACC, mZP, mZPX, mZPY, mABS, mABSX, mABSY,
                   mIND, mINDX, mINDY, mREL);

const

  { CPU and Register Names }

  CPU_6502  = '6502';
  CPU2_6502 = '65C02';
  CPU3_6502 = '65C00/21';

  REGISTERS_6502 = 'A X Y';             // Must be uppercase, for assembler

  IRQ_IDX = 0;
  NMI_IDX = 1;

(*  INTERRUPTS_6502: array[0..1] of TInterruptData =
  ( (Name: 'IRQ'; Index: IRQ_IDX),
    (Name: 'NMI'; Index: NMI_IDX) ); *)

  { Processor Status Word bit definitions }

  P_NEGATIVE     = $80;
  P_OVERFLOW     = $40;
  P_RESERVED     = $20;
  P_BRK          = $10;
  P_DECIMAL      = $08;
  P_IRQ_DISABLED = $04;
  P_ZERO         = $02;
  P_CARRY        = $01;

  { Jump Addresses }

  ADDR_NMI    = $FFFA;
  ADDR_RESET  = $FFFC;
  ADDR_IRQ    = $FFFE;                  // Also used for BRK

  { Define column details for 6502 trace, including registers }

  TRACE_COLS_6502: array[0..9] of TTraceColumns = (
    (Title: 'Addr'; Width: 36; Align: taLeftJustify),    // These should be
    (Title: 'Code'; Width: 60; Align: taLeftJustify),    // common to all CPU
    (Title: ' '; Width: 16; Align: taLeftJustify),       // only varying widths
    (Title: 'Mnem'; Width: 40; Align: taLeftJustify),    //
    (Title: 'Operand'; Width: 80; Align: taLeftJustify), //
    (Title: 'A'; Width: 24; Align: taLeftJustify),
    (Title: 'X'; Width: 24; Align: taLeftJustify),
    (Title: 'Y'; Width: 24; Align: taLeftJustify),
    (Title: 'SP'; Width: 24; Align: taLeftJustify),
    (Title: 'NV-BDIZC'; Width: 72; Align: taLeftJustify)
    );

{ =============================================================================
  This section defines the instruction set for the 6502 processor, and is used
  by the emulator, assembler and disassembler

  Created:	   11 Jul 2003 - based on the table structure from TASM32
  Modifications:   28 Oct 2008 - converted to Delphi array of records
                   16 May 2014 - merged cycles and TAddrMode6502 values
  =============================================================================

  See 'uCpuBase' for record definition TOpcodeRawData and TRule

  M: The mnemonic associated with the instruction opcode, case is irrelevant

  A: The address mode syntax, all characters are taken literally except '*'
     which represents an operand expression, and '@' which represents an
     operand < 256 to show zero page addressing. Double quotes (empty string)
     mean no argument expected

  S: Ordinal value of the TAddrMode6502 addressing mode. Use type casting to
     access its proper value

  O: The opcode value in hexadecimal. Max 6 chars, each pair representing
     single byte, right most pair being placed in the lowest memory position

  N: The number of bytes for the address mode in total, including the
     opcode itself

  C: The basic number of cycles for each opcode. Some opcodes use extra
     cycles depending on the operation and these are added by the relevant
     code handling the opcode

  R: The action / rule to be performed for this instruction and address mode:
        NIL   Do nothing
        ZP    If '*' < 256 then use zero page opcode (next row)
        CR    Combine least significant bytes of first two args
     	      making second one relative to PC
        REL   Relative addressing (single byte offset)

  T: The type field has bits set according to which CPU is supported.
     All opcodes in basic instruction set have bit 0 set (1). Opcodes
     for extended processor instructions have other bits set:
       bit 1 (2) = 65C02
       bit 2 (4) = 65C00/21
  }

  OPCODES_6502: array[0..211] of TOpcodeRawData =
  (
  // This is the undefined opcode data, note Type = 0 (and NumBytes = 0)
  ( M: '???';  A: '*';     S: 00{mNIL};  O: $00; N: 0; C: 0; R: rNIL; T: 0 ),

  ( M: 'ADC';  A: '#*';    S: 02{mIMM};  O: $69; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'ADC';  A: '(*,X)'; S: 11{mINDX}; O: $61; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'ADC';  A: '(*),Y'; S: 12{mINDY}; O: $71; N: 2; C: 5; R: rNIL; T: 1 ),
  ( M: 'ADC';  A: '(*)';   S: 00{mNIL};  O: $72; N: 2; C: 0; R: rNIL; T: 2 ),
  ( M: 'ADC';  A: '*,X';   S: 08{mABSX}; O: $7D; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'ADC';  A: '@,X';   S: 05{mZPX};  O: $75; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'ADC';  A: '*,Y';   S: 09{mABSY}; O: $79; N: 3; C: 4; R: rNIL; T: 1 ),
  ( M: 'ADC';  A: '*';     S: 07{mABS};  O: $6D; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'ADC';  A: '@';     S: 04{mZP};   O: $65; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'AND';  A: '#*';    S: 02{mIMM};  O: $29; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'AND';  A: '(*,X)'; S: 11{mINDX}; O: $21; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'AND';  A: '(*),Y'; S: 12{mINDY}; O: $31; N: 2; C: 5; R: rNIL; T: 1 ),
  ( M: 'AND';  A: '(*)';   S: 00{mNIL};  O: $32; N: 2; C: 0; R: rNIL; T: 2 ),
  ( M: 'AND';  A: '*,X';   S: 08{mABSX}; O: $3D; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'AND';  A: '@,X';   S: 05{mZPX};  O: $35; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'AND';  A: '*,Y';   S: 09{mABSY}; O: $39; N: 3; C: 4; R: rNIL; T: 1 ),
  ( M: 'AND';  A: '*';     S: 07{mABS};  O: $2D; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'AND';  A: '@';     S: 04{mZP};   O: $25; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'ASL';  A: 'A';     S: 03{mACC};  O: $0A; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'ASL';  A: '*,X';   S: 08{mABSX}; O: $1E; N: 3; C: 7; R: rZP;  T: 1 ),
  ( M: 'ASL';  A: '@,X';   S: 05{mZPX};  O: $16; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'ASL';  A: '*';     S: 07{mABS};  O: $0E; N: 3; C: 6; R: rZP;  T: 1 ),
  ( M: 'ASL';  A: '@';     S: 04{mZP};   O: $06; N: 2; C: 5; R: rNIL; T: 1 ),

  ( M: 'BBR0'; A: '*,*';   S: 00{mNIL};  O: $0F; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBR1'; A: '*,*';   S: 00{mNIL};  O: $1F; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBR2'; A: '*,*';   S: 00{mNIL};  O: $2F; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBR3'; A: '*,*';   S: 00{mNIL};  O: $3F; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBR4'; A: '*,*';   S: 00{mNIL};  O: $4F; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBR5'; A: '*,*';   S: 00{mNIL};  O: $5F; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBR6'; A: '*,*';   S: 00{mNIL};  O: $6F; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBR7'; A: '*,*';   S: 00{mNIL};  O: $7F; N: 3; C: 0; R: rCR;  T: 6 ),

  ( M: 'BBS0'; A: '*,*';   S: 00{mNIL};  O: $8F; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBS1'; A: '*,*';   S: 00{mNIL};  O: $9F; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBS2'; A: '*,*';   S: 00{mNIL};  O: $AF; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBS3'; A: '*,*';   S: 00{mNIL};  O: $BF; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBS4'; A: '*,*';   S: 00{mNIL};  O: $CF; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBS5'; A: '*,*';   S: 00{mNIL};  O: $DF; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBS6'; A: '*,*';   S: 00{mNIL};  O: $EF; N: 3; C: 0; R: rCR;  T: 6 ),
  ( M: 'BBS7'; A: '*,*';   S: 00{mNIL};  O: $FF; N: 3; C: 0; R: rCR;  T: 6 ),

  ( M: 'BCC';  A: '*';     S: 13{mREL};  O: $90; N: 2; C: 2; R: rREL; T: 1 ),
  ( M: 'BCS';  A: '*';     S: 13{mREL};  O: $B0; N: 2; C: 2; R: rREL; T: 1 ),
  ( M: 'BEQ';  A: '*';     S: 13{mREL};  O: $F0; N: 2; C: 2; R: rREL; T: 1 ),
  ( M: 'BMI';  A: '*';     S: 13{mREL};  O: $30; N: 2; C: 2; R: rREL; T: 1 ),
  ( M: 'BNE';  A: '*';     S: 13{mREL};  O: $D0; N: 2; C: 2; R: rREL; T: 1 ),
  ( M: 'BPL';  A: '*';     S: 13{mREL};  O: $10; N: 2; C: 2; R: rREL; T: 1 ),
  ( M: 'BRA';  A: '*';     S: 00{mNIL};  O: $80; N: 2; C: 0; R: rREL; T: 6 ),
  ( M: 'BVC';  A: '*';     S: 13{mREL};  O: $50; N: 2; C: 2; R: rREL; T: 1 ),
  ( M: 'BVS';  A: '*';     S: 13{mREL};  O: $70; N: 2; C: 2; R: rREL; T: 1 ),

  ( M: 'BIT';  A: '#*';    S: 00{mNIL};  O: $89; N: 2; C: 0; R: rNIL; T: 2 ),
  ( M: 'BIT';  A: '*,X';   S: 00{mNIL};  O: $3C; N: 3; C: 0; R: rZP;  T: 2 ),
  ( M: 'BIT';  A: '@,X';   S: 04{mZP};   O: $24; N: 2; C: 3; R: rNIL; T: 3 ),
  ( M: 'BIT';  A: '*';     S: 07{mABS};  O: $2C; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'BIT';  A: '@';     S: 04{mZP};   O: $24; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'BRK';  A: '';      S: 01{mIMP};  O: $00; N: 1; C: 7; R: rNIL; T: 1 ),

  ( M: 'CLC';  A: '';      S: 01{mIMP};  O: $18; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'CLD';  A: '';      S: 01{mIMP};  O: $D8; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'CLI';  A: '';      S: 01{mIMP};  O: $58; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'CLV';  A: '';      S: 01{mIMP};  O: $B8; N: 1; C: 2; R: rNIL; T: 1 ),

  ( M: 'CMP';  A: '#*';    S: 02{mIMM};  O: $C9; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'CMP';  A: '(*,X)'; S: 11{mINDX}; O: $C1; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'CMP';  A: '(*),Y'; S: 12{mINDY}; O: $D1; N: 2; C: 5; R: rNIL; T: 1 ),
  ( M: 'CMP';  A: '(*)';   S: 00{mNIL};  O: $D2; N: 2; C: 0; R: rNIL; T: 2 ),
  ( M: 'CMP';  A: '*,X';   S: 08{mABSX}; O: $DD; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'CMP';  A: '@,X';   S: 05{mZPX};  O: $D5; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'CMP';  A: '*,Y';   S: 09{mABSY}; O: $D9; N: 3; C: 4; R: rNIL; T: 1 ),
  ( M: 'CMP';  A: '*';     S: 07{mABS};  O: $CD; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'CMP';  A: '@';     S: 04{mZP};   O: $C5; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'CPX';  A: '#*';    S: 02{mIMM};  O: $E0; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'CPX';  A: '*';     S: 07{mABS};  O: $EC; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'CPX';  A: '@';     S: 04{mZP};   O: $E4; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'CPY';  A: '#*';    S: 02{mIMM};  O: $C0; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'CPY';  A: '*';     S: 07{mABS};  O: $CC; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'CPY';  A: '@';     S: 04{mZP};   O: $C4; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'DEC';  A: 'A';     S: 00{mNIL};  O: $3A; N: 1; C: 0; R: rNIL; T: 2 ),
  ( M: 'DEC';  A: '*,X';   S: 08{mABSX}; O: $DE; N: 3; C: 7; R: rZP;  T: 1 ),
  ( M: 'DEC';  A: '@,X';   S: 05{mZPX};  O: $D6; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'DEC';  A: '*';     S: 07{mABS};  O: $CE; N: 3; C: 6; R: rZP;  T: 1 ),
  ( M: 'DEC';  A: '@';     S: 04{mZP};   O: $C6; N: 2; C: 5; R: rNIL; T: 1 ),

  ( M: 'DEX';  A: '';      S: 01{mIMP};  O: $CA; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'DEY';  A: '';      S: 01{mIMP};  O: $88; N: 1; C: 2; R: rNIL; T: 1 ),

  ( M: 'EOR';  A: '#*';    S: 02{mIMM};  O: $49; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'EOR';  A: '(*,X)'; S: 11{mINDX}; O: $41; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'EOR';  A: '(*),Y'; S: 12{mINDY}; O: $51; N: 2; C: 5; R: rNIL; T: 1 ),
  ( M: 'EOR';  A: '(*)';   S: 00{mNIL};  O: $52; N: 2; C: 0; R: rNIL; T: 2 ),
  ( M: 'EOR';  A: '*,X';   S: 08{mABSX}; O: $5D; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'EOR';  A: '@,X';   S: 05{mZPX};  O: $55; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'EOR';  A: '*,Y';   S: 09{mABSY}; O: $59; N: 3; C: 4; R: rNIL; T: 1 ),
  ( M: 'EOR';  A: '*';     S: 07{mABS};  O: $4D; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'EOR';  A: '@';     S: 04{mZP};   O: $45; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'INC';  A: 'A';     S: 00{mNIL};  O: $1A; N: 1; C: 0; R: rNIL; T: 2 ),
  ( M: 'INC';  A: '*,X';   S: 08{mABSX}; O: $FE; N: 3; C: 7; R: rZP;  T: 1 ),
  ( M: 'INC';  A: '@,X';   S: 05{mZPX};  O: $F6; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'INC';  A: '*';     S: 07{mABS};  O: $EE; N: 3; C: 6; R: rZP;  T: 1 ),
  ( M: 'INC';  A: '@';     S: 04{mZP};   O: $E6; N: 2; C: 5; R: rNIL; T: 1 ),

  ( M: 'INX';  A: '';      S: 01{mIMP};  O: $E8; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'INY';  A: '';      S: 01{mIMP};  O: $C8; N: 1; C: 2; R: rNIL; T: 1 ),

  ( M: 'JMP';  A: '(*,X)'; S: 00{mNIL};  O: $7C; N: 3; C: 0; R: rNIL; T: 2 ),
  ( M: 'JMP';  A: '(*)';   S: 10{mIND};  O: $6C; N: 3; C: 5; R: rNIL; T: 1 ),
  ( M: 'JMP';  A: '*';     S: 07{mABS};  O: $4C; N: 3; C: 3; R: rNIL; T: 1 ),

  ( M: 'JSR';  A: '*';     S: 07{mABS};  O: $20; N: 3; C: 6; R: rNIL; T: 1 ),

  ( M: 'LDA';  A: '#*';    S: 02{mIMM};  O: $A9; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'LDA';  A: '(*,X)'; S: 11{mINDX}; O: $A1; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'LDA';  A: '(*),Y'; S: 12{mINDY}; O: $B1; N: 2; C: 5; R: rNIL; T: 1 ),
  ( M: 'LDA';  A: '(*)';   S: 00{mNIL};  O: $B2; N: 2; C: 0; R: rNIL; T: 2 ),
  ( M: 'LDA';  A: '*,X';   S: 08{mABSX}; O: $BD; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'LDA';  A: '@,X';   S: 05{mZPX};  O: $B5; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'LDA';  A: '*,Y';   S: 09{mABSY}; O: $B9; N: 3; C: 4; R: rNIL; T: 1 ),
  ( M: 'LDA';  A: '*';     S: 07{mABS};  O: $AD; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'LDA';  A: '@';     S: 04{mZP};   O: $A5; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'LDX';  A: '#*';    S: 02{mIMM};  O: $A2; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'LDX';  A: '*,Y';   S: 09{mABSY}; O: $BE; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'LDX';  A: '@,Y';   S: 06{mZPY};  O: $B6; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'LDX';  A: '*';     S: 07{mABS};  O: $AE; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'LDX';  A: '@';     S: 04{mZP};   O: $A6; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'LDY';  A: '#*';    S: 02{mIMM};  O: $A0; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'LDY';  A: '*,X';   S: 08{mABSX}; O: $BC; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'LDY';  A: '@,X';   S: 05{mZPX};  O: $B4; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'LDY';  A: '*';     S: 07{mABS};  O: $AC; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'LDY';  A: '@';     S: 04{mZP};   O: $A4; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'LSR';  A: 'A';     S: 03{mACC};  O: $4A; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'LSR';  A: '*,X';   S: 08{mABSX}; O: $5E; N: 3; C: 7; R: rZP;  T: 1 ),
  ( M: 'LSR';  A: '@,X';   S: 05{mZPX};  O: $56; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'LSR';  A: '*';     S: 07{mABS};  O: $4E; N: 3; C: 6; R: rZP;  T: 1 ),
  ( M: 'LSR';  A: '@';     S: 04{mZP};   O: $46; N: 2; C: 5; R: rNIL; T: 1 ),

  ( M: 'MUL';  A: '';      S: 00{mNIL};  O: $02; N: 1; C: 0; R: rNIL; T: 4 ),

  ( M: 'NOP';  A: '';      S: 01{mIMP};  O: $EA; N: 1; C: 2; R: rNIL; T: 1 ),

  ( M: 'ORA';  A: '#*';    S: 02{mIMM};  O: $09; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'ORA';  A: '(*,X)'; S: 11{mINDX}; O: $01; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'ORA';  A: '(*),Y'; S: 12{mINDY}; O: $11; N: 2; C: 5; R: rNIL; T: 1 ),
  ( M: 'ORA';  A: '(*)';   S: 00{mNIL};  O: $12; N: 2; C: 0; R: rNIL; T: 2 ),
  ( M: 'ORA';  A: '*,X';   S: 08{mABSX}; O: $1D; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'ORA';  A: '@,X';   S: 05{mZPX};  O: $15; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'ORA';  A: '*,Y';   S: 09{mABSY}; O: $19; N: 3; C: 4; R: rNIL; T: 1 ),
  ( M: 'ORA';  A: '*';     S: 07{mABS};  O: $0D; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'ORA';  A: '@';     S: 04{mZP};   O: $05; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'PHA';  A: '';      S: 01{mIMP};  O: $48; N: 1; C: 3; R: rNIL; T: 1 ),
  ( M: 'PHP';  A: '';      S: 01{mIMP};  O: $08; N: 1; C: 3; R: rNIL; T: 1 ),
  ( M: 'PHX';  A: '';      S: 00{mNIL};  O: $DA; N: 1; C: 0; R: rNIL; T: 6 ),
  ( M: 'PHY';  A: '';      S: 00{mNIL};  O: $5A; N: 1; C: 0; R: rNIL; T: 6 ),
  ( M: 'PLA';  A: '';      S: 01{mIMP};  O: $68; N: 1; C: 4; R: rNIL; T: 1 ),
  ( M: 'PLP';  A: '';      S: 01{mIMP};  O: $28; N: 1; C: 4; R: rNIL; T: 1 ),
  ( M: 'PLX';  A: '';      S: 00{mNIL};  O: $FA; N: 1; C: 0; R: rNIL; T: 6 ),
  ( M: 'PLY';  A: '';      S: 00{mNIL};  O: $7A; N: 1; C: 0; R: rNIL; T: 6 ),

  ( M: 'RMB0'; A: '';      S: 00{mNIL};  O: $07; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'RMB1'; A: '';      S: 00{mNIL};  O: $17; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'RMB2'; A: '';      S: 00{mNIL};  O: $27; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'RMB3'; A: '';      S: 00{mNIL};  O: $37; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'RMB4'; A: '';      S: 00{mNIL};  O: $47; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'RMB5'; A: '';      S: 00{mNIL};  O: $57; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'RMB6'; A: '';      S: 00{mNIL};  O: $67; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'RMB7'; A: '';      S: 00{mNIL};  O: $77; N: 2; C: 0; R: rNIL; T: 6 ),

  ( M: 'ROL';  A: 'A';     S: 03{mACC};  O: $2A; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'ROL';  A: '*,X';   S: 08{mABSX}; O: $3E; N: 3; C: 7; R: rZP;  T: 1 ),
  ( M: 'ROL';  A: '@,X';   S: 05{mZPX};  O: $36; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'ROL';  A: '*';     S: 07{mABS};  O: $2E; N: 3; C: 6; R: rZP;  T: 1 ),
  ( M: 'ROL';  A: '@';     S: 04{mZP};   O: $26; N: 2; C: 5; R: rNIL; T: 1 ),

  ( M: 'ROR';  A: 'A';     S: 03{mACC};  O: $6A; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'ROR';  A: '*,X';   S: 08{mABSX}; O: $7E; N: 3; C: 7; R: rZP;  T: 1 ),
  ( M: 'ROR';  A: '@,X';   S: 05{mZPX};  O: $76; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'ROR';  A: '*';     S: 07{mABS};  O: $6E; N: 3; C: 6; R: rZP;  T: 1 ),
  ( M: 'ROR';  A: '@';     S: 04{mZP};   O: $66; N: 2; C: 5; R: rNIL; T: 1 ),

  ( M: 'RTI';  A: '';      S: 01{mIMP};  O: $40; N: 1; C: 6; R: rNIL; T: 1 ),
  ( M: 'RTS';  A: '';      S: 01{mIMP};  O: $60; N: 1; C: 6; R: rNIL; T: 1 ),

  ( M: 'SBC';  A: '#*';    S: 02{mIMM};  O: $E9; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'SBC';  A: '(*,X)'; S: 11{mINDX}; O: $E1; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'SBC';  A: '(*),Y'; S: 12{mINDY}; O: $F1; N: 2; C: 5; R: rNIL; T: 1 ),
  ( M: 'SBC';  A: '(*)';   S: 00{mNIL};  O: $F2; N: 2; C: 0; R: rNIL; T: 2 ),
  ( M: 'SBC';  A: '*,X';   S: 08{mABSX}; O: $FD; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'SBC';  A: '@,X';   S: 05{mZPX};  O: $F5; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'SBC';  A: '*,Y';   S: 09{mABSY}; O: $F9; N: 3; C: 4; R: rNIL; T: 1 ),
  ( M: 'SBC';  A: '*';     S: 07{mABS};  O: $ED; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'SBC';  A: '@';     S: 04{mZP};   O: $E5; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'SEC';  A: '';      S: 01{mIMP};  O: $38; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'SED';  A: '';      S: 01{mIMP};  O: $F8; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'SEI';  A: '';      S: 01{mIMP};  O: $78; N: 1; C: 2; R: rNIL; T: 1 ),

  ( M: 'SMB0'; A: '';      S: 00{mNIL};  O: $87; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'SMB1'; A: '';      S: 00{mNIL};  O: $97; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'SMB2'; A: '';      S: 00{mNIL};  O: $A7; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'SMB3'; A: '';      S: 00{mNIL};  O: $B7; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'SMB4'; A: '';      S: 00{mNIL};  O: $C7; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'SMB5'; A: '';      S: 00{mNIL};  O: $D7; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'SMB6'; A: '';      S: 00{mNIL};  O: $E7; N: 2; C: 0; R: rNIL; T: 6 ),
  ( M: 'SMB7'; A: '';      S: 00{mNIL};  O: $F7; N: 2; C: 0; R: rNIL; T: 6 ),

  ( M: 'STA';  A: '(*,X)'; S: 11{mINDX}; O: $81; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'STA';  A: '(*),Y'; S: 12{mINDY}; O: $91; N: 2; C: 6; R: rNIL; T: 1 ),
  ( M: 'STA';  A: '(*)';   S: 00{mNIL};  O: $92; N: 2; C: 0; R: rNIL; T: 2 ),
  ( M: 'STA';  A: '*,X';   S: 08{mABSX}; O: $9D; N: 3; C: 5; R: rZP;  T: 1 ),
  ( M: 'STA';  A: '@,X';   S: 05{mZPX};  O: $95; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'STA';  A: '*,Y';   S: 09{mABSY}; O: $99; N: 3; C: 5; R: rNIL; T: 1 ),
  ( M: 'STA';  A: '*';     S: 07{mABS};  O: $8D; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'STA';  A: '@';     S: 04{mZP};   O: $85; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'STX';  A: '*,Y';   S: 06{mZPY};  O: $96; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'STX';  A: '*';     S: 07{mABS};  O: $8E; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'STX';  A: '@';     S: 04{mZP};   O: $86; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'STY';  A: '*,X';   S: 05{mZPX};  O: $94; N: 2; C: 4; R: rNIL; T: 1 ),
  ( M: 'STY';  A: '*';     S: 07{mABS};  O: $8C; N: 3; C: 4; R: rZP;  T: 1 ),
  ( M: 'STY';  A: '@';     S: 04{mZP};   O: $84; N: 2; C: 3; R: rNIL; T: 1 ),

  ( M: 'STZ';  A: '*,X';   S: 00{mNIL};  O: $9E; N: 3; C: 0; R: rZP;  T: 2 ),
  ( M: 'STZ';  A: '*,X';   S: 04{mZP};   O: $FF; N: 2; C: 0; R: rNIL; T: 2 ),
  ( M: 'STZ';  A: '*';     S: 00{mNIL};  O: $9C; N: 3; C: 0; R: rZP;  T: 2 ),
  ( M: 'STZ';  A: '*';     S: 04{mZP};   O: $FF; N: 2; C: 0; R: rNIL; T: 2 ),

  ( M: 'TAX';  A: '';      S: 01{mIMP};  O: $AA; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'TAY';  A: '';      S: 01{mIMP};  O: $A8; N: 1; C: 2; R: rNIL; T: 1 ),

  ( M: 'TRB';  A: '*';     S: 00{mNIL};  O: $1C; N: 3; C: 0; R: rZP;  T: 2 ),
  ( M: 'TRB';  A: '*';     S: 04{mZP};   O: $FF; N: 2; C: 0; R: rNIL; T: 2 ),

  ( M: 'TSB';  A: '*';     S: 00{mNIL};  O: $0C; N: 3; C: 0; R: rZP;  T: 2 ),
  ( M: 'TSB';  A: '*';     S: 04{mZP};   O: $FF; N: 2; C: 0; R: rNIL; T: 2 ),

  ( M: 'TSX';  A: '';      S: 01{mIMP};  O: $BA; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'TXA';  A: '';      S: 01{mIMP};  O: $8A; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'TXS';  A: '';      S: 01{mIMP};  O: $9A; N: 1; C: 2; R: rNIL; T: 1 ),
  ( M: 'TYA';  A: '';      S: 01{mIMP};  O: $98; N: 1; C: 2; R: rNIL; T: 1 )

);

implementation


end.
