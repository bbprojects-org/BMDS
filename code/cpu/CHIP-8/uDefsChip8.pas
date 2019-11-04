{ ==============================================================================

  CPU CHIP-8 DEFINITIONS

    Definitions for CHIP-8 Interpreter


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

{ TODO : uDefsChip8 -> do mnemonic table, registers and details for assembler }

unit uDefsChip8;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  //
  uCommon, uCpuBase;

type
  TAddrModeChip8 = (mNIL);

const

  { CPU and Register Names }

  CPU_CHIP8  = 'CHIP-8';
  CPU2_CHIP8 = 'SUPERCHIP-8';
  CPU3_CHIP8 = '**';

  { Define column details for CHIP-8 trace, including regisers }

  TRACE_COLS_CHIP8: array[0..22] of TTraceColumns = (
    (Title: 'Addr'; Width: 36; Align: taLeftJustify),    // These should be
    (Title: 'Code'; Width: 40; Align: taLeftJustify),    // common to all CPU
    (Title: ' '; Width: 16; Align: taLeftJustify),       // only varying widths
    (Title: 'Mnem'; Width: 40; Align: taLeftJustify),    //
    (Title: 'Operand'; Width: 80; Align: taLeftJustify), //
    (Title: 'V0'; Width: 24; Align: taLeftJustify),
    (Title: 'V1'; Width: 24; Align: taLeftJustify),
    (Title: 'V2'; Width: 24; Align: taLeftJustify),
    (Title: 'V3'; Width: 24; Align: taLeftJustify),
    (Title: 'V4'; Width: 24; Align: taLeftJustify),
    (Title: 'V5'; Width: 24; Align: taLeftJustify),
    (Title: 'V6'; Width: 24; Align: taLeftJustify),
    (Title: 'V7'; Width: 24; Align: taLeftJustify),
    (Title: 'V8'; Width: 24; Align: taLeftJustify),
    (Title: 'V9'; Width: 24; Align: taLeftJustify),
    (Title: 'VA'; Width: 24; Align: taLeftJustify),
    (Title: 'VB'; Width: 24; Align: taLeftJustify),
    (Title: 'VC'; Width: 24; Align: taLeftJustify),
    (Title: 'VD'; Width: 24; Align: taLeftJustify),
    (Title: 'VE'; Width: 24; Align: taLeftJustify),
    (Title: 'VF'; Width: 24; Align: taLeftJustify),
    (Title: ' I';  Width: 36; Align: taLeftJustify),
    (Title: 'SP'; Width: 24; Align: taLeftJustify)
    );

  REGISTERS_CHIP8 = 'V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 VA VB VC VD VE VF I DT ST F B';

  CHIP8_FONT: array[0..79] of byte = (
    $F0, $90, $90, $90, $F0,            // 0
    $20, $60, $20, $20, $70,            // 1
    $F0, $10, $F0, $80, $F0,            // 2
    $F0, $10, $F0, $10, $F0,            // 3
    $90, $90, $F0, $10, $10,            // 4
    $F0, $80, $F0, $10, $F0,            // 5
    $F0, $80, $F0, $90, $F0,            // 6
    $F0, $10, $20, $40, $40,            // 7
    $F0, $90, $F0, $90, $F0,            // 8
    $F0, $90, $F0, $10, $F0,            // 9
    $F0, $90, $F0, $90, $90,            // A
    $E0, $90, $E0, $90, $E0,            // B
    $F0, $80, $80, $80, $F0,            // C
    $E0, $90, $90, $90, $E0,            // D
    $F0, $80, $F0, $80, $F0,            // E
    $F0, $80, $F0, $80, $80 );          // F

  SCHIP8_FONT: array[0..159] of byte = (
    $7C, $C6, $CE, $DE, $D6, $F6, $E6, $C6, $7C, $00,   // 0
    $10, $30, $F0, $30, $30, $30, $30, $30, $FC, $00,   // 1
    $78, $CC, $CC, $0C, $18, $30, $60, $C4, $FC, $00,   // 2
    $78, $CC, $0C, $0C, $38, $0C, $0C, $CC, $78, $00,   // 3
    $0C, $1C, $3C, $6C, $CC, $FE, $0C, $0C, $1E, $00,   // 4
    $FC, $C0, $C0, $C0, $F8, $0C, $0C, $CC, $78, $00,   // 5
    $38, $60, $C0, $C0, $F8, $CC, $CC, $CC, $78, $00,   // 6
    $FC, $CC, $0C, $0C, $18, $30, $30, $30, $30, $00,   // 7
    $78, $CC, $CC, $EC, $78, $DC, $CC, $CC, $78, $00,   // 8
    $7C, $C6, $C6, $C6, $7C, $18, $18, $30, $70, $00,   // 9
    $30, $78, $CC, $CC, $CC, $FC, $CC, $CC, $CC, $00,   // A
    $FC, $66, $66, $66, $7C, $66, $66, $66, $FC, $00,   // B
    $3C, $66, $C6, $C0, $C0, $C0, $C6, $66, $3C, $00,   // C
    $F8, $6C, $66, $66, $66, $66, $66, $6C, $F8, $00,   // D
    $FE, $62, $60, $64, $7C, $64, $60, $62, $FE, $00,   // E
    $7F, $33, $31, $32, $3E, $32, $30, $30, $78, $00 ); // F

{ =============================================================================
  This section defines the instruction set for the CHIP-8 processor, and is
  used by the emulator, assembler and disassembler.
  =============================================================================

  See 'uCpuBase' for record definition TOpcodeRawData and TRule

  M: The mnemonic associated with the instruction opcode, case is irrelevant

  A: The address mode syntax, all characters are taken literally except '*'
     which represents an operand expression. Double quotes (empty string)
     mean no argument expected

  S: Ordinal value of the addressing mode. Use type casting to access its
     proper value

  O: The opcode value in hexadecimal. Max 6 chars, each pair representing
     single byte, right most pair being placed in the lowest memory position

  N: The number of bytes for the address mode in total, including the
     opcode itself

  C: The basic number of cycles for each opcode. Some opcodes use extra
     cycles depending on the operation and these are added by the relevant
     code

  R: The action / rule to be performed for this instruction and address mode

  T: The type field has bits set according to which CPU is supported }

  ASSEMBLER_REG_CHIP8 = 'A X Y';        // These must be uppercase
  
  OPCODES_CHIP8: array[0..1] of TOpcodeRawData =
  (
  ( M: 'TXS';  A: '';      S: 01{mINH};  O: $9A; N: 2; C: 2; R: rNIL; T: 1 ),
  ( M: 'TYA';  A: '';      S: 01{mINH};  O: $98; N: 2; C: 2; R: rNIL; T: 1 )
);

implementation


end.
