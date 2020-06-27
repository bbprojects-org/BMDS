{ ==============================================================================

  TYPE DEFINITIONS FOR CPU


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

unit uCpuTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCpuType = (ctNil,
              ct6502, ct65C02,          // 6502 and 65C02
              ctCHIP8, ctSCHIP,         // CHIP-8 and SCHIP
              ct8080asmO,               // 8080 with asm Original format
              ct8080asmZ                // 8080 with asm Z80 format
              );

  // Disassembler data
  TAddrLabel = record
    Address: word;                      // Address of location counter
    LabelStr: string;                   // Associated machine string label
    RW: char;                           // Read/Write flag, '' if either
    Used: boolean;                      // Has this label been used in disassembly?
  end;
  TAddrDefsArray = array of TAddrLabel;

  // Data to support disassembly; machine specific module returns generic
  // data in this record to enable complex disassembly, or just a simple text
  TDisassembledData = record
    Addr: word;                         // Instruction address
    MnemStr: string;                    // Mnemonic value
    Opcode: word;                       // Opcode value, caters for word size opcodes
    NumBytes: integer;                  // Number of bytes for this opcode
    BytesStr: string;                   // Representation of opcode/operand data
    HasOperand: boolean;                // Operand can be replaced by label
    Operand: word;                      // Operand value
    OperandStr: string;                 // Hex string value (2 or 4 char)
    AddrModeStr: string;                // Address mode string
    AddBlankLine: boolean;              // Add blank line in disassembly (after JMP/RTS/etc)
    RegStr: array of string;            // Array of register representations
    Text: string;                       // 'Simple' disassembled string
  end;

  // Definitions array for columns in trace display; defined for each CPU
  TTraceColumns = record
    Title: string;                      // Column title
    Width: integer;                     // Column width
    Align: TAlignment;                  // taCenter, taLeftJustify, taRightJustify
  end;
  TTraceColArray = array of TTraceColumns;

  // Define rules used by Assembler to support 'special' operations

  TRule = (rNIL,                        // Do nothing
           rZP,                         // If operand < 256, use Zero Page opcode
           rREL,                        // Relative instruction, single byte offset
           rCR);                        // ???

  // Define info format for each opcode
  // These support the CPU emulators, assembler, and disassembler

  TOpcodeRawData = record
    M: string[5];                       // Mnemonic
    A: string[10];                      // Address mode mask
    S: byte;                            // Ordinal value of address mode
    O: Longword;		        // Opcode, allows up to 4 bytes
    N: byte;                            // Number of instruction bytes inc opcode
    C: byte;                            // Number of basic cycles for opcode execution
    R: TRule;                           // Rule used to modify instructions
    T: byte;                            // CPU type, for extended instruction set
  end;
  TOpcodeArray = array of TOpcodeRawData;

  // Define data format for a CPU

  TCpuInfo = record
    Name: string;
    RegsReplace: string;
    RegsKeywords: string;
    LittleEndian: boolean;
    SupportsAssembler: boolean;
    SupportsDisassembler: boolean;
    SupportsCpuTest: boolean;
    TraceWidth: integer;
    RegistersHeight: integer;
    Template: string;
  end;

const
  DIS_FORMAT  = '%-8s = %-4s %s';       // opcode bytes, mnemonic, operand
  DIS_LFORMAT = '%-13s %-5s %-18s %s';  // label, mnemonic, operand, comment


implementation

end.

