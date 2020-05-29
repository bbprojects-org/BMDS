{ ==============================================================================

  DISASSEMBLER FOR 8080

    Provides routines to disassemble 8080 code at a specified address into
    readable assembler code


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

unit uDis8080;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  //
  uCpu8080, uDefs8080, uCpuTypes, uCommon;

function Disassemble8080(CPU: TCpu8080; Addr: word): TDisassembledData;


implementation


{ DISASSEMBLE ONE OPCODE }

{ Return disassembled instruction in standard 8080 assembler syntax }

function Disassemble8080(CPU: TCpu8080; Addr: word): TDisassembledData;
var
  AddrMode: TAddrMode8080;
begin
  Result.Opcode := CPU.ReadMem(Addr);
  Result.MnemStr := CPU.DataByOpcode[Result.Opcode].M;
  Result.Operand := 0;
  Result.OperandStr := CPU.DataByOpcode[Result.Opcode].A;
  Result.HasOperand := False;
  AddrMode := TAddrMode8080(CPU.DataByOpcode[Result.Opcode].S);
  Result.NumBytes := CPU.DataByOpcode[Result.Opcode].N;
  case (Result.NumBytes) of

    0: begin
         Result.BytesStr := Format('%.2x', [Result.Opcode]);
         Result.MnemStr := 'Invalid opcode';
         Result.OperandStr := Format('$%.2x', [Result.Opcode]);
         AddrMode := mINH;              // Force AddrModeStr
       end;

    1: begin
         Result.BytesStr := Format('%.2x', [Result.Opcode]);
         // OperandStr already assigned the Opcode table value above
         // OperandStr := '';
       end;

    2: begin
         Result.BytesStr := Format('%.2x %.2x', [Result.Opcode, CPU.ReadMem(Addr+1)]);
         Result.Operand := CPU.ReadMem(Addr+1);
         // Replace '*' (=address) with 8-bit operand
         Result.OperandStr := StringReplace(Result.OperandStr, '*',
                                Format('$%.2x', [Result.Operand]), []);
       end;

    3: begin
         Result.BytesStr := Format('%.2x %.2x %.2x', [Result.Opcode,
                              CPU.ReadMem(Addr+1), CPU.ReadMem(Addr+2)]);
         Result.Operand := CPU.ReadMem(Addr+1) + (word(CPU.ReadMem(Addr+2)) shl 8);
         // Replace '*' (=address) with 16-bit address operand
         Result.OperandStr := StringReplace(Result.OperandStr, '*',
                                Format('$%.4x', [Result.Operand]), []);
       end;
  end;

  case (AddrMode) of
    mINH, mREG, mIMM, mDIR:  Result.AddrModeStr := '%s';
    mIND:                    Result.AddrModeStr := '(%s)';
  end;

  Result.AddBlankLine := ((Result.Opcode = $C3) or   // JP
                          (Result.Opcode = $76) or   // HALT
                          (Result.Opcode = $C9));    // RET
  Result.HasOperand := Result.HasOperand or (AddrMode > mIMM); // Exclude mIMM or get incorrect labelling

  Result.Text := Format('%.4x %-8s = %-4s %s',
                       [Addr, Result.BytesStr, Result.MnemStr,
                        Format(Result.AddrModeStr, [Result.OperandStr])]);
end;


end.

