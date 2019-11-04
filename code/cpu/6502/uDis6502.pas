{ ==============================================================================

  DISASSEMBLER FOR 6502

    Provides routines to disassemble code at a specified address into user
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

unit uDis6502;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  //
  uCpu6502, uDefs6502, uCommon;

function Disassemble6502(CPU: TCpu6502; Addr: word): TDisassembledData;


implementation


{ DISASSEMBLE ONE OPCODE }

{ Return disassembled instruction in standard 6502 assembler syntax }

function Disassemble6502(CPU: TCpu6502; Addr: word): TDisassembledData;
var
  AddrMode: TAddrMode6502;
begin
  Result.Opcode := CPU.MemRead(Addr);
  Result.MnemStr := CPU.OpcodeData(Result.Opcode).M;
  Result.Operand := 0;
  Result.HasOperand := False;
  AddrMode := TAddrMode6502(CPU.OpcodeData(Result.Opcode).S);
  Result.NumBytes := CPU.OpcodeData(Result.Opcode).N;
  case (Result.NumBytes) of

    0: begin
         Result.BytesStr := Format('%.2x', [Result.Opcode]);
         Result.MnemStr := 'Invalid opcode';
         Result.OperandStr := Format('$%.2x', [Result.Opcode]);
       end;

    1: begin
         Result.BytesStr := Format('%.2x', [Result.Opcode]);
         Result.OperandStr := '';
       end;

    2: begin
         Result.BytesStr := Format('%.2x %.2x', [Result.Opcode, CPU.MemRead(Addr+1)]);
         if (AddrMode = mREL) then
           begin
             Result.Operand := Addr + 2 + shortint(CPU.MemRead(Addr+1));
             Result.OperandStr := Format('$%.4x', [Result.Operand]);
             Result.HasOperand := True; // Allows replacement by label
           end
         else
           begin
             Result.Operand := CPU.MemRead(Addr+1);
             Result.OperandStr := Format('$%.2x', [Result.Operand]);
           end;
       end;

    3: begin
         Result.BytesStr := Format('%.2x %.2x%.2x', [Result.Opcode,
                                   CPU.MemRead(Addr+1), CPU.MemRead(Addr+2)]);
         Result.Operand := CPU.MemRead(Addr+1) + (word(CPU.MemRead(Addr+2)) shl 8);
         Result.OperandStr := Format('$%.4x', [Result.Operand]);
       end;
  end;
  case (AddrMode) of
    mIMM:             Result.AddrModeStr := '#%s';
    mACC:             Result.AddrModeStr := 'A';
    mIMP:	      Result.AddrModeStr := '';
    mREL, mABS, mZP:  Result.AddrModeStr := '%s';
    mIND:    	      Result.AddrModeStr := '(%s)';
    mABSX, mZPX:      Result.AddrModeStr := '%s,X';
    mABSY, mZPY:      Result.AddrModeStr := '%s,Y';
    mINDX:   	      Result.AddrModeStr := '(%s,X)';
    mINDY:   	      Result.AddrModeStr := '(%s),Y';
    mNIL:             Result.AddrModeStr := '%s'; // For invalid opcodes
  end;

  Result.AddBlankLine := (Pos(Result.MnemStr, 'RTS/RTI/JMP') > 0);
  Result.HasOperand := Result.HasOperand or (AddrMode >= mZP);

  Result.Text := Format('%.4x %-7s = %s %s',
                       [Addr, Result.BytesStr, Result.MnemStr,
                        Format(Result.AddrModeStr, [Result.OperandStr])]);
end;


end.

