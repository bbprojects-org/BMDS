{ ==============================================================================

  DISASSEMBLER FOR CHIP-8

    Provides routines to disassemble CHIP-8 code at a specified address into
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

unit uDisChip8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  //
  uCpuChip8, uDefsChip8, uCpuTypes;

function DisassembleChip8(CPU: TCpuChip8; Addr: word): TDisassembledData;


implementation

{ DISASSEMBLE ONE OPCODE }

{ Return disassembled instruction in made-up CHIP8 assembler syntax }

function DisassembleChip8(CPU: TCpuChip8; Addr: word): TDisassembledData;
var
  Opcode: word;
  X, Y: byte;
  Operand0FFF, Operand00FF, Operand000F: word;
  MnemonicStr, OperandStr: string;
  IsSCHIP: boolean;
begin
  Opcode := (CPU.MemRead(Addr) shl 8) or CPU.MemRead(Addr+1);
  X := (Opcode and $0F00) shr 8;
  Y := (Opcode and $00F0) shr 4;
  Operand0FFF := Opcode and $0FFF;
  Operand00FF := Opcode and $00FF;
  Operand000F := Opcode and $000F;

  Result.AddBlankLine := False;
  Result.HasOperand := False;
  OperandStr := '';
  Result.AddrModeStr := '%s';           // Default = just print string
  IsSCHIP := False;

  case (Opcode and $F000) of            // Mask off instruction bits

    $0000: // Various, check next bits
       begin
         case (Opcode) of
           $00E0: MnemonicStr := 'CLS';
           $00EE: MnemonicStr := 'RET';
           $00FB: begin                 // Scroll 4 pixels right
                    MnemonicStr := 'SCR';
                    IsSCHIP := True;
                  end;
           $00FC: begin                 // Scroll 4 pixels left
                    MnemonicStr := 'SCL';
                    IsSCHIP := True;
                  end;
           $00FD: begin                 // Exit CHIP interpreter
                    MnemonicStr := 'EXIT';
                    IsSCHIP := True;
                  end;
           $00FE: begin                 // Disable extended screen mode
                    MnemonicStr := 'LOW';
                    IsSCHIP := True;
                  end;
           $00FF: begin                 // Enable extended screen mode
                    MnemonicStr := 'HIGH';
                    IsSCHIP := True;
                  end;
         else
           if ((Opcode and $FFF0) = $00C0) then
             begin                      // $00CN: scroll N lines down
               MnemonicStr := 'SCD';
               OperandStr := Format('$%.1x', [Operand000F]);
               IsSCHIP := True;
             end
           else
             MnemonicStr := 'UNK';
         end;
       end;

    $1000: // $1NNN: jump to address NNN
           begin
             MnemonicStr := 'JP';
             Result.Operand := Operand0FFF;
             Result.HasOperand := True;
             OperandStr := Format('$%.4x', [Result.Operand]);
             Result.AddrModeStr := '%s';
           end;

    $2000: // $2NNN: call subroutine at address NNN
           begin
             MnemonicStr := 'CALL';
             Result.Operand := Operand0FFF;
             Result.HasOperand := True;
             OperandStr := Format('$%.4x', [Result.Operand]);
           end;

    $3000: // $3xNN: skips the next instruction if Vx equals NN
           begin
             MnemonicStr := 'SE';
             OperandStr := Format('V%x,$%x', [X, Operand00FF]);
           end;

    $4000: // $4xNN: skips the next instruction if Vx does not equal NN
           begin
             MnemonicStr := 'SNE';
             OperandStr := Format('V%x,$%x', [X, Operand00FF]);
           end;

    $5000: // $5xy0: skips the next instruction if Vx equals Vy
           begin
             MnemonicStr := 'SE';
             OperandStr := Format('V%x,V%x', [X, Y]);
           end;

    $6000: // $6xNN: set Vx to value NN
           begin
             MnemonicStr := 'LD';
             OperandStr := Format('V%x,$%x', [X, Operand00FF]);
           end;

    $7000: // $7xNN: add value NN to Vx
           begin
             MnemonicStr := 'ADD';
             OperandStr := Format('V%x,$%x', [X, Operand00FF]);
           end;

    $8000: // Various, check next bits
       begin
         case (Opcode and $000F) of
           $0000: // $8xy0: set Vx to value of Vy
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('V%x,V%x', [X, Y]);
              end;

           $0001: // $8xy1: set Vx to (Vx or Vy)
              begin
                MnemonicStr := 'OR';
                OperandStr := Format('V%x,V%x', [X, Y]);
              end;

           $0002: // $8xy2: set Vx to (Vx and Vy)
              begin
                MnemonicStr := 'AND';
                OperandStr := Format('V%x,V%x', [X, Y]);
              end;

           $0003: // $8xy3: set Vx xor (Vx or Vy)
              begin
                MnemonicStr := 'XOR';
                OperandStr := Format('V%x,V%x', [X, Y]);
              end;

           $0004: // $8xy4: Vx := Vx + Vy, VF set to 1 if carry else 0
              begin
                MnemonicStr := 'ADD';
                OperandStr := Format('V%x,V%x', [X, Y]);
              end;

           $0005: // $8xy5: Vx := Vx - Vy, VF set to 0 if borrow else 1
              begin
                MnemonicStr := 'SUB';
                OperandStr := Format('V%x,V%x', [X, Y]);
              end;

           $0006: // $8xy6: VF set to LSB of Vx, Vx := Vx shr 1
              begin
                MnemonicStr := 'SHR';
                OperandStr := Format('V%x', [X]);
              end;

           $0007: // $8xy7: Vx := Vy - Vx, VF set to 0 if borrow else 1
              begin
                MnemonicStr := 'SUBN';
                OperandStr := Format('V%x,V%x', [X, Y]);
              end;

           $000E: // $8xyE: VF set to MSB of Vx, Vx := Vx shl 1
              begin
                MnemonicStr := 'SHL';
                OperandStr := Format('V%x', [X]);
              end;

         else
           MnemonicStr := 'UNK';
         end;
       end;

    $9000: // $9xy0: skips the next instruction if Vx does not equal Vy
           begin
             MnemonicStr := 'SNE';
             OperandStr := Format('V%x,V%x', [X, Y]);
           end;

    $A000: // $ANNN: set I to address NNN
           begin
             MnemonicStr := 'LD';
             Result.Operand := Operand0FFF;
             Result.HasOperand := True;
             OperandStr := Format('$%x', [Result.Operand]);
             Result.AddrModeStr := 'I,%s';
           end;

    $B000: // $BNNN: jump to address (NNN + V0)
           begin
             MnemonicStr := 'JP';
             Result.Operand := Operand0FFF;
             Result.HasOperand := True;
             OperandStr := Format('%x', [Result.Operand]);
             Result.AddrModeStr := 'V0,%s';
           end;

    $C000: // $CxNN: sets Vx to (RandomNumber and NN)
           begin
             MnemonicStr := 'RND';
             OperandStr := Format('V%x,$%x', [X, Operand00FF]);
           end;

    $D000: // $DxyN: draw sprite at coords Vx,Vy, width 8, height N
           //        first row byte starts at memory location I
           //        VF set to 1 if any active screen pixels collide
           begin
             MnemonicStr := 'DRW';
             OperandStr := Format('V%x,V%x,%d', [X, Y, Operand000F]);
             if (Operand000F = 0) then
               IsSCHIP := True;         // If extended screen mode, show 16x16 sprite
           end;

    $E000: // Various, check next bits
       begin
         case (Opcode and $00FF) of
           $009E: // $Ex9E: skip next instruction if key in Vx is pressed
              begin
                MnemonicStr := 'SKP';
                OperandStr := Format('V%x', [X]);
              end;

           $00A1: // $ExA1: skip next instruction if key in Vx is not pressed
              begin
                MnemonicStr := 'SKNP';
                OperandStr := Format('V%x', [X]);
              end;

         else
           MnemonicStr := 'UNK';
         end;
       end;

    $F000: // Various, check next bits
       begin
         case (Opcode and $00FF) of
           $0007: // $Fx07: set Vx to value of the delay timer
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('V%x,DT', [X]);
              end;

           $000A: // $Fx0A: wait for key press, store in Vx
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('V%x,K', [X]);
              end;

           $0015: // $Fx15: set delay timer to value in Vx
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('DT,V%x', [X]);
              end;

           $0018: // $Fx18: set sound timer to value in Vx
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('ST,V%x', [X]);
              end;

           $001E: // $Fx1E: add Vx to I
              begin
                MnemonicStr := 'ADD';
                OperandStr := Format('I,V%x', [X]);
              end;

           $0029: // $Fx29: set I to location of sprite for font character in Vx
                  //        characters are 4x5 pixels (but full byte width)
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('F,V%x', [X]);
              end;

           $0030: // $Fx30: set I to location of sprite for font character in Vx
                  //        10 byte font sprite
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('HF,V%x', [X]);
                IsSCHIP := True;
              end;

           $0033: // $Fx33: store BCD representation of Vx in I, I+1, I+2
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('B,V%x', [X]);
              end;

           $0055: // $Fx55: store V0 to Vx in memory starting at address I
                  //        incrementing I for each register
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('[I],V%x', [X]);
              end;

           $0065: // $Fx65: load V0 to Vx in memory starting at address I
                  //        incrementing I for each register
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('V%x,[I]', [X]);
              end;

           $0075: // $Fx75: store V0 to Vx in HP-48 RPL user flags (x <= 7)
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('R,V%x', [X]);
                IsSCHIP := True;
              end;

           $0085: // $Fx85: read V0 to Vx from HP-48 RPL user flags (x <= 7)
              begin
                MnemonicStr := 'LD';
                OperandStr := Format('V%x,R', [X]);
                IsSCHIP := True;
              end;

         else
           MnemonicStr := 'UNK';
         end;
       end;
  else
    MnemonicStr := 'UNK';
  end;

  if ((IsSCHIP) and (CPU.CpuType <> ctSCHIP)) then
    begin
      MnemonicStr := 'UNK';
      OperandStr := '';
    end;

  Result.Opcode := Opcode;
  Result.BytesStr := Format('%.4x', [Opcode]);
  Result.MnemStr := MnemonicStr;
  Result.NumBytes := 2;                 // Always two for CHIP-8
  Result.OperandStr := OperandStr;
  Result.AddBlankLine := (Pos(MnemonicStr, 'RET,JP') > 0);
  if (MnemonicStr = 'UNK') then
    MnemonicStr := Format('Unknown opcode: $%.4x', [Opcode]);
  Result.Text := Format(DIS_FORMAT,
                        [Result.BytesStr, Result.MnemStr, Result.OperandStr]);
end;


end.

