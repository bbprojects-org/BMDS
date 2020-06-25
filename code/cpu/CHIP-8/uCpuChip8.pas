{ ==============================================================================

  CHIP-8 CPU

    CHIP-8 is an interpreted minimalist programming language that was designed
    by Joseph Weisbecker in the 1970s for use on the RCA COSMAC VIP computer.
    It is not a CPU as such.

    CPU:      The 'CPU' has 16 8-bit general purpose registers, a stack
              pointer, a 16-bit index register and program counter. It has two
              timers; a delay timer and a sound timer, both count down at 60Hz.

    Input:    Input is via 16 hex keys

    Graphics: 64 x 32 pixel screen, instructions support sprites, and a built
              in hexadecimal font

    Based on info from:
    1. How to write an emulator (CHIP-8 interpreter) by Laurence Muller
       <http://www.multigesture.net/articles/how-to-write-an-emulator-chip-8-interpreter/>
    2. Mastering CHIP-8 by Matthew Mikolay
       <http://mattmik.com/chip8.html>


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

unit uCpuChip8;

{$mode objfpc}{$H+}
{$R-}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  //
  uCpuBase, uDefsChip8, uCommon, uCpuTypes, uMachineBase;

type
  TRegsChip8 = record
    V: array[0..15] of byte;            // Sixteen 8-bit general purpose registers
    I: word;                            // Index 16-bit register
    PC: word;                           // Program counter 16-bit
    SP: byte;                           // Stack pointer 8-bit
  end;

  TKeys = array[0..15] of boolean;      // Sixteen key 'keypad'
  TPixels = array[0..64*32-1] of byte;  // Display buffer in pixels
  TStack = array[0..15] of word;

  { TCpuChip8 }

  TCpuChip8 = class(TCpuBase)
  private
    fMachine: TMachineBase;             // Pointer to parent machine properties
    fKeys: TKeys;
    fRegs: TRegsChip8;
    fPixelsArray: TPixels;
    DoneInvalidWarning: boolean;

    fStack: TStack;
    TraceList: array[0..TRACE_MAX] of TRegsChip8;

    procedure ProcessOpcode;
    function  GetKey(Index: byte): boolean;
    procedure SetKey(Index: byte; Value: boolean);
    procedure SetMachine(Value: TMachineBase);
  protected
    function GetPC: word; override;
    function GetTraceColumns: TTraceColArray; override;
    function GetRegs: TRegsChip8;
  public
    constructor Create(ct: TCpuType); override;
    destructor  Destroy; override;
    procedure Reset; override;
    function  ExecuteInstruction: integer; override;
    procedure Interrupt({%H-}AIndex: byte; {%H-}Value: boolean = True); override;
    function  GetTrace(Index: integer): TDisassembledData; override;
    function  GetDisassembly(Addr:word): TDisassembledData; override;
    //
    function  MemRead(addr: word): byte;
    procedure MemWrite(addr: word; value: byte);

    property  Pixels: TPixels read fPixelsArray;
    property  Regs: TRegsChip8 read GetRegs write fRegs;
    property  Stack: TStack read fStack;
    property  Machine: TMachineBase read fMachine write SetMachine;
    property  Key[Index: byte]: boolean read GetKey write SetKey;
  end;


implementation

uses
  uMachineChip8, uDisChip8, uRegistersFrameChip8;


{ CREATE }

constructor TCpuChip8.Create(ct: TCpuType);
var
  ThisTypeMask: byte;
begin
  fRegistersFrame := TRegistersFrameChip8.Create(nil);
  (fRegistersFrame as TRegistersFrameChip8).CpuRef := self;

  fCpuType  := ct;
  fCpuState := csStopped;

  SetInfo(INFO_CHIP8);
  case ct of
    ctCHIP8:  ThisTypeMask := %01;
    ctSCHIP8: begin
               { TODO : uCpuChip8 -> add support for SCHIP }
               MessageWarning('Not Supported', 'SCHIP currently not supported, defaulting to CHIP8');
               //ThisTypeMask := %11;
               ThisTypeMask := %01;
               fInfo.Name := 'SCHIP-8'; // Replace default name
             end;
  end;
  BuildOpcodesData(OPCODES_CHIP8, ThisTypeMask);
  Reset;
end;


{ DESTROY }

destructor TCpuChip8.Destroy;
begin
  inherited;
end;


{ MEMORY ACCESS }

function TCpuChip8.MemRead(addr: word): byte;
begin
  Result := $FF;
  if (Assigned(fOnRead)) then
    fOnRead(self, addr, Result);
end;


procedure TCpuChip8.MemWrite(addr: word; value: byte);
begin
  if (Assigned(fOnWrite)) then
    fOnWrite(self, addr, value);
end;


{ SET MACHINE }

procedure TCpuChip8.SetMachine(Value: TMachineBase);
begin
  fMachine := TMachineChip8(Value);
end;


{ GET / SET KEYS }

function TCpuChip8.GetKey(Index: byte): boolean;
begin
  Result := fKeys[Index];
end;


procedure TCpuChip8.SetKey(Index: byte; Value: boolean);
begin
  fKeys[Index] := Value;
end;


{ GETTERS }

function TCpuChip8.GetTraceColumns: TTraceColArray;
var
  idx: integer;
begin
  SetLength(Result, length(TRACE_COLS_CHIP8));
  for idx := 0 to length(TRACE_COLS_CHIP8)-1 do
    Result[idx] := TRACE_COLS_CHIP8[idx];
end;


function TCpuChip8.GetPC: word;
begin
  Result := fRegs.PC;
end;


function TCpuChip8.GetRegs: TRegsChip8;
begin
  Result := fRegs;
end;


{ RESET }

procedure TCpuChip8.Reset;
var
  i: integer;
begin
  for i := 0 to 15 do                   // Clear registers
    fRegs.V[i] := 0;
  fRegs.PC := $200;                     // Default start addr for CHIP8 [[For ETI-660 it was $600]]
  fRegs.I  := 0;
  fRegs.SP := 0;
  fCpuState := csStopped;

  for i := 0 to 79 do                   // Initialise character fonts
    MemWrite(i, CHIP8_FONT[i]);
  for i := 0 to length(fPixelsArray) - 1 do // Clear screen
    fPixelsArray[i] := 0;

  ResetTrace;
  DoneInvalidWarning := False;
end;


{ INTERRUPT }

procedure TCpuChip8.Interrupt(AIndex: byte; Value: boolean = True);
begin
  // Not supported in CHIP-8, do nothing
end;


{ EXECUTE ONE INSTRUCTION }

{ Return number of clock cycles used by instruction; not used for CHIP-8 }

function TCpuChip8.ExecuteInstruction: integer;
begin
  fCpuState := csRunning;
  ProcessOpcode;
  fCpuState := csStopped;
  Result := 0;
end;


{ PROCESS EACH OPCODE }

procedure TCpuChip8.ProcessOpcode;
var
  Opcode: word;
  i, IndexX, IndexY: byte;
  X, Y, Row, RowOfPixels, Height: byte;
  ThisBit, ThisPixel: integer;
  Unknown, KeyPressed: boolean;
begin
  // Trace Execution; save the location and registers' state before we execute
  if (fTraceIndex >= TRACE_MAX) then
    begin
      fTraceIndex := 0;
      fTraceOverflow := True;
    end;
  TraceList[fTraceIndex] := Regs;       // ... save everything
  Inc(fTraceIndex);

  // Execute the current opcode
  Unknown := False;
  Opcode := (MemRead(fRegs.PC) shl 8) or MemRead(fRegs.PC+1);
  IndexX := (Opcode and $0F00) shr 8;
  IndexY := (Opcode and $00F0) shr 4;

  Inc(fRegs.PC, 2);                     // Each instruction is two bytes

  case (Opcode and $F000) of            // Mask off instruction bits

    $0000: // Various, check next bits
           begin
             case (Opcode and $000F) of

               $0000: // $00E0: clear screen
                  begin
                    for ThisPixel := 0 to (length(fPixelsArray) - 1) do
                      fPixelsArray[ThisPixel] := 0;
                  end;

               $000E: // $00EE: return from subroutine
                  begin
                    Dec(fRegs.SP);      // Restore PC from stack
                    fRegs.SP := fRegs.SP and $0F; // Limit to 16 entries
                    fRegs.PC := fStack[fRegs.SP];
                  end;

             else
               Unknown := True;
             end;
           end;

    $1000: // $1NNN: jump to address NNN
           begin
             fRegs.PC := Opcode and $0FFF;
           end;

    $2000: // $2NNN: call subroutine at address NNN
           begin
             fStack[fRegs.SP] := fRegs.PC; // Save current address in stack
                                        // Note: already incremented
             Inc(fRegs.SP);
             fRegs.SP := fRegs.SP and $0F; // Limit to 16 entries
             fRegs.PC := Opcode and $0FFF;
           end;

    $3000: // $3xNN: skips the next instruction if Vx equals NN
           begin
             if (fRegs.V[IndexX] = (Opcode and $00FF)) then
               Inc(fRegs.PC, 2);        // Already pointing at next instr, point past it
           end;

    $4000: // $4xNN: skips the next instruction if Vx does not equal NN
           begin
             if (fRegs.V[IndexX] <> (Opcode and $00FF)) then
               Inc(fRegs.PC, 2);        // Already pointing at next instr, point past it
           end;

    $5000: // $5xy0: skips the next instruction if Vx equals Vy
           begin
             if (fRegs.V[IndexX] = (fRegs.V[IndexY])) then
               Inc(fRegs.PC, 2);        // Already pointing at next instr, point past it
           end;

    $6000: // $6xNN: set Vx to value NN
           begin
             fRegs.V[IndexX] := Opcode and $00FF;
           end;

    $7000: // $7xNN: add value NN to Vx
           begin
             fRegs.V[IndexX] := fRegs.V[IndexX] + (Opcode and $00FF);
           end;

    $8000: // Various, check next bits
           begin
             case (Opcode and $000F) of

               $0000: // $8xy0: set Vx to value of Vy
                  begin
                    fRegs.V[IndexX] := fRegs.V[IndexY];
                  end;

               $0001: // $8xy1: set Vx to (Vx or Vy)
                  begin
                    fRegs.V[IndexX] := fRegs.V[IndexX] or fRegs.V[IndexY];
                  end;

               $0002: // $8xy2: set Vx to (Vx and Vy)
                  begin
                    fRegs.V[IndexX] := fRegs.V[IndexX] and fRegs.V[IndexY];
                  end;

               $0003: // $8xy3: set Vx xor (Vx or Vy)
                  begin
                    fRegs.V[IndexX] := fRegs.V[IndexX] xor fRegs.V[IndexY];
                  end;

               $0004: // $8xy4: Vx := Vx + Vy, VF set to 1 if carry else 0
                  begin
                    if (fRegs.V[IndexY] > ($FF - fRegs.V[IndexX])) then
                      fRegs.V[$F] := 1
                    else
                      fRegs.V[$F] := 0;
                    fRegs.V[IndexX] := fRegs.V[IndexX] + fRegs.V[IndexY];
                  end;

               $0005: // $8xy5: Vx := Vx - Vy, VF set to 0 if borrow else 1
                  begin
                    if (fRegs.V[IndexY] > fRegs.V[IndexX]) then
                      fRegs.V[$F] := 0
                    else
                      fRegs.V[$F] := 1;
                    fRegs.V[IndexX] := fRegs.V[IndexX] - fRegs.V[IndexY];
                  end;

               $0006: // $8xy6: VF set to LSB of Vx, Vx := Vx shr 1
                  begin
                    fRegs.V[$F] :=  fRegs.V[IndexX] and $01;
                    fRegs.V[IndexX] := fRegs.V[IndexX] shr 1;
                  end;

               $0007: // $8xy7: Vx := Vy - Vx, VF set to 0 if borrow else 1
                  begin
                    if (fRegs.V[IndexX] > fRegs.V[IndexY]) then
                      fRegs.V[$F] := 0
                    else
                      fRegs.V[$F] := 1;
                    fRegs.V[IndexX] := fRegs.V[IndexY] - fRegs.V[IndexX];
                  end;

               $000E: // $8xyE: VF set to MSB of Vx, Vx := Vx shl 1
                  begin
                    fRegs.V[$F] :=  fRegs.V[IndexX] shr 7;
                    fRegs.V[IndexX] := fRegs.V[IndexX] shl 1;
                  end;

             else
               Unknown := True;
             end;
           end;

    $9000: // $9xy0: skips the next instruction if Vx does not equal Vy
           begin
             if (fRegs.V[IndexX] <> fRegs.V[IndexY]) then
               Inc(fRegs.PC, 2);        // Already pointing at next instr, point past it
           end;

    $A000: // $ANNN: set I to address NNN
           begin
             fRegs.I := Opcode and $0FFF;
           end;

    $B000: // $BNNN: jump to address (NNN + V0)
           begin
             fRegs.PC := (Opcode and $0FFF) + fRegs.V[0];
           end;

    $C000: // $CxNN: sets Vx to (RandomNumber and NN)
           begin
             Randomize;
             fRegs.V[IndexX] := Random(255) and (Opcode and $00FF);
           end;

    $D000: // $DxyN: draw sprite at coords Vx,Vy, width 8, height N
           //        first row byte starts at memory location I
           //        VF set to 1 if any active screen pixels collide
           begin
             X := fRegs.V[IndexX];
             Y := fRegs.V[IndexY];
             Height := (Opcode and $000F);
             fRegs.V[$F] := 0;
             for Row := 0 to (Height - 1) do
               begin
                 RowOfPixels := MemRead(fRegs.I + Row);
                 for ThisBit := 0 to 7 do
                   begin
                     if ((RowOfPixels and ($80 shr ThisBit)) <> 0) then
                       begin
                         ThisPixel := X + ThisBit + ((Y + Row) * 64);
                         // Check if collision occurred
                         if (fPixelsArray[ThisPixel] = 1) then
                           fRegs.V[$F] := 1;
                         fPixelsArray[ThisPixel] := fPixelsArray[ThisPixel] xor 1;
                       end;
                   end;
               end;
           end;

    $E000: // Various, check next bits
           begin
             case (Opcode and $00FF) of

               $009E: // $Ex9E: skip next instruction if key in Vx is pressed
                  begin
                    if (fKeys[fRegs.V[IndexX]]) then
                      Inc(fRegs.PC, 2); // Already pointing at next instr, point past it
                  end;

               $00A1: // $ExA1: skip next instruction if key in Vx is not pressed
                  begin
                    if (not fKeys[fRegs.V[IndexX]]) then
                      Inc(fRegs.PC, 2); // Already pointing at next instr, point past it
                  end;

             else
               Unknown := True;
             end;
           end;

    $F000: // Various, check next bits
           begin
             case (Opcode and $00FF) of

               $0007: // $Fx07: set Vx to value of the delay timer
                  begin
                    fRegs.V[IndexX] := (fMachine as TMachineChip8).DelayTimer;
                  end;

               $000A: // $Fx0A: wait for key press, store in Vx
                  begin
                    KeyPressed := False;
                    while (not KeyPressed) do
                      begin
                        for i := 0 to 15 do
                          if (fKeys[i]) then
                            begin
                              fRegs.V[IndexX] := i;
                              KeyPressed := True;
                            end;
                      end;
                  end;

               $0015: // $Fx15: set delay timer to value in Vx
                  begin
                    (fMachine as TMachineChip8).DelayTimer := fRegs.V[IndexX];
                  end;

               $0018: // $Fx18: set sound timer to value in Vx
                  begin
                    (fMachine as TMachineChip8).SoundTimer := fRegs.V[IndexX];
                  end;

               $001E: // $Fx1E: add Vx to I
                  begin
                    if ((fRegs.I + fRegs.V[IndexX]) > $0FFF) then
                      fRegs.V[$F] := 1  // Check for overflow
                    else
                      fRegs.V[$F] := 0;
                    fRegs.I := fRegs.I + fRegs.V[IndexX];
                  end;

               $0029: // $Fx29: set I to location of sprite for font character in Vx
                      //        characters are 4x5 pixels (but full byte width)
                  begin
                    fRegs.I := fRegs.V[IndexX] * 5;
                  end;

               $0033: // $Fx33: store BCD representation of Vx in I, I+1, I+2
                  begin
                    MemWrite(fRegs.I,   fRegs.V[IndexX] div 100);
                    MemWrite(fRegs.I+1, fRegs.V[IndexX] div 10 mod 10);
                    MemWrite(fRegs.I+2, fRegs.V[IndexX] mod 10);
                  end;

               $0055: // $Fx55: store V0 to Vx in memory starting at address I
                  begin
                    for i := 0 to (IndexX) do
                      MemWrite(fRegs.I + i, fRegs.V[i]);
                  end;

               $0065: // $Fx65: load V0 to Vx from memory starting at address I
                  begin
                    for i := 0 to (IndexX) do
                      fRegs.V[i] := MemRead(fRegs.I + i);
                  end;

             else
               Unknown := True;
             end;
           end;

  else
    Unknown := True;
  end;

  if (Unknown and (not DoneInvalidWarning)) then
    begin
      MessageWarning('Not Found', Format('Unknown opcode: $%.4x', [Opcode]));
      DoneInvalidWarning := True;
    end;
end;


{ GET TRACE }

{ Return execution trace for given index into trace list. If index exceeds
  trace count then return empty string }

function TCpuChip8.GetTrace(Index: integer): TDisassembledData;
var
  idx: integer;
  TraceRegs: TRegsChip8;
begin
  Result.Text := '';
  if (fCpuState = csRunning)            // No response if CPU running
     or ((fTraceIndex = 0) and (not fTraceOverflow))
     or ((Index > fTraceIndex) and (not fTraceOverflow)) then
    Result.Text := ''
  else
    begin
      if (fTraceOverflow) then
        Index := (Index + fTraceIndex) and TRACE_MASK; // Adjust for rollover
      TraceRegs := TraceList[Index];    // Set of registers
      Result := GetDisassembly(TraceRegs.PC);
      Result.Addr := TraceRegs.PC;
      SetLength(Result.RegStr, 18);
      for idx := 0 to 15 do
        Result.RegStr[idx] := Format('%.2x', [TraceRegs.V[idx]]);
      Result.RegStr[16] := Format('%.4x', [TraceRegs.I]);
      Result.RegStr[17] := Format('%.2x', [TraceRegs.SP]);
    end;
end;


{ GET DISASSEMBLY }

function TCpuChip8.GetDisassembly(Addr:word): TDisassembledData;
begin
  Result := DisassembleChip8(Self, Addr);
end;


end.
