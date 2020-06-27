{ ==============================================================================

  CHIP-8 CPU

    CHIP-8 is an interpreted minimalist programming language that was designed
    by Joseph Weisbecker in the 1970s for use on the RCA COSMAC VIP computer.
    It is not a CPU as such.

    CPU:      The 'CPU' has 16 8-bit general purpose registers, a 16-word stack
              pointer, a 16-bit index register and program counter. It has two
              timers; a delay timer and a sound timer, both count down at 60Hz.

    Input:    Input is via 16 hex keys

    Graphics: 64 x 32 pixel screen, instructions support sprites, and a built
              in hexadecimal font

    This emulation includes the SCHIP with 128 x 64 pixel screen and
    additional instructions

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

interface

uses
  Forms, Classes, SysUtils, Graphics, Dialogs,
  //
  uCpuBase, uDefsChip8, uCommon, uCpuTypes, uPrefsChip8;

type
  TRegsChip8 = record
    V: array[0..15] of byte;            // Sixteen 8-bit general purpose registers
    I: word;                            // Index register
    PC: word;                           // Program counter
    SP: byte;                           // Stack pointer
  end;

  TKeys = array[0..15] of boolean;      // Sixteen key 'keypad'
  TPixels = array[0..128*64] of byte; // Display buffer in pixels, big enough for SCHIP
  TStack = array[0..15] of word;
  THP48Flags = array[0..15] of byte;

  { TCpuChip8 }

  TCpuChip8 = class(TCpuBase)
  private
    fKeys: TKeys;
    fRegs: TRegsChip8;
    fPixelsArray: TPixels;
    fStack: TStack;
    fHP48Flags: THP48Flags;
    fHiRes: boolean;
    fUpdatedScreen: boolean;
    fDelayTimer: byte;
    fSoundTimer: byte;
    fScreenX: byte;
    fScreenY: byte;
    DoneInvalidWarning: boolean;
    TraceList: array[0..TRACE_MAX] of TRegsChip8;

    procedure ProcessOpcode;
    procedure ScrollRight;
    procedure ScrollLeft;
    procedure ScrollDown(Count: integer);
    function  GetKey(Index: byte): boolean;
    procedure SetHiRes(aValue: boolean);
    procedure SetKey(Index: byte; Value: boolean);
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
    //
    property Pixels: TPixels read fPixelsArray;
    property HiRes: boolean read fHiRes write SetHiRes;
    property ScreenX: byte read fScreenX write fScreenX;
    property ScreenY: byte read fScreenY write fScreenY;
    property UpdatedScreen: boolean read fUpdatedScreen write fUpdatedScreen;
    property Regs: TRegsChip8 read GetRegs write fRegs;
    property Stack: TStack read fStack;
    property Key[Index: byte]: boolean read GetKey write SetKey;
    property DelayTimer: byte read fDelayTimer write fDelayTimer;
    property SoundTimer: byte read fSoundTimer write fSoundTimer;
  end;

const
  HFONT_OFFSET = 80;


implementation

uses
  uMachineBase, uDisChip8, uRegistersFrameChip8;


{ CREATE }

constructor TCpuChip8.Create(ct: TCpuType);
var
  ThisTypeMask: byte;
begin
  fRegistersFrame := TRegistersFrameChip8.Create(nil);
  (fRegistersFrame as TRegistersFrameChip8).CpuRef := self;

  fCpuType  := ct;
  SetInfo(INFO_CHIP8);
  case ct of
    ctCHIP8: begin
               ThisTypeMask := %01;
               SetHiRes(False);
             end;
    ctSCHIP: begin
               ThisTypeMask := %11;
               fInfo.Name := 'SCHIP';
               fInfo.RegsKeywords := fInfo.RegsKeywords + ' HF R';
               SetHiRes(True);
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


{ GETTERS / SETTERS }

function TCpuChip8.GetKey(Index: byte): boolean;
begin
  Result := fKeys[Index];
end;


procedure TCpuChip8.SetKey(Index: byte; Value: boolean);
begin
  fKeys[Index] := Value;
end;


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


procedure TCpuChip8.SetHiRes(aValue: boolean);
begin
  if (fHiRes = aValue) then Exit;

  fHiRes := aValue;
  if (fHiRes) then
    begin
      ScreenX := 128;
      ScreenY := 64;
    end
  else
    begin
      ScreenX := 64;
      ScreenY := 32;
    end;
end;


{ RESET }

procedure TCpuChip8.Reset;
var
  i: integer;
begin
  fCpuRunning := True;
  for i := 0 to 15 do                   // Clear registers
    fRegs.V[i] := 0;
  fRegs.PC := $200;                     // Default start addr for CHIP8 [[For ETI-660 it was $600]]
  fRegs.I  := 0;
  fRegs.SP := 0;
  fDelayTimer := 0;
  fSoundTimer := 0;

  for i := 0 to 79 do                   // Initialise character fonts, LoRes
    MemWrite(i, CHIP8_FONT[i]);
  for i := 0 to 159 do                  // and HiRes (together < 512 limit)
    MemWrite(HFONT_OFFSET + i, SCHIP_FONT[i]);
  for i := 0 to length(fPixelsArray) - 1 do // Clear screen
    fPixelsArray[i] := 0;
  fUpdatedScreen := True;

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
  ProcessOpcode;
  Result := 0;
end;


{ PROCESS EACH OPCODE }

procedure TCpuChip8.ProcessOpcode;
var
  Opcode: word;
  i, OpX, OpY, OpByte, OpNibble: byte;
  X, Y, Row, Col, RowOfPixels, Height: byte;
  OpAddr: word;
  ThisBit, ThisPixel, ThisRow, ThisCol, tmp: integer;
  Unknown, KeyPressed: boolean;

  function AllowSCHIP: boolean;
  begin
    Unknown := Chip8Prefs.Chip8Only;
    Result := not Unknown;
  end;

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
  OpX := (Opcode shr 8) and $000F;
  OpY := (Opcode shr 4) and $000F;
  OpAddr := Opcode and $0FFF;
  OpByte := Opcode and $00FF;
  OpNibble := Opcode and $000F;

  Inc(fRegs.PC, 2);                     // Each instruction is two bytes

  case (Opcode and $F000) of            // Mask off instruction bits

    $0000: // Various, check next bits
           begin
             case (Opcode) of

               $00E0: // CLS - clear screen
                  begin
                    for ThisPixel := 0 to (length(fPixelsArray) - 1) do
                      fPixelsArray[ThisPixel] := 0;
                      fUpdatedScreen := True;
                  end;

               $00EE: // RET - return from subroutine
                  begin
                    Dec(fRegs.SP);      // Restore PC from stack
                    fRegs.SP := fRegs.SP and $0F; // Limit to 16 entries
                    fRegs.PC := fStack[fRegs.SP];
                  end;

               $00FB: // SCR - scroll 4 pixels right
                  if (AllowSCHIP) then
                    ScrollRight;

               $00FC: // SCL - scroll 4 pixels left
                  if (AllowSCHIP) then
                    ScrollLeft;

               $00FD: // EXIT - exit CHIP interpreter
                  if (AllowSCHIP) then
                    begin
                      fCpuRunning := False;
                      Dec(fRegs.PC, 2);   // Stop on this opcode
                    end;

               $00FE: // LOW - disable extended screen mode
                  if (AllowSCHIP) then
                    begin
                      if (fHiRes) then
                        begin
                          fUpdatedScreen := True;
                          SetHiRes(False);
                        end;
                    end;

               $00FF: // HIGH - enable extended screen mode
                  if (AllowSCHIP) then
                    begin
                      if (not fHiRes) then
                        begin
                          fUpdatedScreen := True;
                          SetHiRes(True);
                        end;
                    end;

             else { case (Opcode) of }
               begin
                 if (((Opcode and $FFF0) = $00C0) and (AllowSCHIP)) then
                   begin // $00CN: SCD - scroll N lines down
                     ScrollDown(OpNibble);
                   end

                 else    // Otherwise opcode is not known
                   Unknown := True;
               end;
             end;
           end;

    $1000: // $1NNN: JP - jump to address NNN
           begin
             fRegs.PC := OpAddr;
           end;

    $2000: // $2NNN: CALL - call subroutine at address NNN
           begin
             fStack[fRegs.SP] := fRegs.PC; // Save current address in stack
                                        // Note: PC already incremented
             Inc(fRegs.SP);
             fRegs.SP := fRegs.SP and $0F; // Limit to 16 entries
             fRegs.PC := OpAddr;
           end;

    $3000: // $3xNN: SE - skips the next instruction if Vx equals NN
           begin
             if (fRegs.V[OpX] = OpByte) then
               fRegs.PC := (fRegs.PC + 2) and $0FFF; // Already pointing at next instr, point past it
           end;

    $4000: // $4xNN: SNE - skips the next instruction if Vx does not equal NN
           begin
             if (fRegs.V[OpX] <> OpByte) then
               fRegs.PC := (fRegs.PC + 2) and $0FFF; // Already pointing at next instr, point past it
           end;

    $5000: // $5xy0: SE - skips the next instruction if Vx equals Vy
           begin
             if (fRegs.V[OpX] = (fRegs.V[OpY])) then
               fRegs.PC := (fRegs.PC + 2) and $0FFF; // Already pointing at next instr, point past it
           end;

    $6000: // $6xNN: LD - set Vx to value NN
           begin
             fRegs.V[OpX] := OpByte;
           end;

    $7000: // $7xNN: ADD - add value NN to Vx
           begin
             fRegs.V[OpX] := (fRegs.V[OpX] + OpByte) and $FF;
           end;

    $8000: // Various, check next bits
           begin
             case (Opcode and $000F) of

               $0000: // $8xy0: LD - set Vx to value of Vy
                  begin
                    fRegs.V[OpX] := fRegs.V[OpY];
                  end;

               $0001: // $8xy1: OR - set Vx to (Vx or Vy)
                  begin
                    fRegs.V[OpX] := fRegs.V[OpX] or fRegs.V[OpY];
                  end;

               $0002: // $8xy2: AND - set Vx to (Vx and Vy)
                  begin
                    fRegs.V[OpX] := fRegs.V[OpX] and fRegs.V[OpY];
                  end;

               $0003: // $8xy3: XOR - set Vx xor (Vx or Vy)
                  begin
                    fRegs.V[OpX] := fRegs.V[OpX] xor fRegs.V[OpY];
                  end;

               $0004: // $8xy4: ADD - Vx := Vx + Vy, VF set to 1 if carry else 0
                  begin
                    tmp := fRegs.V[OpX] + fRegs.V[OpY];
                    if (tmp > 255) then
                      fRegs.V[$F] := 1
                    else
                      fRegs.V[$F] := 0;
                    fRegs.V[OpX] := tmp and $FF;
                  end;

               $0005: // $8xy5: SUB - Vx := Vx - Vy, VF set to 0 if borrow else 1
                  begin
                    if (fRegs.V[OpY] > fRegs.V[OpX]) then
                      fRegs.V[$F] := 0
                    else
                      fRegs.V[$F] := 1;
                    fRegs.V[OpX] := (fRegs.V[OpX] - fRegs.V[OpY]) and $FF;
                  end;

               $0006: // $8xy6: SHR - VF set to LSB of Vx, Vx := Vx shr 1
                  begin
                    fRegs.V[$F] :=  fRegs.V[OpX] and $01;
                    fRegs.V[OpX] := fRegs.V[OpX] shr 1;
                  end;

               $0007: // $8xy7: SUBN - Vx := Vy - Vx, VF set to 0 if borrow else 1
                  begin
                    if (fRegs.V[OpX] > fRegs.V[OpY]) then
                      fRegs.V[$F] := 0
                    else
                      fRegs.V[$F] := 1;
                    fRegs.V[OpX] := (fRegs.V[OpY] - fRegs.V[OpX]) and $FF;
                  end;

               $000E: // $8xyE: SHL - VF set to MSB of Vx, Vx := Vx shl 1
                  begin
                    fRegs.V[$F] :=  fRegs.V[OpX] shr 7;
                    fRegs.V[OpX] := (fRegs.V[OpX] shl 1) and $FF;
                  end;

             else
               Unknown := True;
             end;
           end;

    $9000: // $9xy0: SNE - skips the next instruction if Vx does not equal Vy
           begin
             if (fRegs.V[OpX] <> fRegs.V[OpY]) then
               fRegs.PC := (fRegs.PC + 2) and $0FFF; // Already pointing at next instr, point past it
           end;

    $A000: // $ANNN: LD - set I to address NNN
           begin
             fRegs.I := OpAddr;
           end;

    $B000: // $BNNN: JP - jump to address (NNN + V0)
           begin
             fRegs.PC := (OpAddr + fRegs.V[0]) and $0FFF;
           end;

    $C000: // $CxNN: RND - sets Vx to (RandomNumber and NN)
           begin
             Randomize;
             fRegs.V[OpX] := Random(255) and OpByte;
           end;

    $D000: // $DxyN: DRW - draw sprite at coords Vx,Vy, width 8, height N
           //              first row byte starts at memory location I
           //              VF set to 1 if any active screen pixels collide
           //
           // If SCHIP then draw 16x16 sprite, unless LowRes then drop through
           begin
             X := fRegs.V[OpX];
             Y := fRegs.V[OpY];
             fRegs.V[$F] := 0;
             if ((OpNibble = 0) and AllowSCHIP and fHiRes) then
               begin                    // SCHIP, show 16x16 sprite
                 for Row := 0 to 15 do
                   begin
                     ThisRow := Y + Row;
                     if (ThisRow >= fScreenY) then // Gone off bottom?
                       Break;
                     ThisRow := ThisRow * fScreenX;
                     for Col := 0 to 1 do // Left & right bytes
                       begin
                         ThisCol := Col * 8;
                         RowOfPixels := MemRead(fRegs.I + Row*2 + Col);
                         for ThisBit := 0 to 7 do
                           begin
                             if ((RowOfPixels and ($80 shr ThisBit)) <> 0) then
                               begin
                                 ThisPixel := X + ThisCol + ThisBit; // Check RHS
                                 if (ThisPixel >= fScreenX) then
                                   Continue;
                                 ThisPixel := ThisRow + ThisPixel;
                                 // Check if collision occurred
                                 if (fPixelsArray[ThisPixel] = 1) then
                                   fRegs.V[$F] := 1;
                                 fPixelsArray[ThisPixel] := fPixelsArray[ThisPixel] xor 1;
                               end;
                           end;
                       end;
                   end;
               end

             else
               begin                    // Standard CHIP-8 sprite
                 if (OpNibble = 0) then
                   Height := 16         // SCHIP instr, LowRes set 16 pixel high sprite
                 else
                   Height := OpNibble;
                 for Row := 0 to (Height - 1) do
                   begin
                     ThisRow := Y + Row;
                     if (ThisRow >= fScreenY) then // Gone off bottom?
                       Break;
                     ThisRow := ThisRow * fScreenX;
                     RowOfPixels := MemRead(fRegs.I + Row);
                     for ThisBit := 0 to 7 do
                       begin
                         if ((RowOfPixels and ($80 shr ThisBit)) <> 0) then
                           begin
                             ThisPixel := X + ThisBit; // Check RHS
                             if (ThisPixel >= fScreenX) then
                               Continue;
                             ThisPixel := ThisRow + ThisPixel;
                             // Check if collision occurred
                             if (fPixelsArray[ThisPixel] = 1) then
                               fRegs.V[$F] := 1;
                             fPixelsArray[ThisPixel] := fPixelsArray[ThisPixel] xor 1;
                           end;
                       end;
                   end;
               end;
             fUpdatedScreen := True;
           end;

    $E000: // Various, check next bits
           begin
             case (Opcode and $00FF) of

               $009E: // $Ex9E: SKP - skip next instruction if key in Vx is pressed
                  begin
                    if (fKeys[fRegs.V[OpX]]) then
                      fRegs.PC := (fRegs.PC + 2) and $0FFF; // Already pointing at next instr, point past it
                  end;

               $00A1: // $ExA1: SKNP - skip next instruction if key in Vx is not pressed
                  begin
                    if (not fKeys[fRegs.V[OpX]]) then
                      fRegs.PC := (fRegs.PC + 2) and $0FFF; // Already pointing at next instr, point past it
                  end;

             else
               Unknown := True;
             end;
           end;

    $F000: // Various, check next bits
           begin
             case (Opcode and $00FF) of

               $0007: // $Fx07: LD - set Vx to value of the delay timer
                  begin
                    fRegs.V[OpX] := fDelayTimer;
                  end;

               $000A: // $Fx0A: LD - wait for key press, store in Vx
                  begin
                    KeyPressed := False;
                    while (not KeyPressed) do
                      begin
                        for i := 0 to 15 do
                          if (fKeys[i]) then
                            begin
                              fRegs.V[OpX] := i;
                              KeyPressed := True;
                            end;
                      end;
                  end;

               $0015: // $Fx15: LD - set delay timer to value in Vx
                  begin
                    fDelayTimer := fRegs.V[OpX];
                  end;

               $0018: // $Fx18: LD - set sound timer to value in Vx
                  begin
                    fSoundTimer := fRegs.V[OpX];
                  end;

               $001E: // $Fx1E: ADD - add Vx to I
                  begin
                    if ((fRegs.I + fRegs.V[OpX]) > $0FFF) then
                      fRegs.V[$F] := 1  // Check for overflow
                    else
                      fRegs.V[$F] := 0;
                    fRegs.I := (fRegs.I + fRegs.V[OpX]) and $0FFF;
                  end;

               $0029: // $Fx29: LD - set I to location of sprite for font character in Vx
                      //             characters are 4x5 pixels (but full byte width)
                      //             This LowRes font starts at $0000
                  begin
                    fRegs.I := fRegs.V[OpX] * 5;
                  end;

               $0030: // $Fx30: LD - set I to location of sprite for font
                      //             character in Vx, 10 byte font sprite
                      //             This font follows LowRes font at HFONT_OFFSET
                  if (AllowSCHIP) then
                    begin
                      fRegs.I := HFONT_OFFSET + (fRegs.V[OpX] * 10);
                    end;

               $0033: // $Fx33: LD - store BCD representation of Vx in I, I+1, I+2
                  begin
                    MemWrite(fRegs.I,   fRegs.V[OpX] div 100 mod 10);
                    MemWrite(fRegs.I+1, fRegs.V[OpX] div 10 mod 10);
                    MemWrite(fRegs.I+2, fRegs.V[OpX] mod 10);
                  end;

               $0055: // $Fx55: LD - store V0 to Vx in memory starting at address I
                  begin
                    for i := 0 to (OpX) do
                      MemWrite(fRegs.I + i, fRegs.V[i]);
                  end;

               $0065: // $Fx65: LD - load V0 to Vx from memory starting at address I
                  begin
                    for i := 0 to (OpX) do
                      fRegs.V[i] := MemRead(fRegs.I + i);
                  end;

               $0075: // $Fx75: LD - store V0 to Vx in HP-48 RPL user flags (x <= 7)
                  if (AllowSCHIP) then
                    begin
                      fHP48Flags[OpX] := fRegs.V[OpX];
                    end;

               $0085: // $Fx85: LD - read V0 to Vx from HP-48 RPL user flags (x <= 7)
                  if (AllowSCHIP) then
                    begin
                      fRegs.V[OpX] := fHP48Flags[OpX];
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


{ SCROLL RIGHT 4 PX / LEFT 4 PX / DOWN nibble }

procedure TCpuChip8.ScrollRight;
var
  x, y, ThisRow: integer;
begin
  for y := 0 to ScreenY-1 do
    begin
      ThisRow := y * ScreenX;
      for x := ScreenX-1-4 downto 0 do
        fPixelsArray[ThisRow + x + 4] := fPixelsArray[ThisRow + x];
    end;
  for y := 0 to ScreenY-1 do
    begin
      ThisRow := y * ScreenX;
      for x := 0 to 3 do                // Clear left hand 4 pixels
        fPixelsArray[ThisRow + x] := 0;
    end;
  fUpdatedScreen := True;
end;


procedure TCpuChip8.ScrollLeft;
var
  x, y, ThisRow: integer;
begin
  for y := 0 to ScreenY-1 do
    begin
      ThisRow := y * ScreenX;
      for x := 0 to ScreenX-1-4 do
        fPixelsArray[ThisRow + x] := fPixelsArray[ThisRow + x + 4];
    end;
  for y := 0 to ScreenY-1 do
    begin
      ThisRow := y * ScreenX;
      for x := ScreenX-1-4 to ScreenX-1 do // Clear right hand 4 pixels
        fPixelsArray[ThisRow + x] := 0;
    end;
  fUpdatedScreen := True;
end;


procedure TCpuChip8.ScrollDown(Count: integer);
var
  x, y, RowTo, RowFrom: integer;
begin
  if (Count = 0) then Exit;

  for y := ScreenY-1-Count downto 0 do
    begin
      RowFrom := y * ScreenX;
      RowTo := (y + Count) * ScreenX;
      for x := 0 to ScreenX-1 do
        fPixelsArray[RowTo + x] := fPixelsArray[RowFrom + x];
    end;
  for y := 0 to Count-1 do
    begin
      RowTo := y * ScreenX;
      for x := 0 to ScreenX-1 do        // Clear top rows of pixels
        fPixelsArray[RowTo + x] := 0;
    end;
  fUpdatedScreen := True;
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
  if ((fTraceIndex = 0) and (not fTraceOverflow))
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
