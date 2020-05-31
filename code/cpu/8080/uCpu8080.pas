{ ==============================================================================

  8080 CPU

    This class emulates a basic 8080 microprocessor

    Based originally on the version by Alessandro Scotti (www.walkofmind.com)
    tweaked over the years


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

unit uCpu8080;

{$mode objfpc}{$H+}
{$rangechecks off}

interface

uses
  SysUtils,
  //
  uCpuBase, uDefs8080, uCpuTypes;

type
  // Record containing each of 8080 CPU registers
  TRegs8080 = record
    A: byte;
    F: byte;                            // Status flags
    B: byte;
    C: byte;
    D: byte;
    E: byte;
    H: byte;
    L: byte;
    SP: word;                           // Stack Pointer
    PC: word;                           // Program Counter
  end;

  { TCpu8080 }

  TCpu8080 = class(TCpuBase)
  private
    fRegs: TRegs8080;                   // Current CPU registers
    SignFlag: boolean;                  // Status bits that make up the
    ZeroFlag: boolean;                  // ... flags register
    InterruptFlag: boolean;
    AuxCarryFlag: boolean;
    ParityFlag: boolean;
    CarryFlag: boolean;
    Cycles: integer;                    // Clock cycles used by instruction
    tempByte: byte;
    tempWord: word;
    tempDWord: DWord;
    //
    InterruptEnabledPending: boolean;
    InterruptNumber: integer;
    //
    TraceList: array[0..TRACE_MAX-1] of TRegs8080;
    fInvalidFlag: boolean;

    procedure FuncCompare(val: byte);
    procedure FuncXor(val: byte);
    function  ProcessOpcode: integer;
    function  ReadMemWord: Word;
    function  GetBC: Word;
    function  GetDE: Word;
    function  GetHL: Word;
    procedure Push(value: byte);
    procedure PushWord(value: word);
    function  Pop: byte;
    function  PopWord: word;
    procedure SetFlags(value: byte);
    function  GetFlags: byte;
    procedure FuncAdd(val: byte);
    procedure FuncAddCarry(val: byte);
    procedure FuncSub(val: byte);
    procedure FuncSubCarry(val: byte);
    procedure FuncAnd(val: byte);
    procedure FuncOr(val: byte);
    procedure SetFlagsLogic(a: byte);
    procedure SetFlagsArith(w: word);
    procedure SetFlagsSZP(a: byte);
  protected
    function GetPC: word; override;
    function GetTraceColumns: TTraceColArray; override;
    function GetRegs: TRegs8080;
    function GetInfo: TCpuInfo; override;
  public
    constructor Create({%H-}ct: TCpuType); override;
    destructor  Destroy; override;
    procedure Reset; override;
    function  ExecuteInstruction: integer; override;
    procedure Interrupt(Index: byte; {%H-}Value: boolean = True); override;
    function  GetTrace(Index: integer): TDisassembledData; override;
    function  GetDisassembly(Addr:word): TDisassembledData; override;
    procedure TestCpu; override;
    //
    function  ReadMem(Addr: Word): Byte;
    procedure WriteMem(Addr: Word; Value: Byte);
    function  ReadPort(Port: byte): byte;
    procedure WritePort(Port, value: byte);
    //
    property  Regs: TRegs8080 read GetRegs write fRegs;
    property  InvalidOpcode: boolean read fInvalidFlag;
  end;


implementation

uses
  uRegistersFrame8080, uDis8080, uTest8080Form;


{ CREATE }

constructor TCpu8080.Create(ct: TCpuType);
var
  ThisTypeMask: byte;
begin
  fRegistersFrame := TRegistersFrame8080.Create(nil);
  (fRegistersFrame as TRegistersFrame8080).CpuRef := self;

  fCpuType  := ct;
  fCpuState := csStopped;

  Randomize;
  fRegs.B  := Random(256);              // Randomize registers on start up
  fRegs.C  := Random(256);
  fRegs.D  := Random(256);
  fRegs.E  := Random(256);
  fRegs.H  := Random(256);
  fRegs.L  := Random(256);
  fRegs.A  := Random(256);
  fRegs.F  := Random(256);
  fRegs.SP := Random($10000);

  case ct of
    ct8080:  ThisTypeMask := %01;       // No variants
  end;
  BuildOpcodesData(OPCODES_8080, ThisTypeMask);

  InterruptEnabledPending := False;
  Reset;
end;


{ DESTROY }

destructor TCpu8080.Destroy;
begin
  fRegistersFrame.Parent := nil;        // Avoid pointer errors
  fRegistersFrame.Free;
  SetLength(fOpcodesData, 0);
  fOpcodesData := nil;
  inherited;
end;


{ READ/WRITE MEMORY }

function TCpu8080.ReadMem(Addr: Word): Byte;
begin
  Result := $FF;                        // Default
  if (Assigned(fOnRead)) then
    fOnRead(self, addr, Result);
end;


procedure TCpu8080.WriteMem(Addr: Word; Value: Byte);
begin
  if (Assigned(fOnWrite)) then
    fOnWrite(self, addr, value);
end;


{ READ/WRITE PORT }

function TCpu8080.ReadPort(Port: byte): byte;
begin
  Result := $FF;                        // Default
  if (Assigned(OnReadPort)) then
    OnReadPort(self, Port, Result);
end;


procedure TCpu8080.WritePort(Port, value: byte);
begin
  if (Assigned(OnWritePort)) then
    OnWritePort(self, Port, value);
end;


{ GETTERS }

function TCpu8080.GetTraceColumns: TTraceColArray;
var
  idx: integer;
begin
  SetLength(Result, length(TRACE_COLS_8080));
  for idx := 0 to length(TRACE_COLS_8080)-1 do
    Result[idx] := TRACE_COLS_8080[idx];
end;


function TCpu8080.GetPC: word;
begin
  Result := fRegs.PC;
end;


function TCpu8080.GetInfo: TCpuInfo;
begin
  Result := INFO_8080;
end;


function TCpu8080.GetRegs: TRegs8080;
begin
  GetFlags;                             // Ensure flags packed
  Result := fRegs;
end;


{ GET / SET FLAGS }

function  TCpu8080.GetFlags: byte;
begin
  fRegs.F := 0;
  if (CarryFlag) then
    fRegs.F := fRegs.F or FLAG_CARRY;
  if (ParityFlag) then
    fRegs.F := fRegs.F or FLAG_PARITY;
  if (AuxCarryFlag) then
    fRegs.F := fRegs.F or FLAG_AUX_CARRY;
  if (ZeroFlag) then
    fRegs.F := fRegs.F or FLAG_ZERO;
  if (SignFlag) then
    fRegs.F := fRegs.F or FLAG_SIGN;
  Result := fRegs.F;
end;


procedure TCpu8080.SetFlags(value: byte);
begin
  fRegs.F := value;
  CarryFlag := (fRegs.F and FLAG_CARRY) <> 0;
  ParityFlag := (fRegs.F and FLAG_PARITY) <> 0;
  AuxCarryFlag := (fRegs.F and FLAG_AUX_CARRY) <> 0;
  ZeroFlag := (fRegs.F and FLAG_ZERO) <> 0;
  SignFlag := (fRegs.F and FLAG_SIGN) <> 0;
end;


{ PUSH / POP VALUE }

procedure TCpu8080.Push(value: byte);
begin
  WriteMem(fRegs.SP, value);
  Dec(fRegs.SP);
end;


function TCpu8080.Pop: byte;
begin
  Inc(fRegs.SP);
  Result := ReadMem(fRegs.SP);
end;


{ PUSH / POP WORD }

procedure TCpu8080.PushWord(value: word);
begin
  Dec(fRegs.SP);
  WriteMem(fRegs.SP, value shr 8);
  Dec(fRegs.SP);
  WriteMem(fRegs.SP, value and $FF);
end;


function TCpu8080.PopWord: word;
begin
  Result := ReadMem(fRegs.SP);
  Inc(fRegs.SP);
  Result := (ReadMem(fRegs.SP) shl 8) + Result;
  Inc(fRegs.SP);
end;


{ READ WORD FROM MEMORY }

function TCpu8080.ReadMemWord: Word;
begin
  tempWord := ReadMem(fRegs.PC);
  Inc(fRegs.PC);
  Result := (ReadMem(fRegs.PC) shl 8) + tempWord;
  Inc(fRegs.PC);
end;


{ GET 16-BIT REGISTERS }

function TCpu8080.GetBC: Word;
begin
  Result := (fRegs.B shl 8) + fRegs.C;
end;


function TCpu8080.GetDE: Word;
begin
  Result := (fRegs.D shl 8) + fRegs.E;
end;


function TCpu8080.GetHL: Word;
begin
  Result := (fRegs.H shl 8) + fRegs.L;
end;


{ FUNCTION: ADD }

procedure TCpu8080.FuncAdd(val: byte);
begin
  tempWord := fRegs.A + val;
  SetFlagsArith(tempWord);
  fRegs.A := tempWord and $FF;
end;


{ FUNCTION: ADD WITH CARRY }

procedure TCpu8080.FuncAddCarry(val: byte);
begin
  if (CarryFlag) then
    tempWord := fRegs.A + val + 1
  else
    tempWord := fRegs.A + val;
  SetFlagsArith(tempWord);
  fRegs.A := tempWord and $FF;
end;


{ FUNCTION: SUBTRACT }

procedure TCpu8080.FuncSub(val: byte);
begin
  tempWord := fRegs.A - val;
  SetFlagsArith(tempWord);
  fRegs.A := tempWord and $FF;
end;


{ FUNCTION: SUBTRACT WITH CARRY }

procedure TCpu8080.FuncSubCarry(val: byte);
begin
  if (CarryFlag) then
    tempWord := fRegs.A - val - 1
  else
    tempWord := fRegs.A - val;
  SetFlagsArith(tempWord);
  fRegs.A := tempWord and $FF;
end;


{ FUNCTION: AND }

procedure TCpu8080.FuncAnd(val: byte);
begin
  fRegs.A := fRegs.A and val;
  SetFlagsLogic(fRegs.A);
end;


{ FUNCTION: OR }

procedure TCpu8080.FuncOr(val: byte);
begin
  fRegs.A := fRegs.A or val;
  SetFlagsLogic(fRegs.A);
end;


{ FUNCTION: XOR }

procedure TCpu8080.FuncXor(val: byte);
begin
  fRegs.A := fRegs.A xor val;
  SetFlagsLogic(fRegs.A);
end;


{ FUNCTION: COMPARE }

procedure TCpu8080.FuncCompare(val: byte);
begin
  tempByte := fRegs.A - val;
  SetFlagsSZP(tempByte);
  CarryFlag := (fRegs.A < val);
end;


{ SET FLAGS ACCORDING TO LOGIC / ARITHMETIC / SZP }

procedure TCpu8080.SetFlagsLogic(a: byte);
begin
  CarryFlag := False;
  AuxCarryFlag := False;
  ZeroFlag := (a = 0);
  SignFlag := ((a and $80) = $80);
  ParityFlag := Parity(a);
end;


procedure TCpu8080.SetFlagsArith(w: word);
begin
  CarryFlag := (w > $FF);
  ZeroFlag := ((w and $FF) = 0);
  SignFlag := ((w and $80) = $80);
  ParityFlag := Parity(w and $FF);
end;


procedure TCpu8080.SetFlagsSZP(a: byte);
begin
  SignFlag := ((a and $80) = $80);
  ZeroFlag := (a = 0);
  ParityFlag := Parity(a);
end;


{ RESET }

procedure TCpu8080.Reset;
begin
  fRegs.PC := 0;
  SetFlags(0);
  InterruptNumber := NO_INTERRUPT;
  InterruptFlag := True;
  ResetTrace;                           // Initialise trace pointers
end;


{ INTERRUPT }

procedure TCpu8080.Interrupt(Index: byte; Value: boolean);
begin
  InterruptNumber := Index;
end;


{ EXECUTE ONE INSTRUCTION }

{ Return number of clock cycles used by instruction, 0 if invalid opcode }

function TCpu8080.ExecuteInstruction: integer;
begin
  // If interrupts are enabled, check if an interrupt has been generated
  if (InterruptFlag and (InterruptNumber <> NO_INTERRUPT)) then
    begin
      PushWord(fRegs.PC);               // Save current PC to stack
      fRegs.PC := InterruptNumber * 8;  // Set PC to requested RST location
      InterruptNumber := NO_INTERRUPT;
    end;

  // If previous instruction executed was an Enable Interrupt, then another
  // instruction is permitted before the interrupt system is enabled. As the
  // interrupt check has already been made (above), can now enable interrupts
  // again and any interrupt will be 'seen' after the next instruction is executed
  if (InterruptEnabledPending) then
    begin
      InterruptEnabledPending := False;
      InterruptFlag := True;
    end;

  fCpuState := csRunning;
  Result := ProcessOpcode;
  fCpuState := csStopped;
end;


{ PROCESS ONE OPCODE }

function TCpu8080.ProcessOpcode: integer;
var
  Opcode: byte;
begin
  fInvalidFlag := False;
  Cycles := 0;

  // Trace Execution: save all registers before executing opcode, includes PC
  if (fTraceIndex >= TRACE_MAX) then    // Check for roll around in trace array
    begin
      fTraceIndex := 0;
      fTraceOverflow := True;
    end;
  TraceList[fTraceIndex] := Regs;       // Save all register info
  Inc(fTraceIndex);

  // Execute the current opcode
  Opcode := ReadMem(fRegs.PC);          // Read opcode to execute
  Inc(fRegs.PC);
  Inc(Cycles, DataByOpcode[Opcode].C);  // Get basic number of clock cycles

  case (Opcode) of                      // ... and process instruction

    $00: begin                          // NOP
           // Nothing to do
         end;

    $01: begin                          // LD   BC,nn
           fRegs.C := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
           fRegs.B := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
         end;

    $02: WriteMem(GetBC, fRegs.A);      // LD   (BC),A

    $03: begin                          // INC  BC
           Inc(fRegs.C);
           if (fRegs.C = 0) then
             Inc(fRegs.B);
         end;

    $04: begin                          // INC  B
           Inc(fRegs.B);
           SetFlagsSZP(fRegs.B);
         end;

    $05: begin                          // DEC  B
           Dec(fRegs.B);
           SetFlagsSZP(fRegs.B);
         end;

    $06: begin                          // LD   B,n
           fRegs.B := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
         end;

    $07: begin                          // RLCA
           tempByte := fRegs.A;
           fRegs.A := ((tempByte and $80) shr 7) or (tempByte shl 1);
	   CarryFlag := ((tempByte and $80) = $80);
         end;

    $08: begin                          // Invalid
           // MsgForm.Add(Format('Unimplemented instruction $08 at %.4x' [fRegs.PC - 1]));
           fInvalidFlag := True;
         end;

    $09: begin                          // ADD  HL,BC
           tempDWord := GetHL + GetBC;
           fRegs.H := (tempDWord and $FF00) shr 8;
           fRegs.L := (tempDWord and $FF);
           CarryFlag := ((tempDWord and $FFFF0000) <> 0);
         end;

    $0A: fRegs.A := ReadMem(GetBC);     // LD   A,(BC)

    $0B: begin                          // DEC  BC
           if (fRegs.C = 0) then
             Dec(fRegs.B);
           Dec(fRegs.C);
         end;

    $0C: begin                          // INC  C
           Inc(fRegs.C);
           SetFlagsSZP(fRegs.C);
         end;

    $0D: begin                          // DEC  C
           Dec(fRegs.C);
           SetFlagsSZP(fRegs.C);
         end;

    $0E: begin                          // LD   C,n
           fRegs.C := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
         end;

    $0F: begin                          // RRCA
           tempByte := fRegs.A;
           fRegs.A := ((tempByte and 1) shl 7) or (tempByte shr 1);
	   CarryFlag := ((tempByte and 1) = 1);
         end;

    $10: begin                          // Invalid
           // MsgForm.Add(Format('Unimplemented instruction $10 at %.4x' [fRegs.PC - 1]));
           fInvalidFlag := True;
         end;

    $11: begin                          // LD   DE,nn
           fRegs.E := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
           fRegs.D := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
         end;

    $12: WriteMem(GetDE, fRegs.A);      // LD   (DE),A

    $13: begin                          // INC  DE
           Inc(fRegs.E);
           if (fRegs.E = 0) then
             Inc(fRegs.D);
         end;

    $14: begin                          // INC  D
           Inc(fRegs.D);
           SetFlagsSZP(fRegs.D);
         end;

    $15: begin                          // DEC  D
           Dec(fRegs.D);
           SetFlagsSZP(fRegs.D);
         end;

    $16: begin                          // LD   D,n
           fRegs.D := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
         end;

    $17: begin                          // RLA
           tempByte := fRegs.A;
           fRegs.A := tempByte shl 1;
 	   if (CarryFlag) then
             fRegs.A := fRegs.A or $01;
	   CarryFlag := ((tempByte and $80) = $80);
         end;

    $18: begin                          // Invalid
           // MsgForm.Add(Format('Unimplemented instruction $18 at %.4x' [fRegs.PC - 1]));
           fInvalidFlag := True;
         end;

    $19: begin                          // ADD  HL,DE
           tempDWord := GetHL + GetDE;
           fRegs.H := (tempDWord and $FF00) shr 8;
           fRegs.L := (tempDWord and $FF);
           CarryFlag := ((tempDWord and $FFFF0000) <> 0);
         end;

    $1A: fRegs.A := ReadMem(GetDE);     // LD   A,(DE)

    $1B: begin                          // DEC  DE
           if (fRegs.E = 0) then
             Dec(fRegs.D);
           Dec(fRegs.E);
         end;

    $1C: begin                          // INC  E
           Inc(fRegs.E);
           SetFlagsSZP(fRegs.E);
         end;

    $1D: begin                          // DEC  E
           Dec(fRegs.E);
           SetFlagsSZP(fRegs.E);
         end;

    $1E: begin                          // LD   E,n
           fRegs.E := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
         end;

    $1F: begin                          // RRA
           tempByte := fRegs.A;
           fRegs.A := tempByte shr 1;
 	   if (CarryFlag) then
             fRegs.A := fRegs.A or $80;
	   CarryFlag := ((tempByte and 1) = 1);
         end;

    $20: begin                          // Invalid [RIM = special?]
           // MsgForm.Add(Format('Unimplemented instruction $20 at %.4x' [fRegs.PC - 1]));
           fInvalidFlag := True;
         end;

    $21: begin                          // LD   HL,nn
           fRegs.L := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
           fRegs.H := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
         end;

    $22: begin                          // LD   (nn),HL
           tempWord := ReadMemWord;
           WriteMem(tempWord, fRegs.L);
           WriteMem(tempWord + 1, fRegs.H);
         end;

    $23: begin                          // INC  HL
           Inc(fRegs.L);
           if (fRegs.L = 0) then
             Inc(fRegs.H);
         end;

    $24: begin                          // INC  H
           Inc(fRegs.H);
           SetFlagsSZP(fRegs.H);
         end;

    $25: begin                          // DEC  H
           Dec(fRegs.H);
           SetFlagsSZP(fRegs.H);
         end;

    $26: begin                          // LD   H,n
           fRegs.H := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
         end;

    $27: begin                          // DAA                                  { TODO : uCpu8080 -> DAA not working for 8080 CPU Diagnostic test }
           if ((fRegs.A and $0F) > 9) then
             fRegs.A := fRegs.A + $06;
	   if ((fRegs.A and $F0) > $90 ) then
             begin
               tempWord := fRegs.A + $60;
               fRegs.A := tempWord and $FF;
             end;
           SetFlagsArith(tempWord);
         end;

    $28: begin                          // Invalid
           // MsgForm.Add(Format('Unimplemented instruction $28 at %.4x' [fRegs.PC - 1]));
           fInvalidFlag := True;
         end;

    $29: begin                          // ADD  HL,HL
           tempWord := GetHL;
           tempDWord := tempWord + tempWord;
           fRegs.H := (tempDWord and $FF00) shr 8;
           fRegs.L := (tempDWord and $FF);
           CarryFlag := ((tempDWord and $FFFF0000) <> 0);
         end;

    $2A: begin                          // LD   HL,(nn)
           tempWord := ReadMemWord;
           fRegs.L := ReadMem(tempWord);
           fRegs.H := ReadMem(tempWord + 1);
         end;

    $2B: begin                          // DEC  HL
           if (fRegs.L = 0) then
             Dec(fRegs.H);
           Dec(fRegs.L);
         end;

    $2C: begin                          // INC  L
           Inc(fRegs.L);
           SetFlagsSZP(fRegs.L);
         end;

    $2D: begin                          // DEC  L
           Dec(fRegs.L);
           SetFlagsSZP(fRegs.L);
         end;

    $2E: begin                          // LD   L,n
           fRegs.L := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
         end;

    $2F: fRegs.A := $FF - fRegs.A;      // CPL

    $30: begin                          // Invalid [SIM = special?]
           // MsgForm.Add(Format('Unimplemented instruction $30 at %.4x' [fRegs.PC - 1]));
           fInvalidFlag := True;
         end;

    $31: begin
           fRegs.SP := ReadMemWord;     // LD   SP,nn

           { Allow debugform to show correct number of stack bytes }
           fRegistersFrame.StackAssigned := fRegs.SP;
         end;

    $32: begin                          // LD   (nn),A
           WriteMem(ReadMemWord, fRegs.A);
         end;

    $33: Inc(fRegs.SP);                 // INC  SP

    $34: begin                          // INC  (HL)
           tempByte := ReadMem(GetHL) + 1;
           WriteMem(GetHL, tempByte);
           SetFlagsSZP(tempByte);
         end;

    $35: begin                          // DEC  (HL)
           tempByte := ReadMem(GetHL) - 1;
           WriteMem(GetHL, tempByte);
           SetFlagsSZP(tempByte);
         end;

    $36: begin                          // LD   (HL),n
           WriteMem(GetHL, ReadMem(fRegs.PC));
           Inc(fRegs.PC);
         end;

    $37: CarryFlag := True;             // SCF

    $38: begin                          // Invalid
           // MsgForm.Add(Format('Unimplemented instruction $38 at %.4x' [fRegs.PC - 1]));
           fInvalidFlag := True;
         end;

    $39: begin                          // ADD  HL,SP
           tempDWord := GetHL + fRegs.SP;
           fRegs.H := (tempDWord and $FF00) shr 8;
           fRegs.L := (tempDWord and $FF);
           CarryFlag := ((tempDWord and $FFFF0000) <> 0);
         end;

    $3A: fRegs.A := ReadMem(ReadMemWord); // LD   A,(nn)

    $3B: Dec(fRegs.SP);                 // DEC  SP

    $3C: begin                          // INC  A
           Inc(fRegs.A);
           SetFlagsSZP(fRegs.A);
         end;

    $3D: begin                          // DEC  A
           Dec(fRegs.A);
           SetFlagsSZP(fRegs.A);
         end;

    $3E: begin                          // LD   A,n
           fRegs.A := ReadMem(fRegs.PC);
           Inc(fRegs.PC);
         end;

    $3F: CarryFlag := (not CarryFlag);  // CCF

    $40: fRegs.B := fRegs.B;            // LD   B,r
    $41: fRegs.B := fRegs.C;
    $42: fRegs.B := fRegs.D;
    $43: fRegs.B := fRegs.E;
    $44: fRegs.B := fRegs.H;
    $45: fRegs.B := fRegs.L;
    $46: fRegs.B := ReadMem(GetHL);
    $47: fRegs.B := fRegs.A;

    $48: fRegs.C := fRegs.B;            // LD   C,r
    $49: fRegs.C := fRegs.C;
    $4A: fRegs.C := fRegs.D;
    $4B: fRegs.C := fRegs.E;
    $4C: fRegs.C := fRegs.H;
    $4D: fRegs.C := fRegs.L;
    $4E: fRegs.C := ReadMem(GetHL);
    $4F: fRegs.C := fRegs.A;

    $50: fRegs.D := fRegs.B;            // LD   D,r
    $51: fRegs.D := fRegs.C;
    $52: fRegs.D := fRegs.D;
    $53: fRegs.D := fRegs.E;
    $54: fRegs.D := fRegs.H;
    $55: fRegs.D := fRegs.L;
    $56: fRegs.D := ReadMem(GetHL);
    $57: fRegs.D := fRegs.A;

    $58: fRegs.E := fRegs.B;            // LD   E,r
    $59: fRegs.E := fRegs.C;
    $5A: fRegs.E := fRegs.D;
    $5B: fRegs.E := fRegs.E;
    $5C: fRegs.E := fRegs.H;
    $5D: fRegs.E := fRegs.L;
    $5E: fRegs.E := ReadMem(GetHL);
    $5F: fRegs.E := fRegs.A;

    $60: fRegs.H := fRegs.B;            // LD   H,r
    $61: fRegs.H := fRegs.C;
    $62: fRegs.H := fRegs.D;
    $63: fRegs.H := fRegs.E;
    $64: fRegs.H := fRegs.H;
    $65: fRegs.H := fRegs.L;
    $66: fRegs.H := ReadMem(GetHL);
    $67: fRegs.H := fRegs.A;

    $68: fRegs.L := fRegs.B;            // LD   L,r
    $69: fRegs.L := fRegs.C;
    $6A: fRegs.L := fRegs.D;
    $6B: fRegs.L := fRegs.E;
    $6C: fRegs.L := fRegs.H;
    $6D: fRegs.L := fRegs.L;
    $6E: fRegs.L := ReadMem(GetHL);
    $6F: fRegs.L := fRegs.A;

    $70: WriteMem(GetHL, fRegs.B);      // LD   (HL),B
    $71: WriteMem(GetHL, fRegs.C);      // LD   (HL),C
    $72: WriteMem(GetHL, fRegs.D);      // LD   (HL),D
    $73: WriteMem(GetHL, fRegs.E);      // LD   (HL),E
    $74: WriteMem(GetHL, fRegs.H);      // LD   (HL),H
    $75: WriteMem(GetHL, fRegs.L);      // LD   (HL),L

    $76: begin                          // HALT
           fCpuState := csHalted;
           Dec(fRegs.PC);               // Halt on this opcode
         end;

    $77: WriteMem(GetHL, fRegs.A);      // LD   (HL),A

    $78: fRegs.A := fRegs.B;            // LD   A,r
    $79: fRegs.A := fRegs.C;
    $7A: fRegs.A := fRegs.D;
    $7B: fRegs.A := fRegs.E;
    $7C: fRegs.A := fRegs.H;
    $7D: fRegs.A := fRegs.L;
    $7E: fRegs.A := ReadMem(GetHL);
    $7F: {fRegs.A := fRegs.A};

    $80: FuncAdd(fRegs.B);              // ADD  r
    $81: FuncAdd(fRegs.C);
    $82: FuncAdd(fRegs.D);
    $83: FuncAdd(fRegs.E);
    $84: FuncAdd(fRegs.H);
    $85: FuncAdd(fRegs.L);
    $86: FuncAdd(ReadMem(GetHL));
    $87: FuncAdd(fRegs.A);

    $88: FuncAddCarry(fRegs.B);         // ADC  r
    $89: FuncAddCarry(fRegs.C);
    $8A: FuncAddCarry(fRegs.D);
    $8B: FuncAddCarry(fRegs.E);
    $8C: FuncAddCarry(fRegs.H);
    $8D: FuncAddCarry(fRegs.L);
    $8E: FuncAddCarry(ReadMem(GetHL));
    $8F: FuncAddCarry(fRegs.A);

    $90: FuncSub(fRegs.B);              // SUB  r
    $91: FuncSub(fRegs.C);
    $92: FuncSub(fRegs.D);
    $93: FuncSub(fRegs.E);
    $94: FuncSub(fRegs.H);
    $95: FuncSub(fRegs.L);
    $96: FuncSub(ReadMem(GetHL));
    $97: FuncSub(fRegs.A);

    $98: FuncSubCarry(fRegs.B);         // SBC  r
    $99: FuncSubCarry(fRegs.C);
    $9A: FuncSubCarry(fRegs.D);
    $9B: FuncSubCarry(fRegs.E);
    $9C: FuncSubCarry(fRegs.H);
    $9D: FuncSubCarry(fRegs.L);
    $9E: FuncSubCarry(ReadMem(GetHL));
    $9F: FuncSubCarry(fRegs.A);

    $A0: FuncAnd(fRegs.B);              // AND  r
    $A1: FuncAnd(fRegs.C);
    $A2: FuncAnd(fRegs.D);
    $A3: FuncAnd(fRegs.E);
    $A4: FuncAnd(fRegs.H);
    $A5: FuncAnd(fRegs.L);
    $A6: FuncAnd(ReadMem(GetHL));
    $A7: FuncAnd(fRegs.A);

    $A8: FuncXor(fRegs.B);              // XOR  r
    $A9: FuncXor(fRegs.C);
    $AA: FuncXor(fRegs.D);
    $AB: FuncXor(fRegs.E);
    $AC: FuncXor(fRegs.H);
    $AD: FuncXor(fRegs.L);
    $AE: FuncXor(ReadMem(GetHL));
    $AF: FuncXor(fRegs.A);

    $B0: FuncOr(fRegs.B);               // OR   r
    $B1: FuncOr(fRegs.C);
    $B2: FuncOr(fRegs.D);
    $B3: FuncOr(fRegs.E);
    $B4: FuncOr(fRegs.H);
    $B5: FuncOr(fRegs.L);
    $B6: FuncOr(ReadMem(GetHL));
    $B7: FuncOr(fRegs.A);

    $B8: FuncCompare(fRegs.B);          // CP   r
    $B9: FuncCompare(fRegs.C);
    $BA: FuncCompare(fRegs.D);
    $BB: FuncCompare(fRegs.E);
    $BC: FuncCompare(fRegs.H);
    $BD: FuncCompare(fRegs.L);
    $BE: FuncCompare(ReadMem(GetHL));
    $BF: FuncCompare(fRegs.A);

    $C0: begin                          // RET  NZ
           if (not ZeroFlag) then
             begin
               fRegs.PC := PopWord;
               Inc(Cycles, 6);
             end;
         end;

    $C1: begin                          // POP  BC
           fRegs.C := ReadMem(fRegs.SP);
           Inc(fRegs.SP);
           fRegs.B := ReadMem(fRegs.SP);
           Inc(fRegs.SP);
         end;

    $C2: begin                          // JP   NZ,nn
           tempWord := ReadMemWord;
           if (not ZeroFlag) then
             fRegs.PC := tempWord;
         end;

    $C3: begin                          // JP   nn
           fRegs.PC := ReadMemWord;
         end;

    $C4: begin                          // CALL NZ,nn
           tempWord := ReadMemWord;     // Ensure PC incremented whether
           if (not ZeroFlag) then       // call occurs or not
             begin
               PushWord(fRegs.PC);
               fRegs.PC := tempWord;
               Inc(Cycles, 7);          // Extra cycles to handle call
             end;
         end;

    $C5: begin                          // PUSH BC
           Dec(fRegs.SP);
           WriteMem(fRegs.SP, fRegs.B);
           Dec(fRegs.SP);
           WriteMem(fRegs.SP, fRegs.C);
         end;

    $C6: begin                          // ADD  A,n
           FuncAdd(ReadMem(fRegs.PC));
           Inc(fRegs.PC);
         end;

    $C7: begin                          // RST  0
           PushWord(fRegs.PC);
           fRegs.PC := $00;
         end;

    $C8: begin                          // RET  Z
           if (ZeroFlag) then
             begin
               fRegs.PC := PopWord;
               Inc(Cycles, 6);
             end;
         end;

    $C9: begin                          // RET
           fRegs.PC := PopWord;
         end;

    $CA: begin                          // JP   Z,nn
           tempWord := ReadMemWord;
           if (ZeroFlag) then
             fRegs.PC := tempWord;
         end;

    $CB: begin                          // Invalid
           fInvalidFlag := True;
         end;

    $CC: begin                          // CALL Z,nn
           tempWord := ReadMemWord;     // Ensure PC incremented whether
           if (ZeroFlag) then           // call occurs or not
             begin
               PushWord(fRegs.PC);
               fRegs.PC := tempWord;
               Inc(Cycles, 6);          // Extra cycles to handle call
             end;
         end;

    $CD: begin                          // CALL nn
           tempWord := ReadMemWord;
           PushWord(fRegs.PC);
           fRegs.PC := tempWord;
         end;

    $CE: begin                          // ADC  A,n
           FuncAddCarry(ReadMem(fRegs.PC));
           Inc(fRegs.PC);
         end;

    $CF: begin                          // RST  8
           PushWord(fRegs.PC);
           fRegs.PC := $08;
         end;

    $D0: begin                          // RET  NC
           if (not CarryFlag) then
             begin
               fRegs.PC := PopWord;
               Inc(Cycles, 6);          // Extra cycles to handle return
             end;
         end;

    $D1: begin                          // POP  DE
           fRegs.E := ReadMem(fRegs.SP);
           Inc(fRegs.SP);
           fRegs.D := ReadMem(fRegs.SP);
           Inc(fRegs.SP);
         end;

    $D2: begin                          // JP   NC,nn
           tempWord := ReadMemWord;
           if (not CarryFlag) then
             fRegs.PC := tempWord;
         end;

    $D3: begin                          // OUT  (n),A
           WritePort(ReadMem(fRegs.PC), fRegs.A);
           Inc(fRegs.PC);
         end;

    $D4: begin                          // CALL NC,nn
           tempWord := ReadMemWord;     // Ensure PC incremented whether
           if (not CarryFlag) then      // call occurs or not
             begin
               PushWord(fRegs.PC);
               fRegs.PC := tempWord;
               Inc(Cycles, 6);          // Extra cycles to handle call
             end;
         end;

    $D5: begin                          // PUSH DE
           Dec(fRegs.SP);
           WriteMem(fRegs.SP, fRegs.D);
           Dec(fRegs.SP);
           WriteMem(fRegs.SP, fRegs.E);
         end;

    $D6: begin                          // SUB  n
           FuncSub(ReadMem(fRegs.PC));
           Inc(fRegs.PC);
         end;

    $D7: begin                          // RST  10H
           PushWord(fRegs.PC);
           fRegs.PC := $10;
         end;

    $D8: begin                          // RET  C
           if (CarryFlag) then
             begin
               fRegs.PC := PopWord;
               Inc(Cycles, 6);
             end;
         end;

    $D9: begin                          // Invalid
           fInvalidFlag := True;
         end;

    $DA: begin                          // JP   C,nn
           tempWord := ReadMemWord;
           if (CarryFlag) then
             fRegs.PC := tempWord;
         end;

    $DB: begin                          // IN   A,(n)
           fRegs.A := ReadPort(ReadMem(fRegs.PC));
           Inc(fRegs.PC);
         end;

    $DC: begin                          // CALL C,nn
           tempWord := ReadMemWord;     // Ensure PC incremented whether
           if (CarryFlag) then          // call occurs or not
             begin
               PushWord(fRegs.PC);
               fRegs.PC := tempWord;
               Inc(Cycles, 6);          // Extra cycles to handle call
             end;
         end;

    $DD: begin                          // Invalid
           fInvalidFlag := True;
         end;

    $DE: begin                          // SBC  A,n
           FuncSubCarry(ReadMem(fRegs.PC));
           Inc(fRegs.PC);
         end;

    $DF: begin                          // RST  18H
           PushWord(fRegs.PC);
           fRegs.PC := $18;
         end;

    $E0: begin                          // RET  PO
           if (not ParityFlag) then
             begin
               fRegs.PC := PopWord;
               Inc(Cycles, 6);
             end;
         end;

    $E1: begin                          // POP  HL
           fRegs.L := ReadMem(fRegs.SP);
           Inc(fRegs.SP);
           fRegs.H := ReadMem(fRegs.SP);
           Inc(fRegs.SP);
         end;

    $E2: begin                          // JP   PO,nn
           tempWord := ReadMemWord;
           if (not ParityFlag) then
             fRegs.PC := tempWord;
         end;

    $E3: begin                          // EX   (SP),HL
           tempWord := ReadMem(fRegs.SP);
           WriteMem(fRegs.SP, fRegs.L);
           fRegs.L := tempWord;
           tempWord := ReadMem(fRegs.SP + 1);
           WriteMem(fRegs.SP + 1, fRegs.H);
           fRegs.H := tempWord;
         end;

    $E4: begin                          // CALL PO,nn
           tempWord := ReadMemWord;     // Ensure PC incremented whether
           if (not ParityFlag) then     // call occurs or not
             begin
               PushWord(fRegs.PC);
               fRegs.PC := tempWord;
               Inc(Cycles, 6);          // Extra cycles to handle call
             end;
         end;

    $E5: begin                          // PUSH HL
           Dec(fRegs.SP);
           WriteMem(fRegs.SP, fRegs.H);
           Dec(fRegs.SP);
           WriteMem(fRegs.SP, fRegs.L);
         end;

    $E6: begin                          // AND  n
           FuncAnd(ReadMem(fRegs.PC));
           Inc(fRegs.PC);
         end;

    $E7: begin                          // RST  20H
           PushWord(fRegs.PC);
           fRegs.PC := $20;
         end;

    $E8: begin                          // RET  PE
           if (ParityFlag) then
             begin
               fRegs.PC := PopWord;
               Inc(Cycles, 6);
             end;
         end;

    $E9: begin                          // JP   (HL)
           fRegs.PC := GetHL;
         end;

    $EA: begin                          // JP   PE,nn
           tempWord := ReadMemWord;
           if (ParityFlag) then
             fRegs.PC := tempWord;
         end;

    $EB: begin                          // EX   DE,HL
           tempWord := fRegs.D;
           fRegs.D := fRegs.H;
           fRegs.H := tempWord;
           tempWord := fRegs.E;
           fRegs.E := fRegs.L;
           fRegs.L := tempWord;
         end;

    $EC: begin                          // CALL PE,nn
           tempWord := ReadMemWord;     // Ensure PC incremented whether
           if (ParityFlag) then         // call occurs or not
             begin
               PushWord(fRegs.PC);
               fRegs.PC := tempWord;
               Inc(Cycles, 6);          // Extra cycles to handle call
             end;
         end;

    $ED: begin                          // Invalid
           fInvalidFlag := True;
         end;

    $EE: begin                          // XOR  n
           FuncXor(ReadMem(fRegs.PC));
           Inc(fRegs.PC);
         end;

    $EF: begin                          // RST  28H
           PushWord(fRegs.PC);
           fRegs.PC := $28;
         end;

    $F0: begin                          // RET  P
           if (not SignFlag) then
             begin
               fRegs.PC := PopWord;
               Inc(Cycles, 6);
             end;
         end;

    $F1: begin                          // POP  AF
           fRegs.F := ReadMem(fRegs.SP);
           SetFlags(fRegs.F);
           Inc(fRegs.SP);
           fRegs.A := ReadMem(fRegs.SP);
           Inc(fRegs.SP);
         end;

    $F2: begin                          // JP   P,nn
           tempWord := ReadMemWord;
           if (not SignFlag) then
             fRegs.PC := tempWord;
         end;

    $F3: InterruptFlag := False;        // DI

    $F4: begin                          // CALL P,nn
           tempWord := ReadMemWord;     // Ensure PC incremented whether
           if (not SignFlag) then       // call occurs or not
             begin
               PushWord(fRegs.PC);
               fRegs.PC := tempWord;
               Inc(Cycles, 6);          // Extra cycles to handle call
             end;
         end;

    $F5: begin                          // PUSH AF
           Dec(fRegs.SP);
           WriteMem(fRegs.SP, fRegs.A);
           Dec(fRegs.SP);
           GetFlags;
           WriteMem(fRegs.SP, fRegs.F);
         end;

    $F6: begin                          // OR   n
           FuncOr(ReadMem(fRegs.PC));
           Inc(fRegs.PC);
         end;

    $F7: begin                          // RST  30H
           PushWord(fRegs.PC);
           fRegs.PC := $30;
         end;

    $F8: begin                          // RET  M
           if (SignFlag) then
             begin
               fRegs.PC := PopWord;
               Inc(Cycles, 6);
             end;
         end;

    $F9: fRegs.SP := GetHL;             // LD   SP,HL

    $FA: begin                          // JP   M,nn
           tempWord := ReadMemWord;
           if (SignFlag) then
             fRegs.PC := tempWord;
         end;

    $FB: begin                          // EI
           // Interrupt should be enabled only when another instruction
           // (after this EI) has been executed. Setting this flag
           // permits one more instruction before interrupt is re-enabled
           InterruptEnabledPending := True;
         end;

    $FC: begin                          // CALL M,nn
           tempWord := ReadMemWord;     // Ensure PC incremented whether
           if (SignFlag) then           // call occurs or not
             begin
               PushWord(fRegs.PC);
               fRegs.PC := tempWord;
               Inc(Cycles, 6);          // Extra cycles to handle call
             end;
         end;

    $FD: begin                          // Invalid
           fInvalidFlag := True;
         end;

    $FE: begin                          // CP   n
           FuncCompare(ReadMem(fRegs.PC));
           Inc(fRegs.PC);
         end;

    $FF: begin                          // RST  38H
           PushWord(fRegs.PC);
           fRegs.PC := $38;
         end;

  end;

  Result := Cycles;                     // Return number of cycles used
end;


{ GET TRACE }

{ Return trace for given index into trace list. If index exceeds trace count
  then return empty string }

function TCpu8080.GetTrace(Index: integer): TDisassembledData;
begin
  if (fCpuState = csRunning)            // No response if CPU running
     or ((fTraceIndex = 0) and (not fTraceOverflow))
     or ((Index > fTraceIndex) and (not fTraceOverflow)) then
    Result.Text := ''
  else
    begin
      if (fTraceOverflow) then
        Index := (Index + fTraceIndex) and TRACE_MASK; // Adjust for rollover
      with TraceList[Index] do          // Get values for this trace item
        begin
          Result := GetDisassembly(PC);
          Result.Addr := PC;
          SetLength(Result.RegStr, 6);
          Result.RegStr[0] := Format('%.2x', [A]);
          Result.RegStr[1] := Format('%.2x.%.2x', [B, C]);
          Result.RegStr[2] := Format('%.2x.%.2x', [D, E]);
          Result.RegStr[3] := Format('%.2x.%.2x', [H, L]);
          Result.RegStr[4] := Format('%.4x', [SP]);
          Result.RegStr[5] := GetBinary(F);
        end;
    end;
end;


{ GET DISASSEMBLY }

function TCpu8080.GetDisassembly(Addr:word): TDisassembledData;
begin
  Result := Disassemble8080(Self, Addr);
end;


{ TEST CPU }

procedure TCpu8080.TestCpu;
var
  TestForm: TTest8080Form;
begin
  inherited;
  TestForm := TTest8080Form.Create(nil);
  try
    TestForm.ShowModal;
  finally
    TestForm.Free;
  end;
end;


end.
