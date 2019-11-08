{ ==============================================================================

  6502 CPU

    This class emulates a basic 6502 microprocessor

    The CPU code here is based on a mixture of ideas / code reworked into
    Delphi pascal, from the following:
    1. Fabrice Frances' Microtan Java emulator
       <http://www.ifrance.com/oric/microtan/microtan_java.html>
    2. Daryl Rictor's 65C02 emulator written in C++
       <http://sbc.rictor.org/simulator.html>

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

{ TODO : uCpu6502 -> in Create, need to select any CPU variant, currently just basic 6502 }
{ TODO : uCpu6502 -> in TCpuInfo, sort assignment for different register arrays }

unit uCpu6502;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Dialogs,
  //
  uCpuBase, uDefs6502, uCommon;

type

  TRegs6502 = record
    A: byte;                            // Accumulator
    X: byte;                            // Indexes X
    Y: byte;                            // and Y
    SP: byte;                           // Stack Pointer
    PSW: byte;                          // Processor Status Word
    PC: word;                           // Program Counter
  end;

  { TCpu6502 }

  TCpu6502 = class(TCpuBase)
  private
    fRegs: TRegs6502;                   // Current CPU registers
    Nflag: boolean;                     // Status flags that make up the PSW
    Vflag: boolean;
    Bflag: boolean;
    Dflag: boolean;
    Iflag: boolean;
    Zflag: boolean;
    Cflag: boolean;
    //
    OpcodePtrArray: array[0..255] of word;
    Cycles: integer;
    EA: word;                           // Effective Address
    IRQflag: boolean;                   // Interrupt flags
    NMIflag: boolean;
    //
    TempB: byte;                        // Temporary variables
    TempW, Temp3: word;
    TempCarry: byte;

    // Array for maintaining execution trace values
    TraceList: array[0..TRACE_MAX-1] of TRegs6502;

    procedure SetPSW(value: byte);
    function  GetPSW: byte;
    procedure Push(value: byte);
    function  Pop: byte;
    procedure PushW(value: word);
    function  PopW: word;
    procedure SetNZ(value: byte);
    procedure DoBranch;
    procedure ClearNMI;
    procedure CallIRQ;
    procedure CallNMI;
    function  ProcessOpcode: integer;
  protected
    function  GetPC: word; override;
    function  GetAssemblerRegisters: string; override;
    function  GetTraceColumns: TTraceColArray; override;
    function  GetOpcodeDataArray: TOpcodeArray; override;
    function  GetRegs: TRegs6502;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure Reset; override;
    function  ExecuteInstruction: integer; override;
    procedure Interrupt(AIndex: byte; Value: boolean = True); override;
    function  GetTrace(Index: integer): TDisassembledData; override;
    function  GetDisassembly(Addr:word): TDisassembledData; override;
    //
    function  MemRead(addr: word): byte;
    procedure MemWrite(addr: word; value: byte);
    function  OpcodeData(Opcode: byte): TOpcodeRawData;

    property Regs: TRegs6502 read GetRegs write fRegs;
  end;


implementation

uses
  uRegistersFrame6502, uDis6502;


{ CREATE 6502 CPU }

constructor TCpu6502.Create;
var
  i: integer;
  Opcode: byte;
begin
  fRegistersFrame := TRegistersFrame6502.Create(nil);
  (fRegistersFrame as TRegistersFrame6502).CpuRef := self;

  fName                 := CPU_6502;
  fCpuType              := ctR6502;
  fSupportsAssembler    := True;
  fSupportsDisassembler := True;
  fCpuState             := csStopped;
  fTraceWidth           := 440;
  fRegistersHeight      := 205;

  for i := 0 to 255 do
    OpcodePtrArray[i] := 0;             // Initialise array to point at Undefined opcode

  for i := 0 to (Length(OPCODES_6502) - 1) do
    begin                               // Then set opcode pointers into data array
      if ((OPCODES_6502[i].T and 1) <> 1) then
        Continue;                       // Skip if not basic 6502
      Opcode := OPCODES_6502[i].O;
      OpcodePtrArray[Opcode] := i;      // Set pointers into Opcode data
    end;

  Reset;
end;


destructor TCpu6502.Destroy;
begin
  fRegistersFrame.Parent := nil;        // Avoid pointer errors
  fRegistersFrame.Free;
  inherited;
end;


{ MEMORY ACCESS }

function TCpu6502.MemRead(addr: word): byte;
begin
  Result := $FF;
  if (Assigned(fOnRead)) then
    fOnRead(self, addr, Result);
end;


procedure TCpu6502.MemWrite(addr: word; value: byte);
begin
  if (Assigned(fOnWrite)) then
    fOnWrite(self, addr, value);
end;


{ PROPERTY GET / SET ROUTINES }

function TCpu6502.GetOpcodeDataArray: TOpcodeArray;
var
  idx: integer;
begin
 // Cannot assign const array to dynamic array, so copy each item
 SetLength(Result, length(OPCODES_6502));
 for idx := 0 to length(OPCODES_6502)-1 do
   Result[idx] := OPCODES_6502[idx];
end;


function TCpu6502.GetPC: word;
begin
  Result := fRegs.PC;
end;


function TCpu6502.GetRegs: TRegs6502;
begin
  GetPSW;                               // Ensure flags packed
  Result := fRegs;
end;


function TCpu6502.OpcodeData(Opcode: byte): TOpcodeRawData;
begin
  Result := OPCODES_6502[OpcodePtrArray[Opcode]];
end;


function TCpu6502.GetAssemblerRegisters: string;
begin
  Result := REGISTERS_6502;
end;


function TCpu6502.GetTraceColumns: TTraceColArray;
var
  idx: integer;
begin
  SetLength(Result, length(TRACE_COLS_6502));
  for idx := 0 to length(TRACE_COLS_6502)-1 do
    Result[idx] := TRACE_COLS_6502[idx];
end;


{ UTILITIES }

procedure TCpu6502.SetPSW(value: byte);
begin
  fRegs.PSW := value;
  NFlag := (fRegs.PSW and P_NEGATIVE) <> 0;
  VFlag := (fRegs.PSW and P_OVERFLOW) <> 0;
  DFlag := (fRegs.PSW and P_DECIMAL) <> 0;
  Bflag := (fRegs.PSW and P_BRK) <> 0;
  IFlag := (fRegs.PSW and P_IRQ_DISABLED) <> 0;
  ZFlag := (fRegs.PSW and P_ZERO) <> 0;
  CFlag := (fRegs.PSW and P_CARRY) <> 0;
end;


function  TCpu6502.GetPSW: byte;
begin
  fRegs.PSW := P_RESERVED;               // Reserved flag always 1
  if (Nflag) then
    fRegs.PSW := fRegs.PSW or P_NEGATIVE;
  if (Vflag) then
    fRegs.PSW := fRegs.PSW or P_OVERFLOW;
  if (Bflag) then
    fRegs.PSW := fRegs.PSW or P_BRK;
  if (Dflag) then
    fRegs.PSW := fRegs.PSW or P_DECIMAL;
  if (Iflag) then
    fRegs.PSW := fRegs.PSW or P_IRQ_DISABLED;
  if (Zflag) then
    fRegs.PSW := fRegs.PSW or P_ZERO;
  if (Cflag) then
    fRegs.PSW := fRegs.PSW or P_CARRY;
  Result := fRegs.PSW;
end;


procedure TCpu6502.Push(value: byte);
begin
  MemWrite($100 + fRegs.SP, value);
  Dec(fRegs.SP);
end;


function TCpu6502.Pop: byte;
begin
  Inc(fRegs.SP);
  Result := MemRead($100 + fRegs.SP);
end;


procedure TCpu6502.PushW(value: word);
begin
  MemWrite($100 + fRegs.SP, value shr 8);
  Dec(fRegs.SP);
  MemWrite($100 + fRegs.SP, value and $FF);
  Dec(fRegs.SP);
end;


function TCpu6502.PopW: word;
begin
  Inc(fRegs.SP);
  Result := MemRead($100 + fRegs.SP);
  Inc(fRegs.SP);
  Result := Result + (MemRead($100 + fRegs.SP) shl 8);
end;


procedure TCpu6502.SetNZ(value: byte);
begin
  Nflag := ((value and $80) <> 0);
  Zflag := (value = 0);
end;


{ INTERRUPTS }

procedure TCpu6502.Reset;
begin
  fRegs.PC := MemRead(ADDR_RESET) + (MemRead(ADDR_RESET+1) shl 8);
  fRegs.A  := 0;
  fRegs.X  := 0;
  fRegs.Y  := 0;
  fRegs.SP := $FF;
  SetPSW(P_IRQ_DISABLED or P_BRK or P_RESERVED); // Ensure PSW unpacked
  IRQflag := False;
  NMIflag := False;
  ResetTrace;
end;


procedure TCpu6502.Interrupt(aIndex: byte; Value: boolean = True);
begin
  // Interrupts set here, permitting current opcode to be processed
  // before responding to the interrupt request
  if (aIndex = IRQ_IDX) then
    IRQflag := Value                    // Set IRQ state
  else
    NMIflag := Value;                   // Set NMI, auto cleared by NMI routine
end;


procedure TCpu6502.ClearNMI;
begin
  NMIflag := False;
end;


procedure TCpu6502.CallIRQ;
begin
  if (Iflag) then
    Exit;

  PushW(fRegs.PC);
  Push(GetPSW and (not P_BRK));         // Clear flag B and push PSW
  Iflag := True;                        // Set to prevent further IRQ
  fRegs.PC := MemRead(ADDR_IRQ) + (MemRead(ADDR_IRQ+1) shl 8);
  Inc(Cycles, 7);
end;


procedure TCpu6502.CallNMI;
begin
  PushW(fRegs.PC);
  Push(GetPSW and (not P_BRK));         // Clear flag B and push PSW
  Iflag := True;
  NMIflag := False;
  fRegs.PC := MemRead(ADDR_NMI) + (MemRead(ADDR_NMI+1) shl 8);
  Inc(Cycles, 7);
end;


procedure TCpu6502.DoBranch;
begin
  TempW := fRegs.PC and $FF00;
  fRegs.PC := fRegs.PC + shortint(EA); // Add offset to get branch address
  if ((fRegs.PC and $FF00) <> TempW) then
    Inc(Cycles, 2)         // Different address page = 1, branch taken = 1
  else
    Inc(Cycles);           // Branch taken needs extra cycle
end;


{ EXECUTE ONE INSTRUCTION }

{ Return number of clock cycles used by instruction, 0 if invalid opcode }

function TCpu6502.ExecuteInstruction: integer;
begin
  CpuState := csRunning;
  Result := ProcessOpcode;
  CpuState := csStopped;
end;


{ PROCESS ONE OPCODE }

function TCpu6502.ProcessOpcode: integer;
var
  Opcode: byte;
  TracePC, Ptr: word;
  AddrMode: TAddrMode6502;
begin
  Cycles := 0;

  if ((not Iflag) and IRQflag) then     // Check for any interrupts first
    CallIRQ;
  if (NMIflag) then
    CallNMI;

  // Execute the current opcode
  TracePC := fRegs.PC;
  Opcode := MemRead(fRegs.PC);          // Get opcode to execute
  Inc(fRegs.PC);
  MemRead(fRegs.PC);                    // 6502 always reads next location
  Inc(Cycles, OpcodeData(Opcode).C);    // Get basic number of clock cycles

  // Check address mode and set effective address (EA) accordingly
  AddrMode := TAddrMode6502(OpcodeData(Opcode).S);
  case AddrMode of
    mNil:  begin
             // Do nothing
           end;
    mIMP:  begin
             // Do nothing
           end;
    mIMM:  begin
             EA := fRegs.PC;
             Inc(fRegs.PC);
           end;
    mACC:  begin
             // Do nothing
           end;
    mZP:   begin
             EA := MemRead(fRegs.PC) and $FF;
             Inc(fRegs.PC);
           end;
    mZPX:  begin
             // Hi byte is always zero, no page boundary crossings
             EA := (MemRead(fRegs.PC) + fRegs.X) and $FF;
             Inc(fRegs.PC);
           end;
    mZPY:  begin
             // Hi byte is always zero, no page boundary crossings
             EA := (MemRead(fRegs.PC) + fRegs.Y) and $FF;
             Inc(fRegs.PC);
           end;
    mABS:  begin
             EA := MemRead(fRegs.PC);
             EA := EA or (MemRead(fRegs.PC + 1) shl 8);
             Inc(fRegs.PC, 2);
           end;
    mABSX: begin
             EA := MemRead(fRegs.PC) + fRegs.X;
             if (EA > $FF) then
               Inc(Cycles);             // Extra clock cycle to fix high byte of address
             EA := EA + (MemRead(fRegs.PC + 1) shl 8);
             Inc(fRegs.PC, 2);
           end;
    mABSY: begin
             EA := MemRead(fRegs.PC) + fRegs.Y;
             if (EA > $FF) then
               Inc(Cycles);             // Extra clock cycle to fix high byte of address
             EA := EA + (MemRead(fRegs.PC+1) shl 8);
             Inc(fRegs.PC, 2);
           end;
    mIND:  begin
             Ptr := MemRead(fRegs.PC);
             Ptr := Ptr or (MemRead(fRegs.PC + 1) shl 8);
             Inc(fRegs.PC, 2);
             EA := MemRead(Ptr);
             EA := EA or (MemRead(Ptr + 1) shl 8);
           end;
    mINDX: begin
             Ptr := (MemRead(fRegs.PC) + fRegs.X) and $FF;
             Inc(fRegs.PC);
             EA := MemRead(Ptr);
             EA := EA or (MemRead((Ptr + 1) and $FF) shl 8);
           end;
    mINDY: begin
             Ptr := MemRead(fRegs.PC);
             Inc(fRegs.PC);
             EA := MemRead(Ptr);
             EA := (EA or (MemRead((Ptr + 1) and $FF) shl 8)) + fRegs.Y;
             if ((EA and $FF00) <> (fRegs.PC and $FF00)) then
               Inc(Cycles);             // Extra clock cycle to fix high byte of address
           end;
    mREL:  begin
             EA := MemRead(fRegs.PC) and $FF; // Get offset in EA
             Inc(fRegs.PC);
           end;
  end;

  // Then do relevant opcode action
  case (Opcode) of

    ///////////////// LOAD / STORE INSTRUCTIONS ////////////////////////////////

    $a1,$a5,$a9,$ad,$b1,$b5,$b9,$bd:             // LDA
         begin
           fRegs.A := MemRead(EA);
           SetNZ(fRegs.A);
         end;

    $a2,$a6,$ae,$b6,$be:                         // LDX
         begin
           fRegs.X := MemRead(EA);
           SetNZ(fRegs.X);
         end;

    $a0,$a4,$ac,$b4,$bc:                         // LDY
         begin
           fRegs.Y := MemRead(EA);
           SetNZ(fRegs.Y);
         end;

    $81,$85,$8d,$91,$95,$99,$9d:                 // STA
         begin
           MemWrite(EA, fRegs.A);
         end;

    $86,$8e,$96:                                 // STX
         begin
           MemWrite(EA, fRegs.X);
         end;

    $84,$8c,$94:                                 // STY
         begin
           MemWrite(EA, fRegs.Y);
         end;

    ///////////////// ARITHMETIC & LOGIC INSTRUCTIONS //////////////////////////

    $61,$65,$69,$6d,$71,$75,$79,$7d:             // ADC
         begin
           TempB := MemRead(EA);
           if (Cflag) then
             TempCarry := 1
           else
             TempCarry := 0;
           if ((fRegs.A xor TempB) and $80) <> 0 then
             Vflag := False
           else
             Vflag := True;
           if (Dflag) then
             begin
               Inc(Cycles);             // Extra cycle
               TempW := (fRegs.A and $0f) + (TempB and $0f) + TempCarry;
               if (TempW >= 10) then
                 TempW := $10 or ((TempW + 6) and $0f);
               TempW := TempW + (fRegs.A and $f0) + (TempB and $f0);
               if (TempW >= 160) then
                 begin
                   Cflag := True;
                   if (Vflag and (TempW >= $180)) then
                     Vflag := False;
                   TempW := TempW + $60;
                 end
               else
                 begin
                   Cflag := False;
                   if (Vflag and (TempW < $80)) then
                     Vflag := False;
                 end;
             end
           else
             begin
               TempW := fRegs.A + TempB + TempCarry;
               if (TempW >= $100) then
                 begin
                   Cflag := True;
                   if (Vflag and (TempW >= $180)) then
                     Vflag := False;
                 end
               else
                 begin
                   Cflag := False;
                   if (Vflag and (TempW < $80)) then
                     Vflag := False;
                 end;
             end;
             fRegs.A := TempW and $FF;
             SetNZ(fRegs.A);
         end;

    $e1,$e5,$e9,$eb,$ed,$f1,$f5,$f9,$fd:         // SBC
         begin
           TempB := MemRead(EA);
           if (CFlag) then
             TempCarry := 1
           else
             TempCarry := 0;
           if ((fRegs.A xor TempB) and $80) <> 0 then
             Vflag := False
           else
             Vflag := True;
           if (Dflag) then
             begin
               Temp3 := $0F + (fRegs.A and $0F) - (TempB and $0F) + TempCarry;
               if (Temp3 < $10) then
                 begin
                   TempW := 0;
                   Temp3 := Temp3 - 6;
                 end
               else
                 begin
                   TempW := $10;
                   Temp3 := Temp3 - $10;
                 end;
               TempW := TempW + $f0 + (fRegs.A and $f0) - (TempB and $F0);
               if (TempW < $100) then
                 begin
                   Cflag := False;
                   if (Vflag and (TempW < $80)) then
                     Vflag := False;
                   TempW := TempW - $60;
                 end
               else
                 begin
                   Cflag := True;
                   if (Vflag and (TempW >= $180)) then
                     Vflag := False;
                 end;
                 TempW := TempW + Temp3;
             end
           else
             begin
               TempW := $FF + fRegs.A - TempB + TempCarry;
               if (TempW < $100) then
                 begin
                   CFlag := False;
                   if (Vflag and (TempW < $80)) then
                     Vflag := False;
                 end
               else
                 begin
                   Cflag := True;
                   if (Vflag and (tempw >= $180)) then
                     Vflag := False;
                 end;
             end;
             fRegs.A := TempW and $FF;
             SetNZ(fRegs.A);
         end;

    $c1,$c5,$c9,$cd,$d1,$d5,$d9,$dd:             // CMP
         begin
           TempW := fRegs.A - MemRead(EA);
           Cflag := (TempW and $100) = 0;
           Nflag := (TempW and $80) <> 0;
           Zflag := (TempW and $FF) = 0;
         end;

    $e0,$e4,$ec:                                 // CPX
         begin
           TempW := fRegs.X - MemRead(EA);
           Cflag := (TempW and $100) = 0;
           Nflag := (TempW and $80) <> 0;
           Zflag := (TempW and $FF) = 0;
         end;

    $c0,$c4,$cc:                                 // CPY
         begin
           TempW := fRegs.Y - MemRead(EA);
           Cflag := (TempW and $100) = 0;
           Nflag := (TempW and $80) <> 0;
           Zflag := (TempW and $FF) = 0;
         end;

    $0b,$21,$25,$29,$2b,$2d,$31,$35,$39,$3d:     // AND
         begin
           fRegs.A := fRegs.A and MemRead(EA);
           SetNZ(fRegs.A);
         end;

    $01,$05,$09,$0d,$11,$15,$19,$1d:             // ORA
         begin
           fRegs.A := fRegs.A or MemRead(EA);
           SetNZ(fRegs.A);
         end;

    $41,$45,$49,$4d,$51,$55,$59,$5d:             // EOR
         begin
           fRegs.A := fRegs.A xor MemRead(EA);
           SetNZ(fRegs.A);
         end;

    $06,$0e,$16,$1e:                             // ASL
         begin
           TempB := MemRead(EA);
           Cflag := (TempB and $80) <> 0;
           TempB := TempB shl 1;
           SetNZ(TempB);
           MemWrite(EA, TempB);
         end;

    $0a: begin                                   // ASL_A
           Cflag := (fRegs.A and $80) <> 0;
           fRegs.A := fRegs.A shl 1;
           SetNZ(fRegs.A);
         end;

    $46,$4e,$56,$5e:                             // LSR
         begin
           TempB := MemRead(EA);
           Cflag := (TempB and $01) <> 0;
           TempB := TempB shr 1;
           Nflag := False;
           Zflag := (TempB = 0);
           MemWrite(EA, TempB);
         end;

    $4a: begin                                   // LSR_A
           Cflag := ((fRegs.A and $01) <> 0);
           fRegs.A := fRegs.A shr 1;
           Nflag := False;
           Zflag := (fRegs.A = 0);
         end;

    $26,$2e,$36,$3e:                             // ROL
         begin
           TempB := MemRead(EA);
           if (Cflag) then
             TempCarry := 1
           else
             TempCarry := 0;
           Cflag := (TempB and $80) <> 0;
           TempB := (TempB shl 1) or TempCarry;
           SetNZ(TempB);
           MemWrite(EA, TempB);
         end;

    $2a: begin                                   // ROL_A
           if (Cflag) then
             TempCarry := 1
           else
             TempCarry := 0;
           Cflag := (fRegs.A and $80) <> 0;
           fRegs.A := (fRegs.A shl 1) or TempCarry;
           SetNZ(fRegs.A);
         end;

     $66,$6e,$76,$7e:                            // ROR
         begin
           TempB := MemRead(EA);
           if (Cflag) then
             TempCarry := $80
           else
             TempCarry := 0;
           Cflag := (TempB and $01) <> 0;
           TempB := (TempB shr 1) or TempCarry;
           SetNZ(TempB);
           MemWrite(EA, TempB);
         end;

    $6a: begin                                   // ROR_A
           if (Cflag) then
             TempCarry := $80
           else
             TempCarry := 0;
           Cflag := (fRegs.A and $01) <> 0;
           fRegs.A := (fRegs.A shr 1) or TempCarry;
           SetNZ(fRegs.A);
         end;

    ///////////////// INCREMENT & DECREMENT INSTRUCTIONS ///////////////////////

    $e6,$ee,$f6,$fe:                             // INC
         begin
           TempB := MemRead(EA) + 1;
           SetNZ(TempB);
           MemWrite(EA, TempB);
         end;

    $c6,$ce,$d6,$de:                             // DEC
         begin
           TempB := MemRead(EA) - 1;
           SetNZ(TempB);
           MemWrite(EA, TempB);
         end;

    $e8: begin                                   // INX
           Inc(fRegs.X);
           SetNZ(fRegs.X);
         end;

    $c8: begin                                   // INY
           Inc(fRegs.Y);
           SetNZ(fRegs.Y);
         end;

    $ca: begin                                   // DEX
           Dec(fRegs.X);
           SetNZ(fRegs.X);
         end;

    $88: begin                                   // DEY
           Dec(fRegs.Y);
           SetNZ(fRegs.Y);
         end;

    ///////////////// BIT TESTS ////////////////////////////////////////////////

    $24,$2c: // BIT
         begin
           TempB := MemRead(EA);
           Vflag := (TempB and $40) <> 0;
           Nflag := (TempB and $80) <> 0;
           Zflag := (fRegs.A and TempB) = 0;
         end;

    ///////////////// STACK INSTRUCTIONS ///////////////////////////////////////

    $48: Push(fRegs.A);                           // PHA
    $08: Push(GetPSW);                            // PHP, ensure packing of PSW
    $68: begin                                    // PLA
           fRegs.A := Pop;
           SetNZ(fRegs.A);
         end;
    $28: SetPSW(Pop);                             // PLP, ensures unpacking of PSW

    ///////////////// INTERRUPTS ///////////////////////////////////////////////

    $00: begin                                   // BRK
           Inc(fRegs.PC);
           PushW(fRegs.PC);
           Push(GetPSW);                // Ensures packing of PSW occurs
           Iflag := True;
           fRegs.PC := MemRead(ADDR_IRQ);
           fRegs.PC := fRegs.PC or MemRead(ADDR_IRQ + 1);
         end;

    $40: begin                                   // RTI
           SetPSW(Pop);                          // Ensure unpacking of PSW
           fRegs.PC := PopW;
         end;

    ///////////////// JUMPS & BRANCHES /////////////////////////////////////////

    $4c,$6c: fRegs.PC := EA;                     // JMP

    $20: begin                                   // JSR
           // The real 6502 pushes address pointing to last byte of 3 byte
           // instruction so need to restore PC to that
           // Emulated here in case anyone uses address off stack in their code
           Dec(fRegs.PC);
           PushW(fRegs.PC);
           fRegs.PC := EA;
         end;

    $60: begin                                   // RTS
           fRegs.PC := PopW;
           // Real 6502 increments addr pulled to point to next after JSR
           Inc(fRegs.PC);
         end;

    $10: if (not Nflag) then DoBranch;           // BPL
    $30: if (Nflag) then DoBranch;               // BMI

    $50: if (not Vflag) then DoBranch;           // BVC
    $70: if (Vflag) then DoBranch;               // BVS

    $90: if (not Cflag) then DoBranch;           // BCC
    $b0: if (Cflag) then DoBranch;               // BCS

    $d0: if (not Zflag) then DoBranch;           // BNE
    $f0: if (Zflag) then DoBranch;               // BEQ

    ///////////////// TRANSFERS ////////////////////////////////////////////////

    $aa: begin                                   // TAX
           fRegs.X := fRegs.A;
           SetNZ(fRegs.X);
         end;

    $8a: begin                                   // TXA
           fRegs.A := fRegs.X;
           SetNZ(fRegs.A);
         end;

    $a8: begin                                   // TAY
           fRegs.Y := fRegs.A;
           SetNZ(fRegs.Y);
         end;

    $98: begin                                   // TYA
           fRegs.A := fRegs.Y;
           SetNZ(fRegs.A);
         end;

    $9a: fRegs.SP := fRegs.X;                    // TXS, no affected PSW

    $ba: begin                                   // TSX
           fRegs.X := fRegs.SP;
           SetNZ(fRegs.X);
         end;

    ///////////////// STATUS FLAGS /////////////////////////////////////////////

    $18: Cflag := False;                         // CLC
    $38: Cflag := True;                          // SEC
    $58: Iflag := False;                         // CLI
    $78: Iflag := True;                          // SEI
    $b8: Vflag := False;                         // CLV
    $d8: Dflag := False;                         // CLD
    $f8: Dflag := True;                          // SED

    ///////////////// NO OPERATION /////////////////////////////////////////////

    $ea: ;                                       // NOP, nothing to do!

    ///////////////// INVALID INSTRUCTIONS /////////////////////////////////////

    $04,$14,$34,$44,$54,$64,$74,$80,$82,$89,$c2,$d4,$e2,$f4: // INVALID(1)
         begin
           Inc(fRegs.PC);
           Inc(Cycles);
         end;

    $0c,$1c,$3c,$5c,$7c,$dc,$fc:                             // INVALID(2)
         begin
           Inc(fRegs.PC, 2);
           Inc(Cycles);
         end;

    $02,$12,$22,$32,$42,$52,$62,$72,$92,$b2,$d2,$f2:         // INVALID(3)
         begin
           Dec(fRegs.PC);               // Hang
           Inc(Cycles);
         end;

    else
      begin
        // INVALID(0) ... Do nothing
      end;
  end;

  Result := Cycles;

  // TRACK EXECUTION: Save all registers after executing opcode, includes PC
  if (fTraceIndex >= TRACE_MAX) then
    begin
      fTraceIndex := 0;
      fTraceOverflow := True;
    end;
  TraceList[fTraceIndex] := Regs;       // Save all registers
  TraceList[fTraceIndex].PC := TracePC; // Reset saved PC to opcode just executed
  Inc(fTraceIndex);
end;


{ EXECUTION TRACE }

{ Return trace for given index into trace list. If index exceeds
  trace count then return empty string }

  function TCpu6502.GetTrace(Index: integer): TDisassembledData;
begin
  if (CpuState = csRunning)             // No response if CPU running
     or ((fTraceIndex = 0) and (not fTraceOverflow))
     or ((Index > fTraceIndex) and (not fTraceOverflow)) then
    Result.Text := ''
  else
    begin
      if (fTraceOverflow) then
        Index := (Index + fTraceIndex) and TRACE_MASK; // Adjust for rollover
      Result := GetDisassembly(TraceList[Index].PC);
      Result.Addr := TraceList[Index].PC;
      SetLength(Result.RegStr, 5);
      Result.RegStr[0] := Format('%.2x', [TraceList[Index].A]);
      Result.RegStr[1] := Format('%.2x', [TraceList[Index].X]);
      Result.RegStr[2] := Format('%.2x', [TraceList[Index].Y]);
      Result.RegStr[3] := Format('%.2x', [TraceList[Index].SP]);
      Result.RegStr[4] := GetBinary(TraceList[Index].PSW);
    end;
end;


{ GET DISASSEMBLY }

function TCpu6502.GetDisassembly(Addr:word): TDisassembledData;
begin
  Result := Disassemble6502(Self, Addr);
end;


end.
