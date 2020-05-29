{ ==============================================================================

  CPU BASE CLASS

    This unit provides the base class TCpuBase, and type definitions for CPU
    opcode tables used by the assembler


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

unit uCpuBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils,
  //
  {$ifndef test6502}
  uRegistersFrameBase,
  {$endif}
  uCpuTypes;

const
  TRACE_MAX  = $800;                    // Size this so easy to mask off overrun
  TRACE_MASK = $7FF;

type

  TInterruptData = record
    Name: string;
    Index: integer;
  end;
  TInterruptsArray = array of TInterruptData;

  TOnReadEvent = procedure(Sender: TObject; Addr: word; var Value: byte) of object;
  TOnWriteEvent = procedure(Sender: TObject; Addr: word; Value: byte) of object;
  TOnReadPortEvent = procedure(Sender: TObject; Port: byte; var Value: byte) of object;
  TOnWritePortEvent = procedure(Sender: TObject; Port: byte; Value: byte) of object;

  { CPU Base Class }

  TCpuBase = class(TObject)
  private
  protected
    {$ifndef test6502}
    fRegistersFrame: TRegistersFrame;
    {$endif}
    fCpuType: TCpuType;
    fDataCount: integer;
    fCpuState: TCpuState;
    fTraceIndex: integer;
    fTraceOverflow: boolean;
    fOnRead: TOnReadEvent;
    fOnWrite: TOnWriteEvent;
    fOnReadPort: TOnReadPortEvent;
    fOnWritePort: TOnWritePortEvent;
    //
    function GetPC: word; virtual; abstract;
    function GetTraceColumns: TTraceColArray; virtual; abstract;
    function GetDataByIndex(Index: integer): TOpcodeRawData; virtual; abstract;
    function GetDataByOpcode(Opcode: integer): TOpcodeRawData; virtual; abstract;
    function GetInfo: TCpuInfo; virtual; abstract;
  public
    constructor Create(ct: TCpuType); virtual; abstract;
    //
    procedure Reset; virtual; abstract;
    function  ExecuteInstruction: integer; virtual; abstract;
    procedure Interrupt(Index: byte; State: boolean = True); virtual; abstract;
    procedure ResetTrace; virtual;
    function  TraceCount: integer; virtual;
    function  GetTrace(Index: integer): TDisassembledData; virtual; abstract;
    function  GetDisassembly(Addr:word): TDisassembledData; virtual; abstract;
    //
    {$ifndef test6502}
    property RegisterFrame: TRegistersFrame read fRegistersFrame write fRegistersFrame;
    {$endif}
    property CpuType: TCpuType read fCpuType;
    property TraceColumns: TTraceColArray read GetTraceColumns;
    property DataCount: integer read fDataCount;
    property DataByIndex[Index: integer]: TOpcodeRawData read GetDataByIndex;
    property DataByOpcode[Opcode: integer]: TOpcodeRawData read GetDataByOpcode;
    property CpuState: TCpuState read fCpuState write fCpuState;
    property PC: word read GetPC;
    property Info: TCpuInfo read GetInfo;

    property OnRead: TOnReadEvent read fOnRead write fOnRead;
    property OnWrite: TOnWriteEvent read fOnWrite write fOnWrite;
    property OnReadPort: TOnReadPortEvent read fOnReadPort write fOnReadPort;
    property OnWritePort: TOnWritePortEvent read fOnWritePort write fOnWritePort;
  end;


  { Define helper functions }

  function GetAscii(value: byte): string;
  function GetBinary(Value: integer): string;
  function Parity(Value: integer; Size: integer = 8): boolean;


implementation


{ GET ASCII }

{ Convert byte to ASCII string }

function GetAscii(value: byte): string;
begin
  if Char(value) in [#32..#126] then    // Displayable character ?
    Result := Char(value)
  else
    Result := '';
end;


{ GET BINARY }

{ Convert byte to Binary string }

function GetBinary(Value: integer): string;
var
  nBit: integer;
begin
  Result := '';
  for nBit := 1 to 8 do
    begin
      if ((Value and $80) > 0) then     // See if MSB set
        Result := Result + '1'
      else
        Result := Result + '0';
      Value := Value shl 1;             // Shift left one bit
    end;
end;


{ PARITY }

{ Calculate parity for a given value; Size should be 8, 16, etc }

function Parity(Value: integer; Size: integer): boolean;
var
  i, p: integer;
begin
  p := 0;
  Value := Value and ((1 shl Size) - 1);
  for i:= 0 to (Size - 1) do
    begin
      if ((Value and 1) > 0) then
        Inc(p);
      Value := Value shr 1;
    end;
  Result := ((p and 1) = 0);
end;


{ TCpuBase }

{ RESET TRACE }

procedure TCpuBase.ResetTrace;
begin
  fTraceIndex := 0;                     // Reset pointers to 'empty' array
  fTraceOverflow := False;
end;


{ TRACE COUNT }

{ Return number of trace elements, maximum number if roll around / overflow }

function TCpuBase.TraceCount: integer;
begin
  if (fTraceOverflow) then              // Check if array full
    Result := TRACE_MAX
  else
    Result := fTraceIndex;
end;


end.
