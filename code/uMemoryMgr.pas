{ ==============================================================================

  MEMORY MANAGER

    Creates a machine memory allocation, and then allows the machines memory
    map to be built in a simple way. Handles read/writes, handing off any
    special requirements to machine provided routines


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

{ TODO : uMemoryMgr -> add array of RAM areas? }

unit uMemoryMgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //
  uCommon;

const
  MEM_SIZE_64K = $10000;                // 64K memory
  MEM_SIZE_16K = $4000;                 // 16K memory
  MEM_SIZE_4K  = $1000;                 // 4K memory

type
  TMemory = array of byte;              // Size set on creation

  TRamRead = function(Addr: word): Byte of object;
  TRamWrite = procedure(Addr: word; Value: byte) of object;
  TIoRead = function(Port: Byte): Byte of object;
  TIoWrite = procedure(Port, Value: byte) of object;

  TMemoryReadSection = record
    StartMem: integer;                  // Start address
    EndMem: integer;                    // End address
    Handler: TRamRead;                  // Optional: handler function
    Desc: string;                       // Optional: description
  end;
  TMemoryReadSections = array of TMemoryReadSection;

  TMemoryWriteSection = record
    StartMem: integer;
    EndMem: integer;
    Handler: TRamWrite;
    Desc: string;
  end;
  TMemoryWriteSections = array of TMemoryWriteSection;

  TOnMemoryReadWrite = procedure(Sender: TObject; aAddr, aValue: integer) of object;

  { TMemory }

  { TMemoryMgr }

  TMemoryMgr = class(TObject)
  private
    fMemory: TMemory;                   // Memory space
    aMemoryReadSections: TMemoryReadSections;
    aMemoryWriteSections: TMemoryWriteSections;
    fOnMemoryRead: TOnMemoryReadWrite;
    fOnMemoryWrite: TOnMemoryReadWrite;
  protected
  public
    constructor Create(Size: integer);
    destructor  Destroy; override;

    procedure AddRead(nStart, nEnd: integer; Handler: TRamRead=nil; Desc: string ='');
    procedure AddWrite(nStart, nEnd: integer; Handler: TRamWrite=nil; Desc: string ='');
    procedure MemReadHandler(Sender: TObject; Addr: word; var Value: byte);
    procedure MemWriteHandler(Sender: TObject; Addr: word; Value: byte);
    function  Description: string;
    function  IsRAM(Addr: word): boolean;

    property Memory: TMemory read fMemory write fMemory;
    property OnMemoryRead: TOnMemoryReadWrite read fOnMemoryRead write fOnMemoryRead;
    property OnMemoryWrite: TOnMemoryReadWrite read fOnMemoryWrite write fOnMemoryWrite;
  end;


implementation

{ CREATE }

{ Initialise memory manager }

constructor TMemoryMgr.Create(Size: integer);
var
  i: integer;
begin
  SetLength(fMemory, Size);             // Set size of memory area
  for i := 0 to (Size - 1) do
    fMemory[i] := $FF;                  // Default read from non-RAM
end;


{ DESTROY }

destructor TMemoryMgr.Destroy;
begin
  SetLength(fMemory, 0);
  SetLength(aMemoryReadSections, 0);
  SetLength(aMemoryWriteSections, 0);
  inherited Destroy;
end;


{ ADD READ }

{ The machine's memory map is built up by adding memory sections to the
  read/write handler arrays. Each element has a start and end address for the
  memory segment, and an optional handler routine to process any special
  requirements associated with that section's read or write. If it is just a
  'normal' read/write, the handler can be left blank, and default memory
  read/writes will take place. Each section can have a description too }

procedure TMemoryMgr.AddRead(nStart, nEnd: integer; Handler: TRamRead = nil; Desc: string ='');
var
  n: integer;
begin
  n := Length(aMemoryReadSections);
  SetLength(aMemoryReadSections, n + 1);
  aMemoryReadSections[n].StartMem := nStart;
  aMemoryReadSections[n].EndMem := nEnd;
  aMemoryReadSections[n].Handler := Handler;
  aMemoryReadSections[n].Desc := Desc;
end;


{ ADD WRITE }

procedure TMemoryMgr.AddWrite(nStart, nEnd: integer; Handler: TRamWrite = nil; Desc: string ='');
var
  n, i: integer;
begin
  n := Length(aMemoryWriteSections);
  SetLength(aMemoryWriteSections, n + 1);
  aMemoryWriteSections[n].StartMem := nStart;
  aMemoryWriteSections[n].EndMem := nEnd;
  aMemoryWriteSections[n].Handler := Handler;
  aMemoryWriteSections[n].Desc := Desc;

  Randomize;
  for i := nStart to nEnd do            // Randomize RAM
    fMemory[i] := Random(256);
end;


{ Memory Handlers : cycle through allocated memory sections checking whether
  the required address is within its range, then conducting a default access
  of memory, or handing off to external special handler as appropriate

  Default handlers below... }

{ MEMORY READ HANDLER }

procedure TMemoryMgr.MemReadHandler(Sender: TObject; Addr: word; var Value: byte);
var
  i: integer;
begin
  Value := $FF;                         // Default if addr not within defined areas
  for i := 0 to (Length(aMemoryReadSections) - 1) do
    begin
      if ((Addr >= aMemoryReadSections[i].StartMem) and (Addr <= aMemoryReadSections[i].EndMem)) then
        begin
          if (aMemoryReadSections[i].Handler = nil) then
            Value := fMemory[Addr]      // Default read from memory
          else
            Value := aMemoryReadSections[i].Handler(Addr);
          Break;
        end;
    end;

  if Assigned(fOnMemoryRead) then       // Allow check for breakpoints
    fOnMemoryRead(self, Addr, Value);
end;


{ MEMORY WRITE HANDLER }

procedure TMemoryMgr.MemWriteHandler(Sender: TObject; Addr: word; Value: byte);
var
  i: integer;
begin
  for i := 0 to (Length(aMemoryWriteSections) - 1) do
    begin
      if ((Addr >= aMemoryWriteSections[i].StartMem) and (Addr <= aMemoryWriteSections[i].EndMem)) then
        begin
          if (aMemoryWriteSections[i].Handler = nil) then
            fMemory[Addr] := Value      // Default write to memory
          else
            aMemoryWriteSections[i].Handler(Addr, Value);
          Break;
        end;
    end;

  if Assigned(fOnMemoryWrite) then
    fOnMemoryWrite(self, Addr, Value);
end;


{ DESCRIPTION }

function TMemoryMgr.Description: string;
var
  i: integer;
begin
  Result := 'Memory map for this emulation:' + CRLF + 'Read:';
  for i := 0 to (Length(aMemoryReadSections) - 1) do
    begin
      Result := Result + Format(CRLF + '  %.4x-%.4x %s', [aMemoryReadSections[i].StartMem, aMemoryReadSections[i].EndMem, aMemoryReadSections[i].Desc]);
    end;
  Result := Result + CRLF + 'Write:';
  for i := 0 to (Length(aMemoryWriteSections) - 1) do
    begin
      Result := Result + Format(CRLF + '  %.4x-%.4x %s', [aMemoryWriteSections[i].StartMem, aMemoryWriteSections[i].EndMem, aMemoryWriteSections[i].Desc]);
    end;
end;


{ IS RAM - check if address provided is actually writeable }

function TMemoryMgr.IsRAM(Addr: word): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to (Length(aMemoryWriteSections) - 1) do
    begin
      if ((Addr >= aMemoryWriteSections[i].StartMem) and (Addr <= aMemoryWriteSections[i].EndMem)) then
        Continue
      else
        begin
          Result := False;
          Break;
        end;
    end;
end;


end.

