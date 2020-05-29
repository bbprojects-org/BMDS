{ ==============================================================================

  SYMBOLS

    This class maintains the symbol table, providing the methods for adding
    and finding symbols.
    Because a hashtable cannot keep entries in any sorted order, a secondary
    stringlist is used to keep a sorted list. This is then used as the keys
    into the hashtable to generate a sorted symbol table.

    This class also has a further table for opcodes (mnemonics) and macros.
    Each microprocessor provides a data structure to initialise this opcode
    table


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

unit uSymbols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  //
  uHashTables;

type
  ESymbolsError = class(Exception);

  TUseList = set of (symDefine,         // Defined value?
                     symLabel,          // Label?
                     symUsed,           // Has it been used in this pass?
                     symSet);           // Has it been set in this pass?

  TSymbol = class
  public
    Name: string;                       // Symbol identifier
    Value: integer;                     // Value; either PC or assigned
    Use: TUseList;                      // Whether type, used, set, etc
    Line: integer;                      // Line where symbol defined
    SourceIndex: integer;               // Fileindex where symbol defined
  end;

  TInstructionType = (
    itErr,                              // Error

    // ASSEMBLER DIRECTIVES
    itCPU,                              // Select CPU type
    itEnd,                              // End of file
    itOrg,                              // Location control
    itReserve,                          // Reserve memory
    itByte,                             // Byte constant
    itWord,                             // Word constant
    itText,                             // Text constant
    itEqu,                              // Symbol declaration
    itMacro,                            // Macro declaration
    itEndmacro,

    itDoMacro,                          // Macro call

    itMnem );                           // Opcode mnmonics

  TInstruction = class
  public
    Name: string;                       // Instruction name / opcode mnemonic
    InstructionType: TInstructionType;  // Assembler directive / macro / mnemonic
    Value: integer;                     // Used to point into opcode data array
  end;

  TSymbols = class(TObject)
  private
    fSymbols: TStringTable;
    fEndSymbols: boolean;
    fSortedSymbols: TStringList;
    fSortedSymIdx: Integer;
    fInstructions: TStringTable;
    function GetSymbol: TSymbol;
    function GetSymbolCount: integer;
    function GetInstruction: TInstruction;
    function GetInstructionCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    //
    procedure AddSymbol(Name: string; Value: integer; Use: TUseList; Line: integer);
    function  FindSymbol(Name: string): TSymbol;
    procedure FirstSymbol;
    procedure NextSymbol;

    property  DoneSymbols: boolean read fEndSymbols;
    property  Symbol: TSymbol read GetSymbol;
    property  SymbolCount: integer read GetSymbolCount;
    //
    procedure AddInstruction(Name: string; InstrType: TInstructionType; Value: integer);
    function  FindInstruction(Name: string): TInstruction;

    property  Instruction: TInstruction read GetInstruction;
    property  InstructionCount: integer read GetInstructionCount;

  end;

const
  SYMBOL_TABLE         = 'symbol';
  INSTRUCTION_TABLE    = 'instruction';

  MSG_DUPLICATE_SYMBOL = 'Symbol [%s] already defined in %s table';
  MSG_NO_SYMBOLS       = 'No symbols in table';


implementation

{ CREATE }

constructor TSymbols.Create;
begin
  inherited Create;

  fSymbols := TStringTable.Create;      // Default size = 256, will expand automatically
  fEndSymbols := False;

  fSortedSymbols := TStringList.Create;
  fSortedSymbols.Sorted := True;        // Automatically keep symbols sorted
  fSortedSymIdx := 0;

  fInstructions := TStringTable.Create; // Default size = 256
end;


{ DESTROY }

destructor TSymbols.Destroy;
begin
  // Free each symbol in turn
  if (fSymbols.First <> nil) then
    (fSymbols.Current as TSymbol).Free;
  while fSymbols.Next <> nil do
    (fSymbols.Current as TSymbol).Free;
  fSymbols.Free;

  // Free each instruction in turn
  if (fInstructions.First <> nil) then
    (fInstructions.Current as TInstruction).Free;
  while fInstructions.Next <> nil do
    (fInstructions.Current as TInstruction).Free;
  fInstructions.Free;
  
  fSortedSymbols.Free;
  inherited Destroy;
end;


{ ADD SYMBOL}

procedure TSymbols.AddSymbol(Name: string; Value: integer; Use: TUseList; Line: integer);
var
  symData: TSymbol;
begin
  symData := TSymbol.Create;
  symData.Name := Name;
  symData.Value := Value;
  symData.Use := Use;
  symData.Line := Line;
  if (fSymbols.Items[Name] = nil) then
    fSymbols.Insert(Name, symData)
  else
    raise ESymbolsError.CreateFmt(MSG_DUPLICATE_SYMBOL, [Name, SYMBOL_TABLE]);

  fSortedSymbols.Add(Name);             // Separate table for sorted items
end;


{ FIND SYMBOL }

function TSymbols.FindSymbol(Name: string): TSymbol;
begin
  Result := (fSymbols.Items[Name] as TSymbol);
end;


{ FIRST SYMBOL}

procedure TSymbols.FirstSymbol;
begin
  if (fSymbols.Count <> 0) then         // Check there is a first
    begin
      fSortedSymIdx := 0;
      fEndSymbols := False;
      fSymbols.Items[fSortedSymbols[fSortedSymIdx]];
    end
  else
    fEndSymbols := True;
end;


{ NEXT SYMBOL }

procedure TSymbols.NextSymbol;
begin
  if (fSortedSymIdx < (fSymbols.Count - 1)) then
    begin
      Inc(fSortedSymIdx);
      fSymbols.Items[fSortedSymbols[fSortedSymIdx]];
    end
  else
    fEndSymbols := True;
end;


{ GET SYMBOL }

function TSymbols.GetSymbol: TSymbol;
begin
  if (fSymbols.Count > 0) then
    Result := (fSymbols.Current as TSymbol)
  else
    raise ESymbolsError.Create(MSG_NO_SYMBOLS);
end;


{ GET SYMBOL COUNT }

function TSymbols.GetSymbolCount: integer;
begin
  Result := fSymbols.Count;
end;


{ ADD INSTRUCTION }

{ Instruction table contains pseudo ops, CPU mnemonics, and macros names. If
  an entry already exists a Duplicate Error is raised }

procedure TSymbols.AddInstruction(Name: string; InstrType: TInstructionType; Value: integer);
var
  InstrData: TInstruction;
begin
  InstrData := TInstruction.Create;
  InstrData.Name := Name;
  InstrData.InstructionType := InstrType;
  InstrData.Value := Value;
  if (fInstructions.Items[Name] = nil) then
    fInstructions.Insert(Name, InstrData)
  else
    raise ESymbolsError.CreateFmt(MSG_DUPLICATE_SYMBOL, [Name, INSTRUCTION_TABLE]);
end;


{ GET INSTRUCTION }

function TSymbols.GetInstruction: TInstruction;
begin
  Result := (fInstructions.Current as TInstruction);
end;


{ GET INSTRUCTION COUNT }

function TSymbols.GetInstructionCount: integer;
begin
  Result := fInstructions.Count;
end;


{ FIND INSTRUCTION }

function TSymbols.FindInstruction(Name: string): TInstruction;
begin
  Result := (fInstructions.Items[Name] as TInstruction);
end;


end.
