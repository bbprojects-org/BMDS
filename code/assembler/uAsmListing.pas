{ ==============================================================================

  ASSEMBLER LISTING

    A helper class to support generation of assembly listing


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

{ TODO : uAsmListing -> sort config items out }
{ TODO : uAsmListing -> check old uListing, and delete if nothing extra }
{ TODO : uAsmListing -> only permit one error/warning per line, so need to
                        change code from current multiple messages array }

unit uAsmListing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //
  uSymbols, uAsmListHtml;

type
  TListingType = (ltStartEnd, ltInclude, ltMultiBytes, ltMacro);
  TListingTypes = set of TListingType;

  TListing = class(TObject)
  private
    fIsListingHtml: boolean;
    fListingHtml: TListingHtml;

    fListingLines: TStrings;
    fTypes: TListingTypes;
    fListingData: TListingData;
    SourceFilename: string;
    AssemblerStartTime: TTimeStamp;
    function CanList: boolean;
  public
    constructor Create(AsmRef: TObject; Filename: string; DoListHtml: boolean = False);
    destructor  Destroy; override;
    //
    procedure ListHeader;
    procedure ListFooter;
    procedure ListLine;
    procedure ListError(ErrMsg: string);
    procedure ListSymbolTable;
    procedure List(Text: string);
    procedure Write;
    //        
    property Lines: TStrings read fListingLines write fListingLines;
    property Types: TListingTypes read fTypes write fTypes;
    property LineNumber: integer read fListingData.LineNumber write fListingData.LineNumber;
    property SourceLine: string read fListingData.SourceLine write fListingData.SourceLine;
    property ExtraStr: string read fListingData.ExtraStr write fListingData.ExtraStr;
    property HexData: string read fListingData.HexData write fListingData.HexData;
    property LabelStr: string read fListingData.LabelStr write fListingData.LabelStr;
    property OpcodeStr: string read fListingData.OpcodeStr write fListingData.OpcodeStr;
    property OperandStr: string read fListingData.OperandStr write fListingData.OperandStr;
    property CommentStr: string read fListingData.CommentStr write fListingData.CommentStr;
  end;


implementation

uses
  uAssembler;

var
  fAsm: TAssembler;


{ CREATE }

constructor TListing.Create(AsmRef: TObject; Filename: string; DoListHtml: boolean);
begin
  fAsm := TAssembler(AsmRef);
  fListingLines := TStringList.Create;
  SourceFilename := Filename;
  fIsListingHtml := DoListHtml;
  if (fIsListingHtml and CanList) then
    fListingHtml := TListingHtml.Create(AsmRef, Filename);
end;


{ DESTROY }

destructor TListing.Destroy;
begin
  inherited Destroy;
  if (fIsListingHtml and CanList) then
    fListingHtml.Free;
end;


{ LIST HEADER }

{ Do listing heading; showing processor, file name and date/time }

procedure TListing.ListHeader;
var
  ProcessorName: string;
begin
  AssemblerStartTime := DateTimeToTimeStamp(Now);
  fListingLines.Clear;
  fTypes := [ltStartEnd];

  ProcessorName := fAsm.Machine.CPU.Name;
  List('//');
  List(Format('// Assembler for processor %s', [ProcessorName]));
  List('//');
  List(Format('// File "%s"', [ExtractFilename(SourceFilename)]));
  List(Format('// %s', [FormatDateTime('dd mmm yyyy, hh:nn', Now)]));
  List('//');
  List('');

  if (fIsListingHtml and CanList) then
    fListingHtml.ListHeader(ProcessorName);

  fTypes := [];
end;


{ LIST FOOTER }

{ Summarise error / warning state, show time if selected in options }

procedure TListing.ListFooter;
begin
  fTypes := [ltStartEnd];

  List('');
  List(fAsm.Errors.SummaryMessage);
  (*
  if (AsmConfigFrame.ShowTime) then
    List(Format('Assembly completed in %d ms', [DateTimeToTimeStamp(Now).Time - AssemblerStartTime.Time]));
  List('');
  *)
  if (fIsListingHtml and CanList) then
    fListingHtml.ListFooter(fAsm.Errors.ErrorCount, fAsm.Errors.WarningCount);

  fTypes := [];
end;


{ LIST LINE }

{ Generate formatted line; including '+' next to line number to show include
  files, and show any errors/warnings pertinent to this line }

procedure TListing.ListLine;
var
  Text(*, IncludeMarker*): string;
  idx: integer;
begin
  fListingData.IsAssembling := fAsm.IsAssembling;
  if (not fAsm.IsAssembling) then
    fListingData.HexData := '';         // Not assembling, no code to show

  if (fAsm.Files.CurrentFileIndex > 0) then // Check if this is main or include file
    fListingData.ExtraStr := '+'
  else
    fListingData.ExtraStr := ' ';
  fListingData.LineNumber := fAsm.Files.CurrentLineNo + 1; // Line numbers are zero based, so add 1
  Text := Format('%.4d %s  %-14s  %s',
    [fListingData.LineNumber, fListingData.ExtraStr,
     fListingData.HexData, fListingData.SourceLine]);

  List(Text);
  if (fIsListingHtml and CanList) then
    fListingHtml.List(fListingData);

  if (fAsm.Errors.LineErrors.Count > 0) then   // Show any errors on the line
    for idx := 0 to (fAsm.Errors.LineErrors.Count - 1) do
      List(fAsm.Errors.LineErrors[idx]);
  if (fAsm.Errors.LineWarnings.Count > 0) then // Show any warnings on the line
    for idx := 0 to (fAsm.Errors.LineWarnings.Count - 1) do
      List(fAsm.Errors.LineWarnings[idx]);
  fAsm.Errors.LineErrors.Clear;
  fAsm.Errors.LineWarnings.Clear;

  fListingData.SourceLine := '';        // Done list processing, clear line
end;


{ LIST }

{ Check various flags before listing line passed in 'Text' }

function TListing.CanList: boolean;
begin
  Result := True;
  (*
  if ( not AsmConfigFrame.GenerateListing ) or
     ( (fAsm.PassNumber = 1) and (not fAsm.Errors.IsErrorsInLine) and (not (ltStartEnd in fTypes)) ) or
     ( (ltInclude in fTypes) and (not AsmConfigFrame.ListIncludes) ) or
     ( (ltMultiBytes in fTypes) and (not AsmConfigFrame.ListMultiBytes) ) then
    Result := False;
  *)
end;


procedure TListing.List(Text: string);
begin
  if (CanList) then
    fListingLines.Add(Text);
end;


{ LIST ERROR }

{ Used for any generalised errors, not specific to a line }

procedure TListing.ListError(ErrMsg: string);
begin
  List('');
  List(ErrMsg);
end;


{ LIST SYMBOL TABLE }

{ Generates a formatted symbol table in columns of three. Any symbol over 18
  characters is truncated to maintain formatting }

procedure TListing.ListSymbolTable;
var
  cSymType: char;
  Counter: integer;
  sLine, sSymbol: string;
begin
  (*
  if ( (not AsmOptions.ListSymbols) or (fAsm.SymbolTable.SymbolCount = 0) ) then
    Exit;                               // If not in options, or no symbols to list, then exit
  *)

  List(Format('Symbols table (%d):', [fAsm.SymbolTable.SymbolCount]));
  Counter := 1;
  sLine := '';                      // Start new symbol table line

  fAsm.SymbolTable.FirstSymbol;
  while (not fAsm.SymbolTable.DoneSymbols) do
    begin

      cSymType := #32;
      if (symDefine in fAsm.SymbolTable.Symbol.Use) then
        cSymType := char('D');
      if (symLabel in fAsm.SymbolTable.Symbol.Use) then
        cSymType := char('L');

      sSymbol := fAsm.SymbolTable.Symbol.Name;
      if (Length(sSymbol) > 18) then
        sSymbol := LeftStr(sSymbol, 16) + '..';
      sLine := sLine + Format('[%.4d]%s %.4x %-20s', [fAsm.SymbolTable.Symbol.Line, cSymType, fAsm.SymbolTable.Symbol.Value, sSymbol]);

      if ((Counter mod 3) = 0) then // Every three columns, restart for next line
        begin
          List(sLine);
          sLine := '';
        end;
      fAsm.SymbolTable.NextSymbol;
      Inc(Counter);
    end;
  List(sLine);
end;


{ WRITE }

procedure TListing.Write;
begin
  (*
  if (AsmConfigFrame.GenerateListing) then
    fListingLines.SaveToFile(ChangeFileExt(SourceFilename, '.lst'));

  if (AsmOptions.GenerateListing) then
    // Save listing to file, with extension '.lst'
    fLines.SaveToFile(ChangeFileExt(SourceFilename, '.lst'));
  *)
end;


end.

