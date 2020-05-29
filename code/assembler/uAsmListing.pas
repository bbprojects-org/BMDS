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

{ TODO : uAsmListing -> check old uListing, and delete if nothing extra }

unit uAsmListing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //
  uAsmPrefsFrame, uAsmListHtml;

type
  { TListing }

  TListing = class(TObject)
  private
    fIsInclude: boolean;
    fIsMultiBytes: boolean;
    fIsMacro: boolean;
    fIsMacroExp: boolean;
    fPassNumber: integer;
    fCurrentFileName: string;
    //
    fIsListingHtml: boolean;
    fListingHtml: TListingHtml;
    fListingLines: TStrings;
    fListingData: TListingData;
    fSummary: string;
    SourceFileName: string;
    AssemblerStartTime: TTimeStamp;
    function CanList: boolean;
    procedure ClearListingData;
    procedure WriteToFile;
  public
    constructor Create(DoListHtml: boolean = False);
    destructor  Destroy; override;
    //
    procedure Start(FileName: string; CpuName: string);
    procedure SetLine(CurrentFileName: string; CurrentLineNo: integer; SourceLine: string);
    procedure Finish;
    procedure ListLine;
    procedure List(Text: string; Forced: boolean = False);
    procedure ListError(ErrMsg: string);
    //
    property PassNumber: integer write fPassNumber;
    //
    property Lines: TStrings read fListingLines write fListingLines;
    property LineNumber: integer read fListingData.LineNumber write fListingData.LineNumber;
    property SourceLine: string read fListingData.SourceLine write fListingData.SourceLine;
    property IsAssembling: boolean read fListingData.IsAssembling write fListingData.IsAssembling;
    property ExtraStr: string read fListingData.ExtraStr write fListingData.ExtraStr;
    property HexData: string read fListingData.HexData write fListingData.HexData;
    property LabelStr: string read fListingData.LabelStr write fListingData.LabelStr;
    property OpcodeStr: string read fListingData.OpcodeStr write fListingData.OpcodeStr;
    property OperandStr: string read fListingData.OperandStr write fListingData.OperandStr;
    property CommentStr: string read fListingData.CommentStr write fListingData.CommentStr;
    property Summary: string read fSummary write fSummary;
    //
    property IsInclude: boolean read fIsInclude write fIsInclude;
    property IsMultiBytes: boolean read fIsMultiBytes write fIsMultiBytes;
    property IsMacro: boolean read fIsMacro write fIsMacro;
    property IsMacroExp: boolean read fIsMacroExp write fIsMacroExp;
  end;


implementation


{ CREATE }

constructor TListing.Create(DoListHtml: boolean);
begin
  fListingLines := TStringList.Create;
  fIsListingHtml := DoListHtml;
  fPassNumber := 1;
  (*
  if (fIsListingHtml and CanList) then
    fListingHtml := TListingHtml.Create(AsmRef, FileName);
  *)
end;


{ DESTROY }

destructor TListing.Destroy;
begin
  inherited Destroy;
  fListingLines.Free;
  if (fIsListingHtml and CanList) then
    fListingHtml.Free;
end;


{ START }

{ Do listing heading; showing processor, file name and date/time }

procedure TListing.Start(FileName: string; CpuName: string);
begin
  AssemblerStartTime := DateTimeToTimeStamp(Now);
  SourceFileName := FileName;           // Used for listing FileName
  fListingLines.Clear;
  fListingData.IsAssembling := True;
  fSummary := '';

  List('//', True);
  List(Format('// Assembler for processor %s', [CpuName]), True);
  List('//', True);
  List(Format('// File "%s"', [ExtractFileName(SourceFileName)]), True);
  List(Format('// %s', [FormatDateTime('dd mmm yyyy, hh:nn', Now)]), True);
  List('//', True);
  List('', True);

  if (fIsListingHtml and CanList) then
    fListingHtml.ListHeader(CpuName);
end;


{ FINISH }

{ Summarise error / warning state, show time if selected in options, list
  symbol table in Pass 2 }

procedure TListing.Finish;
begin
  List('', True);
  List(fSummary, True);
  if (AsmPrefs.ShowTime) then
    List(Format('Assembly completed in %d ms', [DateTimeToTimeStamp(Now).Time - AssemblerStartTime.Time]));
  (*
  if (fIsListingHtml and CanList) then
    fListingHtml.ListFooter(ErrorCount, 0);
  *)
  WriteToFile;

  fListingLines.Clear;                  // Not needed anymore this session
end;


{ SET CURRENT LINE INFO }

procedure TListing.SetLine(CurrentFileName: string; CurrentLineNo: integer; SourceLine: string);
begin
  fCurrentFileName := CurrentFileName;
  fListingData.LineNumber := CurrentLineNo + 1; // Zero based, so add 1
  fListingData.SourceLine := SourceLine;
  ClearListingData;
end;


{ CLEAR LINE - RESET LINE VARIABLES TO EMPTY }

procedure TListing.ClearListingData;
begin
  fListingData.HexData := '';
  fListingData.LabelStr := '';
  fListingData.OpcodeStr := '';
  fListingData.OperandStr := '';
  fListingData.CommentStr := '';
end;


{ LIST LINE }

{ Generate formatted line; including '+' next to line number to show include
  files, 'm' if a macro, and shows any errors/warnings pertinent to this line }

procedure TListing.ListLine;
var
  Text: string;
begin
  if (not fListingData.IsAssembling) then
    fListingData.HexData := '';         // Not assembling, no code to show

  fListingData.ExtraStr := BoolToStr(fIsInclude, '+', ' ');
  if (fIsMacro or fIsMacroExp) then     // Show macro expansion
    begin
      fListingData.ExtraStr := fListingData.ExtraStr + 'm';
      fIsMacroExp := False;             // Only do for macro name
    end
  else
    fListingData.ExtraStr := fListingData.ExtraStr + ' ';

  Text := Format('%.4d%s  %-14s  %s',
    [fListingData.LineNumber, fListingData.ExtraStr,
     fListingData.HexData, fListingData.SourceLine]);

  List(Text);
  if (fIsListingHtml and CanList) then
    fListingHtml.List(fListingData);
  fListingData.SourceLine := '';        // Done list processing, clear line
end;


{ LIST }

{ Check various flags before listing line passed in 'Text' }

function TListing.CanList: boolean;
begin
  Result := True;
  if ( not AsmPrefs.GenerateListing ) or
     ( fPassNumber = 1 ) or
     ( (fIsInclude) and (not AsmPrefs.ListIncludes) ) or
     ( (fIsMacro) and (not AsmPrefs.ListMacros) ) or
     ( (fIsMultiBytes) and (not AsmPrefs.ListMultiBytes) ) then
    Result := False;
end;


{ List line if criteria above met, or if forced by Error calls }

procedure TListing.List(Text: string; Forced: boolean);
begin
  if (CanList) or (Forced) then
    fListingLines.Add(Text);
end;


{ WRITE LISTING FILE }

procedure TListing.WriteToFile;
begin
  if (AsmPrefs.GenerateListing) then
    fListingLines.SaveToFile(ChangeFileExt(SourceFileName, '.lst'));
end;


{ LIST ERROR }

procedure TListing.ListError(ErrMsg: string);
begin
  List(SourceLine, True);
  List(ErrMsg, True);
end;


end.

