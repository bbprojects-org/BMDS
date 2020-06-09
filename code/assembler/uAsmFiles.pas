{ ==============================================================================

  ASSEMBLER FILES HELPER

    A helper class to support management of files in the Assembler

    This unit handles opening and closing of source files, supporting INCLUDE
    files, and provides the main routine for supplying source lines to the
    parser.

    In addition this unit supports the output data file


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

{ TODO : uAsmFiles -> add CodeAddr-to-SourceLine array for debugger, use a 64K
                      array to allow easy address->sourceline translation }
{ TODO : uAsmFiles -> get record type from Options }

unit uAsmFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  //
  uAsmPrefsFrame, uAsmListing, uReadWriteHex;

type
  TFileInfo = record                    // Used for file stack ...
    FileName: string;                   //   = full file path & name
    Name: string;                       //   = short filename
    Lines: TStrings;                    //   = text lines of file
    LastLine: integer;                  //   = last line number processed
  end;
  TArrayFileInfo = array of TFileInfo;

  TLineInfo = record
    SourceIndex: integer;               // Index to filename in 'SourceFiles'
    LineNumber: integer;                // Current line being processed
    MachineAddr: LongWord;              // PC address for this line of code
  end;
  TArrayLineInfo = array of TLineInfo;

  TMacroInfo = record
    Name: string;
    Lines: TStrings;
  end;
  TMacroArray = array of TMacroInfo;

  TMacroStackItems = record
    MacroIdx: integer;
    LabelIdx: integer;
    LastLine: integer;
    Parameters: TStringArray;
  end;
  TMacroStackArray = array of TMacroStackItems;

  { TFiles }

  TFiles = class
  private
    fSourceFiles: TArrayFileInfo;       // Array of source file names
    fCurrentFileName: string;           // FileName of current file where processing lines
    fCurrentLineNo: integer;            // Current line number, zero based
    fCurrentFileIndex: integer;         // Index to current file in 'fSourceFiles'
    fFlagEnd: boolean;                  // Used to flag that END encountered in file
    FileStack: array of integer;        // Used to push 'fCurrentFileIndex' when get new file
    IncludeFileIndex: integer;          // Used to delay file switch until current line listed
    DataFile: TWriteHex;                // Output file for assembled data bytes (hex)
    // LinesInfo: TArrayFileInfo;          // Used for debugger Code Addr -> Source Line

    MacroDefs: TMacroArray;
    MacroStackArray: TMacroStackArray;
    DefiningMacroNumber: integer;

    CurrentMacroIdx: integer;
    CurrentMacroLineNumber: integer;
    CurrentMacroLabelIdx: integer;
    MacroCount: integer;
    CurrentMacroParams: TStringArray;
    function ExpandParameters(Line: string): string;
    function GetMacroLine: string;
  public
    constructor Create(AsmRef: TObject; FileName: string);
    destructor  Destroy; override;
    procedure Init;
    function  OpenFile(FileName: string): integer;
    procedure GetSourceLine(Sender: TObject; var Line: string; var IsEof: boolean);
    procedure SetDataStart(Address: integer);
    procedure WriteDataByte(Data: byte);
    function AddMacro(Name: string): integer;
    procedure AddMacroLine(Line: string);
    procedure ExpandMacro(Idx: integer; Params: string);
    //
    property CurrentFileIndex: integer read fCurrentFileIndex;
    property CurrentFileName: string read fCurrentFileName;
    property CurrentLineNo: integer read fCurrentLineNo write fCurrentLineNo;
    property SourceFiles: TArrayFileInfo read fSourceFiles;
    property FlagEnd: boolean write fFlagEnd;
  end;


implementation

uses
  uAssembler;

var
  fAsm: TAssembler;

{ CREATE }

{ Save reference to parent Asm, initialise variables, and opens output data
  file if selected in options }

constructor TFiles.Create(AsmRef: TObject; FileName: string);
begin
  fAsm := TAssembler(AsmRef);
  Init;                                 // Ensure variables are initialised

  // Initialise output data file
  if (AsmPrefs.WriteToFile) then
    DataFile := TWriteHex.Create(ChangeFileExt(FileName, '.hex'), rtIntelHex);
end;


{ DESTROY }

destructor TFiles.Destroy;
var
  i: integer;
begin
  if (DataFile <> nil) then
    DataFile.Free;
  SetLength(fSourceFiles, 0);
  fSourceFiles := nil;
  SetLength(FileStack, 0);
  FileStack := nil;
  for i := 0 to Length(MacroDefs)-1 do
    MacroDefs[i].Lines.Free;
  SetLength(MacroDefs, 0);
  MacroDefs := nil;
  inherited Destroy;
end;


{ INIT }

procedure TFiles.Init;
begin
  IncludeFileIndex := 0;                // No current include file
  SetLength(fSourceFiles, 0);           // Initialise lists
  SetLength(FileStack, 0);
  fCurrentFileName := '';               // ... and current values
  fCurrentLineNo := -1;
  fCurrentFileIndex := 0;               // Ready for main source file
  fFlagEnd := False;
end;


{ GET NEXT LINE }

{ This routine is the callback from the parser whenever it needs a new
  line. It manages where that line is taken from; primary source file,
  include file, or macro }

procedure TFiles.GetSourceLine(Sender: TObject; var Line: string; var IsEof: boolean);
var
  Len: integer;
begin
  // If expanding a macro, get lines from there instead
  if (MacroCount > 0) then
    begin
      Line := GetMacroLine;
      Exit;
    end;

  // Check if just processed 'Include' directive, if so swap to new file
  if (IncludeFileIndex > 0) then
    begin
      fCurrentFileIndex := IncludeFileIndex;
      fCurrentFileName := fSourceFiles[IncludeFileIndex].Name;
      fCurrentLineNo := -1;
      IncludeFileIndex := 0;            // Reset flag
      if (AsmPrefs.ListIncludes) then
        fAsm.Listing.List('');          // Add line before start new file
    end;

  Inc(fCurrentLineNo);
  // If there are no more lines in the current file, then close it,
  // noting that Strings are zero based, but Count is total # of lines
  if ( (fCurrentLineNo = fSourceFiles[fCurrentFileIndex].Lines.Count)
       // Also exits the current file if END is encountered in source file
       or (fFlagEnd) ) then
    begin
      // Free up source text just completed
      fSourceFiles[fCurrentFileIndex].Lines.Free;
      // Check if this is the end of the main file, i.e. no more to process
      Len := Length(FileStack);
      if (Len = 0) then
        begin
          IsEof := True;                // ... if so, then set EOF, and exit
          Line := '';
          Exit;
        end;
      // If not, pop file index off stack and restore name & next line to process
      fCurrentFileIndex := FileStack[Len - 1];
      fCurrentFileName := fSourceFiles[fCurrentFileIndex].Name;
      fCurrentLineNo := fSourceFiles[fCurrentFileIndex].LastLine + 1;
      // ... and remove top stack item
      SetLength(FileStack, Len - 1);
      fAsm.Listing.List('');            // List blank line before return to previous file
    end;

  // Notify listing routine whether processing main or an include file
  fAsm.Listing.IsInclude := (fCurrentFileIndex > 0);

  // ... and finally; get the next line for the parser
  Line := fSourceFiles[fCurrentFileIndex].Lines[fCurrentLineNo];
end;


{ OPEN FILE }

{ On Pass 1, open files as required, pushing last file index each time. If
  a file has already been seen, then raise an error

  On Pass 2, should not see repeated filename as it should have been picked
  up as an error on the first pass }

function TFiles.OpenFile(FileName: string): integer;
var
  i, Len1, Len2: integer;
  lcFileName: string;
begin
  lcFileName := LowerCase(FileName);
  Len1 := Length(fSourceFiles);
  for i := 0 to (Len1 - 1) do
    if (fSourceFiles[i].Name = lcFileName) then
      begin
        // Error, already processed this file once, so quit function
        Result := -1;
        Exit;
      end;

  // FileName ok; add to files list, and load text from file (as StringList)
  SetLength(fSourceFiles, Len1 + 1);
  fSourceFiles[Len1].FileName := lcFileName;
  fSourceFiles[Len1].Name := ExtractFileName(FileName);
  fSourceFiles[Len1].LastLine := -1;
  fSourceFiles[Len1].Lines := TStringList.Create;
  fSourceFiles[Len1].Lines.LoadFromFile(FileName);
  Result := Len1;

  if (Len1 > 0) then
    // If we already have a source file, then push its index onto file stack.
    // Current file values will be set on next GetSourceLine
    begin
      Len2 := Length(FileStack);
      SetLength(FileStack, Len2 + 1);
      FileStack[Len2] := fCurrentFileIndex;
      // ... and save line number for when return to this file
      fSourceFiles[fCurrentFileIndex].LastLine := fCurrentLineNo;
      // Notify 'GetSourceLine' that it needs to change file input
      IncludeFileIndex := Len1;
    end
  else
    // Otherwise set current file values for this first file now
    begin
      fCurrentFileIndex := Len1;
      fCurrentFileName := fSourceFiles[Len1].Name;
      fCurrentLineNo := -1;
    end;
end;


{ SET DATA START }

{ Sets start address for data output }

procedure TFiles.SetDataStart(Address: integer);
begin
  if ( (AsmPrefs.WriteToFile) and (fAsm.PassNumber = 2) ) then
    DataFile.SetStart(Address);
end;


{ WRITE DATA }

procedure TFiles.WriteDataByte(Data: byte);
begin
  if ( (AsmPrefs.WriteToFile) and (fAsm.PassNumber = 2) ) then
    DataFile.WriteByte(Data);
end;


///////////////////////////////////////////////////////////////////////////////
// Save lines of macro code to "pseudo files" for recovery later as required
///////////////////////////////////////////////////////////////////////////////

function TFiles.AddMacro(Name: string): integer;
var
  Len: integer;
begin
  Len := Length(MacroDefs);
  SetLength(MacroDefs, Len + 1);        // Make room for new item
  MacroDefs[Len].Name := Name;
  MacroDefs[Len].Lines := TStringList.Create;
  DefiningMacroNumber := Len;
  Result := Len;
end;


procedure TFiles.AddMacroLine(Line: string);
begin
  MacroDefs[DefiningMacroNumber].Lines.Add(Line);
end;


procedure TFiles.ExpandMacro(Idx: integer; Params: string);
var
  Len: integer;
begin
  if (MacroCount > 0) then              // Already expanding a macro, i.e. nested call?
    begin
      // Pushing existing Macro info to stack
      Len := Length(MacroStackArray);
      SetLength(MacroStackArray, Len + 1); // Make room for new item on stack
      MacroStackArray[Len].MacroIdx := CurrentMacroIdx;
      MacroStackArray[Len].LastLine := CurrentMacroLineNumber;
      MacroStackArray[Len].LabelIdx := CurrentMacroLabelIdx;
      MacroStackArray[Len].Parameters := CurrentMacroParams;
    end;
  Inc(MacroCount);                      // Need to INIT to 0
  CurrentMacroIdx := Idx;
  CurrentMacroLineNumber := -1;
  CurrentMacroLabelIdx := 0;
  CurrentMacroParams := Params.Split(',');
end;


function TFiles.GetMacroLine: string;
var
  Len: integer;
begin
  Inc(CurrentMacroLineNumber);
  if (CurrentMacroLineNumber = MacroDefs[CurrentMacroIdx].Lines.Count) then
    begin
      // If done all lines, check if nested macro
      Len := Length(MacroStackArray);
      if (Len > 0) then
        begin
          // Restore last macro info
          CurrentMacroIdx := MacroStackArray[Len].MacroIdx;
          CurrentMacroLineNumber := MacroStackArray[Len].LastLine;
          CurrentMacroLabelIdx := MacroStackArray[Len].LabelIdx;
          CurrentMacroParams := MacroStackArray[Len].Parameters;
          // ... and remove top stack item
          SetLength(FileStack, Len - 1);
        end;
      Dec(MacroCount);
      Result := '';
    end
  else
    Result := ExpandParameters(MacroDefs[CurrentMacroIdx].Lines[CurrentMacroLineNumber]);

  if (MacroCount > 0) then
    fAsm.Listing.IsMacro := True
  else
    fAsm.Listing.IsMacro := False;
end;


{ TODO : uAsmFiles -> consider "exitm \# = expr" to check num params in definition vs provided }

function TFiles.ExpandParameters(Line: string): string;
var
  Params: TStringArray;
  Count, i: integer;
begin
  Count := Length(Params);
  for i := 1 to Count do
    StringReplace(Line, '\'+IntToStr(i), Params[i-1], [rfReplaceAll]);
  Result := Line;
end;


end.

