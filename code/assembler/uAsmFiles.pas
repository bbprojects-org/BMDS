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

{ TODO : uAsmFiles -> add CodeAddr-to-SourceLine array for debugger }
{ TODO : uAsmFiles -> get record type from Options }

unit uAsmFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  //
  uAsmConfigFrame, uAsmListing, uReadWriteHex;

type
  TFileInfo = record                    // Used for file stack ...
    Filename: string;                   //   = full file path & name
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

  { TFiles }

  TFiles = class
  private
    fSourceFiles: TArrayFileInfo;       // Array of source file names
    fCurrentFilename: string;           // Filename of current file where processing lines
    fCurrentLineNo: integer;            // Current line number, zero based
    fCurrentFileIndex: integer;         // Index to current file in 'fSourceFiles'
    fFlagEnd: boolean;                  // Used to flag that END encountered in file
    FileStack: array of integer;        // Used to push 'fCurrentFileIndex' when get new file
    IncludeFileIndex: integer;          // Used to delay file switch until current line listed
    DataFile: TWriteHex;                // Output file for assembled data bytes (hex)
    // LinesInfo: TArrayFileInfo;          // Used for debugger Code Addr -> Source Line
  public
    constructor Create(AsmRef: TObject; Filename: string);
    destructor  Destroy; override;
    procedure Init;
    function  OpenFile(Filename: string): integer;
    procedure GetSourceLine(Sender: TObject; var Line: string; var IsEof: boolean);
    procedure SetDataStart(Address: integer);
    procedure WriteDataByte(Data: byte);
    //
    property CurrentFileIndex: integer read fCurrentFileIndex;
    property CurrentFilename: string read fCurrentFilename;
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

constructor TFiles.Create(AsmRef: TObject; Filename: string);
begin
  fAsm := TAssembler(AsmRef);
  Init;                                 // Ensure variables are initialised

  // Initialise output data file
  if (AsmConfigFrame.WriteToFile) then
    DataFile := TWriteHex.Create(ChangeFileExt(Filename, '.hex')
      {, format : defaults to IntelHex});
end;


{ DESTROY }

destructor TFiles.Destroy;
begin
  if (DataFile <> nil) then
    DataFile.Free;
  SetLength(fSourceFiles, 0);
  fSourceFiles := nil;
  SetLength(FileStack, 0);
  FileStack := nil;
  inherited Destroy;
end;


{ INIT }

procedure TFiles.Init;
begin
  IncludeFileIndex := 0;                // No current include file
  SetLength(fSourceFiles, 0);           // Initialise lists
  SetLength(FileStack, 0);
  fCurrentFilename := '';               // ... and current values
  fCurrentLineNo := -1;
  fCurrentFileIndex := 0;               // Ready for main source file
  fFlagEnd := False;
end;


{ GET NEXT LINE }

{ This routine is the callback from the parser whenever it needs a new
  line. It manages where that line is taken from; primary source file,
  include file, or macro }

procedure TFiles.GetSourceLine(Sender: TObject;
            var Line: string;           // Return value for the next line
            var IsEof: boolean);        // Return value indicating when EOF
var
  Len: integer;
begin
  // Check if just processed 'Include' directive, if so swap to new file
  if (IncludeFileIndex > 0) then
    begin
      fCurrentFileIndex := IncludeFileIndex;
      fCurrentFilename := fSourceFiles[IncludeFileIndex].Name;
      fCurrentLineNo := -1;
      IncludeFileIndex := 0;            // Reset flag
      if (AsmConfigFrame.ListIncludes) then
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
      fCurrentFilename := fSourceFiles[fCurrentFileIndex].Name;
      fCurrentLineNo := fSourceFiles[fCurrentFileIndex].LastLine + 1;
      // ... and remove top stack item
      SetLength(FileStack, Len - 1);
      fAsm.Listing.List('');            // List blank line before return to previous file
    end;

  // Notify listing routine whether processing main or an include file
  if (fCurrentFileIndex = 0) then
    fAsm.Listing.Types := fAsm.Listing.Types - [ltInclude]
  else
    fAsm.Listing.Types := fAsm.Listing.Types + [ltInclude];

  // ... and finally; get the next line for the parser
  Line := fSourceFiles[fCurrentFileIndex].Lines[fCurrentLineNo];
end;


{ OPEN FILE }

{ On Pass 1, open files as required, pushing last file index each time. If
  a file has already been seen, then raise an error

  On Pass 2, should not see repeated filename as it should have been picked
  up as an error on the first pass }

function TFiles.OpenFile(Filename: string): integer;
var
  i, Len1, Len2: integer;
  lcFilename: string;
begin
  lcFilename := LowerCase(Filename);
  Len1 := Length(fSourceFiles);
  for i := 0 to (Len1 - 1) do
    if (fSourceFiles[i].Name = lcFilename) then
      begin
        // Error, already processed this file once, so quit function
        Result := -1;
        Exit;
      end;

  // Filename ok; add to files list, and load text from file (as StringList)
  SetLength(fSourceFiles, Len1 + 1);
  fSourceFiles[Len1].Filename := lcFilename;
  fSourceFiles[Len1].Name := ExtractFilename(Filename);
  fSourceFiles[Len1].LastLine := -1;
  fSourceFiles[Len1].Lines := TStringList.Create;
  fSourceFiles[Len1].Lines.LoadFromFile(Filename);
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
      fCurrentFilename := fSourceFiles[Len1].Name;
      fCurrentLineNo := -1;
    end;
end;


{ SET DATA START }

{ Sets start address for data output }

procedure TFiles.SetDataStart(Address: integer);
begin
  if ( (AsmConfigFrame.WriteToFile) and (fAsm.PassNumber = 2) ) then
    DataFile.SetStart(Address);
end;


{ WRITE DATA }

procedure TFiles.WriteDataByte(Data: byte);
begin
  if ( (AsmConfigFrame.WriteToFile) and (fAsm.PassNumber = 2) ) then
    DataFile.WriteByte(Data);
end;


end.

