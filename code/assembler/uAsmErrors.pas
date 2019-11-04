{ ==============================================================================

  ASSEMBLER ERRORS HELPER

    A helper class to support management of errors and warnings in the
    Assembler


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

unit uAsmErrors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  EAnalyseError = class(Exception);

  TItemData = record
    SourceIndex: integer;               // Index to source file filename (=0 if mainfile)
    LineNumber: integer;                // Current line being processed
    StringData: string;                 // Value of string
  end;
  TArrayErrors = array of TItemData;

  TOnErrorEvent = procedure (Sender: TObject; ErrMsg: string) of object;

  { TErrors }

  TErrors = class
  private
    fErrors: TArrayErrors;              // Used to return errors list
    fErrorCount: integer;
    fWarningCount: integer;
    //
    fLineErrors: TStrings;              // Holds any error messages
    fLineWarnings: TStrings;            // Holds any warnings
  public
    constructor Create(AsmRef: TObject);
    destructor  Destroy; override;
    //
    procedure AddError(ErrMsg: string);
    procedure AddWarning(WarnMsg: string);
    function  IsErrorsInLine: boolean;
    function  SummaryMessage: string;
    //
    property ErrorCount: integer read fErrorCount;
    property WarningCount: integer read fWarningCount;
    property LineErrors: TStrings read fLineErrors;
    property LineWarnings: TStrings read fLineWarnings;
  end;

const
  ADDR_MODE_NOT_RECOGNISED = 'Address mode [%s] not recognised';
  BRANCH_TOO_FAR           = 'Branch too far';
  CANNOT_FIND_INCLUDE_FILE = 'Cannot find include file [%s]';
  ELSE_WITHOUT_IF          = 'ELSE without starting IF';
  ENDIF_WITHOUT_IF         = 'ENDIF without starting IF';
  EXPECTED_NOT_FOUND       = '[%s] expected but [%s] found';
  INSTR_NOT_RECOGNISED     = 'Instruction [%s] not recognised';
  LABEL_MISSING            = 'Label is required for this instruction';
  MEM_MODE_NOT_RECOGNISED  = 'Memory mode [%s] not recognised';
  OPERAND_NOT_FOUND        = 'Operand expected, not found';
  PASS_2_ABORTED           = 'Pass 2 aborted due to errors in Pass 1';
  PHASING_ERROR            = 'Symbol values differ between passes, check addressing mode';
  SYMBOL_NOT_DEFINED       = 'Symbol [%s] not defined';


implementation

uses
  uAssembler;

var
  fAsm: TAssembler;

{ CREATE }

{ Save reference to parent Asm, create error lists, and initialise variables }

constructor TErrors.Create(AsmRef: TObject);
begin
  fAsm := TAssembler(AsmRef);           // Keep ref to parent Asm object
  fLineErrors := TStringList.Create;    // Create error/warning lists
  fLineWarnings := TStringList.Create;
  SetLength(fErrors, 0);                // Ensure error list is empty
  fErrorCount := 0;
  fWarningCount := 0;
end;


{ DESTROY }

destructor TErrors.Destroy;
begin
  SetLength(fErrors, 0);
  fErrors := nil;
  fLineWarnings.Free;
  fLineErrors.Free;
  inherited Destroy;
end;


{ ADD ERROR }

{ Save error message with added filename and linenumber into errors list, to
  be used by listing unit }

procedure TErrors.AddError(ErrMsg: string);
var
  ErrorMsg: string;
  nCounter: integer;
begin
  ErrorMsg := Format('Error in line %d in [%s]: %s',
    [fAsm.Files.CurrentLineNo + 1,      // Line numbers are zero based, so add 1
     fAsm.Files.CurrentFilename,
     ErrMsg]);
  fLineErrors.Add(ErrorMsg);
  Inc(fErrorCount);

  nCounter := Length(fErrors);          // Add error to overall error list
  SetLength(fErrors, nCounter + 1);
  fErrors[nCounter].StringData := ErrorMsg;
  fErrors[nCounter].LineNumber := fAsm.Files.CurrentLineNo;
  fErrors[nCounter].SourceIndex := fAsm.Files.CurrentFileIndex;

  fAsm.Parser.SkipRestOfLine;           // Do not permit more errors
end;


{ ADD WARNING }

procedure TErrors.AddWarning(WarnMsg: string);
begin
  fLineWarnings.Add(Format('Warning in line %d in [%s]: %s',
    [fAsm.Files.CurrentLineNo + 1,      // Line numbers are zero based, so add 1
     fAsm.Files.CurrentFilename,
     WarnMsg]));
  Inc(fWarningCount);
end;


{ IS ERRORS IN LINE }

{ Check whether the current line has encountered any errors / warnings }

function TErrors.IsErrorsInLine: boolean;
begin
  Result := ((fLineErrors.Count + fLineWarnings.Count) > 0);
end;


{ SUMMARY MESSAGE }

{ For the end of the listing, show how many errors and warnings were
  encountered overall during assembly }

function TErrors.SummaryMessage: string;
var
  PluralE, PluralW: string;
begin
  if (fErrorCount <> 1) then
    PluralE := 's'
  else
    PluralE := '';
  if (fWarningCount <> 1) then
    PluralW := 's'
  else
    PluralW := '';
  Result := Format('Assembly generated %d error%s and %d warning%s', [fErrorCount, PluralE, fWarningCount, PluralW]);
end;


end.

