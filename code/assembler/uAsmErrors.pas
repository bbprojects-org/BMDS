{ ==============================================================================

  ERROR TEXTS FOR ASSEMBLER

    This unit defines the error messages, and each has a flag to indicate
    whether that error has been used already.

    Use 'AddError(em)' to add a new error for a particular ErrorMessage value.
    This can have an optional second parameter (default False) to set
    the 'Used' flag for this error at the same time. In addition can use
    'SetNoRepeat' for any error.

    After each error is processed, a callback OnError is generated with the
    error message (empty string if NoRepeat is set) as parameter.


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
  Classes, SysUtils,
  //
  uCommon;

type
  TErrorMessage = (emStart, emAddrModeNotRecognised, emBranchTooFar,
                            emIncludeNotFound, emElseWithoutIf,
                            emEndifWithoutIf, emExpectedNotFound,
                            emInstrNotRecognised, emInstrExpected,
                            emLabelMissing, emMemModeNotRecognised,
                            emOperandNotFound, emPhasingError,
                            emSymbolNotDefined, emMacroNameMissing,
                            emMacroBadInstr, emStringTooLong,
                   emEnd);

  TErrorDef = record
    Msg: string;
    NoRepeat: boolean;
  end;
  TErrorDefs = array[emStart..emEnd] of TErrorDef;

  TOnErrorEvent = procedure (Sender: TObject; ErrMsg: string) of object;

  { TErrors }

  TErrors = class(TObject)
  private
    fErrors: array of string;           // Holds list of errors for summary
    fErrorCount: integer;
    fWarningCount: integer;
    fOnError: TOnErrorEvent;
    fCurrentFileName: string;
    fCurrentLineNo: integer;
    ErrorDefs: TErrorDefs;
    //
    procedure AddToErrorsList(msg: string);
    function GetErrorList: string;
  public
    constructor Create;
    destructor  Destroy; override;
    //
    procedure SetLine(CurrentFileName: string; CurrentLineNo: integer);
    procedure SetNoRepeat(em: TErrorMessage);
    procedure AddError(em: TErrorMessage; NoRepeatFlag: boolean = False);
    procedure AddErrorFmt(em: TErrorMessage; const Args: array of const; NoRepeatFlag: boolean = False);
    procedure AddError(msg: string);
    procedure AddWarning(msg: string);
    //
    property ErrorCount: integer read fErrorCount;
    property WarningCount: integer read fWarningCount;
    property ErrorList: string read GetErrorList;
    property OnError: TOnErrorEvent read fOnError write fOnError;
  end;


implementation

{ CREATE }

constructor TErrors.Create;
var
  i: TErrorMessage;
begin
  SetLength(fErrors, 0);                // Ensure error list is empty
  fErrorCount := 0;
  fWarningCount := 0;
  fCurrentFileName := '';
  fCurrentLineNo := 0;

  ErrorDefs[emAddrModeNotRecognised].Msg := 'Address mode [%s] not recognised';
  ErrorDefs[emBranchTooFar].Msg          := 'Branch too far';
  ErrorDefs[emIncludeNotFound].Msg       := 'Cannot find include file [%s]';
  ErrorDefs[emElseWithoutIf].Msg         := 'ELSE without starting IF';
  ErrorDefs[emEndifWithoutIf].Msg        := 'ENDIF without starting IF';
  ErrorDefs[emExpectedNotFound].Msg      := '[%s] expected but [%s] found';
  ErrorDefs[emInstrNotRecognised].Msg    := 'Instruction [%s] not recognised';
  ErrorDefs[emInstrExpected].Msg         := 'Instruction expected, got [%s]';
  ErrorDefs[emLabelMissing].Msg          := 'Label is required for this instruction';
  ErrorDefs[emMemModeNotRecognised].Msg  := 'Memory mode [%s] not recognised';
  ErrorDefs[emOperandNotFound].Msg       := 'Operand expected, not found';
  ErrorDefs[emPhasingError].Msg          := 'Symbol values differ between passes ($%.4x,$%.4x), check addressing modes above';
  ErrorDefs[emSymbolNotDefined].Msg      := 'Symbol [%s] not defined';
  ErrorDefs[emMacroNameMissing].Msg      := 'Macro name missing';
  ErrorDefs[emMacroBadInstr].Msg         := 'This instruction not permitted in a macro definition';
  ErrorDefs[emStringTooLong].Msg         := 'String ''%s'' too long, single character expected';

  for i := emStart to emEnd do
    ErrorDefs[i].NoRepeat := False;
end;


{ DESTROY }

destructor TErrors.Destroy;
begin
  SetLength(fErrors, 0);
  fErrors := nil;
  inherited;
end;


{ ADD ERROR }

procedure TErrors.AddError(em: TErrorMessage; NoRepeatFlag: boolean);
begin
  if (ErrorDefs[em].NoRepeat) then      // If no repeat error, set empty msg
    AddError('')
  else
    AddError(ErrorDefs[em].Msg);

  if (NoRepeatFlag) then                // Set 'NoRepeat' if parameter True
    SetNoRepeat(em);
end;


procedure TErrors.AddErrorFmt(em: TErrorMessage; const Args: array of const; NoRepeatFlag: boolean);
begin
  if (ErrorDefs[em].NoRepeat) then      // If no repeat error, set empty msg
    AddError('')
  else
    AddError(Format(ErrorDefs[em].Msg, Args));

  if (NoRepeatFlag) then                // Set 'NoRepeat' if parameter True
    SetNoRepeat(em);
end;


procedure TErrors.AddError(msg: string);
var
  ErrorMsg: string;
begin
  ErrorMsg := msg;
  if (msg <> '') then
    begin
      Inc(fErrorCount);
      ErrorMsg := Format('%s(%.3d) Error: %s', [fCurrentFilename, fCurrentLineNo, msg]);
      AddToErrorsList(ErrorMsg);
    end;

  if Assigned(fOnError) then
    fOnError(self, ErrorMsg);           // Allow caller to add to listing, call SkipRestOfLine...
end;


{ ADD WARNING }

procedure TErrors.AddWarning(msg: string);
var
  ErrorMsg: string;
begin
  ErrorMsg := msg;
  if (msg <> '') then
    begin
      Inc(fWarningCount);
      ErrorMsg := Format('%s(%.3d) Warning: %s', [fCurrentFilename, fCurrentLineNo, msg]);
      AddToErrorsList(ErrorMsg);
    end;

  if Assigned(fOnError) then
    fOnError(self, ErrorMsg);           // Allow caller to add to listing, call SkipRestOfLine...
end;


procedure TErrors.AddToErrorsList(msg: string);
var
  Len: integer;
begin
  Len := Length(fErrors);
  SetLength(fErrors, Len + 1);
  fErrors[Len] := msg;
end;


{ GET / SET ROUTINES }

function TErrors.GetErrorList: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to (Length(fErrors) - 1) do
    Result := Result + fErrors[i] + CRLF;
end;


procedure TErrors.SetLine(CurrentFileName: string; CurrentLineNo: integer);
begin
  fCurrentFileName := CurrentFileName;
  fCurrentLineNo := CurrentLineNo + 1;  // Line numbers are zero based, so add 1
end;


procedure TErrors.SetNoRepeat(em: TErrorMessage);
begin
  ErrorDefs[em].NoRepeat := True;
end;


end.

