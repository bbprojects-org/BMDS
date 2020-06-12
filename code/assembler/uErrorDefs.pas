{ ==============================================================================

  ERROR TEXTS FOR ASSEMBLER

    This unit defines the error messages, and each has a flag to indicate
    whether that error has been used already.

    User 'Error(em)' to get the text for a particular ErrorMessage value.
    This can have an optional second parameter (default False) to set
    the 'Used' flag for this error at the same time. In addition can use
    'SetErrorUsed' for any error.

    At each error call the assembler can check the 'IsLastErrorRepeated' flag
    to see if it has happened before and act accordingly, i.e. do nothing if
    do not want to repeat the error message.


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

unit uErrorDefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TErrorMessage = (emStart, emAddrModeNotRecognised, emBranchTooFar,
                            emIncludeNotFound, emElseWithoutIf,
                            emEndifWithoutIf, emExpectedNotFound,
                            emInstrNotRecognised, emInstrExpected,
                            emLabelMissing, emMemModeNotRecognised,
                            emOperandNotFound, emPhasingError,
                            emSymbolNotDefined, emMacroNameMissing,
                            emMacroBadInstr, emPass2Aborted,
                            // uParser
                            emParUnterminatedString, emParIllegalNumber,
                            // uReadWriteHex
                            emRwhNotSupported, emRwhNotValid,
                            emRwhInvalidHex, emRwhMissingColon,
                   emEnd);

  TError = record
    Msg: string;
    NoRepeat: boolean;
  end;
  TErrors = array[emStart..emEnd] of TError;

procedure ErrorsReset;
procedure SetNoRepeatError(em: TErrorMessage);
function  Error(em: TErrorMessage; SetNoRepeat: boolean = False): string;
function  IsNoRepeatError: boolean;


implementation

var
  Errors: TErrors;
  NoRepeatError: boolean;

procedure Init;
begin
  Errors[emAddrModeNotRecognised].Msg := 'Address mode [%s] not recognised';
  Errors[emBranchTooFar].Msg          := 'Branch too far';
  Errors[emIncludeNotFound].Msg       := 'Cannot find include file [%s]';
  Errors[emElseWithoutIf].Msg         := 'ELSE without starting IF';
  Errors[emEndifWithoutIf].Msg        := 'ENDIF without starting IF';
  Errors[emExpectedNotFound].Msg      := '[%s] expected but [%s] found';
  Errors[emInstrNotRecognised].Msg    := 'Instruction [%s] not recognised';
  Errors[emInstrExpected].Msg         := 'Instruction expected, got [%s]';
  Errors[emLabelMissing].Msg          := 'Label is required for this instruction';
  Errors[emMemModeNotRecognised].Msg  := 'Memory mode [%s] not recognised';
  Errors[emOperandNotFound].Msg       := 'Operand expected, not found';
  Errors[emPhasingError].Msg          := 'Symbol values differ between passes ($%.4x,$%.4x), check addressing modes above';
  Errors[emSymbolNotDefined].Msg      := 'Symbol [%s] not defined';
  Errors[emMacroNameMissing].Msg      := 'Macro name missing';
  Errors[emMacroBadInstr].Msg         := 'This instruction not permitted in a macro definition';

  Errors[emPass2Aborted].Msg          := 'Second pass aborted due to %d %s in first pass';

  Errors[emParUnterminatedString].Msg := 'Unterminated String';
  Errors[emParIllegalNumber].Msg      := 'Illegal number format';

  Errors[emRwhNotSupported].Msg       := 'Record type %d not supported';
  Errors[emRwhNotValid].Msg           := 'Record type %d not valid';
  Errors[emRwhInvalidHex].Msg         := '[%s] is not a valid hex number';
  Errors[emRwhMissingColon].Msg       := 'Missing colon at start of line';

  ErrorsReset;
end;


procedure ErrorsReset;
var
  i: TErrorMessage;
begin
  for i := emStart to emEnd do
    Errors[i].NoRepeat := False;
end;


procedure SetNoRepeatError(em: TErrorMessage);
begin
  Errors[em].NoRepeat := True;
end;


function  Error(em: TErrorMessage; SetNoRepeat: boolean): string;
begin
  NoRepeatError := Errors[em].NoRepeat; // Check NoRepeat first
  Result := Errors[em].Msg;
  if (SetNoRepeat) then                 // Set 'NoRepeat' if parameter True
    SetNoRepeatError(em);
end;


function IsNoRepeatError: boolean;
begin
  Result := NoRepeatError;
end;


initialization
  Init;


end.

