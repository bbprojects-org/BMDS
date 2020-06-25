{ ==============================================================================

  ADDRESS DEFINITIONS

    Build array of common machine addresses / values from external ASM file


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

{ TODO : uAddrDefs -> check whether read/write }

unit uAddrDefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //
  uParser, uCpuTypes;

type
  TAddrDefsProcessor = class
  private
    AddrDefsLineNo: integer;
    AddrDefsLines: TStrings;
    procedure GetSourceLine(Sender: TObject; var Line: string; var IsEof: boolean);
  public
    function GetAddrDefs(FileName: string): TAddrDefsArray;
  end;

const
  ERROR_FILE_NOT_FOUND = 'File [%s] not found';


implementation

{ TAddrDefsProcessor }

{ Given filename of Asm file to process, returns an array of addresses/symbols }

function TAddrDefsProcessor.GetAddrDefs(FileName: string): TAddrDefsArray;
var
  Parser: TParser;
  State: (stBOL, stInstruction, stEmptyLine, stEOL);
  NumAddr: integer;
  Instruction: string;
  PC: word;
  //chRW: char;

  function GetNumber: integer;          // Minimal expression handler, ignore
  begin                                 // errors or anything not a number
    Result := 0;
    Parser.GetToken;
    if (Parser.Token.Typ = tkNumber) then
      Result := Parser.Token.NumberVal;
  end;

begin
  Parser := TParser.Create(@GetSourceLine);
  AddrDefsLines := TStringList.Create;
  if (not FileExists(FileName)) then
    raise Exception.CreateFmt(ERROR_FILE_NOT_FOUND, [FileName]);

  AddrDefsLines.LoadFromFile(FileName); // FileName from current machine

  AddrDefsLineNo := -1;
  NumAddr := 0;
  Parser.Initialise;
  State := stBOL;                     // Beginning Of Line ...
  while (Parser.Token.Typ <> tkEOF) do
  try
    case State of

      stBOL: begin
               // Expecting label / comment / EOL / whitespace & instruction identifier
               Parser.GetToken;
               case Parser.Token.Typ of
                 tkLabel:   begin
                              Inc(NumAddr);
                              SetLength(Result, NumAddr);
                              Result[NumAddr-1].LabelStr := Parser.Token.StringVal;
                              Result[NumAddr-1].Used := False;
                              Parser.GetToken;
                              State := stInstruction;
                            end;
                 tkComment: begin
                              Parser.GetToken;    // Skip comment lines
                              State := stEOL;
                            end;
                 tkEOL:     State := stEmptyLine; // Blank line
               else
                 State := stInstruction;
               end;
             end;

    stInstruction:
             begin
               if (Parser.Token.Typ = tkId) then
                 begin
                   Instruction := LowerCase(Parser.Token.StringVal);
                   if (Instruction = 'org') then
                     begin
                       PC := GetNumber;
                     end
                   else if ((Instruction = 'rmb') or (Instruction = 'ds') or (Instruction = 'defs')) then
                     begin
                       Result[NumAddr-1].Address := PC;
                       Inc(PC, GetNumber);
                       Result[NumAddr-1].RW := ' '; // Read/Write allowed
                     end
                   else if (Instruction = 'equ') then
                     begin
                       Result[NumAddr-1].Address := GetNumber;
                       Result[NumAddr-1].RW := ' '; // Read/Write allowed
                     end
                   else
                     Parser.SkipRestOfLine; // Ignore any other mnemonic
                 end
               else
                 Parser.SkipRestOfLine;     // Ignore anything else
               State := stEOL;
             end;

    stEmptyLine:
             begin
               State := stBOL;          // Skip empty lines
             end;

    stEOL:   begin
               if (Parser.PeekNextToken.Typ <> tkEOF) then
                 begin
                   Parser.GetToken;
                   if (Parser.Token.Typ <> tkEOL) then
                     begin
                       Parser.SkipRestOfLine;
                       Parser.GetToken; // Skip over EOL
                     end;
                 end;
               State := stBOL;          // Back to beginning of line
             end;
    end;

  except
    on E: Exception do {AddError(E.Message)};
  end;

  AddrDefsLines.Free;
  Parser.Free;
end;


{ GET SOURCE LINE }

{ Return next line in file until all lines processed }

procedure TAddrDefsProcessor.GetSourceLine(Sender: TObject;
            var Line: string;           // Return value for the next line
            var IsEof: boolean);        // Return value indicating when EOF
begin
  Inc(AddrDefsLineNo);
  if (AddrDefsLineNo = AddrDefsLines.Count) then
    begin
      Line := '';
      IsEof := True;
    end
  else
    begin
      Line := AddrDefsLines.Strings[AddrDefsLineNo];
      IsEof := False;
    end;
end;


end.

