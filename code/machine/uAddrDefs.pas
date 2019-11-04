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
  uParser, uCommon;

type
  TAddrDefsProcessor = class
  private
    ADA: TAddrDefsArray;
    AddrDefsLineNo: integer;
    AddrDefsLines: TStrings;
    procedure GetSourceLine(Sender: TObject; var Line: string; var IsEof: boolean);
  public
    function GetAddrDefs(Filename: string): TAddrDefsArray;
  end;


implementation

{ TAddrDefsProcessor }

function TAddrDefsProcessor.GetAddrDefs(
           Filename: string):           // Filename of ASM file to process
           TAddrDefsArray;              // Returns: array of addresses/symbols
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
  AddrDefsLines.LoadFromFile(Filename); // Filename from current machine

  with Parser do
  begin
    AddrDefsLineNo := -1;
    NumAddr := 0;
    Parser.Initialise;
    State := stBOL;                     // Beginning Of Line ...
    while (Token.Typ <> tkEOF) do
    try
      case State of

        stBOL: begin
                 // Expecting label / comment / whitespace & instruction / EOL
                 GetToken;
                 case Token.Typ of
                   tkLabel:   begin
                                Inc(NumAddr);
                                SetLength(ADA, NumAddr);
                                ADA[NumAddr-1].LabelStr := Token.StringVal;
                                ADA[NumAddr-1].Used := False;
                                GetToken;
                                State := stInstruction;
                              end;
                   tkComment: begin
                                State := stEOL;   // Ignore comment lines
                              end;
                   tkEOL:     begin
                                State := stEmptyLine; // Blank line
                              end;
                 else
                   begin
                     State := stInstruction;
                   end;
                 end;
               end;

      stInstruction:
               begin
                 if (Token.Typ = tkId) then
                   begin
                     Instruction := LowerCase(Token.StringVal);
                     if (Instruction = 'org') then
                       begin
                         PC := GetNumber;
                       end
                     else if ((Instruction = 'rmb') or (Instruction = 'ds') or (Instruction = 'defs')) then
                       begin
                         ADA[NumAddr-1].Address := PC;
                         Inc(PC, GetNumber);
                         ADA[NumAddr-1].RW := ' '; // Read/Write allowed
                       end
                     else if (Instruction = 'equ') then
                       begin
                         ADA[NumAddr-1].Address := GetNumber;
                         ADA[NumAddr-1].RW := ' '; // Read/Write allowed
                       end
                     else
                       SkipRestOfLine;  // Ignore anything else
                   end
                 else
                   SkipRestOfLine;
                 State := stEOL;
               end;

      stEmptyLine:
               begin
                 State := stBOL;
               end;

      stEOL:   begin
                 if (PeekNextToken.Typ = tkComment) then
                   GetToken;            // Skip comment
                 if (PeekNextToken.Typ <> tkEOF) then
                   begin
                     GetToken;
                     if (Token.Typ <> tkEOL) then
                       begin
                         SkipRestOfLine;
                         GetToken;      // Skip over EOL
                       end;
                   end;
                 State := stBOL;        // Back to beginning of line
               end;
      end;

    except
      on E: Exception do {AddError(E.Message)};
    end;

  end;
  Parser.Free;
  Result := ADA;
end;


{ GET SOURCE LINE }

{ Return next line in ASM file until all lines processed }

procedure TAddrDefsProcessor.GetSourceLine(Sender: TObject;
            var Line: string;           // Return value for the next line
            var IsEof: boolean);        // Return value indicating when EOF
begin
  Inc(AddrDefsLineNo);
  if (AddrDefsLineNo = AddrDefsLines.Count) then
    IsEof := True
  else
    begin
      Line := AddrDefsLines.Strings[AddrDefsLineNo];
      IsEof := False;
    end;
end;


end.

