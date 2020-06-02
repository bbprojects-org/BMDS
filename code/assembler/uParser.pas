{ ==============================================================================

  PARSER

    This class provides the parser for the assembler source file, returning
    tokens as they are found. Additional methods provide the capability to
    support #include files, redirecting the source to another external file.
    A lookahead facility is provided via the PeekNextToken method


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

unit uParser;

{$mode objfpc}{$H+}
{$R-}
{.$define parser_debug}

interface

uses Classes, SysUtils, StrUtils;

type
  // EParserError  - defined in classesh.inc

  TTokenType = (tkNil,                  // non token identifier

                tkLabel,                // global label
                tkLocal,                // local label (preceded by '.')
                tkId,                   // identifier
                tkDotId,                // .identifier (preceded by '.')
 	        tkNumber,               // number of hex, binary, octal or decimal
 	        tkString,               // quoted string
                tkComment,              // any valid comment form
                tkHashDirective,        // assembler # directive (eg #endif)
                tkDirective,            // assembler . directive (eg .ds)

                tkHash,                 // #
                tkDollar,               // $
                tkLeftParen,            // (
                tkRightParen,           // )
                tkStar,                 // *
                tkPlus,                 // +
                tkComma,                // ,
                tkColon,                // :
                tkMinus,                // -
                tkSlash,                // /
                tkLower,                // <
                tkGreater,              // >
                tkEqual,                // =
                tkLowerEqual,           // <=
                tkNotEqual,             // <>
                tkGreaterEqual,         // >=

                tkEOL,                  // End Of Line
  	        tkEOF,                  // End Of File, no more source lines
                
 	        tkInvalid);             // anything not covered above !

  TTokenTypes = set of TTokenType;

  TToken = record
    Typ: TTokenType;
    StartPos: integer;
    EndPos: integer;
    NumberVal: integer;
    StringVal: string;
  end;

  TOnGetLineEvent = procedure(Sender: TObject; var Line: string; var IsEof: boolean) of object;

  { TParser }

  TParser = class
  private
    fOnGetLine: TOnGetLineEvent;        // To allow parser to request next line
    fLine: string;                      // Current line being processed
    CharPtr: integer;                   // Current character position in line
    Marker: integer;
    LineLen: integer;                   // Length of current line
    FirstCharIsSet: boolean;            // If first character set, should be a label
    Eof: boolean;
    fToken: TToken;                     // Current token parsed
    NextToken: TToken;                  // Look ahead for next token (not consumed)
    Identifiers: array[#0..#255] of Boolean;
    ProcTable: array[#0..#255] of procedure of object;
    procedure MakeIdentTable;
    procedure MakeProcTable;

    procedure IdentProc;
    procedure NumberProc;
    procedure StringProc;
    procedure HashProc;
    procedure DollarProc;
    procedure BinaryProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure StarProc;
    procedure PlusProc;
    procedure CommaProc;
    procedure MinusProc;
    procedure PointProc;
    procedure SlashProc;
    procedure ColonProc;
    procedure SemiColonProc;
    procedure LowerProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure OctalProc;
    procedure InvalidProc;

    procedure GetLine;
    procedure GetNumber(Radix: integer; Skip2Char: boolean = False; SkipLastChar: boolean = False);
    procedure GetId(tok: TTokenType);
    {$ifdef parser_debug}
    function  ListToken: string;
    {$endif}
  public
    constructor Create(OnGetLine: TOnGetLineEvent); // Caller must pass function
                                                    // for getting source line
    destructor  Destroy; override;
    function  GetToken: TToken;
    function  PeekNextToken: TToken;
    procedure Initialise;               // Reinitialise parser
    procedure SkipRestOfLine;
    procedure MarkPosition; 
    function  GetLineText(aFrom, aTo: integer): string;
    property  Token: TToken read fToken;
    property  SourceLine: string read fLine;
    property  OnGetLine: TOnGetLineEvent read fOnGetLine write fOnGetLine;
  end;


implementation


{ CREATE / DESTROY }

constructor TParser.Create(OnGetLine: TOnGetLineEvent);
begin
  inherited Create;
  fOnGetLine := OnGetLine;
  MakeIdentTable;
  MakeProcTable;
  Initialise;
end;


destructor TParser.Destroy;
begin
  inherited Destroy;
end;


{ INITIALISE }

procedure TParser.Initialise;           // First token assumed End Of Line
begin
  fToken.Typ := tkEOL;
end;


{ GET NEXT LINE }

procedure TParser.GetLine;
begin
  NextToken.Typ := tkNil;
  Eof := False;
  OnGetLine(self, fLine, Eof);          // Get line from Parent, defined in Create
  LineLen := Length(fLine);
  CharPtr := 1;
  {$ifdef parser_debug}
  AppLog.Debug('TParser.GetLine, line="' + fLine + '"');
  {$endif}
end;


{ GET NEXT TOKEN FROM THE LINE }

function TParser.GetToken: TToken;
begin
  if (fToken.Typ = tkEOL) then
    GetLine;
  if (not Eof) then
    begin
      if ( (LineLen > 0) and (CharPtr = 1) and (fLine[1] in [#33..#255]) ) then
        FirstCharIsSet := True
      else
        FirstCharIsSet := False;
      fToken := PeekNextToken;
      NextToken.Typ := tkNil;           // Flag next token as not yet used
      Result := fToken;
    end
  else
    begin
      fToken.Typ := tkEOF;              // No more source lines to process
      NextToken.Typ := tkEOF;
      fLine := '';                      // No source line applies
    end;
  {$ifdef parser_debug}
  AppLog.Debug('TParser.GetToken, tok=' + ListToken);
  {$endif}
end;


{ PEEK AT THE NEXT TOKEN }

function TParser.PeekNextToken: TToken;
begin
  if (NextToken.Typ = tkNil) then       // Get new token only if lookahead not used
    begin
      while (CharPtr <= LineLen) and (fLine[CharPtr] in [#1..#12,#14..#32]) do
        Inc(CharPtr);                   // Skip whitespace
      NextToken.NumberVal := 0;
      NextToken.StartPos := CharPtr;
      if (CharPtr > LineLen) then
        begin
          NextToken.StringVal := '';
          NextToken.Typ := tkEOL;       // No more characters to process
        end
      else
        begin
          NextToken.StringVal := fLine[CharPtr]; // Save single character
          ProcTable[fLine[CharPtr]];    // Jump to character routine
        end;
      NextToken.EndPos := CharPtr;
    end;
  Result := NextToken;
end;


{ BUILD JUMP TABLES FOR CHARACTERS AND PROCEDURES }

procedure TParser.MakeIdentTable;
var
  c: Char;
begin
  for c := #0 to #255 do
  begin
    case c of
      '.', '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[c] := True;
    else
      Identifiers[c] := False;
    end;
  end;
end;


procedure TParser.MakeProcTable;
var
  c: Char;
begin
  for c := #0 to #255 do
    case c of
      'A'..'Z', 'a'..'z', '_': ProcTable[c] := @IdentProc;
      '0'..'9': ProcTable[c] := @NumberProc;
      '''', '"': ProcTable[c] := @StringProc;
      '#': ProcTable[c] := @HashProc;
      '$': ProcTable[c] := @DollarProc;
      '%': ProcTable[c] := @BinaryProc;
      '(': ProcTable[c] := @RoundOpenProc;
      ')': ProcTable[c] := @RoundCloseProc;
      '*': ProcTable[c] := @StarProc;
      '+': ProcTable[c] := @PlusProc;
      ',': ProcTable[c] := @CommaProc;
      '-': ProcTable[c] := @MinusProc;
      '.': ProcTable[c] := @PointProc;
      '/': ProcTable[c] := @SlashProc;
      ':': ProcTable[c] := @ColonProc;
      ';': ProcTable[c] := @SemiColonProc;
      '<': ProcTable[c] := @LowerProc;
      '=': ProcTable[c] := @EqualProc;
      '>': ProcTable[c] := @GreaterProc;
      '@': ProcTable[c] := @OctalProc;
    else
      ProcTable[c] := @InvalidProc;
    end;
end;


{ CHARACTER JUMP PROCEDURES }

procedure TParser.IdentProc;
begin
  if (FirstCharIsSet) then              // If first char in line, must be a label
    GetId(tkLabel)
  else
    GetId(tkId);                        // ... otherwise it's an identifier
end;


{ Look for all possible combinations of numbers using Intel / C-Style or
  straight decimal formats. Motorola style numbers with prefix character are
  handled by own procedures below.
  This code based on principle in SBASM (https://www.sbprojects.net/sbasm/) }

procedure TParser.NumberProc;
var
  tmpLine, NumStr: string;
  Start, tmpCP: integer;

  function TestHex: boolean;
  var i: integer;
  begin
    Result := False;
    for i := 1 to Length(NumStr) do
      if not(NumStr[i] in ['0'..'9', 'A'..'F'])
        then Exit;
    Result := True;
  end;

  function TestOct: boolean;
  var i: integer;
  begin
    Result := False;
    for i := 1 to Length(NumStr) do
      if not(NumStr[i] in ['0'..'7'])
        then Exit;
    Result := True;
  end;

  function TestBin: boolean;
  var i: integer;
  begin
    Result := False;
    for i := 1 to Length(NumStr) do
      if not(NumStr[i] in ['0', '1'])
        then Exit;
    Result := True;
  end;

  function TestDec: boolean;
  var i: integer;
  begin
    Result := False;
    for i := 1 to Length(NumStr) do
      if not(NumStr[i] in ['0'..'9'])
        then Exit;
    Result := True;
  end;

begin
  Start := CharPtr;
  tmpCP := CharPtr;                     // Temporary CharPtr and line
  tmpLine := UpperCase(fLine);
  repeat
    Inc(tmpCP);                         // Get all possible dec/bin/hex/oct characters
  until ((tmpCP > LineLen) or not(tmpLine[tmpCP] in ['0'..'9', 'A'..'F', 'H', 'Q', 'X']));
  NumStr := MidStr(tmpLine, Start, tmpCP-Start);

  if (Length(NumStr) = 1) then          // Must be decimal, just get it
    GetNumber(10)

  // Check for Intel 0nnnnH format, hexadecimal
  else if (tmpLine[tmpCP-1] = 'H') then
    begin
      NumStr := LeftStr(NumStr, Length(NumStr)-1);
      if (TestHex) then
        GetNumber(16, False, True);
    end

  // Check for Intel 0nnnnQ format, octal
  else if (tmpLine[tmpCP-1] = 'Q') then
    begin
      NumStr := LeftStr(NumStr, Length(NumStr)-1);
      if (TestOct) then
        GetNumber(8, False, True);
    end

  // Check for C-style 0xnnnn format, hexadecimal
  else if ((NumStr[1] = '0') and (NumStr[2] = 'X')) then
    begin
      NumStr := RightStr(NumStr, Length(NumStr)-2);
      if (TestHex) then
        GetNumber(16, True, False);
    end

  // Check for C-style 0bnnnn format, binary
  else if ((NumStr[1] = '0') and (NumStr[2] = 'B')) then
    begin
      NumStr := RightStr(NumStr, Length(NumStr)-2);
      if (TestBin) then
        GetNumber(2, True, False);
    end

  // Check for Intel 0nnnnB format, binary
  else if (tmpLine[tmpCP-1] = 'B') then
    begin
      NumStr := LeftStr(NumStr, Length(NumStr)-1);
      if (TestBin) then
        GetNumber(2, False, True);
    end

  // Check for Intel 0nnnnD format, decimal
  else if (tmpLine[tmpCP-1] = 'D') then
    begin
      NumStr := LeftStr(NumStr, Length(NumStr)-1);
      if (TestDec) then
        GetNumber(10, False, True);
    end

  // Can only be decimal now
  else if (TestDec) then
    GetNumber(10)

  else
    begin
      CharPtr := tmpCP;                 // Skip over error text
      raise EParserError.Create('Illegal number format');
    end;
end;


procedure TParser.StringProc;
var
  Delimiter: char;
  Start: integer;
begin
  Delimiter := fLine[CharPtr];          // Track which delimiter used
  Start := CharPtr;
  repeat
    Inc(CharPtr)                        // ... and look for it to terminate string
  until (CharPtr > LineLen) or (fLine[CharPtr] = Delimiter);
  if (CharPtr > LineLen) then
    raise EParserError.Create('Unterminated String');
  Inc(CharPtr);                         // Point past delimiter
  NextToken.Typ := tkString;
  NextToken.StringVal := MidStr(fLine, Start+1, CharPtr-Start-2);
end;


procedure TParser.HashProc;
begin
  Inc(CharPtr);
  if (FirstCharIsSet and (fLine[CharPtr] in ['A'..'Z', 'a'..'z'])) then
    GetId(tkHashDirective)
  else
    NextToken.Typ := tkHash;
end;


procedure TParser.DollarProc;
begin
  Inc(CharPtr);
  if (fLine[CharPtr] in [#1..#32]) then
    NextToken.Typ := tkDollar           // Used as location counter
  else
    GetNumber(16);                      // else hexadecimal value
end;


procedure TParser.BinaryProc;
begin
  Inc(CharPtr);
  if (fLine[CharPtr] in [#1..#32]) then
    NextToken.Typ := tkInvalid
  else
    GetNumber(2);                       // Binary value
end;


procedure TParser.RoundOpenProc;
begin
  Inc(CharPtr);
  NextToken.Typ := tkLeftParen;
end;


procedure TParser.RoundCloseProc;
begin
  Inc(CharPtr);
  NextToken.Typ := tkRightParen;
end;


procedure TParser.StarProc;
begin
  Inc(CharPtr);
  if (fLine[CharPtr] in [#1..#32]) then
    NextToken.Typ := tkDollar           // Treat '*' as location counter
  else
    NextToken.Typ := tkStar;
end;


procedure TParser.PlusProc;
begin
  Inc(CharPtr);
  NextToken.Typ := tkPlus;
end;


procedure TParser.CommaProc;
begin
  Inc(CharPtr);
  NextToken.Typ := tkComma;
end;


procedure TParser.MinusProc;
begin
  Inc(CharPtr);
  NextToken.Typ := tkMinus;
end;


procedure TParser.PointProc;
begin
  if (fLine[CharPtr + 1] in ['A'..'Z', 'a'..'z', '_']) then
    begin
      if (CharPtr = 1) then
        GetId(tkLocal)                  // Local label
      else
        GetId(tkDotId);                 // .Directive or .Identifier
    end
  else
    InvalidProc;
end;


procedure TParser.SlashProc;
begin
  if (fLine[CharPtr+1] = '/') then      // Check if next char = '/', i.e. '//'
    SemiColonProc                       // ... if so, treat as a ';' comment
  else
    begin
      Inc(CharPtr);
      NextToken.Typ := tkSlash;
    end;
end;


procedure TParser.ColonProc;
begin
  Inc(CharPtr);
  NextToken.Typ := tkColon;
end;


procedure TParser.SemiColonProc;        // Handle comment
var
  Start: integer;
begin
  Start := CharPtr;
  while (CharPtr <= LineLen) do
    Inc(CharPtr);                       // Skip to end of line
  NextToken.Typ := tkComment;
  NextToken.StringVal := MidStr(fLine, Start, CharPtr-Start);
end;


procedure TParser.LowerProc;
begin
  Inc(CharPtr);
  case fLine[CharPtr] of                // Check for combined op '<=' or '<>'
    '=': begin
           Inc(CharPtr);
           NextToken.Typ := tkLowerEqual;
         end;
    '>': begin
           Inc(CharPtr);
           NextToken.Typ := tkNotEqual;
         end;
  else
    NextToken.Typ := tkLower;
  end;
end;


procedure TParser.EqualProc;
begin
  Inc(CharPtr);
  NextToken.Typ := tkEqual;
end;


procedure TParser.GreaterProc;
begin
  Inc(CharPtr);
  if (fLine[CharPtr] = '=') then        // Check for combined op '>='
    begin
      Inc(CharPtr);
      NextToken.Typ := tkGreaterEqual;
    end
  else
    NextToken.Typ := tkGreater;
end;


procedure TParser.OctalProc;
begin
  Inc(CharPtr);
  if (fLine[CharPtr] in [#1..#32]) then
    NextToken.Typ := tkInvalid
  else
    GetNumber(8);                       // Octal number
end;


procedure TParser.InvalidProc;
begin
  NextToken.Typ := tkInvalid;           // Invalid character
  Inc(CharPtr);                         // ... skip it
end;


{ SUPPORT ELEMENTS }


{ GET NUMBER - optional params used to skip C-style characters at start, or
               Intel style character at end }

procedure TParser.GetNumber(Radix: integer; Skip2Char: boolean = False; SkipLastChar: boolean = False);
var
  nDigit, nValue: integer;
  bDone: boolean;
  ch: char;
begin
  if (Skip2Char) then
    Inc(CharPtr, 2);                    // Skip over 0x,0b,etc of Intel format
  nValue := 0;
  bDone := False;
  repeat
    ch := fLine[CharPtr];
    if ((Radix = 2) and (ch = '.')) then
      begin
        Inc(CharPtr);                   // Skip optional 'dots' in binary
        ch := fLine[CharPtr];
      end;
    if (ch in ['0'..'9']) then
      nDigit := ord(ch) - ord('0')
    else if (ch in ['A'..'Z']) then
      nDigit := ord(ch)-ord('A')+10
    else if (ch in ['a'..'z']) then
      nDigit := ord(ch)-ord('a')+10
    else nDigit := Radix;               // Force exit

    if (nDigit < Radix) then
      begin
        nValue := nValue * Radix + nDigit;
        Inc(CharPtr);
      end
    else
      bDone := True;
  until bDone or (CharPtr > LineLen);
  if (SkipLastChar) then
    Inc(CharPtr);                       // Skip over B,D,H,Q of Intel format
  NextToken.NumberVal := nValue;        // Assign value to token
  NextToken.Typ := tkNumber;
end;


procedure TParser.GetId(tok: TTokenType);
var
  nStart: integer;
begin
  nStart := CharPtr;
  Inc(CharPtr);
  while Identifiers[fLine[CharPtr]] do  // Check char against Identifiers array
    Inc(CharPtr);
  NextToken.Typ := tok;
  NextToken.StringVal := MidStr(fLine, nStart, CharPtr-nStart);
  if ((tok = tkLabel) and (fLine[CharPtr] = ':')) then
    Inc(CharPtr);                       // Skip colon if provided after label
  FirstCharIsSet := False;              // Got label, clear this
end;


procedure TParser.SkipRestOfLine;
begin
  while not (PeekNextToken.Typ in [tkEOL,tkEOF]) do
    GetToken;
end;


procedure TParser.MarkPosition;
begin
  Marker := CharPtr;
end;


function TParser.GetLineText(aFrom, aTo: integer): string;
begin
  Result := MidStr(fLine, aFrom, aTo - aFrom);
end;


{$ifdef parser_debug}
function TParser.ListToken: string;
begin
  case fToken.Typ of
    tkNil:           Result := 'tkNIL';
    tkEOL:           Result := 'tkEOL';
    tkLabel:         Result := Format('tkLabel [%s]', [fToken.StringVal]);
    tkId:            Result := Format('tkId [%s]', [fToken.StringVal]);
    tkNumber:        Result := Format('tkNumber [%d]', [fToken.NumberVal]);
    tkString:        Result := Format('tkString "%s"', [fToken.StringVal]);
    tkComment:       Result := Format('tkComment [%s]', [fToken.StringVal]);
    tkHashDirective: Result := Format('tkHashDirective [%s]', [fToken.StringVal]);
    tkDirective:     Result := Format('tkDirective [%s]', [fToken.StringVal]);
    tkEOF:           Result := 'tkEOF';
    tkHash:          Result := 'tkHash';
    tkDollar:        Result := 'tkDollar';
    tkLeftParen:     Result := 'tkLeftParen';
    tkRightParen:    Result := 'tkRightParen';
    tkStar:          Result := 'tkStar';
    tkPlus:          Result := 'tkPlus';
    tkComma:         Result := 'tkComma';
    tkColon:         Result := 'tkColon';
    tkMinus:         Result := 'tkMinus';
    tkSlash:         Result := 'tkSlash';
    tkLower:         Result := 'tkLower';
    tkGreater:       Result := 'tkGreater';
    tkEqual:         Result := 'tkEqual';
    tkLowerEqual:    Result := 'tkLowerEqual';
    tkNotEqual:      Result := 'tkNotEqual';
    tkGreaterEqual:  Result := 'tkGreaterEqual';

  else
    Result := '';
  end;
end;
{$endif}


end.
