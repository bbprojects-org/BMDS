{ ==============================================================================

  Modified version of SynHighlighterAny from SynEdit source

  The property Keywords needs setting externally to highlight them properly

============================================================================== }

unit uHighlighterAsm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditHighlighter, SynEditStrConst;

type
  TtkTokenKind = (tkIdentifier, tkKeyword, tkNumber, tkString, tkOperator,
                  tkSymbol, tkDirective, tkComment, tkSpace, tkNull, tkUnknown);

  TProcTableProc = procedure of object;

  { TSynAsmHighlighter }

  TSynAsmHighlighter = class(TSynCustomHighlighter)
  private
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeywordAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    fOperatorAttri: TSynHighlighterAttributes;

    fProcTable: array[#0..#255] of TProcTableProc;
    fKeyWords: TStringList;
    fLine: PChar;
    fLineNumber: integer;
    fTokenPos: integer;
    fTokenID: TtkTokenKind;
    fStringDelimCh: char;
    Run: LongInt;
    fFileExt: string;
    procedure MakeMethodTables;
    procedure SetKeyWords(const Value: string);
    procedure CommentProc;
    procedure HashProc;
    procedure OperatorProc;
    procedure CRProc;
    procedure IdentProc;
    procedure DollarProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure RoundCloseProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetLanguageName: string; override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetToken: string; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function IsKeyword(const AKeyword: string): boolean; override;
    //
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeywordAttri write fKeywordAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property OperatorAttri: TSynHighlighterAttributes read fOperatorAttri write fOperatorAttri;

    property Keywords: string write SetKeyWords;
    property FileExt: string write fFileExt;
  end;


implementation

var
  Identifiers: array[#0..#255] of ByteBool;

const
  LST_CODE_SECT = 24;


procedure MakeIdentTable;
var
  i: char;
  idents: string;
begin
  idents := '_0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  for i := #0 to #255 do
    Identifiers[i] := (pos(i, idents) > 0);
end;


{ TSynAsmHighlighter }

constructor TSynAsmHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);

  fKeyWords := TStringList.Create;
  fKeyWords.Sorted := True;
  fKeyWords.Duplicates := dupIgnore;

  fIdentifierAttri := TSynHighlighterAttributes.Create('Identifier');
  AddAttribute(fIdentifierAttri);
  fIdentifierAttri.Foreground := clBlue;

  fSpaceAttri := TSynHighlighterAttributes.Create('Space');
  AddAttribute(fSpaceAttri);

  fCommentAttri := TSynHighlighterAttributes.Create('Comment');
  fCommentAttri.Foreground := StringToColor('$2222B2');
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fKeywordAttri := TSynHighlighterAttributes.Create('Keyword');
  AddAttribute(fKeywordAttri);

  fNumberAttri := TSynHighlighterAttributes.Create('Number');
  AddAttribute(fNumberAttri);

  fStringAttri := TSynHighlighterAttributes.Create('String');
  AddAttribute(fStringAttri);
  fStringAttri.Foreground := clBlue;

  fSymbolAttri := TSynHighlighterAttributes.Create('Symbol');
  AddAttribute(fSymbolAttri);
  fSymbolAttri.Foreground := clRed;

  fDirectiveAttri := TSynHighlighterAttributes.Create('Directive');
  AddAttribute(fDirectiveAttri);
  fDirectiveAttri.Foreground := clGreen;
  fDirectiveAttri.Style := [fsBold];

  fOperatorAttri := TSynHighlighterAttributes.Create('Operator');
  AddAttribute(fOperatorAttri);
  fOperatorAttri.Foreground := clRed;

  SetAttributesOnChange(@DefHighlightChange);

  MakeMethodTables;
end;


destructor TSynAsmHighlighter.Destroy;
begin
  fKeyWords.free;
  inherited Destroy;
end;


procedure TSynAsmHighlighter.MakeMethodTables;
var
  i: char;
begin
  for i := #0 to #255 do
    case i of
      'A'..'Z', 'a'..'z', '_': fProcTable[i] := @IdentProc;
      '0'..'9': fProcTable[i] := @NumberProc;
      '$': fProcTable[i] := @DollarProc;
      #34, #39: begin
                  fProcTable[i] := @StringProc;
                  fStringDelimCh := i;
                end;
      '<', '>', '+', '-', '*':
        fProcTable[i]:= @OperatorProc;
      '#': fProcTable[i] := @HashProc;
      ';': fProcTable[i] := @CommentProc;
      '/': fProcTable[i] := @SlashProc;
      #13: fProcTable[i] := @CRProc;
      #10: fProcTable[i] := @LFProc;
      #0:  fProcTable[i] := @NullProc;
      '(': fProcTable[i] := @RoundOpenProc;
      ')': fProcTable[i] := @RoundCloseProc;
      #1..#9, #11, #12, #14..#32: fProcTable[i] := @SpaceProc;
    else
      fProcTable[i] := @UnknownProc;
    end;
end;


procedure TSynAsmHighlighter.IdentProc;
var
  aToken: string;
begin
  while Identifiers[fLine[Run]] do
    inc(Run);
  aToken := GetToken;
  if IsKeyWord(aToken) then
    fTokenId := tkKeyword
  else
    fTokenId := tkIdentifier;
end;


procedure TSynAsmHighlighter.HashProc;
begin
  inc(Run);
  if ((Run = 1) or ((Run = (LST_CODE_SECT+1)) and (fFileExt = '.LST'))) then // First position?
    begin
      while Identifiers[fLine[Run]] do
        inc(Run);
      fTokenId := tkDirective;
    end
  else
    fTokenId := tkUnknown;
end;


procedure TSynAsmHighlighter.DollarProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while fLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    inc(Run);
end;


procedure TSynAsmHighlighter.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;


procedure TSynAsmHighlighter.NullProc;
begin
  fTokenID := tkNull;
end;


procedure TSynAsmHighlighter.NumberProc;
begin
  if (Run = 0) then                     // Cannot be an ASM line, assume LST
    begin
      while ((fLine[Run] <> #0) and (Run < LST_CODE_SECT)) do
        Inc(Run);                       // Skip code text
      if (fLine[Run] = #0) then
        fTokenID := tkNull
      else
        fTokenID := tkKeyword;
    end

  else                                  // Otherwise it is a number!
    begin
      inc(Run);
      fTokenID := tkNumber;
      while fLine[Run] in ['0'..'9'] do
        inc(Run);
    end;
end;


procedure TSynAsmHighlighter.RoundOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;


procedure TSynAsmHighlighter.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;


procedure TSynAsmHighlighter.OperatorProc;
begin
  inc(Run);
  fTokenID := tkOperator;
end;


procedure TSynAsmHighlighter.CommentProc;
begin
  inc(Run);
  while (fLine[Run] <> #0) do
    begin
      case fLine[Run] of
        #10, #13: break;
      end;
      inc(Run);
    end;
  fTokenID := tkComment;
end;


procedure TSynAsmHighlighter.SlashProc;
begin
  if (fLine[Run+1] = '/') then
    begin
      inc(Run, 2);
      fTokenID := tkComment;
      while (fLine[Run] <> #0) do
        begin
          case fLine[Run] of
            #10, #13: break;
          end;
          inc(Run);
        end;
    end
  else
    begin
      inc(Run);
      fTokenID := tkOperator;
    end;
  end;


procedure TSynAsmHighlighter.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;


procedure TSynAsmHighlighter.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while fLine[Run] in [#1..#9, #11, #12, #14..#32] do
    inc(Run);
end;


procedure TSynAsmHighlighter.StringProc;
begin
  fTokenID := tkString;
  if (fLine[Run+1] = fStringDelimCh) and (fLine[Run+2] = fStringDelimCh) then
    Inc(Run, 2);
  repeat
    case fLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until fLine[Run] = fStringDelimCh;
  if (fLine[Run] <> #0) then
    inc(Run);
end;


procedure TSynAsmHighlighter.UnknownProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191])
     or ((fLine[Run] <> #0) and (fProcTable[fLine[Run]] = @UnknownProc)) do
     inc(Run);
  fTokenID := tkUnKnown;
end;


procedure TSynAsmHighlighter.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;


procedure TSynAsmHighlighter.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;


function TSynAsmHighlighter.GetEol: Boolean;
begin
  Result := (fTokenId = tkNull);
end;


function TSynAsmHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKeyword: Result := fKeywordAttri;
    tkNumber: Result := fNumberAttri;
    tkDirective: Result := fDirectiveAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkOperator: Result := fOperatorAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;


function TSynAsmHighlighter.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  Result := '';
  SetString(Result, (fLine + fTokenPos), Len);
end;


procedure TSynAsmHighlighter.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart := fLine + fTokenPos;
end;


function TSynAsmHighlighter.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;


function TSynAsmHighlighter.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeywordAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
    SYN_ATTR_NUMBER: Result := fNumberAttri;
    SYN_ATTR_DIRECTIVE: Result := fDirectiveAttri;
  else
    Result := nil;
  end;
end;


function TSynAsmHighlighter.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;


procedure TSynAsmHighlighter.SetKeyWords(const Value: string);
begin
  fKeyWords.Text := Value;
  DefHighLightChange(nil);
end;


function TSynAsmHighlighter.IsKeyword(const aKeyword: string): boolean;
var
  First, Last, i, Compare: integer;
  Token: string;
begin
  First := 0;
  Last := fKeywords.Count - 1;
  Result := False;
  Token := UpperCase(aKeyword);
  while (First <= Last) do
    begin
      i := (First + Last) shr 1;
      Compare := AnsiCompareStr(fKeywords[i], Token);
      if (Compare = 0) then
        begin
          Result := True;
          break;
        end
      else
        if (Compare < 0) then
          First := i + 1
        else
          Last := i - 1;
    end;
end;


function TSynAsmHighlighter.GetSampleSource: string;
begin
  Result := '// Comment' + #13 +
            '           org $FC00  // keyword & number' + #13 +
            '           fcc ''string''' + #13 +
            '#if dosomething       // directive & identifier' + #13 +
            '           lda #12    // keyword, symbol & number' + #13 +
            '#endif' + #13 +
            '           sta dest+1 // operator in expression' + #13 +
            'label      jmp there  // label, keyword, identifier';
end;


class function TSynAsmHighlighter.GetLanguageName: string;
begin
  Result := SYNS_LangGeneral;
end;


initialization
  MakeIdentTable;

end.

