{ ==============================================================================

  FORMATTER

    Generates standard ASM layout for text


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

unit uFormatter;

{$mode objfpc}{$H+}
{.$define formatter_debug}

interface

uses
  Classes, SysUtils, StrUtils, Dialogs
  {$ifdef formatter_debug}, uCommon {$endif};

type
  TTokenType = (tkNil, tkLabel, tkComment, tkCommentAtStart, tkString, tkDirective, tkEOL);

  TToken = record
    TypeVal: TTokenType;
    StringVal: string;
  end;

  TSettings = record
    StartM: integer;
    StartO: integer;
    StartC: integer;
  end;

  { TFiles }

  TFormatter = class(TObject)
  private
    fSettings: TSettings;
    Index: integer;
    CurrentLine: string;
    CurrentToken: TToken;
    function FormatLine(aText: string): string;
    function GetToken: TToken;
  public
    constructor Create;
    destructor  Destroy; override;
    //
    function FormatFile(aText: string): string;
    property Settings: TSettings read fSettings write fSettings;
  end;



implementation

{ CREATE / DESTROY }

constructor TFormatter.Create;
begin
  fSettings.StartM := 15;               // Start of mnemonic section
  fSettings.StartO := 21;               // Operand
  fSettings.StartC := 40;               // Comment
end;


{ DESTROY }

destructor TFormatter.Destroy;
begin
  //
  inherited Destroy;
end;


function TFormatter.FormatFile(aText: string): string;
var
  LinesIn, LinesOut: TStringList;
  i: integer;
begin
  LinesIn := TStringList.Create;
  LinesOut := TStringList.Create;
  try
    LinesIn.Text := aText;
    for i := 0 to (LinesIn.Count - 1) do
      begin
       {$ifdef formatter_debug}
        AppLog.Debug(Format('TFormatter.FormatFile, line %d', [i+1]));
       {$endif}
        LinesOut.Add(FormatLine(LinesIn.Strings[i]));
      end;
    Result := LinesOut.Text;
  finally
    LinesIn.Free;
    LinesOut.Free;
  end;
end;


function TFormatter.FormatLine(aText: string): string;
var
  LabeLstr, MnemStr, OperandStr, CommentStr, FormatStr: string;
  NoMore: boolean;
begin
  CurrentLine := aText + #0;
  Index := 1;
  LabelStr := '';
  MnemStr := '';
  OperandStr := '';
  CommentStr := '';
  NoMore := False;

  // First item can be lone comment, label, opcode, directive
  case GetToken.TypeVal of
    tkEOL:
      begin
        Result := '';                   // Empty line, just exit
        Exit;
      end;

    tkComment:                          // Just return comment at right
      CommentStr := CurrentToken.StringVal;

    tkCommentAtStart:                   // Return comment at first position
      LabelStr := CurrentToken.StringVal;

    tkLabel:
      begin
        LabelStr := CurrentToken.StringVal;
        case GetToken.TypeVal of
          // Next could be opcode, just add in and fall throught to next section
          tkString: MnemStr := CurrentToken.StringVal;
          // or comment
          tkComment: begin
                       CommentStr := CurrentToken.StringVal;
                       NoMore := True;
                     end;
          // or EOL
          tkEol: NoMore := True;
        end;
      end;

    tkDirective:
      begin
        LabelStr := CurrentToken.StringVal;
        if (GetToken.TypeVal = tkString) then
          begin
            // After directive, only expect a symbol
            LabelStr := LabelStr + ' ' + CurrentToken.StringVal;
            // but might have a comment as well
            if (GetToken.TypeVal = tkComment) then
              CommentStr := CurrentToken.StringVal;
            NoMore := True;
          end;
      end;

    tkString:
      begin
        // Its an opcode, set and fall through to next section
        MnemStr := CurrentToken.StringVal;
      end;
  end;

  if (not NoMore) then
    begin
      // At this point, only expecting an operand or comment
      case GetToken.TypeVal of
        tkString:
          begin
            OperandStr := CurrentToken.StringVal;
            // May be several operand elements, get all
            while (GetToken.TypeVal = tkString) do
              OperandStr := OperandStr + ' ' + CurrentToken.StringVal;
            // After that, only a comment allowed
            if (CurrentToken.TypeVal = tkComment) then
              CommentStr := CurrentToken.StringVal;
          end;
        tkComment:
          CommentStr := CurrentToken.StringVal;
      end;
    end;

  FormatStr := '%-' + IntToStr(fSettings.StartM - 2) +
               's %-' + IntToStr(fSettings.StartO - fSettings.StartM - 1) +
               's %-' + IntToStr(fSettings.StartC - fSettings.StartO - 1) +
               's %s';
  Result := Format(FormatStr, [LabelStr, MnemStr, OperandStr, CommentStr]);
end;


function TFormatter.GetToken: TToken;
var
  Start: integer;
  {$ifdef formatter_debug}
  tmp: string;
  {$endif}
begin
  while CurrentLine[Index] in [#1..#32] do  // Skip blanks
    Inc(Index);

  case CurrentLine[Index] of
    #0: CurrentToken.TypeVal := tkEOL;  // EOL ?

    '/': if (CurrentLine[Index+1] = '/') then // Comment ?
           begin
             Start := Index;
             repeat
               Inc(Index)
             until (CurrentLine[Index] = #0);
             CurrentToken.StringVal := MidStr(CurrentLine, Start, Index - Start);
             CurrentToken.TypeVal := tkComment;
             if (Start = 1) then
               CurrentToken.TypeVal := tkCommentAtStart;
           end
         else
           begin
             Inc(Index);
             CurrentToken.StringVal := '/';
             CurrentToken.TypeVal := tkString;
           end;

    ';': begin                          // Alternative comment?
           Start := Index;
           repeat
             Inc(Index)
           until (CurrentLine[Index] = #0);
           CurrentToken.StringVal := MidStr(CurrentLine, Start, Index - Start);
           CurrentToken.TypeVal := tkComment;
           if (Start = 1) then
             CurrentToken.TypeVal := tkCommentAtStart;
         end;

    '#': begin
           Start := Index;
           while (CurrentLine[Index] in [#33..#255]) do
             Inc(Index);
           CurrentToken.StringVal := MidStr(CurrentLine, Start, Index-Start);
           CurrentToken.TypeVal := tkDirective;
           if (Start > 1) then          // Directives at start of line only
             CurrentToken.TypeVal := tkString;
         end
  else
    begin
      Start := Index;                   // Must be string of some form
      while (CurrentLine[Index] in [#33..#255]) do
        Inc(Index);
      CurrentToken.StringVal := MidStr(CurrentLine, Start, Index-Start);
      CurrentToken.TypeVal := tkString;
      if (Start = 1) then
        CurrentToken.TypeVal := tkLabel;
    end;

  end;
  Result := CurrentToken;

  {$ifdef formatter_debug}
  case CurrentToken.TypeVal of
    tkLabel:          tmp := 'tkLabel';
    tkComment:        tmp := 'tkComment';
    tkCommentAtStart: tmp := 'tkCommentAtStart';
    tkString:         tmp := 'tkString';
    tkDirective:      tmp := 'tkDirective';
    tkEOL:            tmp := 'tkEOL';
  end;
  AppLog.Debug(Format('TFormatter.GetToken, tok=%s, val=[%s]', [tmp, CurrentToken.StringVal]));
  {$endif}
end;


end.

