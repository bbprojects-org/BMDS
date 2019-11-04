{ ==============================================================================

  ASSEMBLER LISTING

  This class provides the output listing file for the application. In this
  case the output is in HTML format, but could be modified as required.
  HTML format used here permits syntax highlighting, with stylesheets.
  This class also handles error messages, holding a list in a TStrings list and
  displaying them as appropriate for each line

  =============================================================================}

unit uListing.old;

                                                                                { TODO : uListing -> gray the source code if it not being assembled in an IF/ENDIF situation }
                                                                                { TODO : uListing -> get colours for stylesheet from Options }

{$mode objfpc}
{$H+}

interface

uses Classes, SysUtils;

type
  EListFileError = class(Exception);

  TListing = class
  private
    fProcessorName: string;
    fListF: TextFile;
    fCanList: boolean;
    fLineNumber: integer;
    fHexData: string;
    fLabelStr: string;
    fOpcodeStr: string;
    fOperandStr: string;
    fCommentStr: string;
    fSourceLine: string;
    fErrors: TStrings;
    fErrorCount: integer;
    fWarnings: TStrings;
    fWarningCount: integer;
    function GetErrorsInLine:boolean;
  public
    constructor Create(sProcessor, sFilename, sStyleSheet: string);
    destructor Destroy; override;
    procedure List(sText: string);
    procedure ListStart;
    procedure ListLine(NestedLevels: integer);
    procedure ListEnd;
    procedure ListError(sMsg: string);
    procedure AddError(sMsg: string);
    procedure AddWarning(sMsg: string);
    property ErrorsInLine: boolean read GetErrorsInLine;
    property ErrorCount: integer read fErrorCount;
    property WarningCount: integer read fWarningCount;
    property CanList: boolean read fCanList write fCanList;
    property LineNumber: integer read fLineNumber write fLineNumber;
    property HexData: string read fHexData write fHexData;
    property LabelStr: string read fLabelStr write fLabelStr;
    property OpcodeStr: string read fOpcodeStr write fOpcodeStr;
    property OperandStr: string read fOperandStr write fOperandStr;
    property CommentStr: string read fCommentStr write fCommentStr;
    property SourceLine: string write fSourceLine;
  end;

const
  CRLF = #13#10;


implementation

constructor TListing.Create(sProcessor, sFilename, sStyleSheet: string);
var
  StyleSheet: TStringList;
begin
  fProcessorName := sProcessor;
  StyleSheet := TStringList.Create;
  StyleSheet.LoadFromFile(sStyleSheet); // Get stylesheet entries
  fErrors := TStringList.Create;
  fErrorCount := 0;
  fWarnings := TStringList.Create;
  fWarningCount := 0;
  fCanList := True;                     // This can be used to inhibit listing in Pass 1
  AssignFile(fListF, sFilename);
  Rewrite(fListF);
  Writeln(fListF, '<html>' + CRLF +
                  '<head>' + CRLF +
                  '<style type="text/css">' + CRLF + '<!--' + CRLF +
                  StyleSheet.Text + CRLF +
                  '-->' + CRLF + '</style>' + CRLF +
                  '</head>' + CRLF +
                  '<body>' + CRLF);
end;


destructor TListing.Destroy;
begin
  Writeln(fListF, '</body>' + CRLF + '</html>');
  CloseFile(fListF);
  fWarnings.Free;
  fErrors.Free;
  inherited;
end;


procedure TListing.List(sText: string);
begin
  if (fCanList) then
    Writeln(fListF, sText);
end;


procedure TListing.ListStart;
begin
  List('<table>' + CRLF + //' <tr>' +
       '<tr class=heading><td colspan=7><b>Assembler for ' + fProcessorName + '</b></td>' +
       '<td align=right>' + DateTimeToStr(Now) + '</td></tr>' +
       '<tr><td align=center class=heading>Line</td><td>&nbsp;&nbsp;&nbsp;</td>' +
       '<td class=heading>Hex</td><td>&nbsp;&nbsp;&nbsp;</td>' +
       '<td colspan=4 class=heading>Source</td></tr>' +
       '<tr><td colspan=8>&nbsp;</td></tr>');
end;


procedure TListing.ListLine(NestedLevels: integer);
var
  sLine, sText, sNested: string;
  idx: integer;
begin
  if (fSourceLine <> '') then
    begin
      case (fSourceLine[1]) of
        // Hashdirective, LabelStr = #define, OpcodeStr = remainder exc ... CommentStr = Comment
        '#': sLine := Format('<td></td><td></td>' +
                             '<td class=hashdirective>%s</td><td colspan=2 class=hashdirective>%s</td><td class=comment>%s</td>',
                             [ fLabelStr, fOpcodeStr, fCommentStr ]);
        ';': sLine := Format('<td></td><td></td>' +
                             '<td colspan=4 class=comment>%s</td>',
                             [ fSourceLine ]);
        '/': if (fSourceLine[2] = '/') then
               sLine := Format('<td></td><td></td>' +
                               '<td colspan=4 class=comment>%s</td>',
                               [ fSourceLine ]);
      else
        sLine := Format('<td nowrap>%s</td><td></td>' +
                        '<td class=label>%s</td><td>%s</td><td>%s</td><td class=comment>%s</td>',
                        [ fHexData,
                          fLabelStr, fOpcodeStr, fOperandStr, fCommentStr ]);
      end;
    end
  else
    sLine := '<td colspan=4></td>';
  sNested := '';
  if (NestedLevels > 0) then
    for idx := 1 to NestedLevels do
      sNested := sNested + '+';
  sText := Format('<tr><td align=right>%.4d</td><td>%s</td>%s</tr>',
                  [ fLineNumber, sNested, sLine ]);
  List(sText);

  // Show any errors on the line
  if (fErrors.Count > 0) then
    for idx := 0 to (fErrors.Count - 1) do
    begin
      sText := Format('<tr><td colspan=8 class=error>%s</td></tr>', [fErrors[idx]]);
      List(sText);
    end;
  fErrors.Clear;

  // Show any warnings on the line
  if (fWarnings.Count > 0) then
    for idx := 0 to (fWarnings.Count - 1) do
    begin
      sText := Format('<tr><td colspan=8 class=warning>%s</td></tr>', [fWarnings[idx]]);
      List(sText);
    end;
  fWarnings.Clear;

  fSourceLine := '';
  fHexData := '';
  fLabelStr := '';
  fOpcodeStr := '';
  fOperandStr := '';
  fCommentStr := '';
end;


procedure TListing.ListEnd;
var
  sPlural: char;
begin
  List('</table>' + CRLF);
  if (fErrorCount <> 1) then sPlural := char('s') else sPlural := #32;
  List(Format('<p><span class=error>Assembly generated %d error%s</span>',[fErrorCount, sPlural]));
  if (fWarningCount <> 1) then sPlural := char('s') else sPlural := #32;
  List(Format('<br><span class=warning>Assembly generated %d warning%s</span>',[fWarningCount, sPlural]));
end;


procedure TListing.ListError(sMsg: string);
begin
  List(Format('<tr><td colspan=8 class=error>%s</td></tr>', [sMsg]));
end;


procedure TListing.AddError(sMsg: string);
begin
  fErrors.Add(Format('Error in line %d, %s', [fLineNumber, sMsg]));
  Inc(fErrorCount);
end;


procedure TListing.AddWarning(sMsg: string);
begin
  fWarnings.Add(Format('Warning line %d, %s', [fLineNumber, sMsg]));
  Inc(fWarningCount);
end;


function TListing.GetErrorsInLine: boolean;
begin
  Result := ((fErrors.Count + fWarnings.Count) > 0);
end;


end.
