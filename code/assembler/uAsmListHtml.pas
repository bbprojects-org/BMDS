{ ==============================================================================

  ASSEMBLER LISTING (HTML)

    TListingHtml provides an output listing file in HTML format. It uses CSS as
    a simple syntax highlighter. An external "style.css" file, put in the
    application's data folder can be used to overwrite the default built-in
    CSS codes


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

{ TODO : uAsmListHtml -> Error messages }
{ TODO : uAsmListHtml -> Symbol table generation }

unit uAsmListHtml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //
  uCommon;

type
  TListingData = record
    LineNumber: integer;
    SourceLine: string;
    ExtraStr: string;
    HexData: string;
    LabelStr: string;
    OpcodeStr: string;
    OperandStr: string;
    CommentStr: string;
    IsAssembling: boolean;
  end;

  TListingHtml = class(TObject)
  private
    fListFile: TextFile;
  public
    constructor Create(AsmRef: TObject; aFilename: string);
    destructor  Destroy; override;
    //
    procedure ListHeader(ProcessorName: string);
    procedure List(aListingData: TListingData);
    procedure ListFooter(ErrorCount, WarningCount: integer);
    procedure ListError(ErrMsg: string);
    procedure ListSymbolTable;
  end;

const
  STYLESHEET_FILE    = 'style.css';     // Optional external file to change CSS
  STYLESHEET_DEFAULT = 'body { font-family: Verdana; font-size: 10pt }' + CRLF +
                       'table { font-family: Verdana; font-size: 10pt; border-spacing: 0px; }' + CRLF +
                       '.heading { background-color: silver }' + CRLF +
                       '.hashdirective { color: #228B22 }' + CRLF +
                       '.comment { color: #B22222 }' + CRLF +
                       '.label { color: blue }' + CRLF +
                       '.error { color: red }' + CRLF +
                       '.warning { color: green }' + CRLF +
                       '.bold { font-weight: bold }' + CRLF +
                       '.dim { color: #D3D3D3}';

implementation


{ CREATE }

constructor TListingHtml.Create(AsmRef: TObject; aFilename: string);
var
  StyleSheet: TStrings;
  Filename: string;
begin
  StyleSheet := TStringList.Create;
  Filename := GetAppDataDirectory + STYLESHEET_FILE;
  if FileExists(Filename) then
    StyleSheet.LoadFromFile(Filename)
  else
    Stylesheet.Text := STYLESHEET_DEFAULT;
  AssignFile(fListFile, ChangeFileExt(aFilename, '.htm'));
  Rewrite(fListFile);
  WriteLn(fListFile,  '<html>' + CRLF + '<head>' + CRLF +
                      '<style type="text/css">' + CRLF + '<!--' + CRLF +
                      StyleSheet.Text +
                      '-->' + CRLF + '</style>' + CRLF +
                      '</head>' + CRLF + '<body>' + CRLF);
end;


{ DESTROY }

destructor TListingHtml.Destroy;
begin
  Writeln(fListFile, '</body>' + CRLF + '</html>');
  CloseFile(fListFile);
  inherited;
end;


{ LIST HEADER }

procedure TListingHtml.ListHeader(ProcessorName: string);
begin
  Writeln(fListFile,
            '<table>' + CRLF +
              '<tr class="heading bold">' +
                '<td colspan="6">Assembler for ' + ProcessorName + '</td>' +
                '<td align="right">' + FormatDateTime('dd mmm yyyy, hh:nn', Now) + '</td>' +
              '</tr>' + CRLF +
              '<tr class="heading">' +
                '<td>Line</td>' +
                '<td>&nbsp;</td>' +
                '<td>Hex</td>' +
                '<td colspan="4">Source</td>' +
              '</tr>' + CRLF +
              '<tr>' +
                '<td colspan="7">&nbsp;</td>' +
              '</tr>');
end;


{ LIST FOOTER }

procedure TListingHtml.ListFooter(ErrorCount, WarningCount: integer);
var
  sPlural: string;
begin
  Writeln(fListFile, '</table>' + CRLF);
  if (ErrorCount <> 1) then sPlural := 's' else sPlural := ' ';
  Writeln(fListFile, Format('<p><span class="error">Assembly generated %d error%s</span>',[ErrorCount, sPlural]));
  if (WarningCount <> 1) then sPlural := 's' else sPlural := ' ';
  Writeln(fListFile, Format('<br><span class="warning">Assembly generated %d warning%s</span>',[WarningCount, sPlural]));
end;


{ LIST }

procedure TListingHtml.List(aListingData: TListingData);
var
  sLine: string;
  classC, classL, classDim: string;
begin
  if (aListingData.IsAssembling) then
    begin
      classC := ' class="comment"';
      classL := ' class="label"';
      classDim := ''
    end
  else
    begin
      classC := '';
      classL := '';
      classDim := ' class="dim"';
    end;

  if (aListingData.SourceLine <> '') then
    begin
      case (aListingData.SourceLine[1]) of
        '#': sLine := Format('<td></td>'+
                             '<td colspan="3" class="hashdirective">%s</td>' +
                             '<td%s>%s</td>',
                             [ aListingData.LabelStr + ' ' +aListingData.OperandStr, classC, aListingData.CommentStr ]);
        ';': sLine := Format('<td></td>' +
                             '<td colspan="4"%s>%s</td>',
                             [ classC, aListingData.SourceLine ]);
        '/': if (aListingData.SourceLine[2] = '/') then
               sLine := Format('<td></td>' +
                               '<td colspan="4"%s>%s</td>',
                               [ classC, aListingData.SourceLine ]);
        '+': sLine := Format('<td nowrap>%s</td>' +
                             '<td></td>' +
                             '<td>+</td>' +
                             '<td></td>' +
                             '<td></td>',
                            [ aListingData.HexData ]);
      else
        sLine := Format('<td nowrap>%s</td>' +
                        '<td%s>%s</td>' +
                        '<td>%s</td>' +
                        '<td>%s</td>' +
                        '<td%s>%s</td>',
                        [ aListingData.HexData, classL, aListingData.LabelStr,
                          aListingData.OpcodeStr, aListingData.OperandStr,
                          classC, aListingData.CommentStr ]);
      end;
    end
  else
    sLine := '<td colspan="5"></td>';

  sLine := Format('<tr%s>' +
                    '<td>%.4d</td>' +
                    '<td>%s&nbsp;&nbsp;</td>%s' +
                  '</tr>',
                  [ classDim, aListingData.LineNumber, aListingData.ExtraStr, sLine ]);

  Writeln(fListFile, sLine);
end;


{ LIST ERROR }

procedure TListingHtml.ListError(ErrMsg: string);
begin
  //
end;


{ LIST SYMBOL TABLE }

procedure TListingHtml.ListSymbolTable;
begin
  //
end;


end.

