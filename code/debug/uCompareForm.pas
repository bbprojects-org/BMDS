{ ==============================================================================

  COMPARE FORM

    Permits the byte-by-byte comparison of ROM image files, hex files or
    in memory code. If there are mismatches, these can be viewed in the
    results window.

    Saves its settings and form position in the app's INI file, for each
    machine


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

unit uCompareForm;

{$mode objfpc}{$H+}
{$R-}

{$define compare_debug}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Grids,
  //
  uMachineBase, uCommon, uIniFile, uReadWriteHex;

type
  TByteBuffer = array[0..$FFFF] of byte;

  { TCompareForm }

  TCompareForm = class(TForm)
    btnCompare: TButton;
    btnNext: TButton;
    btnPrev: TButton;
    DrawGrid1: TDrawGrid;
    edMemFrom1: TEdit;
    edMemFrom2: TEdit;
    edHexFrom: TEdit;
    edRomFrom: TEdit;
    edRomTo: TEdit;
    edHexTo: TEdit;
    edMemTo1: TEdit;
    edMemTo2: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    lbMemFrom1: TLabel;
    lbMemFrom2: TLabel;
    lbMemTo1: TLabel;
    lbMemTo2: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    rbRomFrom: TRadioButton;
    rbHexFrom: TRadioButton;
    rbMemFrom: TRadioButton;
    rbRomTo: TRadioButton;
    rbHexTo: TRadioButton;
    rbMemTo: TRadioButton;
    sbHexFrom: TSpeedButton;
    sbRomFrom: TSpeedButton;
    sbRomTo: TSpeedButton;
    sbHexTo: TSpeedButton;
    procedure btnCompareClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; {%H-}aState: TGridDrawState);
    procedure edMemFrom1Change(Sender: TObject);
    procedure edMemTo1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure sbHexClick(Sender: TObject);
    procedure sbRomClick(Sender: TObject);
  private
    BufFrom: TByteBuffer;
    BufTo: TByteBuffer;
    FileNameRomFrom: string;
    FileNameHexFrom: string;
    FileNameRomTo: string;
    FileNameHexTo: string;
    LengthFrom: integer;
    LengthTo: integer;
    ErrMsg: string;
    ErrorLine: integer;
    CurrentErrorLine: integer;
    CompareActive: boolean;
    OffsetFrom: integer;
    OffsetTo: integer;
    TempStartAddr: integer;
    procedure ErrorMsg(Msg: string);
    procedure ErrorTooLong(Len: integer);
    function NoFileName(FN, Txt: string): boolean;
    function ReadRomFile(FileName: string; var Buffer: TByteBuffer): integer;
    function ReadHexFile(FileName: string; var Buffer: TByteBuffer): integer;
    function ReadMem(Ed1, Ed2: TEdit; var Buffer: TByteBuffer): integer;
  public
  end;


implementation

{$R *.lfm}

{ TCompareForm }

const
  INI_PREFIX = 'Compare';
  INI_F1     = 'MemF1';
  INI_F2     = 'MemF2';
  INI_T1     = 'MemT1';
  INI_T2     = 'MemT2';
  INI_FNROMF = 'FnRomF';
  INI_FNHEXF = 'FnHexF';
  INI_FNROMT = 'FnRomT';
  INI_FNHEXT = 'FnHexT';


{ CREATE }

procedure TCompareForm.FormCreate(Sender: TObject);
begin
  Top := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, 20);
  Left := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, 170);
  Visible := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_WDW_VIS, True); // Written out in uMainForm

  edMemFrom1.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_F1, 0)]);
  edMemFrom2.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_F2, $FFFF)]);
  edMemTo1.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_T1, 0)]);
  edMemTo2.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_T2, $FFFF)]);

  FileNameRomFrom := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + INI_FNROMF, '');
  FileNameHexFrom := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + INI_FNHEXF, '');
  FileNameRomTo := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + INI_FNROMT, '');
  FileNameHexTo := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + INI_FNHEXT, '');

  edRomFrom.Text := ExtractFileName(FileNameRomFrom);
  edHexFrom.Text := ExtractFileName(FileNameHexFrom);
  edRomTo.Text := ExtractFileName(FileNameRomTo);
  edHexTo.Text := ExtractFileName(FileNameHexTo);

  CompareActive := False;
end;


{ DESTROY }

procedure TCompareForm.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, Left);

  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_F1, GetHex(edMemFrom1.Text));
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_F2, GetHex(edMemFrom2.Text));
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_T1, GetHex(edMemTo1.Text));
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_T2, GetHex(edMemTo2.Text));

  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + INI_FNROMF, FileNameRomFrom);
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + INI_FNHEXF, FileNameHexFrom);
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + INI_FNROMT, FileNameRomTo);
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + INI_FNHEXT, FileNameHexTo);
end;


{ SHOW }

procedure TCompareForm.FormShow(Sender: TObject);
begin
  btnCompare.SetFocus;
end;


{ COMPARE BUTTON }

procedure TCompareForm.btnCompareClick(Sender: TObject);
var
  i, Count: integer;
  FromStr, ToStr: string;
begin
  Memo1.Lines.Clear;
  ErrMsg := '';

  if (rbRomFrom.Checked) then
    begin
      LengthFrom := ReadRomFile(FileNameRomFrom, BufFrom);
      FromStr := Format('ROM image "%s"', [edRomFrom.Text]);
      OffsetFrom := 0;
    end;
  if (rbHexFrom.Checked) then
    begin
      LengthFrom := ReadHexFile(FileNameHexFrom, BufFrom);
      FromStr := Format('hex file "%s"', [edHexFrom.Text]);
      OffsetFrom := TempStartAddr;      // Set by ReadHexFile
    end;
  if (rbMemFrom.Checked) then
    begin
      LengthFrom := ReadMem(edMemFrom1, edMemFrom2, BufFrom);
      FromStr := Format('memory $%.4x - $%.4x', [GetHex(edMemFrom1.Text), GetHex(edMemFrom2.Text)]);
      OffsetFrom := GetHex(edMemFrom1.Text);
    end;

  if (rbRomTo.Checked) then
    begin
      LengthTo := ReadRomFile(FileNameRomTo, BufTo);
      ToStr := Format('ROM image "%s"', [edRomTo.Text]);
      OffsetTo := 0;
    end;
  if (rbHexTo.Checked) then
    begin
      LengthTo := ReadHexFile(FileNameHexTo, BufTo);
      ToStr := Format('hex file "%s"', [edHexTo.Text]);
      OffsetTo := TempStartAddr;
    end;
  if (rbMemTo.Checked) then
    begin
      LengthTo := ReadMem(edMemTo1, edMemTo2, BufTo);
      ToStr := Format('memory $%.4x - $%.4x', [GetHex(edMemTo1.Text), GetHex(edMemTo2.Text)]);
      OffsetTo := GetHex(edMemTo1.Text);
    end;

  if (LengthFrom <> LengthTo) then
    ErrorMsg(Format('Lengths should be same; %d and %d', [LengthFrom, LengthTo]));

  if (ErrMsg <> '') then
    begin
      MessageDlg('Error:' + LineEnding + ErrMsg, mtWarning, [mbOK], 0);
      Exit;
    end;

  Memo1.Lines.Add('Compared ' + FromStr);
  Memo1.Lines.Add('against ' + ToStr);
  Memo1.Lines.Add('');

  {$ifdef compare_debug}
  AppLog.Debug(Format('TCompareForm.btnCompareClick, %s against %s, len %d', [FromStr, ToStr, LengthFrom]));
  {$endif}

  DrawGrid1.RowCount := LengthFrom;
  for i := 0 to (LengthFrom - 1) do
    begin
      if (BufFrom[i] <> BufTo[i]) then
        begin
          ErrorLine := i;
          CurrentErrorLine := i;
          DrawGrid1.Row := ErrorLine;   // Show first error line
          break;
        end;
    end;

  Count := 0;
  for i := ErrorLine to (LengthFrom - 1) do
    if (BufFrom[i] <> BufTo[i]) then
      Inc(Count);

  if (Count > 0) then
    begin
      Memo1.Lines.Add(Format('Mismatch at offset %d ($%x), total %d differences', [ErrorLine, ErrorLine, Count]));
      btnNext.Enabled := (Count >  1);
      btnPrev.Enabled := (Count >  1);
      DrawGrid1.SetFocus;
      CompareActive := True;
    end
  else
    begin
      Memo1.Lines.Add(Format('Match: %d ($%x) bytes checked', [LengthFrom, LengthFrom]));
      CompareActive := False;
    end;

  DrawGrid1.Invalidate;
end;


{ NEXT / PREVIOUS BUTTONS }

procedure TCompareForm.btnNextClick(Sender: TObject);
var
  ThisLine: integer;
begin
  ThisLine := CurrentErrorLine;
  while (CurrentErrorLine < LengthFrom) do
    begin
      Inc(CurrentErrorLine);
      if (BufFrom[CurrentErrorLine] <> BufTo[CurrentErrorLine]) then
        break;
    end;
  if (CurrentErrorLine = LengthFrom) then
    begin
      btnNext.Enabled := False;
      CurrentErrorLine := ThisLine;     // Point at last error again
    end
  else
    DrawGrid1.Row := CurrentErrorLine;
  btnPrev.Enabled := True;
end;


procedure TCompareForm.btnPrevClick(Sender: TObject);
begin
  while (CurrentErrorLine > 0) do
    begin
      Dec(CurrentErrorLine);
      if (BufFrom[CurrentErrorLine] <> BufTo[CurrentErrorLine]) then
        break;
    end;
  if (CurrentErrorLine < 0) then
    btnPrev.Enabled := False
  else
    DrawGrid1.Row := CurrentErrorLine;
  btnNext.Enabled := True;
end;


{ UPDATE DRAWGRID COMPARISON OF BUFFERS }

procedure TCompareForm.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Compare: string;
begin
  DrawGrid1.Font.Color := clDefault;
  Compare := '<- EQ ->';
  if (BufFrom[aRow] <> BufTo[aRow]) then
    begin
      Compare := '<- NE ->';
      DrawGrid1.Font.Color := clRed;
    end;
  if (CompareActive) then
    case aCol of
      0: DrawGrid1.Canvas.TextOut(aRect.Left, aRect.Top, Format('%.4x', [aRow + OffsetFrom]));
      1: DrawGrid1.Canvas.TextOut(aRect.Left, aRect.Top, Format('%.2x', [BufFrom[aRow]]));
      2: DrawGrid1.Canvas.TextOut(aRect.Left, aRect.Top, Format('%s', [Compare]));
      3: DrawGrid1.Canvas.TextOut(aRect.Left, aRect.Top, Format('%.4x', [aRow + OffsetTo]));
      4: DrawGrid1.Canvas.TextOut(aRect.Left, aRect.Top, Format('%.2x', [BufTo[aRow]]));
    end
  else
    DrawGrid1.Canvas.TextOut(aRect.Left, aRect.Top, '');
end;


procedure TCompareForm.Memo1Click(Sender: TObject);
begin
  DrawGrid1.Row := ErrorLine;           // Show first error line
  DrawGrid1.SetFocus;
  DrawGrid1.Invalidate;
end;


{ COMPARE WITH ROM IMAGE FILE }

procedure TCompareForm.sbRomClick(Sender: TObject);
var
  ThisFileName: string;
begin
  if (OpenDialog1.Execute) then
    ThisFileName := OpenDialog1.FileName
  else
    ThisFileName := '';
  if ((Sender as TSpeedButton).Tag = 1) then  // From
    begin
      rbRomFrom.Checked := True;
      edRomFrom.Text := ExtractFileName(ThisFileName);
      FileNameRomFrom := ThisFileName;
    end
  else
    begin
      rbRomTo.Checked := True;
      edRomTo.Text := ExtractFileName(ThisFileName);
      FileNameRomTo := ThisFileName;
    end;
end;


function TCompareForm.ReadRomFile(FileName: string; var Buffer: TByteBuffer): integer;
var
  RomStream: TMemoryStream;
  RomPtr: PChar;
  idx: integer;
begin
  Result := 0;
  if (NoFileName(FileName, 'ROM image')) then
    Exit;

  RomStream := TMemoryStream.Create;
  try
    RomStream.LoadFromFile(FileName);
    RomPtr := RomStream.Memory;
    Result := RomStream.Size;
    if (Result > $FFFF) then
      ErrorTooLong(Result)
    else
      for idx := 0 to RomStream.Size do
        Buffer[idx] := Byte(RomPtr[idx]);
  finally
    RomStream.Free;
  end;

  {$ifdef compare_debug}
  AppLog.Debug(Format('TCompareForm.ReadRomFile, File %s, length %d', [FileName, Result]));
  {$endif}
end;


{ COMPARE WITH HEX FILE }

procedure TCompareForm.sbHexClick(Sender: TObject);
var
  ThisFileName: string;
begin
  if (OpenDialog2.Execute) then
    ThisFileName := OpenDialog2.FileName
  else
    ThisFileName := '';

  if ((Sender as TSpeedButton).Tag = 2) then  // From
    begin
      rbHexFrom.Checked := True;
      edHexFrom.Text := ExtractFileName(ThisFileName);
      FileNameHexFrom := ThisFileName;
    end
  else
    begin
      rbHexTo.Checked := True;
      edHexTo.Text := ExtractFileName(ThisFileName);
      FileNameHexTo := ThisFileName;
    end;
end;


function TCompareForm.ReadHexFile(FileName: string; var Buffer: TByteBuffer): integer;
var
  ReadHex: TReadHex;
  idx: integer;
begin
  Result := 0;
  if (NoFileName(FileName, 'hexfile')) then
    Exit;

  ReadHex := TReadHex.Create(FileName);
  try
    TempStartAddr := ReadHex.StartAddress;
    Result := ReadHex.BytesRead;
    if (Result > $FFFF) then
      ErrorTooLong(Result)
    else
      for idx := 0 to ReadHex.BytesRead do
        Buffer[idx] := ReadHex.BytesArray[idx];
  finally
    ReadHex.Free;
  end;

  {$ifdef compare_debug}
  AppLog.Debug(Format('TCompareForm.ReadHexFile, File %s, length %d', [FileName, Result]));
  {$endif}
end;


{ Helpers }

function TCompareForm.NoFileName(FN, Txt: string): boolean;
begin
  Result := False;
  if (FN = '') then
    begin
      ErrorMsg(Format('No %s specified', [Txt]));
      Result := True;
    end;
end;


procedure TCompareForm.ErrorTooLong(Len: integer);
begin
  ErrorMsg(Format('Max length is %d ($.5x) bytes, this is %d ($%.5x) bytes', [$10000, $10000, Len, Len]));
end;


{ COMPARE WITH SECTION OF MEMORY }

procedure TCompareForm.edMemFrom1Change(Sender: TObject);
begin
  rbMemFrom.Checked := True;
end;


procedure TCompareForm.edMemTo1Change(Sender: TObject);
begin
  rbMemTo.Checked := True;
end;


function TCompareForm.ReadMem(Ed1, Ed2: TEdit; var Buffer: TByteBuffer): integer;
var
  FromAddr, ToAddr, i: integer;
begin
  Result := 0;
  FromAddr := GetHex(Ed1.Text);
  ToAddr := GetHex(Ed2.Text);
  if (FromAddr < 0) or (FromAddr > $FFFF) or (ToAddr < 0) or (ToAddr > $FFFF) then
    begin
      ErrorMsg('Addresses must be between $0000 and $FFFF');
      Exit;
    end;

  if (FromAddr > ToAddr) then
    begin
      ErrorMsg('End address must be greater than start address');
      Exit;
    end;

  for i := FromAddr to ToAddr do
    Buffer[i - FromAddr] := Machine.Memory[i];
  Result := ToAddr - FromAddr + 1;

  {$ifdef compare_debug}
  AppLog.Debug(Format('TCompareForm.ReadMem, From %.4x, To %.4x, length %d', [FromAddr, ToAddr, Result]));
  {$endif}
end;


procedure TCompareForm.ErrorMsg(Msg: string);
begin
  if (ErrMsg <> '') then
    ErrMsg := ErrMsg + CRLF;
  ErrMsg := ErrMsg + Msg;
end;


end.

