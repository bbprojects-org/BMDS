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
    procedure sbHexFromClick(Sender: TObject);
    procedure sbRomFromClick(Sender: TObject);
  private
    fMachineRef: TMachineBase;
    BufFrom: TByteBuffer;
    BufTo: TByteBuffer;
    FilenameRomFrom: string;
    FilenameHexFrom: string;
    FilenameRomTo: string;
    FilenameHexTo: string;
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
    function ReadRomFile(Filename: string; var Buffer: TByteBuffer): integer;
    function ReadHexFile(Filename: string; var Buffer: TByteBuffer): integer;
    function ReadMem(Ed1, Ed2: TEdit; var Buffer: TByteBuffer): integer;
  public
    property MachineRef: TMachineBase write fMachineRef;
  end;


implementation

{$R *.lfm}

{ TCompareForm }

const
  INI_PREFIX = 'Compare';


{ CREATE }

procedure TCompareForm.FormCreate(Sender: TObject);
begin
  Top := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, 440);
  Left := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, 400);  
  Visible := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_WDW_VIS, True); // Written out in uMainForm

  edMemFrom1.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + 'MemF1', 0)]);
  edMemFrom2.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + 'MemF2', $FFFF)]);
  edMemTo1.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + 'MemT1', 0)]);
  edMemTo2.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + 'MemT2', $FFFF)]);

  FilenameRomFrom := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + 'FnRomF', '');
  FilenameHexFrom := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + 'FnHexF', '');
  FilenameRomTo := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + 'FnRomT', '');
  FilenameHexTo := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + 'FnHexT', '');

  edRomFrom.Text := ExtractFilename(FilenameRomFrom);
  edHexFrom.Text := ExtractFilename(FilenameHexFrom);
  edRomTo.Text := ExtractFilename(FilenameRomTo);
  edHexTo.Text := ExtractFilename(FilenameHexTo);

  CompareActive := False;
end;


{ DESTROY }

procedure TCompareForm.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, Left);

  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + 'MemF1', GetHex(edMemFrom1.Text));
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + 'MemF2', GetHex(edMemFrom2.Text));
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + 'MemT1', GetHex(edMemTo1.Text));
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + 'MemT2', GetHex(edMemTo2.Text));

  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + 'FnRomF', FilenameRomFrom);
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + 'FnHexF', FilenameHexFrom);
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + 'FnRomT', FilenameRomTo);
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + 'FnHexT', FilenameHexTo);
end;


{ SHOW }

procedure TCompareForm.FormShow(Sender: TObject);
begin
  btnCompare.SetFocus;
end;


{ DO COMPARE }

procedure TCompareForm.btnCompareClick(Sender: TObject);
var
  i, Count: integer;
  FromStr, ToStr: string;
begin
  Memo1.Lines.Clear;
  ErrMsg := '';

  if rbRomFrom.Checked then
    begin
      LengthFrom := ReadRomFile(FilenameRomFrom, BufFrom);
      FromStr := Format('ROM image "%s"', [edRomFrom.Text]);
      OffsetFrom := 0;
    end;
  if rbHexFrom.Checked then
    begin
      LengthFrom := ReadHexFile(FilenameHexFrom, BufFrom);
      FromStr := Format('hex file "%s"', [edHexFrom.Text]);
      OffsetFrom := TempStartAddr;
    end;
  if rbMemFrom.Checked then
    begin
      LengthFrom := ReadMem(edMemFrom1, edMemFrom2, BufFrom);
      FromStr := Format('memory $%.4x - $%.4x', [GetHex(edMemFrom1.Text), GetHex(edMemFrom2.Text)]);
      OffsetFrom := GetHex(edMemFrom1.Text);
    end;

  if rbRomTo.Checked then
    begin
      LengthTo := ReadRomFile(FilenameRomTo, BufTo);
      ToStr := Format('ROM image "%s"', [edRomTo.Text]);
      OffsetTo := 0;
    end;
  if rbHexTo.Checked then
    begin
      LengthTo := ReadHexFile(FilenameHexTo, BufTo);
      ToStr := Format('hex file "%s"', [edHexTo.Text]);
      OffsetTo := TempStartAddr;
    end;
  if rbMemTo.Checked then
    begin
      LengthTo := ReadMem(edMemTo1, edMemTo2, BufTo);
      ToStr := Format('memory $%.4x - $%.4x', [GetHex(edMemTo1.Text), GetHex(edMemTo2.Text)]);
      OffsetTo := GetHex(edMemTo1.Text);
    end;

  if (LengthFrom <> LengthTo) then
    ErrorMsg(Format('Lengths should be same; %d and %d', [LengthFrom, LengthTo]));

  if ErrMsg <> '' then
    begin
      MessageDlg('Error:' + LineEnding + ErrMsg, mtWarning, [mbOK], 0);
      Exit;
    end;

  Memo1.Lines.Add('Compared ' + FromStr);
  Memo1.Lines.Add('against ' + ToStr);
  Memo1.Lines.Add('');

  DrawGrid1.RowCount := LengthFrom;
  for i := 0 to (LengthFrom - 1) do
    begin
      if (BufFrom[i] <> BufTo[i]) then
        begin
          ErrorLine := i;
          CurrentErrorLine := i;
          DrawGrid1.Row := ErrorLine;     // Show first error line
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

procedure TCompareForm.sbRomFromClick(Sender: TObject);
var
  ThisFilename: string;
begin
  if (OpenDialog1.Execute) then
    ThisFilename := OpenDialog1.FileName
  else
    ThisFilename := '';
  if ((Sender as TSpeedButton).Tag = 1) then  // From
    begin
      rbRomFrom.Checked := True;
      edRomFrom.Text := ExtractFilename(ThisFilename);
      FilenameRomFrom := ThisFilename;
    end
  else
    begin
      rbRomTo.Checked := True;
      edRomTo.Text := ExtractFilename(ThisFilename);
      FilenameRomTo := ThisFilename;
    end;
end;


function TCompareForm.ReadRomFile(Filename: string; var Buffer: TByteBuffer): integer;
var
  RomStream: TMemoryStream;
  RomPtr: PChar;
  idx: integer;
begin
  Result := 0;
  if (Filename = '') then
    begin
      ErrorMsg('No ROM image file specified');
      Exit;
    end;

  RomStream := TMemoryStream.Create;
  try
    RomStream.LoadFromFile(FileName);
    RomPtr := RomStream.Memory;
    Result := RomStream.Size;
    if (Result > $FFFF) then
      ErrorMsg(Format('Max length is %d ($.5x) bytes, this is %d ($%.5x) bytes', [$10000, $10000, Result, Result]))
    else
      for idx := 0 to RomStream.Size do
        Buffer[idx] := Byte(RomPtr[idx]);
  finally
    RomStream.Free;
  end;
end;


{ COMPARE WITH HEX FILE }

procedure TCompareForm.sbHexFromClick(Sender: TObject);
var
  ThisFilename: string;
begin
  if (OpenDialog2.Execute) then
    ThisFilename := OpenDialog2.FileName
  else
    ThisFilename := '';

  if ((Sender as TSpeedButton).Tag = 2) then  // From
    begin
      rbHexFrom.Checked := True;
      edHexFrom.Text := ExtractFilename(ThisFilename);
      FilenameHexFrom := ThisFilename;
    end
  else
    begin
      rbHexTo.Checked := True;
      edHexTo.Text := ExtractFilename(ThisFilename);
      FilenameHexTo := ThisFilename;
    end;
end;


function TCompareForm.ReadHexFile(Filename: string; var Buffer: TByteBuffer): integer;
var
  ReadHex: TReadHex;
  idx: integer;
begin
  Result := 0;
  if (Filename = '') then
    begin
      ErrorMsg('No hex file specified');
      Exit;
    end;

  ReadHex := TReadHex.Create(Filename);
  try
    TempStartAddr := ReadHex.StartAddress;
    Result := ReadHex.BytesRead;
    if (Result > $FFFF) then
      ErrorMsg(Format('Max length is %d ($.5x) bytes, this is %d ($%.5x) bytes', [$10000, $10000, Result, Result]))
    else
      for idx := 0 to ReadHex.BytesRead do
        Buffer[idx] := ReadHex.BytesArray[idx];
  finally
    ReadHex.Free;
  end;
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
    Buffer[i - FromAddr] := fMachineRef.Memory[i];
  Result := ToAddr - FromAddr + 1;
end;


procedure TCompareForm.ErrorMsg(Msg: string);
begin
  if (ErrMsg <> '') then
    ErrMsg := ErrMsg + CRLF;
  ErrMsg := ErrMsg + Msg;
end;


end.

