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

{.$define compare_debug}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Grids, ExtCtrls,
  //
  uMachineBase, uCommon, uIniFile, uReadWriteHex;

type
  TByteBuffer = array[0..$FFFF] of byte;

  { TCompareForm }

  TCompareForm = class(TForm)
    btnCompare: TButton;
    btnNext: TButton;
    btnPrev: TButton;
    btnClose: TButton;
    DrawGrid1: TDrawGrid;
    edMemFrom1: TEdit;
    edMemFrom2: TEdit;
    edHexFrom: TEdit;
    edRomFrom: TEdit;
    edRomTo: TEdit;
    edHexTo: TEdit;
    edMemTo1: TEdit;
    edMemTo2: TEdit;
    gbResults: TGroupBox;
    gbFrom: TGroupBox;
    gbTo: TGroupBox;
    lbMemFrom1: TLabel;
    lbMemFrom2: TLabel;
    lbMemTo1: TLabel;
    lbMemTo2: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    rgFrom: TRadioGroup;
    rgTo: TRadioGroup;
    sbHexFrom: TSpeedButton;
    sbRomFrom: TSpeedButton;
    sbRomTo: TSpeedButton;
    sbHexTo: TSpeedButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; {%H-}aState: TGridDrawState);
    procedure edMemChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure rgClick(Sender: TObject);
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
    procedure ReadIniSettings;
    procedure WriteIniSettings;
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
  INI_FROM   = 'FromIdx';
  INI_TO     = 'ToIdx';
  INI_FNROMF = 'FnRomF';
  INI_FNHEXF = 'FnHexF';
  INI_FNROMT = 'FnRomT';
  INI_FNHEXT = 'FnHexT';


{ CREATE }

procedure TCompareForm.FormCreate(Sender: TObject);
begin
  edRomFrom.Top := 60;                  // Set tops for those items displaced
  sbRomFrom.Top := 60;                  // for ease during layout
  edHexFrom.Top := 60;
  sbHexFrom.Top := 60;
  lbMemFrom1.Top := 62;
  edMemFrom1.Top := 60;
  lbMemFrom2.Top := 62;
  edMemFrom2.Top := 60;
  //
  edRomTo.Top := 60;
  sbRomTo.Top := 60;
  edHexTo.Top := 60;
  sbHexTo.Top := 60;
  lbMemTo1.Top := 62;
  edMemTo1.Top := 60;
  lbMemTo2.Top := 62;
  edMemTo2.Top := 60;

  ReadIniSettings;
  edRomFrom.Text := ExtractFileName(FileNameRomFrom);
  edHexFrom.Text := ExtractFileName(FileNameHexFrom);
  edRomTo.Text := ExtractFileName(FileNameRomTo);
  edHexTo.Text := ExtractFileName(FileNameHexTo);

  CompareActive := False;
end;


{ DESTROY }

procedure TCompareForm.FormDestroy(Sender: TObject);
begin
  WriteIniSettings;
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

  case (rgFrom.ItemIndex) of
    0: begin
         LengthFrom := ReadRomFile(FileNameRomFrom, BufFrom);
         FromStr := Format('ROM image "%s"', [edRomFrom.Text]);
         OffsetFrom := 0;
       end;
    1: begin
         LengthFrom := ReadHexFile(FileNameHexFrom, BufFrom);
         FromStr := Format('hex file "%s"', [edHexFrom.Text]);
         OffsetFrom := TempStartAddr;      // Set by ReadHexFile below
       end;
    2: begin
         LengthFrom := ReadMem(edMemFrom1, edMemFrom2, BufFrom);
         FromStr := Format('memory $%.4x - $%.4x', [GetHex(edMemFrom1.Text), GetHex(edMemFrom2.Text)]);
         OffsetFrom := GetHex(edMemFrom1.Text);
       end;
  end;

  case (rgTo.ItemIndex) of
    0: begin
         LengthTo := ReadRomFile(FileNameRomTo, BufTo);
         ToStr := Format('ROM image "%s"', [edRomTo.Text]);
         OffsetTo := 0;
       end;
    1: begin
         LengthTo := ReadHexFile(FileNameHexTo, BufTo);
         ToStr := Format('hex file "%s"', [edHexTo.Text]);
         OffsetTo := TempStartAddr;
       end;
    2: begin
         LengthTo := ReadMem(edMemTo1, edMemTo2, BufTo);
         ToStr := Format('memory $%.4x - $%.4x', [GetHex(edMemTo1.Text), GetHex(edMemTo2.Text)]);
         OffsetTo := GetHex(edMemTo1.Text);
       end;
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

  {$ifdef compare_debug}
  AppLog.Debug('TCompareForm.btnCompareClick, %s against %s, len %d', [FromStr, ToStr, LengthFrom]);
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


{ CLOSE }

procedure TCompareForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


{ RADIO GROUP CLICK }

procedure TCompareForm.rgClick(Sender: TObject);
var
  item: integer;
begin
  item := (Sender as TRadioGroup).Tag;
  if (item = 1) then                    // From
    begin
      edRomFrom.Visible := (rgFrom.ItemIndex = 0);
      sbRomFrom.Visible := (rgFrom.ItemIndex = 0);
      edHexFrom.Visible := (rgFrom.ItemIndex = 1);
      sbHexFrom.Visible := (rgFrom.ItemIndex = 1);
      edMemFrom1.Visible := (rgFrom.ItemIndex = 2);
      edMemFrom2.Visible := (rgFrom.ItemIndex = 2);
      lbMemFrom1.Visible := (rgFrom.ItemIndex = 2);
      lbMemFrom2.Visible := (rgFrom.ItemIndex = 2);
    end
  else                                  // To
    begin
      edRomTo.Visible := (rgTo.ItemIndex = 0);
      sbRomTo.Visible := (rgTo.ItemIndex = 0);
      edHexTo.Visible := (rgTo.ItemIndex = 1);
      sbHexTo.Visible := (rgTo.ItemIndex = 1);
      edMemTo1.Visible := (rgTo.ItemIndex = 2);
      edMemTo2.Visible := (rgTo.ItemIndex = 2);
      lbMemTo1.Visible := (rgTo.ItemIndex = 2);
      lbMemTo2.Visible := (rgTo.ItemIndex = 2);
  end;
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
  //
  if ((Sender as TSpeedButton).Tag = 1) then  // From
    begin
      edRomFrom.Text := ExtractFileName(ThisFileName);
      FileNameRomFrom := ThisFileName;
    end
  else
    begin
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
  AppLog.Debug('TCompareForm.ReadRomFile, file [%s], length %d', [ExtractFileName(FileName), Result]);
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
      edHexFrom.Text := ExtractFileName(ThisFileName);
      FileNameHexFrom := ThisFileName;
    end
  else
    begin
      edHexTo.Text := ExtractFileName(ThisFileName);
      FileNameHexTo := ThisFileName;
    end;
end;


function TCompareForm.ReadHexFile(FileName: string; var Buffer: TByteBuffer): integer;
var
  ReadHex: TReadHex;
  idx: integer;
begin
  {$ifdef compare_debug}
  AppLog.Debug('TCompareForm.ReadHexFile, file [%s]', [ExtractFileName(FileName)]);
  {$endif}

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
      for idx := 0 to (ReadHex.BytesRead - 1) do
        Buffer[idx] := ReadHex.BytesArray[idx];
  finally
    ReadHex.Free;
  end;

  {$ifdef compare_debug}
  AppLog.Debug('TCompareForm.ReadHexFile, bytes read %d', [Result]);
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

procedure TCompareForm.edMemChange(Sender: TObject);
var
  ThisEdit: TEdit;
begin
  ThisEdit := (Sender as TEdit);
  ThisEdit.Text := Format('%.4x', [GetHex(ThisEdit.Text) and $FFFF]);
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
  AppLog.Debug('TCompareForm.ReadMem, from $%.4x, to $%.4x, length %d', [FromAddr, ToAddr, Result]);
  {$endif}
end;


procedure TCompareForm.ErrorMsg(Msg: string);
begin
  if (ErrMsg <> '') then
    ErrMsg := ErrMsg + CRLF;
  ErrMsg := ErrMsg + Msg;
end;


{ READ / WRITE INI SETTINGS }

procedure TCompareForm.ReadIniSettings;
begin
  Top := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, 20);
  Left := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, 170);
  Visible := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_WDW_VIS, True); // Written out in uMainForm

  rgFrom.ItemIndex := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_FROM, 0);
  rgTo.ItemIndex := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_TO, 0);
  rgClick(rgFrom);
  rgClick(rgTo);

  edMemFrom1.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_F1, 0)]);
  edMemFrom2.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_F2, $FFFF)]);
  edMemTo1.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_T1, 0)]);
  edMemTo2.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_T2, $FFFF)]);

  FileNameRomFrom := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + INI_FNROMF, '');
  FileNameHexFrom := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + INI_FNHEXF, '');
  FileNameRomTo := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + INI_FNROMT, '');
  FileNameHexTo := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + INI_FNHEXT, '');
end;


procedure TCompareForm.WriteIniSettings;
begin
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, Left);

  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_FROM, rgFrom.ItemIndex);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_TO, rgTo.ItemIndex);

  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_F1, GetHex(edMemFrom1.Text));
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_F2, GetHex(edMemFrom2.Text));
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_T1, GetHex(edMemTo1.Text));
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_T2, GetHex(edMemTo2.Text));

  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + INI_FNROMF, FileNameRomFrom);
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + INI_FNHEXF, FileNameHexFrom);
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + INI_FNROMT, FileNameRomTo);
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + INI_FNHEXT, FileNameHexTo);
end;


end.

