{ ==============================================================================

  DISASSEMBLER

    Provides framework to generate disassembly listing for a range of memory
    addresses. Two listings are provided; a straight disassembly and a
    listing without bytes displayed but with labels, intended to be
    'assembler ready'. Utilises CPU specific disassembly routines provided
    by each CPU unit. Assumes max 64K address space

    Saves form position and size in application's INI file, for each machine

    Saves settings specific to current disassembly to a .DIS file (INI format)


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

{ TODO : uDisassembler -> in ButtonLoadSettingsClick, load filename of previously loaded file }
{ TODO : uDisassembler -> in SetMachine, fix crash if AddrDefs does not exist }
{ TODO : uDisassembler -> in DoHeader, copy header file rather than using equates (keeps comments) }
{ TODO : uDisassembler -> sort data addresses }
{ TODO : uDisassembler -> add option to create mnemonics in lower case ? }
{ TODO : uDisassembler -> add user defined address definition file for labelling ? }

unit uDisassembler;

{$mode objfpc}{$H+}
{$R-}

{.$define disassembler_debug}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, StdCtrls, ActnList, FileUtil,
  //
  uMachineBase, uCpuBase, uCpuTypes, uDefs6502, uAddrDefs, uIniFile, uCommon;

type
  TDataAddress = record
    StartAddr: Word;
    EndAddr: Word;
  end;

  { TDisassemblerForm }

  TDisassemblerForm = class(TForm)
    acLoadSymbolsFile: TAction;
    ActionListDisassembler: TActionList;
    cbSpacings: TComboBox;
    edDesc: TLabeledEdit;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    memoDis: TMemo;
    btnCopyToClipboard: TButton;
    GroupBox1: TGroupBox;
    edStart: TLabeledEdit;
    edEnd: TLabeledEdit;
    GroupBox2: TGroupBox;
    edDataStart: TLabeledEdit;
    edDataTo: TLabeledEdit;
    btnAddData: TButton;
    btnDeleteData: TButton;
    GroupBox3: TGroupBox;
    btnLabelled: TButton;
    btnSimple: TButton;
    lbDataAddresses: TListBox;
    Panel2: TPanel;
    btnSaveSettings: TButton;
    btnLoadSettings: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnSimpleClick(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnLabelledClick(Sender: TObject);
    procedure edStartExit(Sender: TObject);
    procedure btnAddDataClick(Sender: TObject);
    procedure btnDeleteDataClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSaveSettingsClick(Sender: TObject);
    procedure btnLoadSettingsClick(Sender: TObject);
    procedure lbDataAddressesClick(Sender: TObject);
  private
    CurrentAddr: integer;
    StartAddr: word;
    EndAddr: word;
    DataStartAddr: Word;
    DataEndAddr: Word;
    DataAddressArray: array of TDataAddress;
    AddrDefsProcessor: TAddrDefsProcessor;
    AddrDefsArray: TAddrDefsArray;
    AddressesUsed: array[0..$FFFF] of Boolean;
    OutputStr: string;
    DefSpcStr: string;
    DefByteStr: string;
    procedure SetMachine;
    function  CheckOperand(Mnemonic: string; OperandVal: word): string;
    function  CheckAddressesOK(nStart, nEnd: Word): boolean;
    procedure SetDataAddressArray;
    function  InDataBlock: boolean;
    function  InRange(Addr: word): boolean;
    procedure DoHeading(IsSimple: boolean = False);
  public
  end;


implementation

{$R *.lfm}

const
  INI_PREFIX = 'Dis';


{ FORM CREATE }

procedure TDisassemblerForm.FormCreate(Sender: TObject);
begin
  {$ifdef disassembler_debug}
  AppLog.Debug('TDisassemblerForm.FormCreate');
  {$endif}
  Top := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, 20);
  Left := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, 320);
  Width := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_WIDTH, 0);
  Height := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_HEIGHT, 0);
  // Following is written out in uMainForm, but read here
  Visible := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_WDW_VIS, True);
  //
  CurrentAddr := 0;
  btnCopyToClipboard.Enabled := False;
  SetMachine;
end;


{ FORM DESTROY }

procedure TDisassemblerForm.FormDestroy(Sender: TObject);
begin
  {$ifdef disassembler_debug}
  AppLog.Debug('TDisassemblerForm.FormDestroy');
  {$endif}
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_WIDTH, Width);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_HEIGHT, Height);
  if Assigned(AddrDefsProcessor) then
    AddrDefsProcessor.Free;
end;


{ FORM SHOW }

procedure TDisassemblerForm.FormShow(Sender: TObject);
begin
  edStart.SetFocus;
end;


{ SET MACHINE }

procedure TDisassemblerForm.SetMachine;
var
  FileName: string;
begin
  if (Machine.Info.MachineDefsFileName <> '') then
    begin
      AddrDefsProcessor := TAddrDefsProcessor.Create;
      FileName := MachineDataFolder + Machine.Info.MachineDefsFileName;
      AddrDefsArray := AddrDefsProcessor.GetAddrDefs(FileName);
    end;
  Caption := 'BMDS Disassembler for ' + Machine.CPU.Info.Name;

  case Machine.CPU.CpuType of
    ct6502, ct65C02, ctCHIP8:
      begin
        DefSpcStr :=  'RMB';
        DefByteStr := 'FCB';
      end;
    ct8080asmO, ct8080asmZ:
      begin
        DefSpcStr :=  'DS';
        DefByteStr := 'DB';
      end;
  end;
end;


{ CHECK ADDRESSES PROVIDED ARE WITHIN RANGE AND LOWER -> HIGHER }

procedure TDisassemblerForm.edStartExit(Sender: TObject);
var
  Value: integer;
  TempEdit: TLabeledEdit;
begin
  TempEdit := Sender as TLabeledEdit;
  Value := GetHex(TempEdit.Text);
  TempEdit.Text := Format('%.4x', [Value]);
  if ( (Value < 0) or (Value > $FFFF) ) then
    begin
      MessageDlg('Value must be between $0000 and $FFFF', mtWarning, [mbOK], 0);
      TempEdit.SetFocus;
      Exit;
    end;
  case TempEdit.Tag of
    100: StartAddr := Value;
    101: EndAddr := Value;
    200: DataStartAddr := Value;
    201: DataEndAddr := Value;
  end;
end;


{ CHECK ADDRESSES OK }

function TDisassemblerForm.CheckAddressesOK(nStart, nEnd: Word): boolean;
begin
  Result := False;
  if (nEnd >= nStart) then
    Result := True
  else
    MessageDlg('End address must be greater than start address', mtWarning, [mbOK], 0);
end;


{ Manage data section entries ... }

{ ADD DATA ADDRESS }

procedure TDisassemblerForm.btnAddDataClick(Sender: TObject);
begin
  btnAddData.SetFocus;                  // Force exit of edit box
  if (not CheckAddressesOK(DataStartAddr, DataEndAddr)) then
    begin
      edDataStart.SetFocus;
      Exit;
    end;
  lbDataAddresses.Items.Add(Format('$%.4x - $%.4x', [DataStartAddr, DataEndAddr]));
  SetDataAddressArray;
end;


procedure TDisassemblerForm.lbDataAddressesClick(Sender: TObject);
begin
  btnDeleteData.Enabled := (lbDataAddresses.SelCount > 0);
end;


{ DELETE DATA ADDRESS }

procedure TDisassemblerForm.btnDeleteDataClick(Sender: TObject);
var
  idx:integer;
begin
  for idx := (lbDataAddresses.Items.Count - 1) downto 0 do
    if (lbDataAddresses.Selected[idx]) then
      lbDataAddresses.Items.Delete(idx);
  btnDeleteData.Enabled := False;
  SetDataAddressArray;
end;


{ SET DATA ADDRESS ARRAY }

procedure TDisassemblerForm.SetDataAddressArray;
var
  i: integer;
  ThisItem: string;
begin
  SetLength(DataAddressArray, lbDataAddresses.Items.Count);
  if (lbDataAddresses.Items.Count > 0) then
    for i := 0 to (lbDataAddresses.Items.Count - 1) do
      begin
        ThisItem := lbDataAddresses.Items[i];
        DataAddressArray[i].StartAddr := GetHex(Copy(ThisItem, 2, 4));
        DataAddressArray[i].EndAddr   := GetHex(Copy(ThisItem, 10, 4));
      end;
end;


{ IN DATA BLOCK ? }

function TDisassemblerForm.InDataBlock: boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to (Length(DataAddressArray) - 1) do
    if (CurrentAddr >= DataAddressArray[i].StartAddr) and
       (CurrentAddr <= DataAddressArray[i].EndAddr) then
      begin
        Result := True;
        Exit;
      end;
end;


{ IN RANGE ? }

{ Check if the address passed is in the Start-to-End range of the current
  disassembly }

function TDisassemblerForm.InRange(Addr: word): boolean;
begin
  Result := ( (Addr >= StartAddr) and (Addr <= EndAddr) );
end;


{ GENERATE SIMPLE UNLABELLED DISASSEMBLY }

{ Run through address range disassembling any code that is outwith an
  identified data section. For data, just output byte defines }

procedure TDisassemblerForm.btnSimpleClick(Sender: TObject);
var
  CurrentByte: Byte;
  PrevAddress: Word;
  WasInDataBlock: boolean;
  DisData: TDisassembledData;
begin
  // Make sure address range is sensible, i.e. End comes after Start
  if (not CheckAddressesOK(StartAddr, EndAddr)) then
    begin
      edStart.SetFocus;
      Exit;
    end;

  memoDis.Lines.Clear;
  DoHeading(True);
  WasInDataBlock := False;
  CurrentAddr := StartAddr;             // Start address for disassembly
  while (CurrentAddr <= EndAddr) do
    begin
      if (InDataBlock) then
        // If in an identified data block, just output byte definitions
        begin
          CurrentByte := Machine.Memory[CurrentAddr];
          memoDis.Lines.Add(Format('%.4x %-8.2x= %s $%.2x  // %s', [CurrentAddr,
                                 CurrentByte, DefByteStr, CurrentByte, GetAscii(CurrentByte)]));
          Inc(CurrentAddr);
          WasInDataBlock := True;       // Flag for when data block is finished
        end
      else
      // Else should be valid opcode, so disassemble using CPU specific routine
        begin
          if (WasInDataBlock) then
            memoDis.Lines.Add('');      // Blank line after data section
          WasInDataBlock := False;
          PrevAddress := CurrentAddr;
          DisData := Machine.CPU.GetDisassembly(CurrentAddr);
          memoDis.Lines.Add(DisData.Text); // Add disassembled line to output
          if (DisData.AddBlankLine) then // Blank line after RTS/JMP like instructions
            memoDis.Lines.Add('');
          // Increment address to point at next opcode
          Inc(CurrentAddr, DisData.NumBytes);
          // .. and check if stuck due to Invalid opcode, if so skip over byte
          if (CurrentAddr = PrevAddress) then
            Inc(CurrentAddr);
        end;
    end;
  memoDis.SelStart := 0;                // Goto first line
  memoDis.SelLength := 0;
  memoDis.SetFocus;
  btnCopyToClipboard.Enabled := True;
end;


{ GENERATE DISASSEMBLY LISTING WITH LABELS, READY FOR ASSEMBLER }

{ Run through address range in two passes. On first, flag any addresses that
  are referenced in operands so that they can have labels assigned on the
  second pass. Then run through address range again, adding a label wherever
  required. If the machine in use has an array of 'standard' address
  definitions, say for a Monitor program, Game program, then these are
  checked and the appropriate label used where known }

procedure TDisassemblerForm.btnLabelledClick(Sender: TObject);
var
  PrevAddress: Word;
  CurrentByte: Byte;
  i: integer;
  LabelStr, MnemonicStr, OperandStr, CommentStr: string;
  WasInDataBlock: boolean;
  DisData: TDisassembledData;
  Temp: string;
begin
  if (not CheckAddressesOK(StartAddr, EndAddr)) then
    begin
      edStart.SetFocus;
      Exit;
    end;

  memoDis.Lines.Clear;

  case cbSpacings.ItemIndex of
    // Output string for [Label, Mnemonic, Operand, Comment]
    0: OutputStr := '%-13s %-5s %-16s %s';
    1: OutputStr := '%-19s %-5s %-23s %s';
  end;

  for i := 0 to $FFFF do
    AddressesUsed[i] := False;          // Set no addresses referenced
  AddressesUsed[StartAddr] := True;     // Ensure start line has label
  for i := 0 to Length(AddrDefsArray)-1 do
    AddrDefsArray[i].Used := False;     // and no defined addresses used

  // First pass, get addresses referenced
  CurrentAddr := StartAddr;
  WasInDataBlock := False;
  while (CurrentAddr <= EndAddr) do
    begin
      if (InDataBlock) then             // Only check program code
        Inc(CurrentAddr)
      else
        begin
          DisData := Machine.CPU.GetDisassembly(CurrentAddr);
          PrevAddress := CurrentAddr;
          if (DisData.HasOperand) then  // Replaceable operand?
            begin
              AddressesUsed[DisData.Operand] := True; // Flag addresses referenced
              CheckOperand(' ', DisData.Operand);     // and symbols used
            end;
          Inc(CurrentAddr, DisData.NumBytes);
          if (CurrentAddr = PrevAddress) then  // Stuck due to Invalid opcode?
            Inc(CurrentAddr);
        end;
    end;

  // Second pass, build listing
  DoHeading;
  CurrentAddr := StartAddr;
  while (CurrentAddr <= EndAddr) do
    begin
      // Check for line label
      Temp := CheckOperand(' ', CurrentAddr); // Symbol label?
      if (Temp <> '') then
        LabelStr := Temp
      else if (AddressesUsed[CurrentAddr]) then  // Other used addr label?
        LabelStr := Format('Lbl_%.4x', [CurrentAddr])
      else
        LabelStr := '';                 // No label

      CommentStr := '';                 // Default = no comment
      if (InDataBlock) then
        begin                           // If in a data section, just show byte
          CurrentByte := Machine.Memory[CurrentAddr];
          memoDis.Lines.Add(Format(OutputStr, [LabelStr,
                                               DefByteStr,
                                               Format('$%.2x', [CurrentByte]),
                                               '; ' + GetAscii(CurrentByte)]));
          Inc(CurrentAddr);
          WasInDataBlock := True;
          Continue;                     // Loop to next address
        end;

      PrevAddress := CurrentAddr;
      DisData := Machine.CPU.GetDisassembly(CurrentAddr);

      // MnemonicStr := Lowercase(DisData.MnemStr);
      MnemonicStr := DisData.MnemStr;
      OperandStr := '';
      if (DisData.NumBytes = 0) then    // Zero bytes -> invalid opcode
        begin
          MnemonicStr := DefByteStr;
          OperandStr := DisData.OperandStr;
          CommentStr := Format('; Invalid opcode ($%.4x)', [CurrentAddr]);
        end
      else
        begin
          OperandStr := DisData.OperandStr; // Set basic value returned
          if (DisData.HasOperand) then      // Can be replaced by label?
            begin
              // Only use label operands, if they are in range and valid labels
              if (InRange(DisData.Operand)) then
                OperandStr := Format('Lbl_%.4x', [DisData.Operand]);
              // Check if this operand is a predefined machine symbol
              Temp := CheckOperand(MnemonicStr, DisData.Operand);
              if (Temp <> '') then
                OperandStr := Temp;
            end;
        end;

      if (WasInDataBlock) then
        memoDis.Lines.Add('');          // Blank line after data section
      WasInDataBlock := False;

      OperandStr := Format(DisData.AddrModeStr, [OperandStr]);
      memoDis.Lines.Add(Format(OutputStr, [LabelStr,
                                           MnemonicStr,
                                           OperandStr,
                                           CommentStr]));
      if (DisData.AddBlankLine) then
        memoDis.Lines.Add('');          // Blank line after RTS/JMP like instructions

      Inc(CurrentAddr, DisData.NumBytes);
      if (CurrentAddr = PrevAddress) then  // Stuck due to Invalid opcode?
        Inc(CurrentAddr);
    end;
  memoDis.SelStart := 0;                  // Goto first line in output
  memoDis.SelLength := 0;
  memoDis.SetFocus;
  btnCopyToClipboard.Enabled := True;
end;


{ DO HEADING }

{ Initialise assembly output heading; time, type of CPU, address range, list
  of data sections, standard label defines, etc }

procedure TDisassemblerForm.DoHeading(IsSimple: boolean);
var
  i, Count: integer;
  TempStr: string;
begin
  SetLength(TempStr, 78);
  for i := 1 to 78 do
    TempStr[i] := '=';
  memoDis.Lines.Add('//' + TempStr);
  memoDis.Lines.Add('// Disassembly ' + FormatDateTime('dd mmm yyyy, hh:nn', Now));
  if (Length(edDesc.Text) > 0) then
    memoDis.Lines.Add('// Description: ' + edDesc.Text);
  memoDis.Lines.Add('//');
  memoDis.Lines.Add('// CPU: ' + Machine.CPU.Info.Name);
  memoDis.Lines.Add('//');
  memoDis.Lines.Add('// Address range:');
  memoDis.Lines.Add(Format('//   $%.4x - $%.4x', [StartAddr, EndAddr]));
  if Length(DataAddressArray) > 0 then
    begin
      memoDis.Lines.Add('//');
      memoDis.Lines.Add('// Data sections at:');
      for i := 0 to Length(DataAddressArray) -1 do
        memoDis.Lines.Add(Format('//   $%.4x - $%.4x', [DataAddressArray[i].StartAddr, DataAddressArray[i].EndAddr]));
    end;
  memoDis.Lines.Add('//');
  memoDis.Lines.Add('//' + TempStr);
  memoDis.Lines.Add('');

  if (IsSimple) then Exit;

  for i := 0 to Length(AddrDefsArray) - 1 do
    if AddrDefsArray[i].Used then
      memoDis.Lines.Add(Format(OutputStr, [AddrDefsArray[i].LabelStr,
                                           '=',
                                           Format('$%.4x', [AddrDefsArray[i].Address]),
                                           '']));
  memoDis.Lines.Add('');

  for i := 0 to Length(AddressesUsed) - 1 do
    if ( AddressesUsed[i] and (not InRange(i)) ) then
      begin
        Count := 0;
        repeat
          Inc(Count);
        until (AddressesUsed[i + Count] or ((i + Count) = Length(AddressesUsed)-1));
        memoDis.Lines.Add(Format(OutputStr, [Format('Lbl_%.4x', [i]),
                                             DefSpcStr,
                                             Format('%d', [Count]),
                                             '']));
      end;
  memoDis.Lines.Add('');

  memoDis.Lines.Add(Format(OutputStr, ['',
                                       'ORG',
                                       Format('$%.4x', [StartAddr]),
                                       '']));
  memoDis.Lines.Add('');
end;


{ CHECK IF ADDRESS HAS A KNOWN SYMBOL VALUE }

function TDisassemblerForm.CheckOperand(Mnemonic: string; OperandVal: word): string;
var
  i: integer;
  chRW: char;
begin
  Result := '';
  for i := 0 to Length(AddrDefsArray) - 1 do
    if AddrDefsArray[i].Address = OperandVal then
      begin
        chRW := AddrDefsArray[i].RW;
        // Need to check if read/write specific address
        if (chRW <> ' ') and (chRW <> Mnemonic[1]) then
          Continue;
        // Do not flag as used if in Start/End range, as it will be a label
        // and so appear twice in code
        if (not InRange(OperandVal)) then
          AddrDefsArray[i].Used := True;
        Result := AddrDefsArray[i].LabelStr;
        Break;
      end;
end;


{ LOAD / SAVE DISASSEMBLER SETTINGS; per session, in .DIS file }

const
  SECT_DISASSEMBLER = 'DIS';

  DIS_START         = 'Start';
  DIS_END           = 'End';
  DIS_DESCRIPTION   = 'Desc';
  DIS_NUM_DATA_ADDR = 'NumDataAddr';
  DIS_DATA_START    = 'DataStart';
  DIS_DATA_END      = 'DataEnd';

procedure TDisassemblerForm.btnSaveSettingsClick(Sender: TObject);
var
  NumDataAddr, i: integer;
  ThisIni: TAppIni;
begin
  if (SaveDialog1.Execute) then
  begin
    if FileExists(SaveDialog1.FileName) then
      if ( MessageQuery('Save Disassembler Settings', 'File exists. Overwrite?') ) then
        DeleteFile(SaveDialog1.FileName)
      else
        Exit;

    ThisIni := TAppIni.Create(SaveDialog1.FileName);
    try
      ThisIni.WriteInteger(SECT_DISASSEMBLER, DIS_START, StartAddr);
      ThisIni.WriteInteger(SECT_DISASSEMBLER, DIS_END, EndAddr);
      ThisIni.WriteString(SECT_DISASSEMBLER, DIS_DESCRIPTION, edDesc.Text);
      NumDataAddr := Length(DataAddressArray);
      ThisIni.WriteInteger(SECT_DISASSEMBLER, DIS_NUM_DATA_ADDR, Length(DataAddressArray));
      for i := 0 to (NumDataAddr - 1) do
        begin
          ThisIni.WriteInteger(SECT_DISASSEMBLER, DIS_DATA_START+IntToStr(i), DataAddressArray[i].StartAddr);
          ThisIni.WriteInteger(SECT_DISASSEMBLER, DIS_DATA_END+IntToStr(i), DataAddressArray[i].EndAddr);
        end;
    finally
      ThisIni.Free;
    end;
  end;
end;


procedure TDisassemblerForm.btnLoadSettingsClick(Sender: TObject);
var
  NumDataAddr, i: integer;     
  ThisIni: TAppIni;
begin
  OpenDialog1.Title := 'Load disassembler settings';
  { TODO : uDisassembler -> following causes memory leak on Mac, disabled for the moment }
  { Need to test under Windows }
  {$ifndef darwin}
  OpenDialog1.Filter := 'Disassembler files|*.dis';
  {$endif}
  if (OpenDialog1.Execute) then
    begin
      ThisIni := TAppIni.Create(OpenDialog1.FileName);
      try
        StartAddr := ThisIni.ReadInteger(SECT_DISASSEMBLER, DIS_START, 0);
        EndAddr := ThisIni.ReadInteger(SECT_DISASSEMBLER, DIS_END, 0);
        edStart.Text := Format('%.4x', [StartAddr]);
        edEnd.Text := Format('%.4x', [EndAddr]);
        edDesc.Text := ThisIni.ReadString(SECT_DISASSEMBLER, DIS_DESCRIPTION, '');

        NumDataAddr := ThisIni.ReadInteger(SECT_DISASSEMBLER, DIS_NUM_DATA_ADDR, 0);
        SetLength(DataAddressArray, NumDataAddr);
        lbDataAddresses.Items.Clear;
        for i := 0 to (NumDataAddr - 1) do
          begin
            DataAddressArray[i].StartAddr := ThisIni.ReadInteger(SECT_DISASSEMBLER, DIS_DATA_START+IntToStr(i), 0);
            DataAddressArray[i].EndAddr := ThisIni.ReadInteger(SECT_DISASSEMBLER, DIS_DATA_END+IntToStr(i), 0);
            lbDataAddresses.Items.Add(Format('$%.4x - $%.4x',
                   [DataAddressArray[i].StartAddr, DataAddressArray[i].EndAddr]));
          end;
      finally
        ThisIni.Free;
      end;
    end;
end;


{ COPY MEMO CONTENT TO CLIPBOARD }

procedure TDisassemblerForm.btnCopyToClipboardClick(Sender: TObject);
begin
  memoDis.SelectAll;
  memoDis.CopyToClipboard;
  memoDis.SelLength := 0;
end;


end.
