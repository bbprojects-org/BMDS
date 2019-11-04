{ ==============================================================================

  REGISTERS FORM FOR CHIP-8 CPU

    Displays all CHIP8 registers. Also shows bytes on the stack, and a
    disassembly of the next opcode to be executed


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
                                                           
{ TODO : uRegistersFormChip8 -> rework as inherited from TRegistersFrame }

{ TODO : uRegistersFormChip8 -> adjust start position to line up with other form default settings }

{ TODO : uRegistersFormChip8 -> show stack: ensure correct bytes shown as stack gets deeper }

{ TODO : uRegistersFormChip8 -> add register display in ASCII, Binary }

unit uRegistersFormChip8;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Math,
  //
  uCpuBase, uCpuChip8, uDefsChip8, uRegistersFormBase, uIniFile, uCommon;

type

  { TRegistersFormChip8 }

  TRegistersFormChip8 = class(TRegistersFormBase)
    edA3: TEdit;
    edI: TLabeledEdit;
    edV0: TLabeledEdit;
    edA2: TEdit;
    edInstruction: TEdit;
    edPC: TLabeledEdit;
    edSP: TLabeledEdit;
    edSP2: TEdit;
    edV1: TLabeledEdit;
    edVA: TLabeledEdit;
    edVC: TLabeledEdit;
    edVB: TLabeledEdit;
    edVD: TLabeledEdit;
    edVE: TLabeledEdit;
    edVF: TLabeledEdit;
    edV3: TLabeledEdit;
    edV4: TLabeledEdit;
    edV5: TLabeledEdit;
    edV6: TLabeledEdit;
    edV7: TLabeledEdit;
    edV8: TLabeledEdit;
    edV9: TLabeledEdit;
    edX2: TEdit;
    edV2: TLabeledEdit;
    edX3: TEdit;
    edY10: TEdit;
    edY11: TEdit;
    edY12: TEdit;
    edY13: TEdit;
    edY2: TEdit;
    edY3: TEdit;
    edY4: TEdit;
    edY5: TEdit;
    edY6: TEdit;
    edY7: TEdit;
    edY8: TEdit;
    edY9: TEdit;
    GroupBox1: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbNflagClick(Sender: TObject);
    procedure edRegisterExit(Sender: TObject);
  private
    procedure ShowStack;
  protected
    fCPU: TCpuChip8;
    procedure SetCPU(Value: TCpuBase); override;
  public
    procedure Refresh; override;
  end;


implementation

uses
  uMainForm;

{$R *.lfm}

const
  INI_PREFIX = 'Reg';

{ FORM CREATE }

procedure TRegistersFormChip8.FormCreate(Sender: TObject);
var
  IniSect: string;
begin
(*
  IniSect := GetIniSect;
  Top := IniReadInteger(IniSect, INI_PREFIX + WDW_TOP,
         MainForm.Machine.ScreenPosition.Y - TOP_BAR_HEIGHT);
  Left := IniReadInteger(IniSect, INI_PREFIX + WDW_LEFT,
          MainForm.Machine.ScreenPosition.X + MainForm.Machine.ScreenSize.X);
*)
end;


{ FORM DESTROY }

procedure TRegistersFormChip8.FormDestroy(Sender: TObject);
var
  IniSect: string;
begin
(*
  IniSect := GetIniSect;
  IniWriteInteger(IniSect, INI_PREFIX + WDW_TOP, Top);
  IniWriteInteger(IniSect, INI_PREFIX + WDW_LEFT, Left);
*)
end;


{ FORM SHOW }

procedure TRegistersFormChip8.FormShow(Sender: TObject);
begin
  Refresh;
end;


{ SET CPU }

procedure TRegistersFormChip8.SetCPU(Value: TCpuBase);
begin
  fCPU := TCpuChip8(Value);
end;


{ UPDATE REGISTER DISPLAY }

procedure TRegistersFormChip8.Refresh;
begin
  if Assigned(fCPU) then
  with fCPU.Regs do
  begin
    edPC.Text := Format('%.4X', [PC]);
    edInstruction.Text := Format('%.4X', [(fMemory[PC] shl 8) or fMemory[PC+1]]);
    edV0.Text := Format('%.2X', [V[$0]]);
    edV1.Text := Format('%.2X', [V[$1]]);
    edV2.Text := Format('%.2X', [V[$2]]);
    edV3.Text := Format('%.2X', [V[$3]]);
    edV4.Text := Format('%.2X', [V[$4]]);
    edV5.Text := Format('%.2X', [V[$5]]);
    edV6.Text := Format('%.2X', [V[$6]]);
    edV7.Text := Format('%.2X', [V[$7]]);
    edV8.Text := Format('%.2X', [V[$8]]);
    edV9.Text := Format('%.2X', [V[$9]]);
    edVA.Text := Format('%.2X', [V[$A]]);
    edVB.Text := Format('%.2X', [V[$B]]);
    edVC.Text := Format('%.2X', [V[$C]]);
    edVD.Text := Format('%.2X', [V[$D]]);
    edVE.Text := Format('%.2X', [V[$E]]);
    edVF.Text := Format('%.2X', [V[$F]]);
    edSP.Text := Format('%.2X', [SP]);
    edI.Text := Format('%.4X', [I]);

    {
    edX2.Text := Format('%d, ''%s'', %s', [X, GetAscii(X), GetBinary(X)]);
    edV2.Text := Format('%.2X', [Y]);
    edY2.Text := Format('%d, ''%s'', %s', [Y, GetAscii(Y), GetBinary(Y)]);
    }
    edInstruction.Text := fCPU.GetDisassembly(PC).Text;
  end;
end;


{ SHOW STACK }

procedure TRegistersFormChip8.ShowStack;
var
  nIndex, nCount: word;
  sText: string;
begin
  sText := '';
  nCount := $FF - fCPU.Regs.SP;     // How many bytes on stack?
  nCount := Min(nCount, 15);
  for nIndex := 1 to nCount do
    sText := sText + Format('%.2X ', [fMemory[$200 - nIndex]]);
  edSP2.Text := sText;
end;


procedure TRegistersFormChip8.edRegisterExit(Sender: TObject);
var
  tempRegs: TRegsChip8;
begin
  tempRegs := fCPU.Regs;
  case (Sender as TEdit).Tag of
    $0: tempRegs.V[$0] := GetHex(edV0.Text);
    $1: tempRegs.V[$1] := GetHex(edV1.Text);
    $2: tempRegs.V[$2] := GetHex(edV2.Text);
    $3: tempRegs.V[$3] := GetHex(edV3.Text);
    $4: tempRegs.V[$4] := GetHex(edV4.Text);
    $5: tempRegs.V[$5] := GetHex(edV5.Text);
    $6: tempRegs.V[$6] := GetHex(edV6.Text);
    $7: tempRegs.V[$7] := GetHex(edV7.Text);
    $8: tempRegs.V[$8] := GetHex(edV8.Text);
    $9: tempRegs.V[$9] := GetHex(edV9.Text);
    $A: tempRegs.V[$A] := GetHex(edVA.Text);
    $B: tempRegs.V[$B] := GetHex(edVB.Text);
    $C: tempRegs.V[$C] := GetHex(edVC.Text);
    $D: tempRegs.V[$D] := GetHex(edVD.Text);
    $E: tempRegs.V[$E] := GetHex(edVE.Text);
    $F: tempRegs.V[$F] := GetHex(edVF.Text);
    //
    16: tempRegs.PC := GetHex(edPC.Text);
    17: tempRegs.I  := GetHex(edI.Text);
    18: tempRegs.SP := GetHex(edSP.Text);
  end;
  fCPU.Regs := tempRegs;
  Refresh;
end;


procedure TRegistersFormChip8.cbNflagClick(Sender: TObject);
var
  tempRegs: TRegsChip8;
begin
  tempRegs := fCPU.Regs;
  (*
  if (Sender as TCheckBox).Checked then
    PSW := PSW or (Sender as TCheckBox).Tag
  else
    PSW := PSW and not((Sender as TCheckBox).Tag);
  *)
  Refresh;
end;


end.
