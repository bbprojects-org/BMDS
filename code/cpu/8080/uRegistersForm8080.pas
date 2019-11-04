{ ==============================================================================

  REGISTERS FRAME FOR 8080 CPU

    Displays all 8080 registers. Also shows bytes on the stack, and a
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

{ TODO : uRegistersForm8080 -> rework as inherited from TRegistersFrame }

unit uRegistersForm8080;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  //
  uCpuBase, uCpu8080, uDefs8080, uRegistersFormBase, uIniFile, uCommon;

type

  { TRegistersForm8080 }

  TRegistersForm8080 = class(TRegistersFormBase)
    cbHflag: TCheckBox;
    cbCflag: TCheckBox;
    Checkbox2: TCheckBox;
    cbPflag: TCheckBox;
    cbSflag: TCheckBox;
    cbZflag: TCheckBox;
    Checkbox3: TCheckBox;
    CheckBox1: TCheckBox;
    edA2: TEdit;
    edC: TLabeledEdit;
    edD: TLabeledEdit;
    edE: TLabeledEdit;
    edH: TLabeledEdit;
    edL: TLabeledEdit;
    edInstruction: TEdit;
    edPC: TLabeledEdit;
    edF: TLabeledEdit;
    edSP: TLabeledEdit;
    edSP2: TEdit;
    edA: TLabeledEdit;
    edB: TLabeledEdit;
    edB2: TEdit;
    edC2: TEdit;
    edD2: TEdit;
    edE2: TEdit;
    edH2: TEdit;
    edL2: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbNflagClick(Sender: TObject);
    procedure edRegisterExit(Sender: TObject);
  private
    fCPU: TCpu8080;
    procedure ShowStack;
  protected
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

procedure TRegistersForm8080.FormCreate(Sender: TObject);
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

procedure TRegistersForm8080.FormDestroy(Sender: TObject);
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

procedure TRegistersForm8080.FormShow(Sender: TObject);
begin
  Refresh;
end;


{ SET CPU }

procedure TRegistersForm8080.SetCPU(Value: TCpuBase);
begin
  fCPU := TCpu8080(Value);
end;


{ UPDATE REGISTERS }

procedure TRegistersForm8080.Refresh;
begin
  if Assigned(fCPU) then
    with fCPU.Regs do
      begin
        edPC.Text := Format('%.4X', [PC]);
        edInstruction.Text := fCPU.GetDisassembly(PC).Text;
        edA.Text := Format('%.2X', [A]);
        edA2.Text := Format('%d, ''%s'', %s', [A, GetAscii(A), GetBinary(A)]);
        edB.Text := Format('%.2X', [B]);
        edB2.Text := Format('%d, ''%s'', %s', [B, GetAscii(B), GetBinary(B)]);
        edC.Text := Format('%.2X', [C]);
        edC2.Text := Format('%d, ''%s'', %s', [C, GetAscii(C), GetBinary(C)]);
        edD.Text := Format('%.2X', [D]);
        edD2.Text := Format('%d, ''%s'', %s', [D, GetAscii(D), GetBinary(D)]);
        edE.Text := Format('%.2X', [E]);
        edE2.Text := Format('%d, ''%s'', %s', [E, GetAscii(E), GetBinary(E)]);
        edH.Text := Format('%.2X', [H]);
        edH2.Text := Format('%d, ''%s'', %s', [H, GetAscii(H), GetBinary(H)]);
        edL.Text := Format('%.2X', [L]);
        edL2.Text := Format('%d, ''%s'', %s', [L, GetAscii(L), GetBinary(L)]);
        edF.Text := Format('%.2X', [F]);
        edSP.Text := Format('%.4X', [SP]);
        cbSflag.Checked := ((F and FLAG_SIGN) > 0);
        cbZflag.Checked := ((F and FLAG_ZERO) > 0);
        cbHflag.Checked := ((F and FLAG_AUX_CARRY) > 0);
        cbPflag.Checked := ((F and FLAG_PARITY) > 0);
        cbCflag.Checked := ((F and FLAG_CARRY) > 0);
      end;
end;


{ SHOW STACK }

procedure TRegistersForm8080.ShowStack;
(*
var
  nIndex, nCount: word;
  sText: string;
*)
begin
  (*
  { TODO : Ensure correct bytes shown as stack gets deeper }
  sText := '';
    begin
      nCount := $FF - fCPU.Regs.SP;     // How many bytes on stack?
      if nCount > 15 then
        nCount := 15;
      for nIndex := 1 to nCount do
        sText := sText + Format('%.2X ', [fMemory[$200 - nIndex]]);
      edSP2.Text := sText;
    end;
  *)
end;


{ EDIT REGISTER EXIT }

procedure TRegistersForm8080.edRegisterExit(Sender: TObject);
begin
  with fCPU.Regs do
    begin
      case (Sender as TLabeledEdit).Tag of
         0: A := GetHex(edA.Text);
         1: B := GetHex(edB.Text);
         2: C := GetHex(edC.Text);
         3: D := GetHex(edD.Text);
         4: E := GetHex(edE.Text);
         5: H := GetHex(edH.Text);
         6: L := GetHex(edL.Text);
        99: PC := GetHex(edPC.Text);
      end;
    end;
  Refresh;
end;


procedure TRegistersForm8080.cbNflagClick(Sender: TObject);
begin
  with fCPU.Regs do
    begin
      if (Sender as TCheckBox).Checked then
        F := F or (Sender as TCheckBox).Tag
      else
        F := F and not((Sender as TCheckBox).Tag);
    end;
  Refresh;
end;


end.
