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

unit uRegistersFrame8080;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Math,
  //
  uRegistersFrameBase, uCpuBase, uCpu8080, uDefs8080, uCommon;

type

  { TRegistersFrame8080 }

  TRegistersFrame8080 = class(TRegistersFrame)
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
    procedure cbNflagClick(Sender: TObject);
    procedure edRegisterExit(Sender: TObject);
  private
    fCPURef: TCpu8080;
    procedure ShowStack;
  public                      
    procedure Initialise; override;
    procedure Refresh; override;
    //
    property CpuRef: TCpu8080 read fCpuRef write fCpuRef;
  end;


implementation

{$R *.lfm}

procedure TRegistersFrame8080.Initialise;
begin
  fStackAssigned := -1;                 // Stack pointer not yet assigned
end;


{ UPDATE REGISTER DISPLAY }

procedure TRegistersFrame8080.Refresh;
var
  ThisRegs: TRegs8080;
begin
  if Assigned(fCpuRef) then
    begin
        ThisRegs := fCpuRef.Regs;
        edPC.Text := Format('%.4X', [ThisRegs.PC]);
        edInstruction.Text := fCpuRef.GetDisassembly(ThisRegs.PC).Text;
        edA.Text := Format('%.2X', [ThisRegs.A]);
        edA2.Text := Format('%d, ''%s'', %s', [ThisRegs.A, GetAscii(ThisRegs.A), GetBinary(ThisRegs.A)]);
        edB.Text := Format('%.2X', [ThisRegs.B]);
        edB2.Text := Format('%d, ''%s'', %s', [ThisRegs.B, GetAscii(ThisRegs.B), GetBinary(ThisRegs.B)]);
        edC.Text := Format('%.2X', [ThisRegs.C]);
        edC2.Text := Format('%d, ''%s'', %s', [ThisRegs.C, GetAscii(ThisRegs.C), GetBinary(ThisRegs.C)]);
        edD.Text := Format('%.2X', [ThisRegs.D]);
        edD2.Text := Format('%d, ''%s'', %s', [ThisRegs.D, GetAscii(ThisRegs.D), GetBinary(ThisRegs.D)]);
        edE.Text := Format('%.2X', [ThisRegs.E]);
        edE2.Text := Format('%d, ''%s'', %s', [ThisRegs.E, GetAscii(ThisRegs.E), GetBinary(ThisRegs.E)]);
        edH.Text := Format('%.2X', [ThisRegs.H]);
        edH2.Text := Format('%d, ''%s'', %s', [ThisRegs.H, GetAscii(ThisRegs.H), GetBinary(ThisRegs.H)]);
        edL.Text := Format('%.2X', [ThisRegs.L]);
        edL2.Text := Format('%d, ''%s'', %s', [ThisRegs.L, GetAscii(ThisRegs.L), GetBinary(ThisRegs.L)]);
        edF.Text := Format('%.2X', [ThisRegs.F]);
        edSP.Text := Format('%.4X', [ThisRegs.SP]);
        ShowStack;
        cbSflag.Checked := ((ThisRegs.F and FLAG_SIGN) > 0);
        cbZflag.Checked := ((ThisRegs.F and FLAG_ZERO) > 0);
        cbHflag.Checked := ((ThisRegs.F and FLAG_AUX_CARRY) > 0);
        cbPflag.Checked := ((ThisRegs.F and FLAG_PARITY) > 0);
        cbCflag.Checked := ((ThisRegs.F and FLAG_CARRY) > 0);
    end;
end;


{ SHOW STACK }

procedure TRegistersFrame8080.ShowStack;
const
  MAX_BYTES = 8;
var
  nIndex, nCount: word;
  sText: string;
begin
  sText := '';
  if (fStackAssigned <> -1) then                  // Has stack pointer been set?
    begin
      nCount := fStackAssigned - fCpuRef.Regs.SP; // How many bytes on stack?
      for nIndex := 1 to Min(nCount, MAX_BYTES) do
        sText := sText + Format('%.2X ', [fMemoryRef[fCpuRef.Regs.SP + nIndex - 1]]);
      if (nCount > MAX_BYTES) then
        sText := sText + '...';
    end;
  edSP2.Text := sText;
end;


{ EDIT REGISTER EXIT }

procedure TRegistersFrame8080.edRegisterExit(Sender: TObject);
var
  ThisRegs: TRegs8080;
begin
  if Assigned(fCpuRef) then
    begin
      ThisRegs := fCpuRef.Regs;
      case (Sender as TLabeledEdit).Tag of
         0: ThisRegs.A := GetHex(edA.Text);
         1: ThisRegs.B := GetHex(edB.Text);
         2: ThisRegs.C := GetHex(edC.Text);
         3: ThisRegs.D := GetHex(edD.Text);
         4: ThisRegs.E := GetHex(edE.Text);
         5: ThisRegs.H := GetHex(edH.Text);
         6: ThisRegs.L := GetHex(edL.Text);
        99: ThisRegs.PC := GetHex(edPC.Text);
      end;
      fCpuRef.Regs := ThisRegs;
      Refresh;
    end;
end;


procedure TRegistersFrame8080.cbNflagClick(Sender: TObject);
var
  ThisRegs: TRegs8080;
begin
  if Assigned(fCpuRef) then
    begin
      ThisRegs := fCpuRef.Regs;
      if (Sender as TCheckBox).Checked then
        ThisRegs.F := ThisRegs.F or (Sender as TCheckBox).Tag
      else
        ThisRegs.F := ThisRegs.F and not((Sender as TCheckBox).Tag);
      fCpuRef.Regs := ThisRegs;
      Refresh;
    end;
end;


end.
