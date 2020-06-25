{ ==============================================================================

  REGISTERS FRAME FOR 6502 PROCESSOR


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

{ TODO : uRegistersFrame6502 -> BUG: not updating registers properly on Step (6502 or all?) }
{ TODO : uRegistersFrame6502 -> BUG: editing registers not working properly? }

unit uRegistersFrame6502;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Math,
  //
  uRegistersFrameBase, uCpuBase, uCpu6502, uDefs6502, uCommon;

type

  { TRegistersFrame6502 }

  TRegistersFrame6502 = class(TRegistersFrame)
    cbBflag: TCheckBox;
    cbCflag: TCheckBox;
    cbDflag: TCheckBox;
    cbIflag: TCheckBox;
    cbNflag: TCheckBox;
    cbVflag: TCheckBox;
    cbZflag: TCheckBox;
    CheckBox1: TCheckBox;
    edA: TLabeledEdit;
    edA2: TEdit;
    edInstruction: TEdit;
    edPC: TLabeledEdit;
    edPSW: TLabeledEdit;
    edSP: TLabeledEdit;
    edSP2: TEdit;
    edX: TLabeledEdit;
    edX2: TEdit;
    edY: TLabeledEdit;
    edY2: TEdit;
    Label1: TLabel;
    procedure cbNflagClick(Sender: TObject);
    procedure edRegisterExit(Sender: TObject);
  private
    fCpuRef: TCpu6502;
    procedure ShowStack;
  public                       
    procedure Initialise; override;
    procedure Refresh; override;
    //
    property CpuRef: TCpu6502 read fCpuRef write fCpuRef;
  end;

implementation

{$R *.lfm}


procedure TRegistersFrame6502.Initialise;
begin
  fStackAssigned := -1;                 // Stack pointer not yet assigned
end;


{ UPDATE REGISTER DISPLAY }

procedure TRegistersFrame6502.Refresh;
var
  ThisRegs: TRegs6502;
  StatusFlags: byte;
begin
  if Assigned(fCpuRef) then
    begin
      ThisRegs := fCpuRef.Regs;
      StatusFlags := ThisRegs.PSW;

      edPC.Text := Format('%.4X', [ThisRegs.PC]);
      edInstruction.Text := fCpuRef.GetDisassembly(ThisRegs.PC).Text;
      edA.Text := Format('%.2X', [ThisRegs.A]);
      edA2.Text := Format('%d, ''%s'', %s', [ThisRegs.A, GetAscii(ThisRegs.A), GetBinary(ThisRegs.A)]);
      edX.Text := Format('%.2X', [ThisRegs.X]);
      edX2.Text := Format('%d, ''%s'', %s', [ThisRegs.X, GetAscii(ThisRegs.X), GetBinary(ThisRegs.X)]);
      edY.Text := Format('%.2X', [ThisRegs.Y]);
      edY2.Text := Format('%d, ''%s'', %s', [ThisRegs.Y, GetAscii(ThisRegs.Y), GetBinary(ThisRegs.Y)]);
      edSP.Text := Format('%.2X', [ThisRegs.SP]);
      ShowStack;
      edPSW.Text := Format('%.2X', [StatusFlags]);
      cbNflag.Checked := ((StatusFlags and P_NEGATIVE) > 0);
      cbVflag.Checked := ((StatusFlags and P_OVERFLOW) > 0);
      cbBflag.Checked := ((StatusFlags and P_BRK) > 0);
      cbDflag.Checked := ((StatusFlags and P_DECIMAL) > 0);
      cbIflag.Checked := ((StatusFlags and P_IRQ_DISABLED) > 0);
      cbZflag.Checked := ((StatusFlags and P_ZERO) > 0);
      cbCflag.Checked := ((StatusFlags and P_CARRY) > 0);
    end;
end;


{ SHOW STACK }

procedure TRegistersFrame6502.ShowStack;
const
  MAX_BYTES = 9;
var
  nIndex, nCount: word;
  sText: string;
begin
  sText := '';
  if (fStackAssigned <> -1) then                  // Has stack pointer been set?
    begin
      nCount := fStackAssigned - fCpuRef.Regs.SP; // How many bytes on stack?
      for nIndex := 1 to Min(nCount, MAX_BYTES) do
        sText := sText + Format('%.2X ', [fMemoryRef[$100 + fCpuRef.Regs.SP + nIndex]]);
      if (nCount > MAX_BYTES) then
        sText := sText + '...';
    end;
  edSP2.Text := sText;
end;


{ EDIT REGISTER EXIT }

procedure TRegistersFrame6502.edRegisterExit(Sender: TObject);
var
  ThisRegs: TRegs6502;
begin
  if Assigned(fCpuRef) then
    begin
      ThisRegs := fCpuRef.Regs;
        case (Sender as TLabeledEdit).Tag of
          0: ThisRegs.A := GetHex(edA.Text);
          1: ThisRegs.X := GetHex(edX.Text);
          2: ThisRegs.Y := GetHex(edY.Text);
          3: ThisRegs.PSW := GetHex(edPSW.Text);
          4: ThisRegs.SP := GetHex(edSP.Text);
          5: ThisRegs.PC := GetHex(edPC.Text);
        end;
      fCpuRef.Regs := ThisRegs;
      Refresh;
    end;
end;


{ STATUS FLAG CLICK }

{ Each checkbox has its Tag set to the relevant bit in the PSW. This is
  used to set / reset the actual PSW register itself }

procedure TRegistersFrame6502.cbNflagClick(Sender: TObject);
var
  ThisRegs: TRegs6502;
begin
  if Assigned(fCpuRef) then
    begin
      ThisRegs := fCpuRef.Regs;
      if (Sender as TCheckBox).Checked then
        ThisRegs.PSW := ThisRegs.PSW or (Sender as TCheckBox).Tag
      else
        ThisRegs.PSW := ThisRegs.PSW and not((Sender as TCheckBox).Tag);
      fCpuRef.Regs := ThisRegs;
      Refresh;
    end;
end;


end.

