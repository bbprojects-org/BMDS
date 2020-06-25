{ ==============================================================================

  REGISTERS FORM FOR CHIP-8 PROCESSOR

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
                                                           
{ TODO : uRegistersFormChip8 -> show stack: ensure correct bytes shown as stack gets deeper }
{ TODO : uRegistersFormChip8 -> add register display in ASCII, Binary }

unit uRegistersFrameChip8;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Math,
  //
  uCpuBase, uCpuChip8, uDefsChip8, uRegistersFrameBase, uIniFile, uCommon;

type

  { TRegistersFrameChip8 }

  TRegistersFrameChip8 = class(TRegistersFrame)
    ed8: TEdit;
    edI: TLabeledEdit;
    edV0: TLabeledEdit;
    ed0: TEdit;
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
    ed1: TEdit;
    edV2: TLabeledEdit;
    ed9: TEdit;
    edC: TEdit;
    edD: TEdit;
    edE: TEdit;
    edF: TEdit;
    ed2: TEdit;
    ed3: TEdit;
    ed4: TEdit;
    ed5: TEdit;
    ed6: TEdit;
    ed7: TEdit;
    edA: TEdit;
    edB: TEdit;
    GroupBox1: TGroupBox;
    procedure edRegisterExit(Sender: TObject);
  private
    fCpuRef: TCpuChip8;
    procedure ShowStack;
  public
    procedure Initialise; override;
    procedure Refresh; override;
    //
    property CpuRef: TCpuChip8 read fCpuRef write fCpuRef;
  end;


implementation

{$R *.lfm}

procedure TRegistersFrameChip8.Initialise;
begin
  fStackAssigned := -1;                 // Stack pointer not yet assigned
end;



{ UPDATE REGISTER DISPLAY }

procedure TRegistersFrameChip8.Refresh;
var
  ThisRegs: TRegsChip8;
  StatusFlags: byte;
begin
  if Assigned(fCpuRef) then
    with fCpuRef.Regs do
      begin
        edPC.Text := Format('%.4X', [PC]);
        edInstruction.Text := fCpuRef.GetDisassembly(PC).Text;

        edV0.Text := Format('%.2X', [V[$0]]);
        ed0.Text  := Format('%d, ''%s''', [V[$0], GetAscii(V[$0])]);
        edV1.Text := Format('%.2X', [V[$1]]);
        ed1.Text  := Format('%d, ''%s''', [V[$1], GetAscii(V[$1])]);
        edV2.Text := Format('%.2X', [V[$2]]);
        ed2.Text  := Format('%d, ''%s''', [V[$2], GetAscii(V[$2])]);
        edV3.Text := Format('%.2X', [V[$3]]);
        ed3.Text  := Format('%d, ''%s''', [V[$3], GetAscii(V[$3])]);
        edV4.Text := Format('%.2X', [V[$4]]);
        ed4.Text  := Format('%d, ''%s''', [V[$4], GetAscii(V[$4])]);
        edV5.Text := Format('%.2X', [V[$5]]);
        ed5.Text  := Format('%d, ''%s''', [V[$5], GetAscii(V[$5])]);
        edV6.Text := Format('%.2X', [V[$6]]);
        ed6.Text  := Format('%d, ''%s''', [V[$6], GetAscii(V[$6])]);
        edV7.Text := Format('%.2X', [V[$7]]);
        ed7.Text  := Format('%d, ''%s''', [V[$7], GetAscii(V[$7])]);
        edV8.Text := Format('%.2X', [V[$8]]);
        ed8.Text  := Format('%d, ''%s''', [V[$8], GetAscii(V[$8])]);
        edV9.Text := Format('%.2X', [V[$9]]);
        ed9.Text  := Format('%d, ''%s''', [V[$9], GetAscii(V[$9])]);
        edVA.Text := Format('%.2X', [V[$A]]);
        edA.Text  := Format('%d, ''%s''', [V[$A], GetAscii(V[$A])]);
        edVB.Text := Format('%.2X', [V[$B]]);
        edB.Text  := Format('%d, ''%s''', [V[$B], GetAscii(V[$B])]);
        edVC.Text := Format('%.2X', [V[$C]]);
        edC.Text  := Format('%d, ''%s''', [V[$C], GetAscii(V[$C])]);
        edVD.Text := Format('%.2X', [V[$D]]);
        edD.Text  := Format('%d, ''%s''', [V[$D], GetAscii(V[$D])]);
        edVE.Text := Format('%.2X', [V[$E]]);
        edE.Text  := Format('%d, ''%s''', [V[$E], GetAscii(V[$E])]);
        edVF.Text := Format('%.2X', [V[$F]]);
        edF.Text  := Format('%d, ''%s''', [V[$F], GetAscii(V[$F])]);

        edI.Text  := Format('%.4X', [I]);
        edSP.Text := Format('%.2X', [SP]);
        ShowStack;
      end;
end; 


{ SHOW STACK }

procedure TRegistersFrameChip8.ShowStack;
const
  MAX_WORDS = 5;
var
  tmp, nIndex: word;
  sText: string;
begin
  sText := '';
  tmp := fCpuRef.Regs.SP;
  for nIndex := 1 to Min(fCpuRef.Regs.SP, MAX_WORDS) do
    sText := sText + Format('%.4X ', [fCpuRef.Stack[fCpuRef.Regs.SP - 1]]);
  if (fCpuRef.Regs.SP > MAX_WORDS) then
    sText := sText + '...';
  edSP2.Text := sText;
end;


procedure TRegistersFrameChip8.edRegisterExit(Sender: TObject);
var
  tempRegs: TRegsChip8;
begin
  if Assigned(fCpuRef) then
    begin
      tempRegs := fCpuRef.Regs;
      case (Sender as TLabeledEdit).Tag of
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
      fCpuRef.Regs := tempRegs;
      Refresh;
    end;
end;


end.
