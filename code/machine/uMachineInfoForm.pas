{ ==============================================================================

  MACHINE INFORMATION FORM

    A simple form that shows the current machine's information text


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

unit uMachineInfoForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  //
  uMachineBase;

type
  TMachineInfoForm = class(TForm)
    memoInfo: TMemo;              
    btnOK: TButton;
  private
    fMachine: TMachineBase;
    procedure SetMachine(const Value: TMachineBase);
  public
    procedure AddText(aText: string);
    //
    property Machine: TMachineBase write SetMachine;
  end;


implementation

{$R *.lfm}

procedure TMachineInfoForm.SetMachine(const Value: TMachineBase);
begin
  fMachine := Value;
  memoInfo.Text := fMachine.Description;
end;


procedure TMachineInfoForm.AddText(aText: string);
begin
  memoInfo.Lines.Add(aText);
end;


end.

