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
  uMachineBase, uIniFile;

type
  
  { TMachineInfoForm }

  TMachineInfoForm = class(TForm)
    memoInfo: TMemo;              
    btnOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
  public
    procedure SetPosition(x,y: integer);
  end;

const
  SECT_MIF = 'MachInfoForm';            // INI file settings


implementation

{$R *.lfm}

{ CREATE }

procedure TMachineInfoForm.FormCreate(Sender: TObject);
begin
  Left := AppIni.ReadInteger(SECT_MIF, INI_WDW_LEFT, -1);
  Top := AppIni.ReadInteger(SECT_MIF, INI_WDW_TOP, -1);
  Width := AppIni.ReadInteger(SECT_MIF, INI_WDW_WIDTH, 640);
  Height := AppIni.ReadInteger(SECT_MIF, INI_WDW_HEIGHT, 480);
  //
  memoInfo.Text := Machine.Description;
end;


{ DESTROY }

procedure TMachineInfoForm.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_MIF, INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_MIF, INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_MIF, INI_WDW_WIDTH, Width);
  AppIni.WriteInteger(SECT_MIF, INI_WDW_HEIGHT, Height);
end;


procedure TMachineInfoForm.btnOKClick(Sender: TObject);
begin
  Close;
end;


{ SET POSITION }

procedure TMachineInfoForm.SetPosition(x, y: integer);
begin
  if (Left = -1) then                   // Only set first time
    begin
      Left := x;
      Top := y;
    end;
end;


end.

