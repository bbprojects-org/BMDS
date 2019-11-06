{ ==============================================================================

  GET HEX ADDRESS FORM

    Get start memory address in Hex. Used by the memory display form
    Saves form position in application's INI file


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

unit uGetAddrForm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  //
  uIniFile, uCommon;

type

  { TGetAddressBox }

  TGetAddressBox = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    edAddress: TEdit;
    Label1: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fAddress: word;
    procedure SetAddress(Value: word);
  public
    property Address: word read fAddress write SetAddress;
  end;



implementation

{$R *.lfm}

const
  SECT_INI = 'MemAddrForm';


procedure TGetAddressBox.FormCreate(Sender: TObject);
begin
  Top := AppIni.ReadInteger(SECT_INI, INI_WDW_TOP, 20);
  Left := AppIni.ReadInteger(SECT_INI, INI_WDW_LEFT, 20);
end;


procedure TGetAddressBox.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_INI, INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_INI, INI_WDW_LEFT, Left);
end;


procedure TGetAddressBox.FormShow(Sender: TObject);
begin
  edAddress.SetFocus;
  edAddress.SelectAll;
end;


procedure TGetAddressBox.SetAddress(Value: word);
begin
  edAddress.Text := Format('%.4x', [Value]);
  fAddress := Value;
end;


procedure TGetAddressBox.btnOkClick(Sender: TObject);
begin
  fAddress := GetHex(edAddress.Text);
end;


end.
