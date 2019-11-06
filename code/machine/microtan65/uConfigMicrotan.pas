{ ==============================================================================

  CONFIG FRAME FOR MICROTAN 65

    Permits choice of monitor version (TANBUG/XBUG) and screen colour.
    Saves settings in the application's INI file


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

unit uConfigMicrotan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  //
  uMachineConfigBase, uIniFile, uCommon;

type

  { TConfigMicrotan }

  TConfigMicrotan = class(TMachineConfigFrame)
    btnResetDefaults: TButton;
    Label1: TLabel;
    rgColour: TRadioGroup;
    rgRom: TRadioGroup;
    procedure btnResetDefaultsClick(Sender: TObject);
    procedure rgColourClick(Sender: TObject);
    procedure rgRomClick(Sender: TObject);
  private
    function GetRomIndex: integer;
    function GetColourIndex: integer;
    procedure ReadIniItems;
  public
    procedure Init(OnChange: TConfigChange); override;
    destructor Destroy; override;
    //
    property RomIndex: integer read GetRomIndex;
    property ColourIndex: integer read GetColourIndex;
  end;


implementation

{$R *.lfm}        

const
  INI_ROM    = 'CfgROM';
  INI_COLOUR = 'CfgColour';


{ INITIALISE CONFIG FRAME... read settings }

procedure TConfigMicrotan.Init(OnChange: TConfigChange);
begin
  ReadIniItems;
  fOnChange := OnChange;                // Must be after INI items changed
end;


procedure TConfigMicrotan.ReadIniItems;
begin
  rgROM.ItemIndex := AppIni.ReadInteger(SECT_CUSTOM, INI_ROM, 0);
  rgColour.ItemIndex := AppIni.ReadInteger(SECT_CUSTOM, INI_COLOUR, 0);
end;


{ DESTROY }

destructor TConfigMicrotan.Destroy;
begin
  AppIni.WriteInteger(SECT_CUSTOM, INI_ROM, rgRom.ItemIndex);
  AppIni.WriteInteger(SECT_CUSTOM, INI_COLOUR, rgColour.ItemIndex);
  inherited Destroy;
end;


{ GET OPTION STATES }

function TConfigMicrotan.GetRomIndex: integer;
begin
  Result := rgRom.ItemIndex;
end;


function TConfigMicrotan.GetColourIndex: integer;
begin
  Result := rgColour.ItemIndex;
end;


{ ON CLICK... notify machine of change }

procedure TConfigMicrotan.rgRomClick(Sender: TObject);
begin
  // Cannot change ROM dynamically, need to rerun the program (or change machine and back)
end;


procedure TConfigMicrotan.rgColourClick(Sender: TObject);
begin
  if Assigned(fOnChange) then
    fOnChange(1);
end;


{ RESET DEFAULTS }

procedure TConfigMicrotan.btnResetDefaultsClick(Sender: TObject);
begin
  if (ConfirmResetDefault) then
    begin
      AppIni.DeleteKey(SECT_CUSTOM, INI_ROM);
      AppIni.DeleteKey(SECT_CUSTOM, INI_COLOUR);
      ReadIniItems;
    end;
end;


end.

