{ ==============================================================================

  CONFIG FRAME FOR SPACE INVADERS

    Provides form to be shown in Machine Config tab of the main preferences
    permitting user to amend settings pertinent to the Space Invaders machine.
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

{ TODO : uConfigSI -> fix reset defaults button }

unit uConfigSpaceInvaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uMachineConfigBase, uIniFile, uCommon;

type

  { TConfigSpaceInvaders }

  TConfigSpaceInvaders = class(TMachineConfigFrame)
    btnResetDefaults: TButton;
    cbColourFilters: TCheckBox;
    gbDisplay: TGroupBox;
    rgBonusPoints: TRadioGroup;
    rgNumberBases: TRadioGroup;
    procedure btnResetDefaultsClick(Sender: TObject);
  private
    procedure ReadIniItems;
    function GetColourFilters: boolean;
    function GetNumberBases: integer;
    function GetBonusPoints: integer;
  public    
    procedure Init; override;
    destructor Destroy; override;
    //
    property ColourFilters: boolean read GetColourFilters;
    property NumberBases: integer read GetNumberBases;
    property BonusPoints: integer read GetBonusPoints;
  end;


implementation

{$R *.lfm}

const
  INI_COLOUR = 'CfgColour';
  INI_BASES  = 'CfgBases';
  INI_BONUS  = 'CfgBonus';


{ INITIALISE CONFIG FRAME... read settings }

procedure TConfigSpaceInvaders.Init;
begin
  ReadIniItems;
end;


procedure TConfigSpaceInvaders.ReadIniItems;
begin
  cbColourFilters.Checked := AppIni.ReadBool(SECT_CUSTOM, INI_COLOUR, True);
  rgNumberBases.ItemIndex := AppIni.ReadInteger(SECT_CUSTOM, INI_BASES, 0);
  rgBonusPoints.ItemIndex := AppIni.ReadInteger(SECT_CUSTOM, INI_BONUS, 0);
end;


{ DESTROY }

destructor TConfigSpaceInvaders.Destroy;
begin
  AppIni.WriteBool(SECT_CUSTOM, INI_COLOUR, cbColourFilters.Checked);
  AppIni.WriteInteger(SECT_CUSTOM, INI_BASES, rgNumberBases.ItemIndex);
  AppIni.WriteInteger(SECT_CUSTOM, INI_BONUS, rgBonusPoints.ItemIndex);
  inherited Destroy;
end;


{ GET OPTION STATES }

function TConfigSpaceInvaders.GetColourFilters: boolean;
begin
  Result := cbColourFilters.Checked;
end;


function TConfigSpaceInvaders.GetNumberBases: integer;
begin
  Result := rgNumberBases.ItemIndex;
end;


function TConfigSpaceInvaders.GetBonusPoints: integer;
begin
  Result := rgBonusPoints.ItemIndex;
end;


{ RESET DEFAULTS }

procedure TConfigSpaceInvaders.btnResetDefaultsClick(Sender: TObject);
begin
  if (ConfirmResetDefault) then
    begin
      AppIni.DeleteKey(SECT_CUSTOM, INI_COLOUR);
      AppIni.DeleteKey(SECT_CUSTOM, INI_BASES);    
      AppIni.DeleteKey(SECT_CUSTOM, INI_BONUS);
      ReadIniItems;
    end;
end;


end.

