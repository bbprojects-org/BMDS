{ ==============================================================================

  PREFERENCES FRAME FOR SPACE INVADERS

    Permits choice of screen colour and other pertinent settings.
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

unit uPrefsSpaceInvaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uPrefsFrameBase, uIniFile, uCommon;

type

  { TSIPrefsFrame }

  TSIPrefsFrame = class(TPrefsFrame)
    cbColourFilters: TCheckBox;
    gbDisplay: TGroupBox;
    rgBonusPoints: TRadioGroup;
    rgNumberBases: TRadioGroup;
    procedure btnResetDefaultClick(Sender: TObject);
    procedure ItemClick(Sender: TObject);
  private
    procedure ReadIniItems;
    procedure WriteIniItems;
    function GetColourFilters: boolean;
    function GetNumberBases: integer;
    function GetBonusPoints: integer;
  public
    procedure Init; override;
    destructor Destroy; override;
    procedure SaveChanges; override;
    procedure CancelChanges; override;
    //
    property ColourFilters: boolean read GetColourFilters;
    property NumberBases: integer read GetNumberBases;
    property BonusPoints: integer read GetBonusPoints;
  end;


implementation

{$R *.lfm}

const
  SECT_SIPREFS = 'SIPrefs';
  INI_COLOUR   = 'Colour';
  INI_BASES    = 'Bases';
  INI_BONUS    = 'Bonus';


{ INITIALISE CONFIG FRAME... read settings }

procedure TSIPrefsFrame.Init;
begin
  inherited Init;
  ReadIniItems;
end;


{ DESTROY }

destructor TSIPrefsFrame.Destroy;
begin
  inherited Destroy;
end;


{ ON CLICK... notify change }

{ TODO : uPrefsSI -> Move this into base frame, as protected DoChange procedure? }

procedure TSIPrefsFrame.ItemClick(Sender: TObject);
begin
  fChanged := True;
  if Assigned(fOnChange) then
    begin
      if (Sender.ClassType = TCheckBox) then
        fOnChange(self, 1)                            // 1=Colour
      else
        fOnChange(self, (Sender as TRadioGroup).Tag); // 2=Bases or 3=Bonus
    end;
end;


{ GET OPTION STATES }

function TSIPrefsFrame.GetColourFilters: boolean;
begin
  Result := cbColourFilters.Checked;
end;


function TSIPrefsFrame.GetNumberBases: integer;
begin
  Result := rgNumberBases.ItemIndex;
end;


function TSIPrefsFrame.GetBonusPoints: integer;
begin
  Result := rgBonusPoints.ItemIndex;
end;


{ READ / WRITE SETTINGS TO INI FILE }

procedure TSIPrefsFrame.ReadIniItems;
begin
  cbColourFilters.Checked := AppIni.ReadBool(SECT_SIPREFS, INI_COLOUR, True);
  rgNumberBases.ItemIndex := AppIni.ReadInteger(SECT_SIPREFS, INI_BASES, 0);
  rgBonusPoints.ItemIndex := AppIni.ReadInteger(SECT_SIPREFS, INI_BONUS, 0);
end;


procedure TSIPrefsFrame.WriteIniItems;
begin
  AppIni.WriteBool(SECT_SIPREFS, INI_COLOUR, cbColourFilters.Checked);
  AppIni.WriteInteger(SECT_SIPREFS, INI_BASES, rgNumberBases.ItemIndex);
  AppIni.WriteInteger(SECT_SIPREFS, INI_BONUS, rgBonusPoints.ItemIndex);
end;


{ SAVE CHANGES / CANCEL CHANGES / RESET DEFAULTS }

procedure TSIPrefsFrame.SaveChanges;
begin
  if (fChanged) then
    WriteIniItems;
end;


procedure TSIPrefsFrame.CancelChanges;
begin
  if (fChanged) then
    ReadIniItems;
end;


procedure TSIPrefsFrame.btnResetDefaultClick(Sender: TObject);
begin
  if (ConfirmResetDefault) then
    begin
      AppIni.DeleteSection(SECT_SIPREFS);
      ReadIniItems;
    end;
end;


end.

