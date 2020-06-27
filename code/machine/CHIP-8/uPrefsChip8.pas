{ ==============================================================================

  PREFERENCES FRAME FOR CHIP-8

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

unit uPrefsChip8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  //
  uPrefsFrameBase, uIniFile, uCommon;

type
  
  { TChip8PrefsFrame }

  TChip8PrefsFrame = class(TPrefsFrame)
    cbCHIP8: TCheckBox;
    gbCHIP8: TGroupBox;
    gbCHIP9: TGroupBox;
    seStepsPerFrame: TSpinEdit;
    procedure btnResetDefaultClick(Sender: TObject);
    procedure FlagChange(Sender: TObject);
  private
    function GetCHIP8: boolean;
    function GetStepsPerFrame: integer;
    procedure ReadIniItems;
    procedure WriteIniItems;
  public
    procedure Init; override;
    destructor Destroy; override;
    procedure SaveChanges; override;
    procedure CancelChanges; override;
    //
    property Chip8Only: boolean read GetCHIP8;
    property StepsPerFrame: integer read GetStepsPerFrame;
  end;

var
  Chip8Prefs: TChip8PrefsFrame;


implementation

{$R *.lfm}

const
  SECT_CHIP8PREFS = 'Chip8Prefs';
  INI_CHIP8_ONLY  = 'Chip8Only';
  INI_STEPS       = 'StepsPerFrame';


{ INITIALISE CONFIG FRAME... read settings }

procedure TChip8PrefsFrame.Init;
begin
  inherited;
  ReadIniItems;
end;


{ DESTROY }

destructor TChip8PrefsFrame.Destroy;
begin
  inherited;
end;


{ GET OPTION STATES }

function TChip8PrefsFrame.GetCHIP8: boolean;
begin
  Result := cbCHIP8.Checked;
end;


function TChip8PrefsFrame.GetStepsPerFrame: integer;
begin
  Result := seStepsPerFrame.Value;
end;


{ ON CLICK... notify change }

procedure TChip8PrefsFrame.FlagChange(Sender: TObject);
begin
  fChanged := True;
  if Assigned(fOnChange) then
    begin
      if (Sender.ClassType = TCheckBox) then
        fOnChange(self, 1)              // 1=CHIP8 mode
      else if (Sender.ClassType = TSpinEdit) then
        fOnChange(self, 2);             // 2=Steps per frame
    end;
end;


{ READ / WRITE SETTINGS TO INI FILE }

procedure TChip8PrefsFrame.ReadIniItems;
begin
  cbCHIP8.Checked := AppIni.ReadBool(SECT_CHIP8PREFS, INI_CHIP8_ONLY, True);
  seStepsPerFrame.Value := AppIni.ReadInteger(SECT_CHIP8PREFS, INI_STEPS, 10);
end;


procedure TChip8PrefsFrame.WriteIniItems;
begin
  AppIni.WriteBool(SECT_CHIP8PREFS, INI_CHIP8_ONLY, cbCHIP8.Checked);
  AppIni.WriteInteger(SECT_CHIP8PREFS, INI_STEPS, seStepsPerFrame.Value);
end;


{ SAVE CHANGES / CANCEL CHANGES / RESET DEFAULTS }

procedure TChip8PrefsFrame.SaveChanges;
begin
  if (fChanged) then
    WriteIniItems;
end;


procedure TChip8PrefsFrame.CancelChanges;
begin
  if (fChanged) then
    begin
      ReadIniItems;
      if Assigned(fOnChange) then
        fOnChange(self, 0);             // 0=All
    end;
end;


procedure TChip8PrefsFrame.btnResetDefaultClick(Sender: TObject);
begin
  if (ConfirmResetDefault) then
    begin
      AppIni.DeleteSection(SECT_CHIP8PREFS);
      ReadIniItems;
      if Assigned(fOnChange) then
        fOnChange(self, 0);             // 0=All
    end;
end;


end.
