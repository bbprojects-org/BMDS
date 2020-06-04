{ ==============================================================================

  GENERAL PREFERENCES FRAME

    Provides facility to amend general options

    The LED settings are saved per machine


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

{ TODO : uGenPrefsFrame -> add button to select App Data Folder }

unit uGenPrefsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  //
  uPrefsFrameBase, uIniFile, uCommon;

type
  
  { TGenPrefsFrame }

  TGenPrefsFrame = class(TPrefsFrame)
    cbLEDsEnabled: TCheckBox;
    comboColour: TComboBox;
    edDataPath: TLabeledEdit;
    GroupBox1: TGroupBox;
    gbLEDs: TGroupBox;
    ImageOff: TImage;
    ImageOn: TImage;
    ImageListLeds: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    edAddress: TLabeledEdit;
    procedure btnResetDefaultClick(Sender: TObject);
    procedure cbLEDsEnabledChange(Sender: TObject);
    procedure comboColourChange(Sender: TObject);
    procedure edAddressChange(Sender: TObject);
  private
    function GetDataPath: string;
    function GetLedsEnabled: boolean;
    function GetLedsColourIndex: integer;
    function GetLedAddress: word;
    procedure FlagChange(Sender: TObject);
    procedure ReadIniItems;
    procedure WriteIniItems;
    procedure ShowSettings;
  public
    procedure Init; override;
    destructor Destroy; override;
    procedure SaveChanges; override;
    procedure CancelChanges; override;
    //
    property DataPath: string read GetDataPath;
    property LedImages: TImageList read ImageListLeds;
    property LedsEnabled: boolean read GetLedsEnabled;
    property LedsColourIndex: integer read GetLedsColourIndex;
    property LedAddress: word read GetLedAddress;
  end;

var
  GenPrefs: TGenPrefsFrame;


implementation

{$R *.lfm}

const
  SECT_GENPREFS = 'GenPrefs';
  INI_DATAPATH  = 'DataPath';
  INI_ENABLED   = 'LedsEnabled';
  INI_COLOURIDX = 'LedsColourIndex';
  INI_ADDRESS   = 'LedsAddress';


{ INITIALISE CONFIG FRAME... read settings }

procedure TGenPrefsFrame.Init;
begin
  inherited Init;
  ReadIniItems;
  ImageListLEDs.GetBitmap(0, ImageOff.Picture.Bitmap); // Off, always same
end;


{ DESTROY }

destructor TGenPrefsFrame.Destroy;
begin
  inherited Destroy;
end;


{ GET OPTION STATES }

function TGenPrefsFrame.GetDataPath: string;
begin
  Result := edDataPath.Text;
end;


function TGenPrefsFrame.GetLedsEnabled: boolean;
begin
  Result := cbLEDsEnabled.Checked;
end;


function TGenPrefsFrame.GetLedsColourIndex: integer;
begin
  Result := comboColour.ItemIndex + 1;
end;


function TGenPrefsFrame.GetLedAddress: word;
begin
  Result := GetHex(edAddress.Text);
end;


{ ON CLICK... notify change }

procedure TGenPrefsFrame.FlagChange(Sender: TObject);
begin
  fChanged := True;
  if Assigned(fOnChange) then
    begin
      if (Sender.ClassType = TEdit) then
        fOnChange(self, 1)              // 1=DataPath
      else if (Sender.ClassType = TCheckBox) then
        fOnChange(self, 2)              // 2=LedsEnabled
      else if (Sender.ClassType = TComboBox) then
        fOnChange(self, 3)              // 3=LedsColour
      else
        fOnChange(self, 4);             // 4=LedsAddress
    end;
end;


procedure TGenPrefsFrame.cbLEDsEnabledChange(Sender: TObject);
begin
  ShowSettings;
  FlagChange(cbLEDsEnabled);
end;


procedure TGenPrefsFrame.comboColourChange(Sender: TObject);
begin
  ShowSettings;
  FlagChange(comboColour);
end;


procedure TGenPrefsFrame.edAddressChange(Sender: TObject);
begin
  edAddress.Text := Format('%.4x', [GetHex(edAddress.Text) and $FFFF]);
  FlagChange(edAddress);
end;


procedure TGenPrefsFrame.ShowSettings;
begin
  comboColour.Enabled := cbLEDsEnabled.Checked;
  edAddress.Enabled := cbLEDsEnabled.Checked;
  ImageListLEDs.GetBitmap(comboColour.ItemIndex+1, ImageOn.Picture.Bitmap); // On
end;


{ READ / WRITE SETTINGS TO INI FILE }

procedure TGenPrefsFrame.ReadIniItems;
begin
  edDataPath.Text := AppIni.ReadString(SECT_GENPREFS, INI_DATAPATH, GetAppDataDirectory);
  cbLEDsEnabled.Checked := AppIni.ReadBool(SECT_CUSTOM, INI_ENABLED, True);
  comboColour.ItemIndex := AppIni.ReadInteger(SECT_CUSTOM, INI_COLOURIDX, 1);
  edAddress.Text := Format('%.4x', [AppIni.ReadInteger(SECT_CUSTOM, INI_ADDRESS, $BC00)]);
  ShowSettings;
end;


procedure TGenPrefsFrame.WriteIniItems;
begin
  AppIni.WriteString(SECT_GENPREFS, INI_DATAPATH, edDataPath.Text);
  AppIni.WriteBool(SECT_CUSTOM, INI_ENABLED, cbLEDsEnabled.Checked);
  AppIni.WriteInteger(SECT_CUSTOM, INI_COLOURIDX, comboColour.ItemIndex);
  AppIni.WriteInteger(SECT_CUSTOM, INI_ADDRESS, GetHex(edAddress.Text));
end;


{ SAVE CHANGES / CANCEL CHANGES / RESET DEFAULTS }

procedure TGenPrefsFrame.SaveChanges;
begin
  if (fChanged) then
    WriteIniItems;
end;


procedure TGenPrefsFrame.CancelChanges;
begin
  if (fChanged) then
    begin
      ReadIniItems;
      if Assigned(fOnChange) then
        fOnChange(self, 0);             // 0=All
    end;
end;


procedure TGenPrefsFrame.btnResetDefaultClick(Sender: TObject);
begin
  if (ConfirmResetDefault) then
    begin
      AppIni.DeleteSection(SECT_GENPREFS);
      ReadIniItems;
      if Assigned(fOnChange) then
        fOnChange(self, 0);             // 0=All
    end;
end;


end.

