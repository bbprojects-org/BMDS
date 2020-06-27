{ ==============================================================================

  PREFERENCES FRAME FOR MICROTAN 65

    Permits choice of screen colour and frequency
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

unit uPrefsMicrotan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  //
  uPrefsFrameBase, uGfxMgr, uIniFile, uCommon;

type

  { TM65PrefsFrame }

  TM65PrefsFrame = class(TPrefsFrame)
    ColorDialog1: TColorDialog;
    comboFreq: TComboBox;
    gbFrequency: TGroupBox;
    gbColour: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    lblFreq: TLabel;
    panelFore: TPanel;
    panelBack: TPanel;
    procedure btnResetDefaultClick(Sender: TObject);
    procedure panelBackClick(Sender: TObject);
    procedure panelForeClick(Sender: TObject);
    procedure FlagChange(Sender: TObject);
  private
    fColourF: TGfxColour;
    fColourB: TGfxColour;
    function ConvertPasToSDL(Val: TColor): TGfxColour;
    function GetFrequency: integer;
    procedure ReadIniItems;
    procedure WriteIniItems;
  public
    procedure Init; override;
    destructor Destroy; override;
    procedure SaveChanges; override;
    procedure CancelChanges; override;
    //
    property ColourF: TGfxColour read fColourF;
    property ColourB: TGfxColour read fColourB;
    property Frequency: integer read GetFrequency;
  end;


implementation

{$R *.lfm}        

const
  SECT_M65PREFS = 'M65Prefs';
  INI_COLOURF   = 'ColourF';
  INI_COLOURB   = 'ColourB';
  INI_FREQ      = 'Freq';


{ INITIALISE CONFIG FRAME... read settings }

procedure TM65PrefsFrame.Init;
begin
  inherited;
  ReadIniItems;
end;


{ DESTROY }

destructor TM65PrefsFrame.Destroy;
begin
  inherited;
end;


{ GET OPTION STATES }

function TM65PrefsFrame.GetFrequency: integer;
begin
  case comboFreq.ItemIndex of
    0: Result := 750;
    1: Result := 1000;
    2: Result := 2000;
    3: Result := 4000;
  end;
end;


{ ON CLICK... notify change }

procedure TM65PrefsFrame.FlagChange(Sender: TObject);
begin
  fChanged := True;
  if Assigned(fOnChange) then
    begin
      if (Sender.ClassType = TPanel) then
        fOnChange(self, 1)              // 1=Colour
      else
        fOnChange(self, 2);             // 2=Frequency
    end;
end;


procedure TM65PrefsFrame.panelForeClick(Sender: TObject);
begin
  ColorDialog1.Color := panelFore.Color;
  if ColorDialog1.Execute then
    begin
      panelFore.Color := (ColorDialog1.Color);
      fColourF := ConvertPasToSDL(panelFore.Color);
      FlagChange(panelFore);
    end;
end;


procedure TM65PrefsFrame.panelBackClick(Sender: TObject);
begin
  ColorDialog1.Color := panelBack.Color;
  if ColorDialog1.Execute then
    begin
      panelBack.Color := (ColorDialog1.Color);
      fColourB := ConvertPasToSDL(panelBack.Color);
      FlagChange(panelBack);
    end;
end;


function TM65PrefsFrame.ConvertPasToSDL(Val: TColor): TGfxColour;
begin
  Result := TGfxColour((Red(Val) shl 24) or (Green(Val) shl 16) or (Blue(Val) shl 8) or $FF);
end;


{ READ / WRITE SETTINGS TO INI FILE }

procedure TM65PrefsFrame.ReadIniItems;
begin
  panelFore.Color := TColor(AppIni.ReadInteger(SECT_M65PREFS, INI_COLOURF, $00BFFF)); // Amber
  fColourF := ConvertPasToSDL(panelFore.Color);
  panelBack.Color := TColor(AppIni.ReadInteger(SECT_M65PREFS, INI_COLOURB, $000000)); // Black
  fColourB := ConvertPasToSDL(panelBack.Color);
  comboFreq.ItemIndex := AppIni.ReadInteger(SECT_M65PREFS, INI_FREQ, 0);
end;


procedure TM65PrefsFrame.WriteIniItems;
begin
  AppIni.WriteInteger(SECT_M65PREFS, INI_COLOURF, Integer(panelFore.Color));
  AppIni.WriteInteger(SECT_M65PREFS, INI_COLOURB, Integer(panelBack.Color));
  AppIni.WriteInteger(SECT_M65PREFS, INI_FREQ, comboFreq.ItemIndex);
end;


{ SAVE CHANGES / CANCEL CHANGES / RESET DEFAULTS }

procedure TM65PrefsFrame.SaveChanges;
begin
  if (fChanged) then
    WriteIniItems;
end;


procedure TM65PrefsFrame.CancelChanges;
begin
  if (fChanged) then
    begin
      ReadIniItems;
      if Assigned(fOnChange) then
        fOnChange(self, 0);             // 0=All
    end;
end;


procedure TM65PrefsFrame.btnResetDefaultClick(Sender: TObject);
begin
  if (ConfirmResetDefault) then
    begin
      AppIni.DeleteSection(SECT_M65PREFS);
      ReadIniItems;
      if Assigned(fOnChange) then
        fOnChange(self, 0);             // 0=All
    end;
end;


end.

