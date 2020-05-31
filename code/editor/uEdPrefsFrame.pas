{ ==============================================================================

  EDITOR GENERAL PREFERENCES FRAME

    Provides facility to amend editor general options
    Saves settings to application's INI file


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

unit uEdPrefsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ColorBox,
  ExtCtrls, Spin, SynEdit, SynGutter,
  //
  uHighlighterAsm, uPrefsFrameBase, uIniFile, uCommon;

type
  TEdPrefs = record
    TabsToSpaces: boolean;
    TabWidth: integer;
    SmartTabs: boolean;
    VisibleRightMargin: boolean;
    RightMargin: integer;
    VisibleGutter: boolean;
    //
    FontName: string;
    FontSize: integer;
    Quality: TFontQuality;
  end;

  { TEdPrefsFrame }

  TEdPrefsFrame = class(TPrefsFrame)
    Bevel1: TBevel;
    cbAntialiasing: TCheckBox;
    cbTabsToSpaces: TCheckBox;
    cbSmartTabs: TCheckBox;
    cbVisibleRightMargin: TCheckBox;
    cbVisibleGutter: TCheckBox;
    FontBtn: TButton;
    FontDialog: TFontDialog;
    gbFont: TGroupBox;
    gbGeneral: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblFont: TLabel;
    SampleEditor: TSynEdit;
    seTabWidth: TSpinEdit;
    seRightMargin: TSpinEdit;
    procedure btnResetDefaultsClick(Sender: TObject);
    procedure cbChange(Sender: TObject);
    procedure FontBtnClick(Sender: TObject);
  private
    fHL: TSynAsmHighlighter;
    function GetPrefs: TEdPrefs;
    procedure ReadIniItems;
    procedure UpdateEditor;
    procedure WriteIniItems;
  public
    procedure Init; override;
    destructor Destroy; override;
    procedure SaveChanges; override;
    procedure CancelChanges; override;
    //
    property Prefs: TEdPrefs read GetPrefs;
  end;

var
  EdPrefs: TEdPrefsFrame;


implementation

{$R *.lfm}

const
  SECT_EDPREFS     = 'EdPrefs';
  //
  INI_TABSTOSPACES = 'TabsToSpc';
  INI_TABWIDTH     ='TabWidth';
  INI_SMARTTABS    = 'SmartTabs';
  INI_VISRMARGIN   = 'VisRMargin';
  INI_RMARGIN      = 'RMargin';
  INI_VISGUTTER    = 'VisGutter';
  INI_FONTNAME     = 'FontName';
  INI_FONTSIZE     = 'FontSize';
  INI_ANTIALIAS    = 'AntiAlias';

{ INIT }

procedure TEdPrefsFrame.Init;
begin
  inherited Init;
  fHL := TSynAsmHighlighter.Create(self);
  SampleEditor.Highlighter := fHL;
  SampleEditor.Text := fHL.SampleSource;
  ReadIniItems;
end;


{ DESTROY }

destructor TEdPrefsFrame.Destroy;
begin
  fHL.Free;
  inherited Destroy;
end;


{ FONT BUTTON }

procedure TEdPrefsFrame.FontBtnClick(Sender: TObject);
begin
  FontDialog.Font.Name := SampleEditor.Font.Name;
  FontDialog.Font.Size := SampleEditor.Font.Size;
  FontDialog.Options := FontDialog.Options - [fdNoStyleSel];
  if (FontDialog.Execute) then
    begin
      fChanged := True;
      SampleEditor.Font.Name := FontDialog.Font.Name;
      SampleEditor.Font.Size := FontDialog.Font.Size;
      UpdateEditor;
    end;
end;


{ ANTI-ALIASING CHANGE }

procedure TEdPrefsFrame.cbChange(Sender: TObject);
begin
  fChanged := True;
  UpdateEditor;
end;


{ UPDATE EDITOR - show changes visually }

procedure TEdPrefsFrame.UpdateEditor;
begin
  // Tabs
  if (cbTabsToSpaces.Checked) then
    SampleEditor.Options := SampleEditor.Options + [eoTabsToSpaces]
  else
    SampleEditor.Options := SampleEditor.Options - [eoTabsToSpaces];
  SampleEditor.TabWidth := seTabWidth.Value;
  if (cbSmartTabs.Checked) then
    SampleEditor.Options := SampleEditor.Options + [eoSmartTabs]
  else
    SampleEditor.Options := SampleEditor.Options - [eoSmartTabs];
  // Right margin
  if (cbVisibleRightMargin.Checked) then
    SampleEditor.Options := SampleEditor.Options - [eoHideRightMargin]
  else
    SampleEditor.Options := SampleEditor.Options + [eoHideRightMargin];
  SampleEditor.RightEdge := seRightMargin.Value;
  // Gutter
  SampleEditor.Gutter.Visible := cbVisibleGutter.Checked;
  // Font
  lblFont.Caption := SampleEditor.Font.Name + ' ' + IntToStr(SampleEditor.Font.Size) + ' pt';
  if (cbAntialiasing.Checked) then
    SampleEditor.Font.Quality := fqDefault
  else
    SampleEditor.Font.Quality := fqNonAntialiased;
end;


{ SAVE CHANGES / CANCEL CHANGES / RESET DEFAULTS }

procedure TEdPrefsFrame.SaveChanges;
begin
  if (fChanged) then
    begin
      WriteIniItems;
      if Assigned(fOnChange) then
        fOnChange(self, 0);
    end;
end;


procedure TEdPrefsFrame.CancelChanges;
begin
  if (fChanged) then
    ReadIniItems;
end;


procedure TEdPrefsFrame.btnResetDefaultsClick(Sender: TObject);
begin
  if (ConfirmResetDefault) then
    begin
      AppIni.DeleteSection(SECT_EDPREFS);
      ReadIniItems;
      UpdateEditor;
    end;
end;


{ READ / WRITE ITEMS TO INI FILE }

procedure TEdPrefsFrame.ReadIniItems;
begin
  cbTabsToSpaces.Checked := AppIni.ReadBool(SECT_EDPREFS, INI_TABSTOSPACES, True);
  seTabWidth.Value := AppIni.ReadInteger(SECT_EDPREFS, INI_TABWIDTH, 2);
  cbSmartTabs.Checked := AppIni.ReadBool(SECT_EDPREFS, INI_SMARTTABS, True);
  cbVisibleRightMargin.Checked := AppIni.ReadBool(SECT_EDPREFS, INI_VISRMARGIN, True);
  seRightMargin.Value := AppIni.ReadInteger(SECT_EDPREFS, INI_RMARGIN, 80);
  cbVisibleGutter.Checked := AppIni.ReadBool(SECT_EDPREFS, INI_VISGUTTER, True);
  //
  SampleEditor.Font.Name := AppIni.ReadString(SECT_EDPREFS, INI_FONTNAME, 'Courier New');
  SampleEditor.Font.Size := AppIni.ReadInteger(SECT_EDPREFS, INI_FONTSIZE, 10);
  cbAntialiasing.Checked := AppIni.ReadBool(SECT_EDPREFS, INI_ANTIALIAS, True);
  UpdateEditor;
end;


procedure TEdPrefsFrame.WriteIniItems;
begin
  AppIni.WriteBool(SECT_EDPREFS, INI_TABSTOSPACES, cbTabsToSpaces.Checked);
  AppIni.WriteInteger(SECT_EDPREFS, INI_TABWIDTH, seTabWidth.Value);
  AppIni.WriteBool(SECT_EDPREFS, INI_SMARTTABS, cbSmartTabs.Checked);
  AppIni.WriteBool(SECT_EDPREFS, INI_VISRMARGIN, cbVisibleRightMargin.Checked);
  AppIni.WriteInteger(SECT_EDPREFS, INI_RMARGIN, seRightMargin.Value);
  AppIni.WriteBool(SECT_EDPREFS, INI_VISGUTTER, cbVisibleGutter.Checked);
  //
  AppIni.WriteString(SECT_EDPREFS, INI_FONTNAME, SampleEditor.Font.Name);
  AppIni.WriteInteger(SECT_EDPREFS, INI_FONTSIZE, SampleEditor.Font.Size);
  AppIni.WriteBool(SECT_EDPREFS, INI_ANTIALIAS, cbAntialiasing.Checked);
end;


{ GET PREFERENCES }

function TEdPrefsFrame.GetPrefs: TEdPrefs;
begin
  Result.TabsToSpaces := cbTabsToSpaces.Checked;
  Result.TabWidth := seTabWidth.Value;
  Result.SmartTabs := cbSmartTabs.Checked;
  Result.VisibleRightMargin := cbVisibleRightMargin.Checked;
  Result.RightMargin := seRightMargin.Value;
  Result.VisibleGutter := cbVisibleGutter.Checked;
  //
  Result.FontName := SampleEditor.Font.Name;
  Result.FontSize := SampleEditor.Font.Size;
  Result.Quality := SampleEditor.Font.Quality;
end;


end.

