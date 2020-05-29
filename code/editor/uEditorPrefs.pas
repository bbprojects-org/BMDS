{ ==============================================================================

  EDITOR PREFERENCES FRAME

    Provides user with facility to amend editor options


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

  { TODO : uEditorPrefs -> allow colour changes for highlighting }

unit uEditorPrefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ColorBox, SynEdit, SynEditHighlighter,
  //
  uHighlighterAsm, uIniFile, uCommon;

type
  TEditorPrefs = record
    FontName: string;
    FontSize: integer;
    FontAntialiasing: boolean;
  end;

  { TEditorPrefsForm }

  TEditorPrefsForm = class(TForm)
    btnResetDefaults: TButton;
    btnRevertF: TButton;
    btnRevertF1: TButton;
    cbAntialiasing: TCheckBox;
    Bevel1: TBevel;
    btnCancel: TButton;
    btnOK: TButton;
    colorBackground: TColorBox;
    colorForeground: TColorBox;
    comboAttribute: TComboBox;
    edBackground: TEdit;
    edForeground: TEdit;
    FontBtn: TButton;
    FontDialog: TFontDialog;
    gbColours: TGroupBox;
    Label11: TLabel;
    lblBackground: TLabel;
    lblFont: TLabel;
    gbFont: TGroupBox;
    lblForeground: TLabel;
    SampleEditor: TSynEdit;
    procedure colorBackgroundSelect(Sender: TObject);
    procedure colorForegroundSelect(Sender: TObject);
    procedure comboAttributeSelect(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnResetDefaultsClick(Sender: TObject);
    procedure cbAntialiasingChange(Sender: TObject);
    procedure FontBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fPrefs: TEditorPrefs;
    fEditorRef: TSynEdit;
    fHL: TSynAsmHighlighter;
    procedure ReadIniItems;
    procedure WriteIniItems;
    procedure UpdateEditor;
  public
    property EditorRef: TSynEdit write fEditorRef;
  end;

var
  EditorPrefsForm: TEditorPrefsForm;


implementation

{$R *.lfm}

const
  INI_SECT      = 'EdPrefs';
  INI_FONTNAME  = 'FontName';
  INI_FONTSIZE  = 'FontSize';
  INI_ANTIALIAS = 'AntiAlias';

{ CREATE }

procedure TEditorPrefsForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  fHL := TSynAsmHighlighter.Create(self);
  SampleEditor.Highlighter := fHL;
  SampleEditor.Text := fHL.SampleSource;
  //
  Top := AppIni.ReadInteger(INI_SECT, INI_WDW_TOP, 60);
  Left := AppIni.ReadInteger(INI_SECT, INI_WDW_LEFT, 60);
  ReadIniItems;

  for i := 0 to fHL.AttrCount-1 do
    comboAttribute.Items.Add(fHL.Attribute[i].Caption^);
  comboAttribute.ItemIndex := 0;
  UpdateEditor;
end;


{ DESTROY }

procedure TEditorPrefsForm.FormDestroy(Sender: TObject);
begin
  fHL.Free;
  AppIni.WriteInteger(INI_SECT, INI_WDW_TOP, Top);
  AppIni.WriteInteger(INI_SECT, INI_WDW_LEFT, Left);
end;


procedure TEditorPrefsForm.FontBtnClick(Sender: TObject);
begin
  FontDialog.Font.Name := fPrefs.FontName;
  FontDialog.Font.Size := fPrefs.FontSize;
  FontDialog.Options := FontDialog.Options - [fdNoStyleSel];
  if (FontDialog.Execute) then
    begin
      fPrefs.FontName := FontDialog.Font.Name;
      fPrefs.FontSize := FontDialog.Font.Size;
      UpdateEditor;
    end;
end;


procedure TEditorPrefsForm.UpdateEditor;
var
  c: TColor;
begin
  if (comboAttribute.ItemIndex >= 0) then
    begin
      c := fHL.Attribute[comboAttribute.ItemIndex].Foreground;
      colorForeground.Selected := c;
      edForeground.Text := ColorToString(c);
      c := fHL.Attribute[comboAttribute.ItemIndex].Background;
      colorBackground.Selected := c;
      edBackground.Text := ColorToString(c);
    end;

  lblFont.Caption := fPrefs.FontName + ' ' + IntToStr(fPrefs.FontSize) + ' pt';
  SampleEditor.Font.Name := fPrefs.FontName;
  SampleEditor.Font.Size := fPrefs.FontSize;
  //SampleEditor.Font.Color := FProfile.Attributes.Default.Foreground;
  //SampleEditor.Color := FProfile.Attributes.Default.Background;
  //SampleEditor.SelectedColor.Foreground := FProfile.Attributes.Selected.Foreground;
  //SampleEditor.SelectedColor.Background := FProfile.Attributes.Selected.Background;
  if (fPrefs.FontAntialiasing) then
    SampleEditor.Font.Quality := fqDefault
  else
    SampleEditor.Font.Quality := fqNonAntialiased;
end;


procedure TEditorPrefsForm.cbAntialiasingChange(Sender: TObject);
begin
  fPrefs.FontAntialiasing := cbAntialiasing.Checked;
  UpdateEditor;
end;


procedure TEditorPrefsForm.comboAttributeSelect(Sender: TObject);
var
  thisAttribute: TSynHighlighterAttributes;
begin
  (*
  case TtkTokenKind(comboAttribute.ItemIndex) of
    tkIdentifier: thisAttribute := SampleEditor.Highlighter.Attribute[tnIdentifier];
//    tkKeyword:    thisAttribute := fKeywordAttri;
  end;
  fIdentifierAttri: TSynHighlighterAttributes;
  fKeywordAttri: TSynHighlighterAttributes;
  fCommentAttri: TSynHighlighterAttributes;
  fNumberAttri: TSynHighlighterAttributes;
  fSpaceAttri: TSynHighlighterAttributes;
  fStringAttri: TSynHighlighterAttributes;
  fSymbolAttri: TSynHighlighterAttributes;
  fDirectiveAttri: TSynHighlighterAttributes;
  fOperatorAttri: TSynHighlighterAttributes;

  fIdentifierAttri.Foreground := clBlue;
  *)
  UpdateEditor;
end;


procedure TEditorPrefsForm.colorForegroundSelect(Sender: TObject);
begin
  fHL.Attribute[comboAttribute.ItemIndex].Foreground := colorForeground.Selected;
  UpdateEditor;
end;


procedure TEditorPrefsForm.colorBackgroundSelect(Sender: TObject);
begin
  fHL.Attribute[comboAttribute.ItemIndex].Background := colorBackground.Selected;
  UpdateEditor;
end;


procedure TEditorPrefsForm.btnOKClick(Sender: TObject);
begin
  WriteIniItems;
  Close;
end;


procedure TEditorPrefsForm.btnCancelClick(Sender: TObject);
begin
  ReadIniItems;
  Close;
end;


procedure TEditorPrefsForm.btnResetDefaultsClick(Sender: TObject);
begin
  if (ConfirmResetDefault) then
    begin
      AppIni.DeleteSection(INI_SECT);
      ReadIniItems;
      UpdateEditor;
    end;
end;


procedure TEditorPrefsForm.ReadIniItems;
begin
  fPrefs.FontName := AppIni.ReadString(INI_SECT, INI_FONTNAME, 'Courier New');
  fPrefs.FontSize := AppIni.ReadInteger(INI_SECT, INI_FONTSIZE, 10);
  fPrefs.FontAntialiasing := AppIni.ReadBool(INI_SECT, INI_ANTIALIAS, True);
  cbAntialiasing.Checked := fPrefs.FontAntialiasing;
end;


procedure TEditorPrefsForm.WriteIniItems;
begin
  AppIni.WriteString(INI_SECT, INI_FONTNAME, fPrefs.FontName);
  AppIni.WriteInteger(INI_SECT, INI_FONTSIZE, fPrefs.FontSize);
  AppIni.WriteBool(INI_SECT, INI_ANTIALIAS, cbAntialiasing.Checked);
end;


end.

