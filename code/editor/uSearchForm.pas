{ ==============================================================================

  SEARCH FORM

    General purpose find and replace form

    Saves its settings in the app's INI file, for each machine


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

unit uSearchForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, ActnList, SynEdit, SynEditSearch, SynEditTypes, LCLProc,
  //
  uIniFile, uCommon;

type

  { TSearchForm }

  TSearchForm = class(TForm)
    actReplaceWith: TAction;
    btnFind: TButton;
    btnReplaceAll: TButton;
    btnCancel: TButton;
    cbReplace: TCheckBox;
    cbCaseSensitive: TCheckBox;
    cbWholeWords: TCheckBox;
    cbPromptOnReplace: TCheckBox;
    edSearch: TLabeledEdit;
    edReplace: TEdit;
    gbOptions: TGroupBox;
    gbFrom: TGroupBox;
    rgFromBeginning: TRadioButton;
    rgFromCursor: TRadioButton;
    procedure btnFindClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnReplaceAllClick(Sender: TObject);
    procedure cbReplaceChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgFromChange(Sender: TObject);
  private
    ReplaceFlag: boolean;
    ReplaceAllFlag: boolean;
    ForwardFlag: boolean;
    function  GetFindText: string;
    procedure SetFindText(const NewText: string);
    function  GetReplaceText: string;
    procedure SetReplaceText(const NewText: string);
    //
    procedure SetOptions(NewOptions: TSynSearchOptions);
    function GetOptions: TSynSearchOptions;
  public
    procedure ReadIniSettings;
    procedure WriteIniSettings;
    //
    property FindText: string read GetFindText write SetFindText;
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property Options: TSynSearchOptions read GetOptions write SetOptions;
  end;

var
  SearchForm: TSearchForm;


implementation

{$R *.lfm}

const
  INI_PREFIX     = 'Search';
  INI_FIND       = 'Find';
  INI_REPLACE    = 'Replace';
  INI_CASE       = 'Case';
  INI_WHOLEWORDS = 'Words';
  INI_PROMPT     = 'Prompt';
  INI_FROM       = 'From';


{ CREATE / DESTROY }

procedure TSearchForm.FormCreate(Sender: TObject);
begin
  ReadIniSettings;
  ForwardFlag := True;
  ReplaceFlag := False;
  ReplaceAllFlag := False;
  cbReplace.Checked := False;
  cbReplaceChange(cbReplace);
end;


procedure TSearchForm.FormDestroy(Sender: TObject);
begin
  WriteIniSettings;
end;


{ GETTERS / SETTERS }

function TSearchForm.GetFindText: string;
begin
  Result := edSearch.Text;
end;


procedure TSearchForm.SetFindText(const NewText: string);
begin
  edSearch.Text := NewText;
end;


function TSearchForm.GetReplaceText: string;
begin
  Result := edReplace.Text;
end;


procedure TSearchForm.SetReplaceText(const NewText: string);
begin
  edReplace.Text := NewText;
end;


function TSearchForm.GetOptions: TSynSearchOptions;
begin
  Result := [];
  if (cbCaseSensitive.Checked) then Include(Result, ssoMatchCase);
  if (cbWholeWords.Checked) then Include(Result, ssoWholeWord);
  if (cbPromptOnReplace.Checked) then Include(Result, ssoPrompt);
  //
  if (rgFromBeginning.Checked) then Include(Result, ssoEntireScope);
  if (not ForwardFlag) then Include(Result, ssoBackwards);
  if (ReplaceFlag) then Include(Result, ssoReplace);
  if (ReplaceAllFlag) then Include(Result, ssoReplaceAll);
end;


procedure TSearchForm.SetOptions(NewOptions: TSynSearchOptions);
begin
  cbCaseSensitive.Checked := ssoMatchCase in NewOptions;
  cbWholeWords.Checked := ssoWholeWord in NewOptions;
  cbPromptOnReplace.Checked := ssoPrompt in NewOptions;

  rgFromBeginning.Checked := (ssoEntireScope in NewOptions);
  rgFromCursor.Checked := (not rgFromBeginning.Checked);

  cbReplace.Checked := (ssoReplace in NewOptions);
  cbReplaceChange(cbReplace);
  ReplaceAllFlag := (ssoReplaceAll in NewOptions);
end;


{ UPDATE GUI STATE }

procedure TSearchForm.cbReplaceChange(Sender: TObject);
begin
  edReplace.Enabled := cbReplace.Checked;
  cbPromptOnReplace.Enabled := cbReplace.Checked;
  if (cbReplace.Checked) then
    begin
      btnFind.Caption := 'Replace';
      Caption := 'Replace';
      btnReplaceAll.Visible := True;
      btnFind.Left := 323;
    end
  else
    begin
      btnFind.Caption := 'Find';
      Caption := 'Find';
      btnReplaceAll.Visible := False;
      btnFind.Left := 346;
    end;
  btnCancel.Left := btnFind.Left - 76;
  btnReplaceAll.Left := btnCancel.Left - 120;
end;


procedure TSearchForm.rgFromChange(Sender: TObject);
begin
  if (Sender = rgFromBeginning) then
    rgFromCursor.Checked := not rgFromBeginning.Checked
  else
    rgFromBeginning.Checked := not rgFromCursor.Checked;
end;


{ BUTTONS }

procedure TSearchForm.btnFindClick(Sender: TObject);
begin
  ReplaceFlag := (btnFind.Caption = 'Replace');
  ReplaceAllFlag := False;
  // ModalResult set to mrOk in button
end;


procedure TSearchForm.btnReplaceAllClick(Sender: TObject);
begin
  ReplaceFlag := True;
  ReplaceAllFlag := True;
  // ModalResult set to mrAll in button
end;


procedure TSearchForm.btnCancelClick(Sender: TObject);
begin
  // ModalResult set to mrCancel in button
end;


{ READ / WRITE INI SETTINGS }

procedure TSearchForm.ReadIniSettings;
begin
  Top := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, 20);
  Left := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, 170);
  //
  edSearch.Text := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + INI_FIND, '');
  edReplace.Text := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + INI_REPLACE, '');
  cbCaseSensitive.Checked := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_CASE, False);
  cbWholeWords.Checked := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_WHOLEWORDS, False);
  cbPromptOnReplace.Checked := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_PROMPT, False);
  rgFromCursor.Checked := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_FROM, False);
  rgFromChange(rgFromCursor);
end;


procedure TSearchForm.WriteIniSettings;
begin
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, Left);
  //
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + INI_FIND, edSearch.Text);
  AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + INI_REPLACE, edReplace.Text);
  AppIni.WriteBool(SECT_CUSTOM, INI_PREFIX + INI_CASE, cbCaseSensitive.Checked);
  AppIni.WriteBool(SECT_CUSTOM, INI_PREFIX + INI_WHOLEWORDS, cbWholeWords.Checked);
  AppIni.WriteBool(SECT_CUSTOM, INI_PREFIX + INI_PROMPT, cbPromptOnReplace.Checked);
  AppIni.WriteBool(SECT_CUSTOM, INI_PREFIX + INI_FROM, rgFromCursor.Checked);
end;


end.

