{ ==============================================================================

  SEARCH FORM


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

{ TODO : uSearchForm -> make replace edit box dependent on checkbox  }
{ TODO : uSearchForm -> need whole scope for ReplaceAll }
{ TODO : uSearchForm -> save settings on close }

unit uSearchForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, SynEdit, SynEditSearch, SynEditTypes;

type

  { TSearchForm }

  TSearchForm = class(TForm)
    btnFindNext: TButton;
    btnReplace: TButton;
    btnReplaceAll: TButton;
    btnFindPrev: TButton;
    cbReplace: TCheckBox;
    cbWholeWords: TCheckBox;
    cbCaseSensitive: TCheckBox;
    edSearch: TLabeledEdit;
    edReplace: TEdit;
    lblStatus: TLabel;
    procedure btnFindNextClick(Sender: TObject);
    procedure btnFindPrevClick(Sender: TObject);
    procedure btnReplaceAllClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure cbReplaceChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fSynEdit: TSynEdit;
    procedure DoSearch(flagReplace: boolean; flagAll: boolean; flagForward: boolean);
  public
    property Editor: TSynEdit read fSynEdit write fSynEdit;
  end;

var
  SearchForm: TSearchForm;


implementation

{$R *.lfm}

procedure TSearchForm.FormCreate(Sender: TObject);
begin
  fSynEdit := nil;
  cbReplace.Checked := False;
  cbReplaceChange(nil);
  lblStatus.Caption := '';
end;


procedure TSearchForm.btnReplaceAllClick(Sender: TObject);
begin
  DoSearch(True, True, True);
end;


procedure TSearchForm.btnReplaceClick(Sender: TObject);
begin
  DoSearch(True, False, True);
end;


procedure TSearchForm.btnFindNextClick(Sender: TObject);
begin
  DoSearch(False, False, True);
end;


procedure TSearchForm.btnFindPrevClick(Sender: TObject);
begin
  DoSearch(False, False, False);
end;


procedure TSearchForm.DoSearch(flagReplace: boolean; flagAll: boolean; flagForward: boolean);
var
  Opts: TSynSearchOptions;
  count: integer;
  tmp: string;
begin
  Opts := []; // [ssoFindContinue];
  if (cbWholeWords.Checked) then
    Opts := Opts + [ssoWholeWord];
  if (cbCaseSensitive.Checked) then
    Opts := Opts + [ssoMatchCase];
  if (not flagForward) then
    Opts := Opts + [ssoBackwards];
  if (flagReplace) then
    Opts := Opts + [ssoReplace];
  if (flagAll) then
    Opts := Opts + [ssoReplaceAll];

  if (cbReplace.Checked) then
    begin
      count := fSynEdit.SearchReplace(edSearch.Text, edReplace.Text, Opts);
      tmp := 'Replaced ';
    end
  else
    begin
      count := fSynEdit.SearchReplace(edSearch.Text, '', Opts);
      tmp := 'Found ';
    end;

  lblStatus.Caption := tmp + IntToStr(count);
end;


procedure TSearchForm.cbReplaceChange(Sender: TObject);
begin
  edReplace.Enabled := cbReplace.Checked;
  btnReplace.Enabled := cbReplace.Checked;
  btnReplaceAll.Enabled := cbReplace.Checked;
end;


end.

