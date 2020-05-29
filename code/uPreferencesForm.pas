{ ==============================================================================

  PREFERENCES FORM

    Provides user with facility to amend various options for the machines, the
    assembler, and the editor.
    Saves form position to application's INI file


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

unit uPreferencesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls,
  //
  uPrefsFrameBase, uGenPrefsFrame, uAsmPrefsFrame, uEdPrefsFrame,
  uEdColourPrefsFrame, uIniFile;

type
  // These must match up with TreeView items
  TCategoryIndex = (ciGeneral, ciMachine, ciMachName, ciAsm,
                    ciEditor, ciDisplay, ciColours);

  TPrefsFrames = array[ciGeneral..ciColours] of TPrefsFrame;

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Button1: TButton;
    PanelFrames: TPanel;
    PanelBottom: TPanel;
    tvCategory: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);    
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure tvCategoryChange(Sender: TObject; Node: TTreeNode);
    procedure SetCategory(category: TCategoryIndex);
    function  GetFrame(i: TCategoryIndex): TPrefsFrame;
  private
    fPrefsFrames: TPrefsFrames;
    fCategory: TCategoryIndex;
    fCurrentFrame: TPrefsFrame;
    procedure AddFrames;
  public
    procedure SetMachineConfigFrame(aName: string; aConfigFrame: TPrefsFrame);
    //
    property Category: TCategoryIndex write SetCategory;
    property Frames[i: TCategoryIndex]: TPrefsFrame read GetFrame;
  end;

var
  PreferencesForm: TPreferencesForm;


implementation

{$R *.lfm}

const
  SECT_PREFS = 'PrefsForm';
  INI_CAT    = 'Category';

{ CREATE }

procedure TPreferencesForm.FormCreate(Sender: TObject);
var
  tmp: TCategoryIndex;
begin
  {$ifdef darwin}
    Caption := 'Preferences';
  {$else}
    Caption := 'Options';
  {$endif}
  Left := AppIni.ReadInteger(SECT_PREFS, INI_WDW_LEFT, 60);
  Top := AppIni.ReadInteger(SECT_PREFS, INI_WDW_TOP, 60);
  Width := AppIni.ReadInteger(SECT_PREFS, INI_WDW_WIDTH, 600);
  Height := AppIni.ReadInteger(SECT_PREFS, INI_WDW_HEIGHT, 400);
  tmp := TCategoryIndex(AppIni.ReadInteger(SECT_PREFS, INI_CAT, 0));

  AddFrames;
  SetCategory(tmp);                     // Set last selected category
end;


{ DESTROY }

procedure TPreferencesForm.FormDestroy(Sender: TObject);
var
  i: TCategoryIndex;
  ThisFrame: TPrefsFrame;
begin
  AppIni.WriteInteger(SECT_PREFS, INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_PREFS, INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_PREFS, INI_WDW_WIDTH, Width);
  AppIni.WriteInteger(SECT_PREFS, INI_WDW_HEIGHT, Height);
  AppIni.WriteInteger(SECT_PREFS, INI_CAT, Ord(fCategory));

  for i := Low(TCategoryIndex) to High(TCategoryIndex) do
    begin
      ThisFrame := fPrefsFrames[i];
      if Assigned(ThisFrame) then
        begin
          ThisFrame.Parent := nil;      // Avoids pointer errors
          ThisFrame.Free;
        end;
    end;
end;


{ ADD FRAMES - Assembler, Editor, Editor Colours }

procedure TPreferencesForm.AddFrames;
var
  i: TCategoryIndex;
  ThisFrame: TPrefsFrame;
begin
  fPrefsFrames[ciGeneral]  := TGenPrefsFrame.Create(self);
  // MachineFrame is assigned separately, see SetMachineConfigFrame below
  { TODO : uPreferencesForm -> gray frame if CPU has no assembler support }
  fPrefsFrames[ciAsm]      := TAsmPrefsFrame.Create(self);
  fPrefsFrames[ciDisplay]  := TEdPrefsFrame.Create(self);
  fPrefsFrames[ciColours]  := TEdColourPrefsFrame.Create(self);
  for i := Low(TCategoryIndex) to High(TCategoryIndex) do
    begin
      ThisFrame := fPrefsFrames[i];
      if Assigned(ThisFrame) then
        begin
          ThisFrame.Init;
          ThisFrame.Parent := PanelFrames;
          ThisFrame.Visible := False;
        end;
    end;
end;


{ SET CATEGORY IN TREEVIEW }

procedure TPreferencesForm.SetCategory(category: TCategoryIndex);
begin
  fCategory := category;
  tvCategory.Selected := tvCategory.Items[Ord(category)];
end;


{ BUTTON OK }

procedure TPreferencesForm.btnOKClick(Sender: TObject);
var
  i: TCategoryIndex;
begin
  for i := Low(TCategoryIndex) to High(TCategoryIndex) do
    if Assigned(fPrefsFrames[i]) then
      fPrefsFrames[i].SaveChanges;
  Close;
end;


{ BUTTON CANCEL }

procedure TPreferencesForm.btnCancelClick(Sender: TObject);
var
  i: TCategoryIndex;
begin
  for i := Low(TCategoryIndex) to High(TCategoryIndex) do
    if Assigned(fPrefsFrames[i]) then
      fPrefsFrames[i].CancelChanges;
  Close;
end;


{ TREE VIEW CLICK }

procedure TPreferencesForm.tvCategoryChange(Sender: TObject; Node: TTreeNode);
begin
  if (fCurrentFrame <> nil) then
    fCurrentFrame.Visible := False;

  fCategory := TCategoryIndex(Node.AbsoluteIndex);
  if (fCategory in [ciMachine, ciEditor]) then
    begin
      fCategory := Succ(fCategory);     // Skip 'parent' nodes, select first child
      SetCategory(fCategory);
    end;

  fCurrentFrame := fPrefsFrames[fCategory];
  fCurrentFrame.Visible := True;
end;


{ GET / SET ROUTINES }

function TPreferencesForm.GetFrame(i: TCategoryIndex): TPrefsFrame;
begin
  Result := fPrefsFrames[i];
end;


{ SET MACHINE CONFIG FRAME }

procedure TPreferencesForm.SetMachineConfigFrame(aName: string; aConfigFrame: TPrefsFrame);
begin
  if (aConfigFrame <> nil) and (fPrefsFrames[ciMachName] <> aConfigFrame) then // Do assignment once only
    begin
      fPrefsFrames[ciMachName] := aConfigFrame;
      fPrefsFrames[ciMachName].Parent := PanelFrames;
      fPrefsFrames[ciMachName].Visible := False;
      tvCategory.Items[Ord(ciMachName)].Text := aName;
    end;
end;


end.

