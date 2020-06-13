{ ==============================================================================

  EDITOR FRAME

    Provides all functions for a single editor window. Can have a syntax
    highlighter assigned externally, and has a StatusBar reference that can
    be used to update editor state (row, col, modified, etc) to a status bar.
    Breakpoints can be set, and a callback is provided to feedback this info
    externally


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

unit uEditorFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, Menus,
  SynEdit, SynEditHighlighter, SynEditTypes, SynEditMarks, Dialogs,
  SynEditKeyCmds, Graphics, LCLType,
  //
  uHighlighterAsm;

type
  TOnBreakpointEvent = procedure(Sender: TObject; LineNumber: integer; IsAdd: boolean) of object;

  { TEditorFrame }

  TEditorFrame = class(TFrame)
    EditorComponent: TSynEdit;
    imagesGutter: TImageList;
    menuPopupClose: TMenuItem;
    N1: TMenuItem;
    menuPopupFind: TMenuItem;
    menuPopupPaste: TMenuItem;
    menuPopupSelectAll: TMenuItem;
    N2: TMenuItem;
    menuPopupCopy: TMenuItem;
    menuPopupCut: TMenuItem;
    popupEditor: TPopupMenu;
    procedure EditorComponentChange(Sender: TObject);
    procedure EditorComponentDropFiles(Sender: TObject; {%H-}X, {%H-}Y: integer; aFiles: TStrings);
    procedure EditorComponentGutterClick(Sender: TObject; X, {%H-}Y, Line: integer; {%H-}mark: TSynEditMark);
    procedure EditorComponentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fNew: boolean;
    fFileName: string;
    fStatusBar: TStatusBar;
    fOnBreakpoint: TOnBreakpointEvent;
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    function HasBreakpoint(LineNumber: integer; var Index: integer): boolean;
    procedure OnReplaceText(Sender: TObject; const aSearch, aReplace: string; {%H-}aLine, {%H-}aColumn: integer; var aAction: TSynReplaceAction);
    function GetHighlighter: TSynAsmHighlighter;
    procedure SetHighlighter(aValue: TSynAsmHighlighter);
    procedure UpdateModifiedState;
    function GetIsEdited: boolean;
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SaveCurrentFile;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure AddBreakpoint(LineNumber: integer);
    procedure DeleteBreakpoint(LineNumber: integer);
    //
    property IsNew: boolean read fNew;
    property IsEdited: boolean read GetIsEdited;
    property FileName: string read fFileName;
    property StatusBarRef: TStatusBar write fStatusBar;
    property Highlighter: TSynAsmHighlighter read GetHighlighter write SetHighlighter;
    property OnBreakpoint: TOnBreakpointEvent read fOnBreakpoint write fOnBreakpoint;
  end;


implementation

{$R *.lfm}

uses uAssemblerForm;

{ CREATE / DESTROY }

constructor TEditorFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Parent := TWinControl(aOwner);
  Visible := True;
  fNew := True;
  EditorComponent.OnStatusChange := @EditorStatusChange;
  EditorComponent.OnReplaceText := @OnReplaceText;
end;


destructor TEditorFrame.Destroy;
begin
  inherited Destroy;
end;


{ SAVE }

procedure TEditorFrame.SaveCurrentFile;
begin
  EditorComponent.Lines.SaveToFile(fFileName);
  UpdateModifiedState;
end;


procedure TEditorFrame.SaveToFile(FileName: string);
begin
  fFileName := FileName;
  EditorComponent.Lines.SaveToFile(fFileName);
  UpdateModifiedState;
end;


{ LOAD }

procedure TEditorFrame.LoadFromFile(FileName: string);
begin
  fFileName := FileName;
  EditorComponent.Lines.LoadFromFile(fFileName);
  UpdateModifiedState;
end;


procedure TEditorFrame.EditorComponentDropFiles(Sender: TObject; X, Y: integer; aFiles: TStrings);
begin
  { TODO : uEditorFrame -> need to remove this reference }
  TAssemblerForm(Parent).LoadFiles(aFiles);
end;


{ GETTERS / SETTERS }

function TEditorFrame.GetHighlighter: TSynAsmHighlighter;
begin
  Result := TSynAsmHighlighter(EditorComponent.Highlighter);
end;


procedure TEditorFrame.SetHighlighter(aValue: TSynAsmHighlighter);
begin
  EditorComponent.Highlighter := aValue;
end;


function TEditorFrame.GetIsEdited: boolean;
begin
  Result := (TPage(Owner).Caption[1] = '*');
end;


{ STATUS MONITORING }

procedure TEditorFrame.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  ed: TSynEdit;
begin
  if (not Assigned(fStatusBar)) then
    Exit;

  if (Sender = nil) or (not (Sender is TSynEdit)) then
    begin
      fStatusBar.Panels[0].Text := '';
      fStatusBar.Panels[1].Text := '';
      fStatusBar.Panels[2].Text := '';
      fStatusBar.Panels[3].Text := '';
      Exit;
    end;

  ed := (Sender as TSynEdit);

  if (scCaretX in Changes) or (scCaretY in Changes) then
    fStatusBar.Panels[0].Text := Format(' %6d:%4d', [ed.CaretX, ed.CaretY]);

  if (scModified in Changes) then
    fStatusBar.Panels[1].Text := 'Modified'
  else
    fStatusBar.Panels[1].Text := '';

  if (scInsertMode in Changes) then
    if (ed.InsertMode) then
      fStatusBar.Panels[2].Text := 'INS'
    else
      fStatusBar.Panels[2].Text := 'OVR';

  if (scSelection in Changes) then
    AssemblerForm.UpdateActionStates;   // Text selected, update actions to reflect
end;


procedure TEditorFrame.EditorComponentChange(Sender: TObject);
begin
  if (TPage(Owner).Caption[1] <> '*') then
    TPage(Owner).Caption := '*' + TPage(Owner).Caption;
end;


procedure TEditorFrame.UpdateModifiedState;
begin
  fNew := False;
  TPage(Owner).Caption := ExtractFileName(fFileName);
  if Assigned(fStatusBar) then
    fStatusBar.Panels[3].Text := fFileName;
end;


{ ON KEY DOWN }

procedure TEditorFrame.EditorComponentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Windows keys already defined, add Cmd-keys for Mac
  {$ifdef darwin}
  if (Shift = [ssMeta]) then
    case Key of
      VK_A: EditorComponent.CommandProcessor(TSynEditorCommand(ecSelectAll), ' ', nil);
      VK_C: EditorComponent.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
      VK_V: EditorComponent.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
      VK_X: EditorComponent.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
      VK_Z: EditorComponent.CommandProcessor(TSynEditorCommand(ecUndo), ' ', nil);
    end;
  if (Shift = [ssShift, ssMeta]) then
    case Key of
      VK_Z: EditorComponent.CommandProcessor(TSynEditorCommand(ecRedo), ' ', nil);
    end;
  {$endif}
end;


{ ON GUTTER CLICK - select line }

procedure TEditorFrame.EditorComponentGutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
var
  idx: integer;
begin
  if (X < 20) then
    begin
      if HasBreakpoint(Line, idx{%H-}) then
        DeleteBreakpoint(Line)
      else
        AddBreakpoint(Line);
    end
  else
    begin
      EditorComponent.CaretY := Line;
      EditorComponent.SelectLine(True);
    end;
end;


{ ADD / DELETE BREAKPOINT AT LINE }

procedure TEditorFrame.AddBreakpoint(LineNumber: integer);
var
  idx: integer;
  m: TSynEditMark;
begin
  if ((LineNumber > EditorComponent.Lines.Count) or HasBreakpoint(LineNumber, idx{%H-})) then
    Exit;

  m := TSynEditMark.Create(EditorComponent);
  m.Line := LineNumber;
  m.ImageList := imagesGutter;
  m.ImageIndex := 1;
  m.Visible := True;
  EditorComponent.Marks.Add(m);

  if Assigned(fOnBreakpoint) then
    fOnBreakpoint(self, LineNumber, True);
end;


procedure TEditorFrame.DeleteBreakpoint(LineNumber: integer);
var
  idx: integer;
begin
  if HasBreakpoint(LineNumber, idx{%H-}) then
    begin
      EditorComponent.Marks.Delete(idx);
      EditorComponent.Invalidate;
      if Assigned(fOnBreakpoint) then
        fOnBreakpoint(self, LineNumber, False); // NEEDS TO BE CODE ADDRESS
    end;
end;


{ TODO : uEditorFrame -> on assembly put green 'LEDs' next to code lines,
                         only these can be made into breakpoints (red 'LEDs'}

function TEditorFrame.HasBreakpoint(LineNumber: integer; var Index: integer): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to (EditorComponent.Marks.Count - 1) do
    if (EditorComponent.Marks.Items[i].Line = LineNumber) then
      begin
        Index := i;
        Result := True;
        Break;
      end;
end;


{ ON REPLACE }

{ If PromptOnReplace set in SearchForm, then this routine is called by
  TSynEdit to ask user whether to replace, or not }

procedure TEditorFrame.OnReplaceText(Sender: TObject; const aSearch, aReplace: string; aLine, aColumn: integer; var aAction: TSynReplaceAction);

  function Shorten(const s: string): string;
  const
    MAX_LEN = 100;
  begin
    Result := s;
    if (Length(Result) > MAX_LEN) then
      Result := LeftStr(Result, MAX_LEN) + '...';
  end;

var
  ThisText: string;
  ThisAction: integer;
begin
  ThisText := 'Replace this occurrence of "%s"' + LineEnding + 'with "%s"?';
  ThisText := Format(ThisText, [Shorten(aSearch), Shorten(aReplace)]);
  ThisAction := MessageDlg(ThisText, mtconfirmation, [mbYes, mbYesToAll, mbNo, mbCancel], 0);
  case ThisAction of
    mrYes: aAction := raReplace;
    mrNo: aAction := raSkip;
    mrAll, mrYesToAll: aAction := raReplaceAll;
  else
    aAction := raCancel;
  end;
end;


end.

