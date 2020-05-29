{ ==============================================================================

  BMDS ASSEMBLER

    Cross assembler for a number of processors, currently 6502, 8080 and CHIP-8


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

unit uAssemblerForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  ActnList, Menus, StdCtrls, SynEdit, SynEditTypes, ClipBrd, ExtendedNotebook,
  StrUtils,
  //
  uEditorFrame, uSearchForm, uAssembler, uIniFile, uMRU, uHighlighterAsm,
  uCpuTypes, uMachineBase, uEdPrefsFrame, uEdColourPrefsFrame;

type
  TAsmLineInfo = record
    FileIndex: byte;                    // Max 256 files, 0 = main
    LineNumber: word;                   // Max 65535 lines in document, assume ok!
  end;
  TasmLinesInfo = array[0..$FFFF] of TAsmLineInfo;

  { TAssemblerForm }

  TAssemblerForm = class(TForm)
    actEditCopy: TAction;
    actFileClose: TAction;
    actFileExit: TAction;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAll: TAction;
    actFileSaveAs: TAction;
    actEditCut: TAction;
    actEditRedo: TAction;
    actEditUndo: TAction;
    actEditPaste: TAction;
    actEditSelectAll: TAction;
    actEditFind: TAction;
    ActionListFile: TActionList;
    ActionListEdit: TActionList;
    btnAssemble: TButton;
    FindDialog: TFindDialog;
    memoLog: TMemo;
    Notebook: TExtendedNotebook;
    OpenDialog: TOpenDialog;
    panelBottom: TPanel;
    panelLeft: TPanel;
    ReplaceDialog: TReplaceDialog;
    SaveDialog: TSaveDialog;
    panelRight: TPanel;
    splitterMiddle: TSplitter;
    splitterBottom: TSplitter;
    StatusBar1: TStatusBar;
    tbSep1: TToolButton;
    tbSep0: TToolButton;
    tbSep2: TToolButton;
    tbSep3: TToolButton;
    tbSep4: TToolButton;
    ToolBar1: TToolBar;
    tbNew: TToolButton;
    tbOpen: TToolButton;
    tbClose: TToolButton;
    tbSave: TToolButton;
    tbCut: TToolButton;
    tbCopy: TToolButton;
    tbPaste: TToolButton;
    tbAssemble: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    tbFind: TToolButton;
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditFindExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure btnAssembleClick(Sender: TObject);
    procedure DoReplace(Sender: TObject);
    procedure FindDialogClose(Sender: TObject);
    procedure DoFind(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure menuSearchFindNextClick(Sender: TObject);
    procedure menuSearchFindPreviousClick(Sender: TObject);
    procedure menuSearchReplaceClick(Sender: TObject);
    procedure NotebookChange(Sender: TObject);
  private
    MRU: TMRU;
    FindText: string;
    ReplaceText: string;
    FindOptions: TSynSearchOptions;
    ReplaceOptions: TSynSearchOptions;
    hltAsm: TSynAsmHighlighter;
    procedure SetEdColourPreferences(prefs: TEdColourPrefs);
    procedure BuildKeywordList;
    function GetActiveEditorFrame: TEditorFrame;
    function GetCurrentEditor: TSynEdit;
    procedure LoadFile(FileName: string);
    procedure SetActionStates;
    procedure SetMruMenuRef(aItem: TMenuItem);

    procedure StatusLog(msg: string);
    procedure BrkptHandler(Sender: TObject; LineNumber: integer; IsAdd: boolean);
  public
    procedure LoadFiles(Files: TStrings);
    procedure UpdateActionStates;
    procedure SetEditorPreferences(prefs: TEdPrefs);
    //
    property MruMenuRef: TMenuItem write SetMruMenuRef;
  end;

var
  AssemblerForm: TAssemblerForm;

const
  // Ensure any changes made here are reflected in 'TAssembler.AddDirectives'
  DIRECTIVES: string =                  // For highlighter
    'END' + #13 + 'ORG' + #13 + 'RMB' + #13 + 'DS' + #13 + 'DEFS' + #13 +
    'BYTE'  + #13 + 'FCB' + #13 + 'DB' + #13 + 'DEFB' + #13 + 'WORD' + #13 +
    'FDB' + #13 + 'DW' + #13 + 'DEFW' + #13 + 'TEXT' + #13 + 'FCC' + #13 +
    'EQU' + #13;


implementation

{$R *.lfm}

const
  SECT_ASM      = 'AssemblerForm';      // INI file settings
  INI_PREFIX    = 'Asm';
  PANELB_HEIGHT = 'PBHeight';
  PANELR_WIDTH  = 'PRWidth';


{ CREATE }

procedure TAssemblerForm.FormCreate(Sender: TObject);
begin
  Left := AppIni.ReadInteger(SECT_ASM, INI_WDW_LEFT, 20);
  Top := AppIni.ReadInteger(SECT_ASM, INI_WDW_TOP, 20);
  Width := AppIni.ReadInteger(SECT_ASM, INI_WDW_WIDTH, 640);
  Height := AppIni.ReadInteger(SECT_ASM, INI_WDW_HEIGHT, 480);
  panelBottom.Height := AppIni.ReadInteger(SECT_ASM, PANELB_HEIGHT, 50);
  panelRight.Width  := AppIni.ReadInteger(SECT_ASM, PANELR_WIDTH, 150);
  // Following is written out in MainForm.WriteIniSettings, but read here
  Visible := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_WDW_VIS, True);

  hltAsm := TSynAsmHighlighter.Create(self);
  BuildKeywordList;

  MRU := TMRU.Create;                   // Need to seperately assign MenuRef
  MRU.OnMenuClick := @LoadFile;
  UpdateActionStates;
end;


{ BUILD HIGHLIGHTER KEYWORD LIST }

procedure TAssemblerForm.BuildKeywordList;
var
  i: integer;
  list, mnem, lastmnem: string;
begin
  list := '';
  lastmnem := '';
  for i := 1 to Machine.CPU.DataCount-1 do //Length(oa)-1 do
    begin
      mnem := Machine.CPU.DataByIndex[i].M; // oa[i].M;
      if (mnem <> lastmnem) then
        begin
          list := list + mnem + #13;
          lastmnem := mnem;
        end;
    end;
  // Then add in list of assembler directives, and the CPU registers list
  list := UpperCase(list) +
          DIRECTIVES +                  // Includes a #13 at end
          ReplaceStr(Machine.CPU.Info.Registers, ' ', #13);
  hltAsm.Keywords := list;
end;


{ DESTROY }

procedure TAssemblerForm.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_ASM, INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_ASM, INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_ASM, INI_WDW_WIDTH, Width);
  AppIni.WriteInteger(SECT_ASM, INI_WDW_HEIGHT, Height);
  AppIni.WriteInteger(SECT_ASM, PANELB_HEIGHT, panelBottom.Height);
  AppIni.WriteInteger(SECT_ASM, PANELR_WIDTH, panelRight.Width);
  hltAsm.Free;
  MRU.Free;
end;


{ MENU - ASSEMBLER ITEMS }

procedure TAssemblerForm.actFileExitExecute(Sender: TObject);
begin
  Application.MainForm.SetFocus;
  Close;
end;


{ MENU - FILE ITEMS }

procedure TAssemblerForm.actFileNewExecute(Sender: TObject);
var
  NewTabSheet: TTabSheet;
  ef: TEditorFrame;
begin
  NewTabSheet := Notebook.AddTabSheet;
  NewTabSheet.Caption := Format('Untitled%d', [Notebook.PageCount]);
  TEditorFrame.Create(NewTabSheet);
  Notebook.ActivePage := NewTabSheet;
  ef := TEditorFrame(Notebook.ActivePage.Controls[0]);
  ef.StatusBarRef := StatusBar1;
  ef.OnBreakpoint := @BrkptHandler;
  ef.Highlighter := hltAsm;
  ef.EditorComponent.Text := Machine.CPU.Info.Template;
  NewTabSheet.SetFocus;
  UpdateActionStates;
end;


procedure TAssemblerForm.actFileOpenExecute(Sender: TObject);
begin
  if (OpenDialog.Execute) then
    LoadFiles(OpenDialog.Files);
end;


procedure TAssemblerForm.LoadFile(FileName: string);
begin
  actFileNewExecute(nil);
  GetActiveEditorFrame.LoadFromFile(FileName);
end;


procedure TAssemblerForm.actFileSaveExecute(Sender: TObject);
begin
  if (GetActiveEditorFrame <> nil) then
    if (GetActiveEditorFrame.IsNew) then
      actFileSaveAsExecute(Sender)      // Newly created file, call Save As instead
    else
      GetActiveEditorFrame.SaveCurrentFile;
  UpdateActionStates;
end;


procedure TAssemblerForm.actFileSaveAsExecute(Sender: TObject);
begin
  if ((GetActiveEditorFrame <> nil) and SaveDialog.Execute) then
    begin
      GetActiveEditorFrame.SaveToFile(SaveDialog.FileName);
      MRU.AddToRecent(SaveDialog.FileName);
    end;
  UpdateActionStates;
end;


procedure TAssemblerForm.actFileCloseExecute(Sender: TObject);
var
  i: integer;
begin
  if (GetActiveEditorFrame <> nil) then
    begin
      if (GetActiveEditorFrame.IsEdited)
          and (MessageDlg('File changed', 'The file you are going to close has changed.'
               + LineEnding + 'Would you like to save it first?',
               mtWarning, [mbYes, mbNo], 0) = mrYes) then
        actFileSaveExecute(Sender);

      i := Notebook.ActivePageIndex;
      Notebook.SelectNextPage(True);
      TCustomTabControl(Notebook).Pages.Delete(i);
    end;
  UpdateActionStates;
end;


{ MENU - EDIT ITEMS }

procedure TAssemblerForm.actEditUndoExecute(Sender: TObject);
var
  ed: TSynEdit;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    if (ed.CanUndo) then
      ed.Undo;
end;


procedure TAssemblerForm.actEditRedoExecute(Sender: TObject);
var
  ed: TSynEdit;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    if (ed.CanRedo) then
      ed.Redo;
end;


procedure TAssemblerForm.actEditCutExecute(Sender: TObject);
var
  ed: TSynEdit;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    if (ed.SelAvail) then
      ed.CutToClipboard;
end;


procedure TAssemblerForm.actEditCopyExecute(Sender: TObject);
var
  ed: TSynEdit;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    begin
      if (not ed.SelAvail) then
        ed.SelectWord;
      ed.CopyToClipboard;
    end;
end;


{ TODO : uAssemblerForm -> cmd-V does not work, but ctrl-V does ? }

procedure TAssemblerForm.actEditPasteExecute(Sender: TObject);
var
  ed: TSynEdit;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    if (ed.SelAvail) then
      ed.PasteFromClipboard;
end;


procedure TAssemblerForm.actEditSelectAllExecute(Sender: TObject);
var
  ed: TSynEdit;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    ed.SelectAll;
end;


{ MENU SEARCH ITEMS }

procedure TAssemblerForm.actEditFindExecute(Sender: TObject);
var
  ed: TSynEdit;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    begin
      if (ed.SelAvail) and (ed.BlockBegin.Y = ed.BlockEnd.Y) then
        FindDialog.FindText := ed.SelText;
      FindDialog.Execute;
    end;
end;


procedure TAssemblerForm.menuSearchFindNextClick(Sender: TObject);
var
  opt: TSynSearchOptions;
  ed: TSynEdit;
begin
  if (FindText = '') then
    Exit;

  ed := GetCurrentEditor;
  if Assigned(ed) then
    begin
      opt := FindOptions - [ssoBackWards];
      if (ed.SearchReplace(FindText, '', opt) = 0) then
        ShowMessage(Format('Text "%s", not found', [FindText]));
    end;
end;


procedure TAssemblerForm.menuSearchFindPreviousClick(Sender: TObject);
var
  opt: TSynSearchOptions;
  ed: TSynEdit;
begin
  if (FindText = '') then
    Exit;

  ed := GetCurrentEditor;
  if Assigned(ed) then
    begin
      opt := FindOptions + [ssoBackWards];
      if (ed.SearchReplace(FindText, '', opt) = 0) then
        ShowMessage(Format('Text "%s", not found', [FindText]));
    end;
end;


procedure TAssemblerForm.menuSearchReplaceClick(Sender: TObject);
var
  ed: TSynEdit;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    ReplaceDialog.Execute;
end;


{ DEBUG - BUTTONS - ASSEMBLE }

procedure TAssemblerForm.btnAssembleClick(Sender: TObject);
var
  ThisInputFile, tmp: string;
  ThisAssembler: TAssembler;
begin
  if (GetActiveEditorFrame <> nil) then
    begin
      memoLog.Lines.Clear;
      ThisInputFile := GetActiveEditorFrame.FileName;
      ThisAssembler := TAssembler.Create;
      ThisAssembler.OnLog := @StatusLog;
      try
        ThisAssembler.Execute(ThisInputFile);
      finally
        ThisAssembler.Free;
      end;
    end;

  tmp := ChangeFileExt(ThisInputFile, '.lst');
  if FileExists(tmp) then
    LoadFile(tmp);
end;


{ FORM CLOSE QUERY }

procedure TAssemblerForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  while (Notebook.PageCount > 0) do
    begin
      Notebook.ActivePage := Notebook.Pages[0];
      actFileCloseExecute(self);
    end;
  CanClose := True;
end;


{ GET ACTIVE EDITOR FRAME }

function TAssemblerForm.GetActiveEditorFrame: TEditorFrame;
begin
  Result := nil;
  if (Notebook.ActivePage <> nil) then
    Result := TEditorFrame(Notebook.ActivePage.Controls[0]);
end;


function TAssemblerForm.GetCurrentEditor: TSynEdit;
begin
  Result := nil;
  if (GetActiveEditorFrame <> nil) then
    Result := GetActiveEditorFrame.EditorComponent;
end;


{ FIND / REPLACE ROUTINES }

procedure TAssemblerForm.DoFind(Sender: TObject);
var
  ed: TSynEdit;
  fd: TFindDialog;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    begin
      fd := (Sender as TFindDialog);
      ReplaceOptions := [];
      if (not (frDown in fd.Options)) then
        ReplaceOptions := ReplaceOptions + [ssoBackWards];
      if (frMatchCase in fd.Options) then
        ReplaceOptions := ReplaceOptions + [ssoMatchCase];
      if (frWholeWord in fd.Options) then
        ReplaceOptions := ReplaceOptions + [ssoWholeWord];
      FindText := fd.FindText;
      if (ed.SearchReplace(FindText, '', ReplaceOptions) = 0) then
        ShowMessage(Format('Text "%s" not found', [FindText]));
    end;
end;


procedure TAssemblerForm.FindDialogClose(Sender: TObject);
begin
  self.BringToFront;
end;


procedure TAssemblerForm.DoReplace(Sender: TObject);
var
  ed: TSynEdit;
  rd: TReplaceDialog;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    begin
      rd := (Sender as TReplaceDialog);
      FindOptions := [];
      if (not (frDown in rd.Options)) then
        FindOptions := FindOptions + [ssoBackWards];
      if (frMatchCase in rd.Options) then
        FindOptions := FindOptions + [ssoMatchCase];
      if (frWholeWord in rd.Options) then
        FindOptions := FindOptions + [ssoWholeWord];
      if (frReplace in rd.Options) then
        ReplaceOptions := ReplaceOptions + [ssoReplace];
      if (frReplaceAll in rd.Options) then
        ReplaceOptions := ReplaceOptions + [ssoReplaceAll];
      if (frFindNext in rd.Options) then
        ReplaceOptions := ReplaceOptions - [ssoReplace, ssoReplaceAll];
      if (ed.SelAvail) then
        ReplaceOptions := ReplaceOptions + [ssoSelectedOnly];

      FindText := rd.FindText;
      ReplaceText := rd.ReplaceText;

      if (ed.SearchReplace(FindText, ReplaceText, ReplaceOptions) = 0) then
        ShowMessage(Format('Text "%s" not found', [FindText]))
      else
        if ((ssoReplace in ReplaceOptions) and not (ssoReplaceAll in ReplaceOptions)) then
          begin
            ReplaceOptions := ReplaceOptions - [ssoReplace];
            ed.SearchReplace(FindText, '', ReplaceOptions); //Search and select next occurence
          end;
    end;
end;


{ DROP FILES }

procedure TAssemblerForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  Files: TStrings;
  FileName: string;
begin
  Files := TStringList.Create;
  try
    for FileName in FileNames do
      Files.Add(FileName);
    LoadFiles(Files);
  finally
    FreeAndNil(Files);
  end;
end;


{ LOAD FILES }

procedure TAssemblerForm.LoadFiles(Files: TStrings);
var
  FileName: string;
begin
  for FileName in Files do
    begin
      actFileNewExecute(nil);
      GetActiveEditorFrame.LoadFromFile(FileName);
      MRU.AddToRecent(FileName);
    end;
end;


{ SEND STATUS MESSAGE TO MEMO }

procedure TAssemblerForm.StatusLog(msg: string);
begin
  memoLog.Lines.Add(msg);
end;


{ UPDATE ACTIONS, AND ASSOCIATED MENU ITEMS }

procedure TAssemblerForm.SetActionStates;
begin
  actFileSave.Enabled := False;
  actFileSaveAs.Enabled := False;
  actFileSaveAll.Enabled := False;
  actFileClose.Enabled := False;
  //menuFileCloseAll.Enabled := False;  // NEED TO USE ACTION

  actEditUndo.Enabled := False;
  actEditRedo.Enabled := False;
  actEditCut.Enabled := False;
  actEditCopy.Enabled := False;
  actEditPaste.Enabled := False;
  actEditSelectAll.Enabled := False;
  actEditFind.Enabled := False;
end;


procedure TAssemblerForm.UpdateActionStates;
var
  ed: TSynEdit;
  HasEditor, HasSelection, HasClipPaste: boolean;
begin
  SetActionStates;                      // Set all off
  ed := GetCurrentEditor;
  if Assigned(ed) then
    begin                               // Then reassign as appropriate
      HasEditor := (Notebook.PageCount > 0);
      HasSelection := HasEditor and (ed.SelAvail);
      HasClipPaste := HasEditor and (ClipBoard.AsText <> '');

      actFileSave.Enabled := HasEditor;
      actFileSaveAs.Enabled := HasEditor;
      actFileSaveAll.Enabled := HasEditor;
      actFileClose.Enabled := HasEditor;
      actEditSelectAll.Enabled := HasEditor;
      actEditFind.Enabled := HasEditor;

      actEditUndo.Enabled := ed.CanUndo;
      actEditRedo.Enabled := ed.CanRedo;

      actEditCut.Enabled := HasSelection;
      actEditCopy.Enabled := HasSelection;

      actEditPaste.Enabled := HasClipPaste;
    end;
end;


procedure TAssemblerForm.NotebookChange(Sender: TObject);
begin
  UpdateActionStates;
end;


procedure TAssemblerForm.BrkptHandler(Sender: TObject; LineNumber: integer; IsAdd: boolean);
begin
  if IsAdd then
    memoLog.Lines.Add(Format('Adding %d', [LineNumber]))
  else
    memoLog.Lines.Add(Format('Removing %d', [LineNumber]));
end;


{ SET EDITOR PREFERENCES }

procedure TAssemblerForm.SetEditorPreferences(prefs: TEdPrefs);
var
  i: integer;
  ef: TSynEdit;
begin
  if (Notebook.PageCount = 0) then
    Exit;

  for i := 0 to Notebook.PageCount-1 do
    begin
      ef := TEditorFrame(Notebook.Pages[i].Controls[0]).EditorComponent;
      ef.Font.Name := prefs.FontName;
      ef.Font.Size := prefs.FontSize;
      ef.Font.Quality := prefs.Quality;
      ef.TabWidth := prefs.TabWidth;
      ef.RightEdge := prefs.RightMargin;
      ef.Gutter.Visible := prefs.VisibleGutter;
      if (prefs.TabsToSpaces) then
        ef.Options := ef.Options + [eoTabsToSpaces]
      else
        ef.Options := ef.Options - [eoTabsToSpaces];
      if (prefs.SmartTabs) then
        ef.Options := ef.Options + [eoSmartTabs]
      else
        ef.Options := ef.Options - [eoSmartTabs];
      if (prefs.VisibleRightMargin) then
        ef.Options := ef.Options - [eoHideRightMargin]
      else
        ef.Options := ef.Options + [eoHideRightMargin];
    end;
end;


{ SET EDITOR COLOUR PREFERENCES }

procedure TAssemblerForm.SetEdColourPreferences(prefs: TEdColourPrefs);
var
  i: integer;
  ef: TSynEdit;
begin
  if (Notebook.PageCount = 0) then
    Exit;

  for i := 0 to Notebook.PageCount-1 do
    begin
      ef := TEditorFrame(Notebook.Pages[i].Controls[0]).EditorComponent;
      // apply prefs
    end;
end;


{ SET MAIN MENU REFERENCE }

procedure TAssemblerForm.SetMruMenuRef(aItem: TMenuItem);
begin
  MRU.RecentMenuItem := aItem;
end;


end.

