{ ==============================================================================

  BMDS ASSEMBLER

    Cross assembler for a number of processors, currently 6502, 8080 and CHIP-8

    This form handles the management of multiple editors including standard
    editing functions (cut, copy, paste, etc) and find & replace. It calls the
    main assembler module to build code

    The form save its state in the application's INI file


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

{ TODO : uAssemblerForm -> save changed files before Asm Execute }


unit uAssemblerForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  ActnList, Menus, StdCtrls, SynEdit, SynEditTypes, ClipBrd, ExtendedNotebook,
  StrUtils, LCLProc,
  //
  uEditorFrame, uSearchForm, uAssembler, uIniFile, uMRU, uHighlighterAsm,
  uCpuTypes, uMachineBase, uEdPrefsFrame, uEdColourPrefsFrame, uFormatter,
  uCommon;

type
  TOnLogEvent = procedure(Msg: string) of object;

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
    actFormatter: TAction;
    actAssembler: TAction;
    actSearchFindPrev: TAction;
    actSearchFindNext: TAction;
    actSearchReplace: TAction;
    ActionListFile: TActionList;
    ActionListEdit: TActionList;
    ActionListSearch: TActionList;
    actSearchFind: TAction;
    btnAssemble: TButton;
    memoLog: TMemo;
    Notebook: TExtendedNotebook;
    OpenDialog: TOpenDialog;
    panelBottom: TPanel;
    panelLeft: TPanel;
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
    procedure actAssemblerExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actFormatterExecute(Sender: TObject);
    procedure actButtonsUpdate(Sender: TObject);
    procedure actSearchFindExecute(Sender: TObject);
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
    procedure actSearchFindNextExecute(Sender: TObject);
    procedure actSearchFindPrevExecute(Sender: TObject);
    procedure actSearchReplaceExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure NotebookChange(Sender: TObject);
  private
    fOnLog: TOnLogEvent;                // Callback with status/log info
    MRU: TMRU;
    SearchForm: TSearchForm;
    hltAsm: TSynAsmHighlighter;
    CurrentEd: TSynEdit;
    procedure AssignActionShortcuts;
    function DoFindAndReplace(aFindText, aReplaceText: string; aOptions: TSynSearchOptions): integer;
    procedure StartFindReplace(ReplaceFlag: boolean);
    procedure ReadIniSettings;
    procedure SetEdColourPreferences(prefs: TEdColourPrefs);
    procedure BuildKeywordList;
    function GetActiveEditorFrame: TEditorFrame;
    function GetCurrentEditor: TSynEdit;
    procedure LoadFile(FileName: string);
    procedure SetActionStates;
    procedure SetMruMenuRef(aItem: TMenuItem);

    procedure StatusLog(msg: string; DestMain: boolean);
    procedure BrkptHandler(Sender: TObject; LineNumber: integer; IsAdd: boolean);
    procedure WriteIniSettings;
  public
    procedure LoadFiles(Files: TStrings);
    procedure UpdateActionStates;
    procedure SetEditorPreferences(prefs: TEdPrefs);
    //
    property MruMenuRef: TMenuItem write SetMruMenuRef;
    property OnLog: TOnLogEvent read fOnLog write fOnLog;
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


{ CREATE / DESTROY }

procedure TAssemblerForm.FormCreate(Sender: TObject);
begin
  AssignActionShortcuts;
  ReadIniSettings;

  hltAsm := TSynAsmHighlighter.Create(self);
  BuildKeywordList;

  MRU := TMRU.Create;                   // Need to seperately assign MenuRef
  MRU.OnMenuClick := @LoadFile;
  SearchForm := TSearchForm.Create(nil);
  UpdateActionStates;
end;


procedure TAssemblerForm.FormDestroy(Sender: TObject);
begin
  WriteIniSettings;
  hltAsm.Free;
  MRU.Free;
  SearchForm.Free;
end;


{ BUILD HIGHLIGHTER KEYWORD LIST }

procedure TAssemblerForm.BuildKeywordList;
var
  i: integer;
  list, mnem, lastmnem: string;
begin
  list := '';
  lastmnem := '';
  for i := 1 to (Machine.CPU.DataCount - 1) do
    begin
      mnem := Machine.CPU.DataByIndex[i].M;
      if (mnem <> lastmnem) then
        begin
          list := list + mnem + #13;
          lastmnem := mnem;
        end;
    end;
  // Then add in list of assembler directives, and the CPU registers list
  list := UpperCase(list) +
          DIRECTIVES +                  // Includes a #13 at end
          ReplaceStr(Machine.CPU.Info.RegsKeywords + ' ' + Machine.CPU.Info.RegsReplace, ' ', #13);
  hltAsm.Keywords := list;
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
  if (FileExists(FileName)) then
    begin
      actFileNewExecute(nil);               // Create empty editor, load to it
      MRU.AddToRecent(FileName);            // Put this latest to top of MRU
      GetActiveEditorFrame.LoadFromFile(FileName);
      GetActiveEditorFrame.Highlighter.FileExt := UpperCase(ExtractFileExt(FileName));
    end
  else
    MessageWarning('File Not Found', Format('Requested file ''%s'' cannot be found!', [FileName]));
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
      if (GetActiveEditorFrame.IsEdited) and
         (MessageDlg('File changed', 'The file you are going to close has changed.'
                     + LineEnding + 'Would you like to save it first?',
                     mtWarning, [mbYes, mbNo], 0) = mrYes) then
        actFileSaveExecute(Sender);

      i := Notebook.ActivePageIndex;
      Notebook.SelectNextPage(True);
      TCustomTabControl(Notebook).Pages.Delete(i);
    end;
  UpdateActionStates;
end;


{ EDIT ITEMS }

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


procedure TAssemblerForm.actEditPasteExecute(Sender: TObject);
var
  ed: TSynEdit;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    //if (ed.SelAvail) then
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


{ SEARCH ITEMS }

{ If the PromptOnReplace option is selected, the TSynEdit component calls
  the OnReplaceText event which is handled by the 'OnReplaceText' procedure
  in uEditorFrame to ask the user for a decision each time }

procedure TAssemblerForm.actSearchFindExecute(Sender: TObject);
begin
  StartFindReplace(False);
end;


procedure TAssemblerForm.actSearchReplaceExecute(Sender: TObject);
begin
  StartFindReplace(True);
end;


procedure TAssemblerForm.StartFindReplace(ReplaceFlag: boolean);
var
  Opts: TSynSearchOptions;
begin
  CurrentEd := GetCurrentEditor;
  if (not Assigned(CurrentEd)) then Exit;

  SearchForm.WriteIniSettings;
  Opts := SearchForm.Options;
  if (ReplaceFlag) then
    Opts := Opts + [ssoReplace, ssoReplaceAll]
  else
    Opts := Opts - [ssoReplace, ssoReplaceAll];
  SearchForm.Options := Opts;

  if (CurrentEd.SelAvail) and (CurrentEd.BlockBegin.Y = CurrentEd.BlockEnd.Y) then
    SearchForm.FindText := CurrentEd.SelText
  else
    SearchForm.FindText := '';

  if (SearchForm.ShowModal = mrCancel) then
    begin
      SearchForm.ReadIniSettings;
      Exit;
    end
  else
    DoFindAndReplace(SearchForm.FindText, SearchForm.ReplaceText, SearchForm.Options);
end;


function TAssemblerForm.DoFindAndReplace(aFindText, aReplaceText: string; aOptions: TSynSearchOptions): integer;
var
  aText, aCaption: string;
  OldEntireScope, Again: boolean;
begin
  Result := 0;
  if ((ssoReplace in aOptions) and CurrentEd.ReadOnly) then
    Exit;

  OldEntireScope := (ssoEntireScope in aOptions);
  if (ssoBackwards in aOptions) then
    // Caret in the last line and last character?
    Again := ((CurrentEd.CaretY >= CurrentEd.Lines.Count) and (CurrentEd.CaretX > Length(CurrentEd.LineText)))
  else
    // Caret in first position, top/left?
    Again := ((CurrentEd.CaretY = 1) and (CurrentEd.CaretX = 1));

  repeat
    Result := CurrentEd.SearchReplace(aFindText, aReplaceText, aOptions);
    if ((Result = 0) and not (ssoReplaceAll in aOptions)) then
      begin
        aCaption := 'Not Found';
        aText := Format('Search string ''%s'' not found.', [aFindText]);
        if (not (Again or OldEntireScope)) then
          begin
            if (ssoBackwards in aOptions) then
              aText := aText + ' Continue search from the end?'
            else
              aText := aText + ' Continue search from the beginning?';
            Again := MessageQuery(aCaption, aText);
            aOptions := aOptions + [ssoEntireScope];
          end
        else
          begin
            Again := False;
            MessageWarning(aCaption, aText);
          end;
      end
    else
      Again := False;
  until (not Again);
end;


procedure TAssemblerForm.actSearchFindNextExecute(Sender: TObject);
var
  Opts: TSynSearchOptions;
begin
  if (SearchForm.FindText = '') then
    StartFindReplace(False)
  else
    begin
      Opts := SearchForm.Options - [ssoEntireScope] + [ssoFindContinue];
      DoFindAndReplace(SearchForm.FindText, SearchForm.ReplaceText, Opts);
    end;
end;


procedure TAssemblerForm.actSearchFindPrevExecute(Sender: TObject);
var
  Opts: TSynSearchOptions;
begin
  if (SearchForm.FindText = '') then
    StartFindReplace(False)
  else
    begin
      Opts := SearchForm.Options - [ssoEntireScope] + [ssoFindContinue];
      if (ssoBackwards in Opts) then
        Opts := Opts - [ssoBackwards]
      else
        Opts := Opts + [ssoBackwards];
      DoFindAndReplace(SearchForm.FindText, SearchForm.ReplaceText, Opts);
    end;
end;


{ DEBUG - BUTTONS - ASSEMBLE }

procedure TAssemblerForm.actAssemblerExecute(Sender: TObject);
var
  ThisInputFile, tmp: string;
  ThisAssembler: TAssembler;
begin
  if (GetActiveEditorFrame <> nil) then
    begin
      memoLog.Lines.Clear;
      ThisInputFile := GetActiveEditorFrame.FileName;
      if (LowerCase(ExtractFileExt(ThisInputFile)) <> '.asm') then
        if (not MessageQuery('Incorrect File Type?', 'This file is not a .ASM file. Do you really want to try to assemble it?')) then
          Exit;
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

procedure TAssemblerForm.StatusLog(msg: string; DestMain: boolean);
begin
  if (DestMain) then                    // Message for main form?
    begin
      if Assigned(fOnLog) then          // then pass upwards if handler assigned
        fOnLog(msg)
    end
  else
    memoLog.Lines.Add(msg);             // else report locally
end;


{ UPDATE ACTIONS, AND ASSOCIATED MENU ITEMS }

procedure TAssemblerForm.SetActionStates;
begin
  actFileSave.Enabled := False;
  actFileSaveAs.Enabled := False;
  actFileSaveAll.Enabled := False;
  actFileClose.Enabled := False;
  //menuFileCloseAll.Enabled := False;  // NEED TO USE ACTION
  //
  actEditUndo.Enabled := False;
  actEditRedo.Enabled := False;
  actEditCut.Enabled := False;
  actEditCopy.Enabled := False;
  actEditPaste.Enabled := False;
  actEditSelectAll.Enabled := False;
  //
  actSearchFind.Enabled := False;
  actSearchReplace.Enabled := False;
  actSearchFindNext.Enabled := False;
  actSearchFindPrev.Enabled := False;
end;


procedure TAssemblerForm.UpdateActionStates;
var
  ed: TSynEdit;
  HasEditor, HasSelection, HasClipPaste: boolean;
begin
  SetActionStates;                      // Set all off, then update ...
  ed := GetCurrentEditor;
  if Assigned(ed) then
    begin                               // ... if Editor exists
      HasEditor := (Notebook.PageCount > 0);
      HasSelection := HasEditor and (ed.SelAvail);
      HasClipPaste := HasEditor and (ClipBoard.AsText <> '');

      actFileSave.Enabled := HasEditor;
      actFileSaveAs.Enabled := HasEditor;
      actFileSaveAll.Enabled := HasEditor;
      actFileClose.Enabled := HasEditor;

      actEditSelectAll.Enabled := HasEditor;
      actEditUndo.Enabled := ed.CanUndo;
      actEditRedo.Enabled := ed.CanRedo;
      actEditCut.Enabled := HasSelection;
      actEditCopy.Enabled := HasSelection;
      actEditPaste.Enabled := HasClipPaste;

      actSearchFind.Enabled := HasEditor;
      actSearchReplace.Enabled := HasEditor;
      actSearchFindNext.Enabled := HasEditor and (SearchForm.FindText <> '');
      actSearchFindPrev.Enabled := HasEditor and (SearchForm.FindText <> '');
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

  for i := 0 to (Notebook.PageCount - 1) do
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

  for i := 0 to (Notebook.PageCount - 1) do
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


{ ASSIGN KEY SHORTCUTS }

procedure TAssemblerForm.AssignActionShortcuts;
begin
  {$ifdef darwin}
     actFileNew.ShortCut := TextToShortCut('Meta+N');
     actFileOpen.ShortCut := TextToShortCut('Meta+O');
     actFileClose.ShortCut := TextToShortCut('Meta+W');
     actFileSave.ShortCut := TextToShortCut('Meta+S');
     actFileSaveAs.ShortCut := TextToShortCut('Shift+Meta+S');
     //
     actEditUndo.ShortCut := TextToShortCut('Meta+Z');
     actEditRedo.ShortCut := TextToShortCut('Shift+Meta+Z');
     actEditCut.ShortCut := TextToShortCut('Meta+X');
     actEditCopy.ShortCut := TextToShortCut('Meta+C');
     actEditPaste.ShortCut := TextToShortCut('Meta+P');
     actEditSelectAll.ShortCut := TextToShortCut('Meta+A');
     //
     actSearchFind.ShortCut := TextToShortCut('Meta+F');
     actSearchFindNext.ShortCut := TextToShortCut('Meta+G');
     actSearchFindPrev.ShortCut := TextToShortCut('Shift+Meta+G');
  {$endif}

  { TODO : uAssemblerForm -> check Windows shortcuts }
  {$ifdef windows}
     actFileNew.ShortCut := TextToShortCut('Ctrl+N');
     actFileOpen.ShortCut := TextToShortCut('Ctrl+O');
     actFileClose.ShortCut := TextToShortCut('Ctrl+W');
     actFileSave.ShortCut := TextToShortCut('Ctrl+S');
     actFileSaveAs.ShortCut := TextToShortCut('Shift+Ctrl+S');
     //
     actEditUndo.ShortCut := TextToShortCut('Ctrl+Z');
     actEditRedo.ShortCut := TextToShortCut('Shift+Ctrl+Z');
     actEditCut.ShortCut := TextToShortCut('Ctrl+X');
     actEditCopy.ShortCut := TextToShortCut('Ctrl+C');
     actEditPaste.ShortCut := TextToShortCut('Ctrl+P');
     actEditSelectAll.ShortCut := TextToShortCut('Ctrl+A');
     //
     actSearchFind.ShortCut := TextToShortCut('Ctrl+F');
     actSearchFindNext.ShortCut := TextToShortCut('Ctrl+G');
     actSearchFindPrev.ShortCut := TextToShortCut('Shift+Ctrl+G');
  {$endif}
end;


{ READ / WRITE INI SETTINGS }

procedure TAssemblerForm.ReadIniSettings;
begin
  Left := AppIni.ReadInteger(SECT_ASM, INI_WDW_LEFT, 20);
  Top := AppIni.ReadInteger(SECT_ASM, INI_WDW_TOP, 20);
  Width := AppIni.ReadInteger(SECT_ASM, INI_WDW_WIDTH, 640);
  Height := AppIni.ReadInteger(SECT_ASM, INI_WDW_HEIGHT, 480);
  panelBottom.Height := AppIni.ReadInteger(SECT_ASM, PANELB_HEIGHT, 50);
  panelRight.Width  := AppIni.ReadInteger(SECT_ASM, PANELR_WIDTH, 150);
  // Following is written out in MainForm.WriteIniSettings, but read here
  Visible := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_WDW_VIS, True);
end;


procedure TAssemblerForm.WriteIniSettings;
begin
  AppIni.WriteInteger(SECT_ASM, INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_ASM, INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_ASM, INI_WDW_WIDTH, Width);
  AppIni.WriteInteger(SECT_ASM, INI_WDW_HEIGHT, Height);
  AppIni.WriteInteger(SECT_ASM, PANELB_HEIGHT, panelBottom.Height);
  AppIni.WriteInteger(SECT_ASM, PANELR_WIDTH, panelRight.Width);
end;


{ FORMAT ASSEMBLY CODE }

procedure TAssemblerForm.actFormatterExecute(Sender: TObject);
var
  ed: TSynEdit;
  formatter: TFormatter;
begin
  ed := GetCurrentEditor;
  if Assigned(ed) then
    begin
      if (MessageQuery('Format Code', 'Formatting code text cannot be undone. Confirm?')) then
        begin
          formatter := TFormatter.Create;
          try
            ed.Text := formatter.FormatFile(ed.Text);
          finally
            formatter.Free;
          end;
        end;
    end;
end;


procedure TAssemblerForm.actButtonsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (Notebook.PageCount > 0);
end;


end.

