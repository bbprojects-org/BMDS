{ ==============================================================================

  MAIN USER INTERFACE

    This form has two states:

    1. Running the machine emulator only; has options to resize and reset
       machine

    2. Developer mode; additional debug window and source file editor are
       shown. Supports assembly, single step, register modification, trace, etc


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

                                                                             
{ TODO : uMainForm -> recheck Cross-Platform guidance for Free Pascal, adjust
                      as necessary (http://wiki.lazarus.freepascal.org/Multiplatform_Programming_Guide) }

unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, Menus, Buttons, ActnList, StdActns, LCLType,
  StdCtrls, fphtml,
  {$ifdef debug}
  EventLog,
  {$endif}
  //
  uCommon, uIniFile, uAboutForm, uPreferencesForm, uDisassembler, uCompareForm,
  uMachineBase, uDebugForm, uAssemblerForm, uGenPrefsFrame, uEdPrefsFrame,
  uDefs8080, uMachineInfoForm, uReadWriteHex;

type

  { TMainForm }

  TMainForm = class(TForm)
    actShowAssembler: TAction;
    actShowDebug: TAction;
    actMaxSpeed: TAction;
    actShowDisassembler: TAction;
    actMachineConfig: TAction;
    actShowCompare: TAction;
    actMode: TAction;
    actStep: TAction;
    actStepOver: TAction;
    actInterrupt: TAction;
    actQuit: TAction;
    actReset: TAction;
    actStop: TAction;
    actRun: TAction;
    ActionList1: TActionList;
    actLoad: TFileOpen;
    actSave: TFileSaveAs;
    DebugToolBar: TToolBar;
    memoLog: TMemo;
    memoDebug: TMemo;
    menuAsm: TMenuItem;
    menuAsmNew: TMenuItem;
    menuAsmOpen: TMenuItem;
    menuAsmRecent: TMenuItem;
    menuAsmSave: TMenuItem;
    menuAsmSaveAs: TMenuItem;
    menuAsmSaveAll: TMenuItem;
    menuAsmClose: TMenuItem;
    menuAsmCloseAll: TMenuItem;
    menuEdit: TMenuItem;
    menuEditUndo: TMenuItem;
    menuEditRedo: TMenuItem;
    menuEditCut: TMenuItem;
    menuEditCopy: TMenuItem;
    menuEditPaste: TMenuItem;
    menuEditSelectAll: TMenuItem;
    MenuItem1: TMenuItem;
    menuFormat: TMenuItem;
    N12: TMenuItem;
    menuTestCPU: TMenuItem;
    menuSearchReplace: TMenuItem;
    menuSearchFindPrev: TMenuItem;
    menuSearchFindNext: TMenuItem;
    menuSearchFind: TMenuItem;
    menuSearch: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    menuShowAssembler: TMenuItem;
    menuOptionsWindows: TMenuItem;
    menuDisassemble: TMenuItem;
    menuCompare: TMenuItem;
    N7: TMenuItem;
    N6: TMenuItem;
    N11: TMenuItem;
    menuInfo: TMenuItem;
    menuSize4: TMenuItem;
    menuMaxSpeed: TMenuItem;
    menuMac: TMenuItem;
    menuAboutMac: TMenuItem;
    N5: TMenuItem;
    menuMachineConfig: TMenuItem;
    menuPreferencesMac: TMenuItem;
    menuMode: TMenuItem;
    menuShowDebug: TMenuItem;
    menuStep: TMenuItem;
    N10: TMenuItem;
    menuSep5: TMenuItem;
    menuSelect: TMenuItem;
    ImageList1: TImageList;
    MainMenu: TMainMenu;
    menuHelpWindows: TMenuItem;
    menuAboutWindows: TMenuItem;
    N9: TMenuItem;
    N8: TMenuItem;
    menuDebug: TMenuItem;
    menuStepOver: TMenuItem;
    menuQuit: TMenuItem;
    menuSaveCode: TMenuItem;
    menuLoadCode: TMenuItem;
    menuSize3: TMenuItem;
    menuSize2: TMenuItem;
    menuSize1: TMenuItem;
    menuReset: TMenuItem;
    menuStop: TMenuItem;
    menuRun: TMenuItem;
    menuMachine: TMenuItem;
    OpenDialog1: TOpenDialog;
    StatusBar: TStatusBar;
    tbRun: TToolButton;
    tbShowAssembler: TToolButton;
    tbShowCompare: TToolButton;
    tbShowDebug: TToolButton;
    tbShowDisassembler: TToolButton;
    TimerUpdateState: TTimer;
    MainToolBar: TToolBar;
    tbSaveCode: TToolButton;
    tbSep1: TToolButton;
    tbReset: TToolButton;
    tbStop: TToolButton;
    tbLoadCode: TToolButton;
    TimerDelayChange: TTimer;
    procedure actMachineConfigExecute(Sender: TObject);
    procedure actMaxSpeedExecute(Sender: TObject);
    procedure actShowAssemblerExecute(Sender: TObject);
    procedure actShowDebugExecute(Sender: TObject);
    procedure actShowCompareExecute(Sender: TObject);
    procedure actShowDisassemblerExecute(Sender: TObject);
    procedure actLoadAccept(Sender: TObject);
    procedure actLoadBeforeExecute(Sender: TObject);
    procedure actModeExecute(Sender: TObject);
    procedure actQuitExecute(Sender: TObject);
    procedure actResetExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actSaveAccept(Sender: TObject);
    procedure actSaveBeforeExecute(Sender: TObject);
    procedure actStepExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnAssembleClick(Sender: TObject);
    procedure Log(Msg: string; const Args: array of const);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const {%H-}Files: array of string);
    procedure FormKeyDown(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure menuAboutClick(Sender: TObject);
    procedure menuMachineInfoClick(Sender: TObject);
    procedure menuOptionsMacClick(Sender: TObject);
    procedure menuMachineClick(Sender: TObject);
    procedure menuSizeClick(Sender: TObject);
    procedure menuTestCPUClick(Sender: TObject);
    procedure TimerDelayChangeTimer(Sender: TObject);
    procedure TimerUpdateStateTimer(Sender: TObject);
  private     // Forms
    DebugForm: TDebugForm;
    DisassemblerForm: TDisassemblerForm;
    CompareForm: TCompareForm;
    procedure MakeDeveloperForms;
    procedure FreeDeveloperForms;
    procedure MakePreferencesForm;
    procedure MakeDebugForm;
    procedure MakeDisassemblerForm;
    procedure MakeCompareForm;
    procedure MakeAssemblerForm;
    procedure ShowMachineStatus;
    procedure ShowPreferences(category: TCategoryIndex);
    procedure ShowFormsState;
    procedure OnShowHideAsmForm(Sender: TObject);
  private     // Machine
    CurrentMachineID: string;
    DeveloperMode: boolean;             // Show debug windows?
    CalculatedFPS: integer;
    tmpScreenSize: TPoint;
    tmpScreenPosition: TPoint;
    NewID: string;
    procedure DoMachineChange;
    procedure StopMachine;
    procedure MakeMachine;
    procedure MakeMachineMenu;
    procedure SetMachineScreenSize(aMultiplier: integer);
    procedure DoMachineConfigChange(Sender: TObject; {%H-}ChangedItem: TMachineChangedItem);
    procedure LoadFile(FileName: string);
    procedure LoadFileMachine(FileName: string);
    procedure LoadFileBinary(FileName: string);
    procedure LoadFileHex(FileName: string);
    function ValidExt(Ext: string): boolean;
    function BuildFilter(FileExts: TFileExtSet): string;
    function GetAddr(FileName: string): integer;
    function MaxStr(str: string; max: integer): string;
  private     // INI file
    procedure CreateAppIni;
    procedure ReadIniSettings;
    procedure WriteIniSettings;
    procedure DoCustomSection(var aSect: string);
    procedure WriteMachineID(aID: string);
  private     // LEDs
    LEDs: array[0..7] of TImage;
    LedsSetValue: integer;
    procedure CreateLEDs;
    procedure SetLEDs(Value: integer);
    procedure ShowLEDs(State: boolean);
    procedure DoCheckLEDsWrite(Sender: TObject; Addr, Value: integer);
  private
    procedure DoConfigChange(Sender: TObject; ChangedItem: integer);
    procedure DoAssemblerLog(msg: string);
    procedure MakeMachineAndForms;
    procedure FreeMachineAndForms;
    procedure SetButtonsState(IsRunning: boolean);
    procedure UpdateStatus;
    procedure UpdateEditorPrefs(Sender: TObject; {%H-}ChangedItem: integer);
    procedure DoDebugButton(Sender: TObject; button: TDebugButton);
  public
    //
  end;

const
  feNAMES: array[TFileExt] of string = ('.m65', '.si', '.c8', '.bin', '.hex', '.*');
  MAX_LEN_FN = 32;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

const
  INI_FILE      = 'BMDS.ini';           // INI file settings

  SECT_MAIN     = 'MainForm';
  INI_DEV_MODE  = 'Mode';
  INI_MACH_ID   = 'MachineID';
  INI_SCREEN    = 'Scr';

  {$ifdef darwin}
    FORM_EXTRA_HEIGHT = 44;
  {$else}
    FORM_EXTRA_HEIGHT = 44;             { TODO : uMainForm -> Windows to be checked }
  {$endif}
  INITIAL_SCALE_FACTOR = 3;
  DEBUGFORM_TRACE_ADJ  = 360;


{ FORM CREATE }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  {$ifdef debug}
  AppLog := TEventLog.Create(self);     // Create debug logfile
  AppLog.LogType := ltFile;
  AppLog.FileName := GetAppDataDirectory + 'BMDS_log.txt';
  AppLog.Active := True;
  AppLog.Debug('TMainForm.FormCreate');
  {$endif}

  {$ifdef darwin}
    menuMac.Caption := #$EF#$A3#$BF;    // 'Apple logo', add this to App menu
    menuMac.Visible := True;            //  Default for this is not visible
  {$else}
    menuHelpWindows.Visible := True;    // Default for these is not visible
  {$endif}

  MachineDataFolder := '';
  CreateAppIni;
  ReadIniSettings;
  MakeMachineMenu;
  MakeMachineAndForms;
  ShowFormsState;
  CalculatedFPS := 0;
  CreateLEDs;
  TimerUpdateState.Enabled := True;     // Maintains GUI in sync with windows etc
end;


procedure TMainForm.CreateAppIni;                 
var
  tmpFolder: string;
begin
  tmpFolder := GetAppDataDirectory;
  if (not DirectoryExists(tmpFolder)) then // Ensure there is an App Data folder
    ForceDirectories(tmpFolder);
  AppIni := TAppIni.Create(tmpFolder + INI_FILE, @DoCustomSection);
end;


{ FORM DESTROY }

{ Write settings to INI file, free up created forms }

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  TimerUpdateState.Enabled := False;
  StopMachine;
  WriteIniSettings;
  FreeMachineAndForms;
  AppIni.Free;
  {$ifdef debug}
  AppLog.Free;
  {$endif}
end;


{ FORM DROP FILES }

procedure TMainForm.FormDropFiles(Sender: TObject; const Files: array of string);
begin
  if (Length(Files) = 1) then
    LoadFile(Files[0])
  else
    MessageError('Only one file permitted');
end;


{ MAKE MACHINE MENU }

{ Create machine select list; one entry for each machine }

procedure TMainForm.MakeMachineMenu;
var
  idx: integer;
  NewItem: TMenuItem;
begin
  for idx := 0 to (MachineFactory.Count-1) do // One menu item for each machine
    begin
      NewItem := TMenuItem.Create(self);
      NewItem.Caption := MachineFactory.GetIdForIdx(idx);
      NewItem.OnClick := @menuMachineClick;
      NewItem.Tag := idx;               // Menu tag -> MACHINES[tag]
      menuSelect.Add(NewItem);
    end;
  if (CurrentMachineID = '') then       // Set default to first machine
    CurrentMachineID := MachineFactory.GetIdForIdx(0);
end;


{ MACHINE SELECT }

{ If machine selected is not the current machine, close it and create new one }

procedure TMainForm.menuMachineClick(Sender: TObject);
begin
  NewID := (Sender as TMenuItem).Caption;
  if (NewID <> CurrentMachineID) then
    begin
      TimerUpdateState.Enabled := False;
      StopMachine;                      // Shutdown current machine
      TimerDelayChange.Enabled := True; // Wait for stop, then action below
    end;
end;


procedure TMainForm.DoMachineChange;
begin
  TimerDelayChange.Enabled := False;
  WriteIniSettings;                 // CurrentMachineIndex still old machine
  FreeMachineAndForms;
  AppIni.Free;

  CreateAppIni;
  WriteMachineID(NewID);            // Set ID to new machine
  ReadIniSettings;                  // and create it...
  MakeMachineAndForms;
  TimerUpdateState.Enabled := True;
  ShowFormsState;
end;


{ MAKE MACHINE & FORMS }

{ Create machine and all the relevant forms and assign parameters }

procedure TMainForm.MakeMachineAndForms;
begin
  MakeMachine;                          // Includes making MachineConfig frame
  MakePreferencesForm;
  if (DeveloperMode) then
    MakeDeveloperForms;
  UpdateStatus;
end;


procedure TMainForm.MakeDeveloperForms;
begin
  MakeDebugForm;
  MakeAssemblerForm;
  MakeDisassemblerForm;
  MakeCompareForm;
end;


{ FREE MACHINE AND FORMS }

procedure TMainForm.FreeMachineAndForms;
begin
  if (DeveloperMode) then
    FreeDeveloperForms;
  PreferencesForm.Free;
  Machine.Free;
end;


procedure TMainForm.FreeDeveloperForms;
begin
  CompareForm.Free;
  DisassemblerForm.Free;
  AssemblerForm.Free;
  DebugForm.Free;
end;


{ MAKE MACHINE }

procedure TMainForm.MakeMachine;
var
  Idx: integer;
begin
  MachineDataFolder := GetAppResourcesDirectory + CurrentMachineID + DIRECTORY_SEPARATOR;
  Machine := MachineFactory.CreateMachineFromID(CurrentMachineID);
  Machine.Name := CurrentMachineID;
  Machine.MemoryMgr.OnMemoryWrite := @DoCheckLedsWrite;
  Machine.OnConfigChange := @DoMachineConfigChange;
  if (Machine.Info.HasCustomMenu) then
    Machine.SetCustomMenuItem(MenuItem1);
  MenuItem1.Visible := Machine.Info.HasCustomMenu;

  // If ScreenSize is defined in INI file, then set machine screen size, else
  // leave default size
  if (tmpScreenSize.X <> -1) then
    Machine.ScreenSize := tmpScreenSize
  else
    begin
      SetMachineScreenSize(INITIAL_SCALE_FACTOR); // Set initial size as large
      menuSize3.Checked := True;
    end;

  // If ScreenPosition not defined in INI file, put Screen below MainForm
  if (tmpScreenPosition.X = -1) then
    begin
      tmpScreenPosition.X := self.Left;
      tmpScreenPosition.Y := self.Top + self.Height + FORM_EXTRA_HEIGHT;
    end;
  Machine.ScreenPosition := tmpScreenPosition;
  Machine.ScreenCaption := Format('%s (%d)', [CurrentMachineID, Machine.Info.Year]);
  Machine.Reset;

  menuTestCpu.Enabled := Machine.CPU.Info.SupportsCpuTest;

  Caption := Format('%s - %s',  [APP_NAME, CurrentMachineID]);
  for Idx := 0 to menuSelect.Count-1 do
    menuSelect.Items[Idx].Checked := False;
  Idx := MachineFactory.FindIndexForID(CurrentMachineID);
  menuSelect.Items[Idx].Checked := True;
  actRun.Enabled := Machine.HasCode;    // Enabled if ROMS loaded

  ShowMachineStatus;
end;


procedure TMainForm.ShowMachineStatus;
var
  ThisFreq: integer;
begin
  StatusBar.Panels[1].Text := Machine.CPU.Info.Name;
  ThisFreq := Machine.Info.CpuFreqKhz;
  if (ThisFreq > 0) then
    if (ThisFreq < 1000) then
      StatusBar.Panels[2].Text := Format('%d kHz', [ThisFreq])
    else
      StatusBar.Panels[2].Text := Format('%.1f MHz', [ThisFreq / 1000])
  else
    StatusBar.Panels[2].Text := '';
end;


{ MAKE PREFERENCES FORM }

{ Use callbacks to update Editor's preferences }

procedure TMainForm.MakePreferencesForm;
begin
  PreferencesForm := TPreferencesForm.Create(nil);
  PreferencesForm.Frames[ciDisplay].OnChange := @UpdateEditorPrefs;
  PreferencesForm.Frames[ciColours].OnChange := @UpdateEditorPrefs;
  PreferencesForm.SetMachineConfigFrame(Machine.Name, Machine.ConfigFrame);
  GenPrefs := PreferencesForm.Frames[ciGeneral] as TGenPrefsFrame;
  GenPrefs.OnChange := @DoConfigChange;
end;


procedure TMainForm.UpdateEditorPrefs(Sender: TObject; ChangedItem: integer);
begin
  if (Sender is TEdPrefsFrame) then
    AssemblerForm.SetEditorPreferences((Sender as TEdPrefsFrame).Prefs);
end;


{ MAKE DEBUG FORM }

procedure TMainForm.MakeDebugForm;
begin
  DebugForm := TDebugForm.Create(nil);
  DebugForm.Width := Machine.CPU.Info.TraceWidth + DEBUGFORM_TRACE_ADJ;
  DebugForm.Constraints.MinWidth := DebugForm.Width;
  DebugForm.OnDebugButton := @DoDebugButton;
end;


{ MAKE ASSEMBLER FORM }

procedure TMainForm.MakeAssemblerForm;
begin
  AssemblerForm := TAssemblerForm.Create(nil);
  AssemblerForm.OnShow := @OnShowHideAsmForm;
  AssemblerForm.OnHide := @OnShowHideAsmForm;
  AssemblerForm.OnLog  := @DoAssemblerLog;
  AssemblerForm.MruMenuRef := menuAsmRecent;
end;


{ Only show editor menu items when the editor form is actually in view }

procedure TMainForm.OnShowHideAsmForm(Sender: TObject);
begin
  menuAsm.Visible := AssemblerForm.Visible;
  menuEdit.Visible := AssemblerForm.Visible;
  menuSearch.Visible := AssemblerForm.Visible;
end;


{ MAKE DISASSEMBLER FORM }

{ Only created if the CPU has disassembler support }

procedure TMainForm.MakeDisassemblerForm;
begin
  if (Machine.CPU.Info.SupportsDisassembler) then
    begin
      DisassemblerForm := TDisassemblerForm.Create(nil);
      actShowDisassembler.Enabled := True;
    end
  else
    actShowDisassembler.Enabled := False;
end;


{ MAKE COMPARE FORM }

procedure TMainForm.MakeCompareForm;
begin
  CompareForm := TCompareForm.Create(nil);
end;


{ MAIN FORM CLOSE }

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (Machine <> nil) then
    Machine.Stop;
  TimerUpdateState.Enabled := False;
  CloseAction := caFree;
end;


{ SHOW / HIDE FORMS }

procedure TMainForm.ShowFormsState;
begin
  actMode.Checked := DeveloperMode;
  DebugToolbar.Visible := DeveloperMode;
  menuDebug.Visible := DeveloperMode;
  menuAsm.Visible  := DeveloperMode;
  menuEdit.Visible  := DeveloperMode;
  menuSearch.Visible  := DeveloperMode;
  if (DeveloperMode) then
    begin
      actShowDebug.Checked := DebugForm.Visible;
      actShowAssembler.Checked := AssemblerForm.Visible;
      OnShowHideAsmForm(self);
      actShowDisassembler.Checked := DisassemblerForm.Visible;
      actShowCompare.Checked := CompareForm.Visible;
    end;
end;


{ MENU - SET SCREEN SIZE }

procedure TMainForm.menuSizeClick(Sender: TObject);
var
  Scale: integer;
begin
  Scale := (Sender as TMenuItem).Tag;
  SetMachineScreenSize(Scale);
end;


procedure TMainForm.SetMachineScreenSize(aMultiplier: integer);
var
  Size: TPoint;
begin
  Size.X := Machine.Info.ScreenWidthPx * aMultiplier * Machine.Info.ScaleModifier;
  Size.Y := Machine.Info.ScreenHeightPx * aMultiplier * Machine.Info.ScaleModifier;
  Machine.ScreenSize := Size;
end;


{ MENU - ABOUT }

{ TODO : uMainForm -> showing the About form using ShowModal seems to be causing
  a small memory leak, even with just the form and no code, perhaps a widgetset
  bug? Using Show and freeing elsewhere does not cause a leak }

procedure TMainForm.menuAboutClick(Sender: TObject);
var
  af: TAboutForm;
begin
  af := TAboutForm.Create(nil);
  try
    af.ShowModal;
  finally
    af.Free;
  end;
end;


{ MENU - PREFERENCES / OPTIONS }

procedure TMainForm.menuOptionsMacClick(Sender: TObject);
begin
  ShowPreferences(ciGeneral);
end;


{ MENU - MACHINE CONFIG }

procedure TMainForm.actMachineConfigExecute(Sender: TObject);
begin
  ShowPreferences(ciMachName);
end;


procedure TMainForm.ShowPreferences(category: TCategoryIndex);
begin
  if (category <> ciGeneral) then       // Machine config call?
    PreferencesForm.Category := category;
  PreferencesForm.ShowModal;
end;


procedure TMainForm.DoMachineConfigChange(Sender: TObject; ChangedItem: TMachineChangedItem);
begin
  (*
  case ChangedItem of
    mcFreq: ShowMachineStatus;
  end;
  *)
  ShowMachineStatus;
end;


{ MENU - MACHINE INFO }

procedure TMainForm.menuMachineInfoClick(Sender: TObject);
var
  mif: TMachineInfoForm;
begin
  mif := TMachineInfoForm.Create(nil);
  try
    mif.SetPosition(Left + Width, Top);
    mif.ShowModal;
  finally
    mif.Free;
  end;
end;


{ MENU - TOGGLE DEVELOPER MODE }

procedure TMainForm.actModeExecute(Sender: TObject);
begin
  DeveloperMode := (not DeveloperMode);
  if (DeveloperMode) then
    begin
      MakeDeveloperForms;
      DebugForm.Visible := True;        // Show on change to DevMode
    end
  else
    FreeDeveloperForms;
  ShowFormsState;
end;


{ MENU - LOAD CODE FOR THIS MACHINE }

procedure TMainForm.actLoadBeforeExecute(Sender: TObject);
begin
  actLoad.Dialog.Filter := BuildFilter(Machine.Info.FileExts);
end;


procedure TMainForm.actLoadAccept(Sender: TObject);
var
  FileName: string;
begin
  StopMachine;                          // Stop machine if running
  FileName := actLoad.Dialog.FileName;
  LoadFile(FileName);
end;


function TMainForm.BuildFilter(FileExts: TFileExtSet): string;
var
  ThisFileExt: TFileExt;
  FilterItems: string;
begin
  Result := '';
  FilterItems := '';
  for ThisFileExt in FileExts do
    begin
      if (Result <> '') then
        Result := Result + ';';
      Result := Result + '*' + feNAMES[ThisFileExt];

      if (FilterItems <> '') then
        FilterItems := FilterItems + '|';
      case ThisFileExt of
        feM65: FilterItems := FilterItems + 'Microtan file (*' + feNAMES[ThisFileExt] + ')|*' + feNAMES[ThisFileExt];
        feSI:  FilterItems := FilterItems + 'Space Invaders file (*' + feNAMES[ThisFileExt] + ')|*' + feNAMES[ThisFileExt];
        feC8:  FilterItems := FilterItems + 'CHIP-8 file (*' + feNAMES[ThisFileExt] + ')|*' + feNAMES[ThisFileExt];
        feBIN: FilterItems := FilterItems + 'Binary file (*' + feNAMES[ThisFileExt] + ')|*' + feNAMES[ThisFileExt];
        feHEX: FilterItems := FilterItems + 'HEX file (*' + feNAMES[ThisFileExt] + ')|*' + feNAMES[ThisFileExt];
      end;
    end;

  Result := 'BMDS File (' + Result + ')|' + Result
            + '|' + FilterItems
            + '|' + 'All files (*.*)|*.*';
end;


procedure TMainForm.LoadFile(FileName: string);
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  if (ValidExt(Ext)) then
    begin
      case Ext of
        '.m65', '.si', '.c8': LoadFileMachine(FileName);
        '.bin':               LoadFileBinary(FileName);
        '.hex':               LoadFileHex(FileName);
      end;
      Machine.Reset;                    // Auto reset
      actRunExecute(nil);               // and start TANBUG
    end
  else
    MessageError(Format('File type "*%s" not valid for this machine [%s]', [Ext, Machine.Name]));
end;


function TMainForm.ValidExt(Ext: string): boolean;
var
  ThisFileExt: TFileExt;
begin
  Result := False;
  for ThisFileExt in Machine.Info.FileExts do
    if (Ext = feNAMES[ThisFileExt]) then
      begin
        Result := True;
        Exit;
      end;
end;


procedure TMainForm.LoadFileMachine(FileName: string);
begin
  Machine.LoadFromFile(FileName);
  if (DeveloperMode) then
    DebugForm.Refresh;
  Log('Machine file "%s" loaded to %s', [MaxStr(ExtractFileName(FileName), MAX_LEN_FN), Machine.Name]);
end;


procedure TMainForm.LoadFileBinary(FileName: string);
var
  Stream: TFileStream;
  LoadAddr: integer;
  Extra: string;
begin
  Extra := '';
  Stream := TFileStream.Create(FileName, fmOpenRead);
  LoadAddr := GetAddr(FileName);
  if (LoadAddr = -1) then
    begin
      LoadAddr := Machine.Info.DefaultLoadAddr;
      Extra := ' [DEFAULT ADDR]';
    end;
  try
    Stream.Read(Machine.Memory[LoadAddr], Stream.Size);
  finally
    Stream.Free;
  end;
  Log('Binary file "%s" loaded to $%.4x on %s%s', [MaxStr(ExtractFileName(FileName), MAX_LEN_FN), LoadAddr, Machine.Name, Extra]);
end;


procedure TMainForm.LoadFileHex(FileName: string);
var
  ReadHex: TReadHex;
  i: integer;
begin
  ReadHex := TReadHex.Create(FileName);
  try
    for i := 0 to (ReadHex.BytesRead - 1) do
      Machine.Memory[ReadHex.StartAddress + i] := ReadHex.BytesArray[i];
    if (ReadHex.ErrorMessage <> '') then
      MessageError(ReadHex.ErrorMessage)
    else
      Log('Hex file "%s" loaded to $%.4x on %s', [MaxStr(ExtractFileName(FileName), MAX_LEN_FN), ReadHex.StartAddress, Machine.Name]);
  finally
    ReadHex.Free;
  end;
end;


{ Get hex load address from filename; e.g. 'MyFileName (xxxx).bin' }

function TMainForm.GetAddr(FileName: string): integer;
var
  Pos1, Pos2: integer;
  AddrStr: string;
begin
  Pos1 := Pos('(', FileName);
  Pos2 := Pos(')', FileName);
  if ((Pos1 > 0) and (Pos2 > Pos1)) then
    begin
      AddrStr := Copy(FileName, Pos1 + 1, Pos2 - Pos1 - 1);
      Result := GetHex(AddrStr);
    end
  else
    Result := -1;
end;


{ Limit string to a max length, adding '...' if too long }

function TMainForm.MaxStr(str: string; max: integer): string;
begin
  if (Length(str) > max) then
    Result := LeftStr(str, max-2) + '...'
  else
    Result := str;
end;


{ MENU - SAVE CODE FOR THIS MACHINE }

procedure TMainForm.actSaveBeforeExecute(Sender: TObject);
var
  FirstExt: TFileExt;
begin
  FirstExt := Low(Machine.Info.FileExts);
  actSave.Dialog.DefaultExt := feNAMES[FirstExt];
end;


{ TODO : uMainForm -> have config option for 8K/47K save in M65 }

procedure TMainForm.actSaveAccept(Sender: TObject);
var
  FileName: string;
begin
  FileName := actSave.Dialog.FileName;
  Machine.SaveToFile(FileName, False);  // Save compatible .M65 format for now
  Log('Machine file for %s saved to "%s"', [Machine.Name, MaxStr(ExtractFileName(FileName), MAX_LEN_FN)]);
end;


{ RUN MACHINE }

{ This is the main execution loop of the emulator. Having setup the GUI
  it runs a loop executing one frame at a time }

procedure TMainForm.actRunExecute(Sender: TObject);
begin
  Machine.CPU.ResetTrace;
  SetButtonsState(True);

  Machine.State := msRunning;
  UpdateStatus;
  Machine.SetFocus;
  while (Machine.State = msRunning) do  // Run until state changes
    begin
      Machine.RunForOneFrame;
      Inc(CalculatedFPS);               // Count each frame to report FPS
    end;

  if (Machine.State = msStoppedOnBrkpt) then
    begin
      SetButtonsState(False);
      UpdateStatus;
    end;
end;


{ SET BUTTONS STATE }

procedure TMainForm.SetButtonsState(IsRunning: boolean);
begin
  actRun.Enabled := (not IsRunning);
  actStop.Enabled := IsRunning;
  actStep.Enabled := (not IsRunning);
  actStepOver.Enabled := (not IsRunning);
end;


{ STOP }

procedure TMainForm.actStopExecute(Sender: TObject);
begin
  StopMachine;
end;


procedure TMainForm.StopMachine;
begin
  SetButtonsState(False);
  Machine.Stop;
  UpdateStatus;
end;


{ RESET }

procedure TMainForm.actResetExecute(Sender: TObject);
begin
  Machine.Reset;
end;


{ QUIT }

procedure TMainForm.actQuitExecute(Sender: TObject);
begin
  Close;
end;


{ STEP }

{ Execute a single opcode instruction, then update GUI }

procedure TMainForm.actStepExecute(Sender: TObject);
begin
  Machine.Step;
  UpdateStatus;                         // Includes set TraceCount
end;


{ STEP OVER }

{ Execute the next instruction, stopping at the address following.
  This is useful for running all of the code of a JSR then stopping after
  the RTS brings execution back to the address }

procedure TMainForm.actStepOverExecute(Sender: TObject);
begin
  { TODO : uMainForm -> do 'Step Over' code }
end;


{ ON DEBUG FORM BUTTONS - callback from DebugForm }

procedure TMainForm.DoDebugButton(Sender: TObject; button: TDebugButton);
begin
  case button of
    dbRun:      actRunExecute(nil);
    dbStop:     actStopExecute(nil);
    dbReset:    actResetExecute(nil);
    dbStep:     actStepExecute(nil);
    dbStepOver: actStepOverExecute(nil);
  end;
end;


{ SHOW / HIDE FORMS }

{ Debug menu options and debug toolbar only avilable to user if in Developer
  Mode, so these forms should exist }

procedure TMainForm.actShowDebugExecute(Sender: TObject);
begin
  DebugForm.Visible := (not DebugForm.Visible);
  actShowDebug.Checked := DebugForm.Visible;
end;


procedure TMainForm.actShowCompareExecute(Sender: TObject);
begin
  CompareForm.Visible := (not CompareForm.Visible);
  actShowCompare.Checked := CompareForm.Visible;
end;

procedure TMainForm.actShowAssemblerExecute(Sender: TObject);
begin
  AssemblerForm.Visible := (not AssemblerForm.Visible);
  actShowAssembler.Checked := AssemblerForm.Visible;
end;

procedure TMainForm.actShowDisassemblerExecute(Sender: TObject);
begin
  DisassemblerForm.Visible := (not DisassemblerForm.Visible);
  actShowDisassembler.Checked := DisassemblerForm.Visible;
end;


procedure TMainForm.btnAssembleClick(Sender: TObject);
begin
  AssemblerForm.Show;
end;


{ MAX SPEED }

{ Normally each machine will run at a frame speed appropriate to the original
  CPU frequency, display etc, to give experience as per the original machine
  performance. As modern computers are substantially faster, this is done by
  waiting a set amount of time after each frame has been executed. This 'wait'
  can be switched off to run at 'full speed' of the emulating system. The
  resultant FPS will be displayed on the main GUI bar, and is usually just
  used to check performance changes to program architecture, etc }

procedure TMainForm.actMaxSpeedExecute(Sender: TObject);
begin
  Machine.MaxSpeed := (not Machine.MaxSpeed);
  actMaxSpeed.Checked := Machine.MaxSpeed;
end;


{ TEST 6502 CPU }

procedure TMainForm.menuTestCPUClick(Sender: TObject);
begin
  if (Machine.CPU.Info.SupportsCpuTest) then
    Machine.CPU.TestCpu;
end;


{ UPDATE STATUS }

procedure TMainForm.UpdateStatus;
var
  StateStr: string;
begin
  case (Machine.State) of
    msRunning:        StateStr := 'Running';
    msStopped:        StateStr := 'Stopped';
    msStoppedOnBrkpt: StateStr := Format('Breakpoint $%.4x', [Machine.CPU.PC]);
  end;
  StatusBar.Panels[0].Text := StateStr;
  if (DeveloperMode) then
    begin
      DebugForm.Status := StateStr;
      DebugForm.Refresh;
    end;
end;


{ INIFILE READ / WRITE }

{ This procedure builds Custom Section header based on selected machine, it
  is called from uIniFile if SECT_CUSTOM is used to identify the section }

procedure TMainForm.DoCustomSection(var aSect: string);
begin
  aSect := CurrentMachineID;
end;


procedure TMainForm.WriteMachineID(aID: string);
begin
  AppIni.WriteString(SECT_MAIN, INI_MACH_ID, aID);
end;


procedure TMainForm.ReadIniSettings;
begin
  CurrentMachineID := AppIni.ReadString(SECT_MAIN, INI_MACH_ID, '');
  Left := AppIni.ReadInteger(SECT_CUSTOM, INI_WDW_LEFT, 20);
  Top := AppIni.ReadInteger(SECT_CUSTOM, INI_WDW_TOP, 20);
  Height := AppIni.ReadInteger(SECT_CUSTOM, INI_WDW_HEIGHT, 74);
  DeveloperMode := AppIni.ReadBool(SECT_CUSTOM, INI_DEV_MODE, False);

  tmpScreenPosition.X := AppIni.ReadInteger(SECT_CUSTOM, INI_SCREEN + INI_WDW_LEFT, -1);
  tmpScreenPosition.Y := AppIni.ReadInteger(SECT_CUSTOM, INI_SCREEN + INI_WDW_TOP, -1);
  tmpScreenSize.X := AppIni.ReadInteger(SECT_CUSTOM, INI_SCREEN + INI_WDW_WIDTH, -1);
  tmpScreenSize.Y := AppIni.ReadInteger(SECT_CUSTOM, INI_SCREEN + INI_WDW_HEIGHT, -1);
end;


procedure TMainForm.WriteIniSettings;
begin
  WriteMachineID(CurrentMachineID);
  AppIni.WriteInteger(SECT_CUSTOM, INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_CUSTOM, INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_CUSTOM, INI_WDW_HEIGHT, Height);
  AppIni.WriteBool(SECT_CUSTOM, INI_DEV_MODE, DeveloperMode);
  AppIni.WriteInteger(SECT_CUSTOM, INI_SCREEN + INI_WDW_LEFT, Machine.ScreenPosition.X);
  AppIni.WriteInteger(SECT_CUSTOM, INI_SCREEN + INI_WDW_TOP, Machine.ScreenPosition.Y);
  AppIni.WriteInteger(SECT_CUSTOM, INI_SCREEN + INI_WDW_WIDTH, Machine.ScreenSize.X);
  AppIni.WriteInteger(SECT_CUSTOM, INI_SCREEN + INI_WDW_HEIGHT, Machine.ScreenSize.Y);

  // Cannot write these in DebugForm's or others OnDestroy events as forms
  // always appear to be invisible at that point. So write state here, but read
  // in each forms OnCreate event
  if (DeveloperMode) then
    begin
      AppIni.WriteBool(SECT_CUSTOM, 'Dbg' + INI_WDW_VIS, DebugForm.Visible);   
      AppIni.WriteBool(SECT_CUSTOM, 'Asm' + INI_WDW_VIS, AssemblerForm.Visible);
      AppIni.WriteBool(SECT_CUSTOM, 'Dis' + INI_WDW_VIS, DisassemblerForm.Visible);
      AppIni.WriteBool(SECT_CUSTOM, 'Compare' + INI_WDW_VIS, CompareForm.Visible);
    end;
end;


procedure TMainForm.Log(Msg: string; const Args: array of const);
begin
  memoLog.Lines.Add(Format(Msg, Args));
end;


{ TIMER EVENTS }

{ Update window states every one second. Maintains track of whether a window
  is open/closed (visible) without using callbacks from each form itself.
  Since this is called every second, showing frame count each time gives FPS }

procedure TMainForm.TimerUpdateStateTimer(Sender: TObject);
begin
  ShowLEDs(GenPrefs.LedsEnabled);
  if (DeveloperMode) then
    begin
      actShowDebug.Checked := DebugForm.Visible;      
      actShowDisassembler.Checked := DisassemblerForm.Visible;
      actShowAssembler.Checked := AssemblerForm.Visible;
      actShowCompare.Checked := CompareForm.Visible;
    end;
  StatusBar.Panels[3].Text := Format('%d fps', [CalculatedFPS]);
  CalculatedFPS := 0;                   // Reset each second
end;


{ If user changes machine whilst it is running, need to give time for machine
  to actually respond to the Stop command, so use this timer to delay the
  machine change }

procedure TMainForm.TimerDelayChangeTimer(Sender: TObject);
begin
  DoMachineChange;
end;


{ KEY DOWN }

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Machine.State = msRunning) then // Only get key presses when running
    ShowMessage('Ensure machine window in focus to accept key presses');
end;


{ GENERAL CONFIG CHANGE }

procedure TMainForm.DoConfigChange(Sender: TObject; ChangedItem: integer);
begin
  case ChangedItem of
    0: begin                            // All
        SetLEDs(LedsSetValue);
       end;
    1: {nothing};                       // DataPath
    2: {nothing};                       // LedsEnabled
    3: SetLEDs(LedsSetValue);           // LedsColour
    4: {nothing};                       // LedsAddress
  end;
end;


{ MESSAGE FROM ASSEMBLER }

procedure TMainForm.DoAssemblerLog(msg: string);
begin
  Log(msg, []);
  actRun.Enabled := Machine.HasCode;    // Update
end;


{ LED BAR }

procedure TMainForm.CreateLEDs;
var
  i: integer;
begin
  for i := 0 to 7 do
    begin
      LEDs[i] := TImage.Create(self);
      LEDs[i].Parent := self;
      LEDs[i].Left := 480 - i*24;       // Right = bit0 to left = bit7
      LEDs[i].Top := 3;
      LEDs[i].Height := 21;
      LEDs[i].Width := 22;
    end;
  SetLEDs(0);
end;


procedure TMainForm.SetLEDs(Value: integer);
var
  i: integer;
begin
  LedsSetValue := Value;
  for i := 0 to 7 do
    begin
      if (Value and (1 shl i) > 0 ) then
        GenPrefs.LedImages.GetBitmap(GenPrefs.LedsColourIndex, LEDs[i].Picture.Bitmap) // On
      else
        GenPrefs.LedImages.GetBitmap(0, LEDs[i].Picture.Bitmap); // Off
    end;
end;


procedure TMainForm.ShowLEDs(State: boolean);
var
  i: integer;
begin
  for i := 0 to 7 do
    LEDs[i].Visible := State;
end;


procedure TMainForm.DoCheckLEDsWrite(Sender: TObject; Addr, Value: integer);
begin
  if (GenPrefs.LedsEnabled) then
    if (Addr = GenPrefs.LedAddress) then
      SetLEDs(Value);
end;


end.
