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
                      as necessary
                      (http://wiki.lazarus.freepascal.org/Multiplatform_Programming_Guide) }

{ TODO : uMainForm -> do 'Step Over' code }

{ TODO : uMainForm -> add OnDropFiles functionality }

{ TODO : uMainForm -> general - check Tab order for all forms / frames }


{ APPLICATION BUGS LIST ========================================================

  - Setting DeveloperMode causing severe Memory Leak
  - RegistersFrame: Not updating registers properly on Step (6502 or all?)
  - RegistersFrame: Editing regs not working properly?
  - M65: single step/proceed not working as per M65 manual
  - Disassembler: sort data addresses
  - CHIP-8: ensure Mach/Config disabled if no Config form

  OSX ISSUES / DIFFERENCES

  - Text sizes

}

unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, Buttons, ActnList, StdActns, LCLType, StdCtrls, fphtml,
  //EventLog,
  //
  uCommon, uIniFile, uAboutForm, uPreferencesForm, {uSourceEditorForm,}
  uAssembler, uDisassembler, uCompareForm, uMachineBase, uDebugForm,
  uMachineInfoForm;

type
  TProcParameter = procedure(sender: TObject) of Object;

  { TMainForm }

  TMainForm = class(TForm)
    actShowDebug: TAction;
    actMaxSpeed: TAction;
    actShowDisassembler: TAction;
    actMachineConfig: TAction;
    actShowCompare: TAction;
    actShowBreakpoints: TAction;
    actMode: TAction;
    actShowTrace: TAction;
    actShowMemory: TAction;
    actStep: TAction;
    actStepOver: TAction;
    actInterrupt: TAction;
    actAssemble: TAction;
    actQuit: TAction;
    actReset: TAction;
    actStop: TAction;
    actRun: TAction;
    ActionList1: TActionList;
    actLoadCode: TFileOpen;
    actSaveCode: TFileSaveAs;
    btnDebug: TButton;
    btnAssemble: TButton;
    cbAutoStep: TCheckBox;
    lblDebug: TLabel;
    menuOptionsWindows: TMenuItem;
    menuDisassemble: TMenuItem;
    menuCompare: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    menuInfo: TMenuItem;
    menuSize4: TMenuItem;
    menuMaxSpeed: TMenuItem;
    menuMac: TMenuItem;
    menuAboutMac: TMenuItem;
    MenuItem6: TMenuItem;
    menuMachineConfig: TMenuItem;
    menuPreferencesMac: TMenuItem;
    menuMode: TMenuItem;
    menuShowDebug: TMenuItem;
    menuStep: TMenuItem;
    menuSep6: TMenuItem;
    menuSep5: TMenuItem;
    menuSelect: TMenuItem;
    ImageList1: TImageList;
    MainMenu: TMainMenu;
    menuHelpWindows: TMenuItem;
    menuAboutWindows: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    menuSepQuit: TMenuItem;
    menuSep2: TMenuItem;
    menuAssemble: TMenuItem;
    menuCloseFile: TMenuItem;
    menuCut: TMenuItem;
    menuCopy: TMenuItem;
    menuDebug: TMenuItem;
    menuStepOver: TMenuItem;
    menuPaste: TMenuItem;
    menuQuit: TMenuItem;
    menuSaveFileAs: TMenuItem;
    menuSaveFile: TMenuItem;
    menuOpenFile: TMenuItem;
    menuNewFile: TMenuItem;
    menuOpenRecent: TMenuItem;
    menuSep4: TMenuItem;
    menuEditor: TMenuItem;
    menuSaveCode: TMenuItem;
    menuLoadCode: TMenuItem;
    menuSize3: TMenuItem;
    menuSize2: TMenuItem;
    menuSize1: TMenuItem;
    menuReset: TMenuItem;
    menuStop: TMenuItem;
    menuRun: TMenuItem;
    menuMachine: TMenuItem;
    ButtonsPanel: TPanel;
    OpenDialog1: TOpenDialog;
    panelButtons: TPanel;
    StatusBar: TStatusBar;
    tbRun: TToolButton;
    Timer1: TTimer;
    MainToolBar: TToolBar;
    DebugToolBar: TToolBar;
    TimerAutoStep: TTimer;
    tbSaveCode: TToolButton;
    tbSep1: TToolButton;
    tbReset: TToolButton;
    tbStop: TToolButton;
    tbLoadCode: TToolButton;
    tbStepOver: TToolButton;
    tbSep0: TToolButton;
    TrackBarAutoStep: TTrackBar;
    procedure actAssembleExecute(Sender: TObject);
    procedure actMachineConfigExecute(Sender: TObject);
    procedure actMaxSpeedExecute(Sender: TObject);
    procedure actShowDebugExecute(Sender: TObject);
    procedure actShowCompareExecute(Sender: TObject);
    procedure actShowDisassemblerExecute(Sender: TObject);
    procedure actLoadCodeAccept(Sender: TObject);
    procedure actLoadCodeBeforeExecute(Sender: TObject);
    procedure actModeExecute(Sender: TObject);
    procedure actQuitExecute(Sender: TObject);
    procedure actResetExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actSaveCodeAccept(Sender: TObject);
    procedure actSaveCodeBeforeExecute(Sender: TObject);
    procedure actStepExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnDebugClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure cbAutoStepChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const {%H-}FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure menuAboutWindowsClick(Sender: TObject);
    procedure menuMachineInfoClick(Sender: TObject);
    procedure menuOptionsWindowsClick(Sender: TObject);
    procedure menuMachineClick(Sender: TObject);
    procedure menuSize1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TimerAutoStepTimer(Sender: TObject);
    procedure TrackBarAutoStepChange(Sender: TObject);
  private    
    PreferencesForm: TPreferencesForm;
    DebugForm: TDebugForm;
    DisassemblerForm: TDisassemblerForm;
    CompareForm: TCompareForm;
    //
    CurrentMachineId: integer;
    DeveloperMode: boolean;             // Show debug windows?
    CalculatedFPS: integer;
    //
    procedure CreateAppIni;
procedure FreeDeveloperForms;
    procedure MakeDebugForm;
    procedure MakeDeveloperForms;
    procedure MakeDisassemblerForm;
    procedure MakeCompareForm;
    procedure MakeMachine;
    procedure MakeMachineMenu;
    procedure MakePreferencesForm;
    procedure MakeMachineAndForms;
    procedure FreeMachineAndForms;
    procedure SetButtons(IsRunning: boolean);
    procedure SetMachineScreenSize(aMultiplier: integer);
    procedure ShowFormsState;
    procedure StopMachine;
    procedure UpdateStatus;
    procedure ReadIniSettings;
    procedure WriteIniSettings;
    procedure DoCustomSection(var aSect: string);
    procedure WriteMachineID(aID: integer);
    procedure ShowPreferences(aTab: Integer);
  public
    Machine: TMachineBase;              // Current machine instance
  end;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

const
  SECT_MAIN      = 'MainForm';          // INI file settings
  DEVELOPER_MODE = 'Mode';
  MACHINE_ID     = 'MachineID';
  SCREEN         = 'Scr';

  {$ifdef darwin}
    FORM_EXTRA_HEIGHT = 44;
  {$else}
    FORM_EXTRA_HEIGHT = 44;             { TODO : To Be Checked }
  {$endif}
  INITIAL_SCALE_FACTOR = 3;

{ FORM CREATE }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //AppLog := TEventLog.Create(self);
  //AppLog.LogType := ltFile;
  //AppLog.FileName := ExpandFileName('~/') + 'BMDS_log.txt';
  //AppLog.Active := True;

  {$ifdef darwin}
    menuMac.Caption := #$EF#$A3#$BF;    // 'Apple logo', add this to App menu
    menuMac.Visible := True;            //  Default for this is not visible
  {$else}
    menuHelpWindows.Visible := True;    // Default for these is not visible
  {$endif}

  GlobalVars.MachineDataFolder := '';   { TODO : Replace all global vars }
  CreateAppIni;
  ReadIniSettings;
  MakeMachineMenu;
  MakeMachineAndForms;
  ShowFormsState;
  CalculatedFPS := 0;
  Timer1.Enabled := True;               // Maintain GUI in sync with windows etc
end;


procedure TMainForm.CreateAppIni;                 
var
  tmpFolder: string;
begin
  tmpFolder := GetAppDataDirectory;
  if (not DirectoryExists(tmpFolder)) then // Ensure there is an App Data folder
    ForceDirectories(tmpFolder);

  //AppLog.Debug('Creating AppIni');
  AppIni := TAppIni.Create(tmpFolder + 'BMDS.ini', @DoCustomSection);
end;


{ FORM DESTROY }

{ Write settings to INI file, free up created forms }

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  WriteIniSettings;
  FreeMachineAndForms;
  AppIni.Free;
  //AppLog.Free;
end;


{ FORM DROP FILES }

{ Appears there is a bug in Lazarus/FPC, on OSX at least, that dropping anything
  on any window in a Lazarus application, will only activate the main form's
  OnDropFiles event, irrespective of whether the AllowDropFiles property is set
  to True or not!
  So have used this procedure to pass source files to editor form }

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  (*
  if (EditorForm <> nil) then
    EditorForm.DropFiles(Filenames);
  *)
end;


{ MAKE MACHINE MENU }

{ Create machine select list; one entry for each machine }

procedure TMainForm.MakeMachineMenu;
var
  idx: integer;
  NewItem: TMenuItem;
  temp: string;
begin
  for idx := 0 to (Length(MACHINES) - 1) do // One menu item for each machine
    begin
      NewItem := TMenuItem.Create(Self);
      NewItem.Caption := MACHINES[idx].Name;
      temp := MACHINES[idx].Version;
      if (temp <> '') then              // Check if this is a machine variant
        NewItem.Caption := NewItem.Caption + ' (' + temp + ')';
      NewItem.OnClick := @menuMachineClick;
      NewItem.Tag := idx;               // Menu tag -> MachineIndex (into array)
      NewItem.RadioItem := True;        // Enable auto deselect of items...
      NewItem.GroupIndex := 2;          // when new one clicked
      menuSelect.Add(NewItem);
    end;
end;


{ MACHINE SELECT }

{ If machine selected is not the current machine, close it and create new one }

procedure TMainForm.menuMachineClick(Sender: TObject);
var
  Index: integer;
begin
  Index := (Sender as TMenuItem).Tag;   // Get index into MACHINES array
  if (Index <> CurrentMachineId) then
    begin
      Timer1.Enabled := False;
      StopMachine;                      // Shutdown current machine
      WriteIniSettings;                 // CurrentMachineIndex still old machine
      FreeMachineAndForms;
      AppIni.Free;

      CreateAppIni;
      WriteMachineID(Index);            // Set ID to new machine
      ReadIniSettings;                  // and create it...
      MakeMachineAndForms;
      Timer1.Enabled := True;
    end;
  ShowFormsState;
end;


{ MAKE MACHINE & FORMS }

{ Create machine and all the relevant forms and assign parameters }

procedure TMainForm.MakeMachineAndForms;
begin
  //AppLog.Debug('MakeMachineAndForms');
  MakeMachine;
  MakePreferencesForm;
  if (DeveloperMode) then
    MakeDeveloperForms;
  UpdateStatus;
end;


procedure TMainForm.MakeDeveloperForms;
begin
  MakeDebugForm;
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
  DebugForm.Free;
end;


{ MAKE MACHINE }

procedure TMainForm.MakeMachine;
var
  ThisFreq: integer;
begin
  Machine := TMachineBase.CreateMachine(CurrentMachineId);

  // If ScreenSize is defined in INI file, then set machine screen size, else
  // leave default size
  if (GlobalVars.ScreenSize.X <> -1) then
    Machine.ScreenSize := GlobalVars.ScreenSize
  else
    begin
      SetMachineScreenSize(INITIAL_SCALE_FACTOR); // Set initial size as large
      menuSize3.Checked := True;
    end;

  // If ScreenPosition undefined in INI file, put Screen below MainForm
  if (GlobalVars.ScreenPosition.X = -1) then
    begin
      GlobalVars.ScreenPosition.X := self.Left;
      GlobalVars.ScreenPosition.Y := self.Top + MainForm.Height + FORM_EXTRA_HEIGHT;
    end;
  Machine.ScreenPosition := GlobalVars.ScreenPosition;
  Machine.ScreenCaption := Format('%s (%d)', [Machine.Info.Name, Machine.Info.Year]);
  Machine.Reset;

  Caption := Format('%s [%s]',  [APP_NAME, Machine.Info.Name]);
  menuSelect.Items[CurrentMachineId].Checked := True;
  actRun.Enabled := Machine.Info.HasCodeToExecute; // Enabled if ROMS loaded
  actMachineConfig.Enabled := (Machine.ConfigFrame <> nil);

  // These items only change with Machine, update once only here
  StatusBar.Panels[1].Text := Machine.CPU.Name;
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

{ The assembler tab is only shown if the current CPU supports it, and the
  machine config tab is assigned to that for the current machine }

procedure TMainForm.MakePreferencesForm;
begin
  PreferencesForm := TPreferencesForm.Create(nil);
  PreferencesForm.MachineConfigFrame := Machine.ConfigFrame;
  PreferencesForm.ShowAsmConfig := Machine.CPU.SupportsAssembler;
end;


{ MAKE DEBUG FORM }

procedure TMainForm.MakeDebugForm;
begin
  DebugForm := TDebugForm.Create(nil);
  DebugForm.MachineRef := Machine;
  DebugForm.TraceWidth := Machine.CPU.TraceWidth;
  DebugForm.RegistersHeight := Machine.CPU.RegistersHeight;
end;


{ MAKE DISASSEMBLER FORM }

{ Only created if the CPU has disassembler support }

procedure TMainForm.MakeDisassemblerForm;
begin
  if (Machine.CPU.SupportsDisassembler) then
    begin
      DisassemblerForm := TDisassemblerForm.Create(nil);
      DisassemblerForm.MachineRef := Machine;
      actShowDisassembler.Enabled := True;
    end
  else
    actShowDisassembler.Enabled := False;
end;


{ MAKE COMPARE FORM }

procedure TMainForm.MakeCompareForm;
begin
  CompareForm := TCompareForm.Create(nil);
  CompareForm.MachineRef := Machine;
end;


{ FORM CLOSE }

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (Machine <> nil) then
    Machine.State := msStopped;
  Timer1.Enabled := False;
  CloseAction := caFree;
end;


{ SHOW / HIDE FORMS }

procedure TMainForm.ShowFormsState;
begin
  //if (DeveloperMode) then
    //begin
      actShowDebug.Checked := DeveloperMode and DebugForm.Visible;
      actShowDisassembler.Checked := DeveloperMode and DisassemblerForm.Visible;
      actShowCompare.Checked := DeveloperMode and CompareForm.Visible;
    //end;
  actMode.Checked := DeveloperMode;
  DebugToolbar.Visible := DeveloperMode;
  menuDebug.Visible := DeveloperMode;
  menuEditor.Visible := DeveloperMode;
end;


{ MENU - SET SCREEN SIZE }

{ GlobalVars.ScaleModifier is an adjustment for very small screen sizes,
  like the CHIP-8 }
{ TODO : Need to replace ScaleModifier GLOBAL }

procedure TMainForm.menuSize1Click(Sender: TObject);
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
  Size.X := Machine.Info.ScreenWidthPx * aMultiplier * GlobalVars.ScaleModifier;
  Size.Y := Machine.Info.ScreenHeightPx * aMultiplier * GlobalVars.ScaleModifier;
  Machine.ScreenSize := Size;
end;


{ MENU - ABOUT }

procedure TMainForm.menuAboutWindowsClick(Sender: TObject);
var
  aboutForm: TAboutForm;
begin
  aboutForm := TAboutForm.Create(nil);
  try
    aboutForm.MachineDataFolder := GlobalVars.MachineDataFolder;
    aboutForm.ShowModal;
  finally
    aboutForm.Free;
  end;
end;


{ MENU - PREFERENCES / OPTIONS }

procedure TMainForm.menuOptionsWindowsClick(Sender: TObject);
begin
  ShowPreferences(MAIN_TAB);
end;


{ MENU - MACHINE CONFIG }

{ Show config form; it exists or menu option would be disabled }

procedure TMainForm.actMachineConfigExecute(Sender: TObject);
begin
  ShowPreferences(MACH_CONFIG_TAB);
end;


procedure TMainForm.ShowPreferences(aTab: Integer);
begin
  PreferencesForm.ConfigTabCaption := Machine.Info.Name + ' Config';
  PreferencesForm.Tab := aTab;
  PreferencesForm.ShowModal;
end;


{ MENU - MACHINE INFO }
procedure TMainForm.menuMachineInfoClick(Sender: TObject);
var
  MachineInfoForm: TMachineInfoForm;
begin
  MachineInfoForm := TMachineInfoForm.Create(nil);
  MachineInfoForm.Machine := Machine;
  MachineInfoForm.ShowModal;
  MachineInfoForm.Free;
end;


{ MENU - TOGGLE DEVELOPER MODE }

procedure TMainForm.actModeExecute(Sender: TObject);
begin
  DeveloperMode := (not DeveloperMode); 
  actMode.Checked := DeveloperMode;
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

procedure TMainForm.actLoadCodeBeforeExecute(Sender: TObject);
begin
  actLoadCode.Dialog.DefaultExt := '.c8';                                       { TODO : uMainForm -> put these in each Machine's defs }
  actLoadCode.Dialog.Filter := 'CHIP-8 Files|*.c8';
end;


procedure TMainForm.actLoadCodeAccept(Sender: TObject);
var
  InStream: TMemoryStream;
  MemPtr: ^Byte;
  idx: integer;
begin
  OpenDialog1.Filter := 'Binary Files|*.bin';
  if (OpenDialog1.Execute) then
    begin
      StopMachine;                      // Stop machine if running
      InStream := TMemoryStream.Create;
      try
        InStream.LoadFromFile(OpenDialog1.FileName);
        MemPtr := InStream.Memory;
        for idx := 0 to (InStream.Size - 1) do
          begin
            Machine.Memory[$E800 + idx] := MemPtr^;   (* Loading to $E800, rather than $200 here *)
            Inc(MemPtr);
          end;
      finally
        InStream.Free;
      end;
      Machine.Reset;                    // In case machine was running before
    end;
end;


{ MENU - SAVE CODE FOR THIS MACHINE }

procedure TMainForm.actSaveCodeBeforeExecute(Sender: TObject);
begin
  //
end;


procedure TMainForm.actSaveCodeAccept(Sender: TObject);
begin
  //
end;


{ RUN MACHINE }

{ This is the main execution loop of the emulator. Having setup the GUI
  it runs a loop executing one frame at a time }

procedure TMainForm.actRunExecute(Sender: TObject);
begin
  Machine.CPU.ResetTrace;
  SetButtons(True);

  Machine.State := msRunning;
  UpdateStatus;
  while (Machine.Info.State = msRunning) do // Run until state changes
    begin
      Machine.RunForOneFrame;
      Inc(CalculatedFPS);               // Count each frame to report FPS
    end;

  if (Machine.Info.State = msStoppedOnBrkpt) then
    begin
      SetButtons(False);
      UpdateStatus;
    end;

  Machine.SetFocus;
end;


{ SET BUTTONS }

procedure TMainForm.SetButtons(IsRunning: boolean);
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
  SetButtons(False);
  Machine.State := msStopped;
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
  Machine.State := msRunning;
  Machine.Step;
  Machine.State := msStopped;
  UpdateStatus;                         // Includes set TraceCount
end;


{ STEP OVER }

{ Execute the next instruction, stopping at the address following.
  This is useful for running all of the code of a JSR then stopping after
  the RTS brings execution back to the address }

procedure TMainForm.actStepOverExecute(Sender: TObject);
begin
  //
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

procedure TMainForm.actShowDisassemblerExecute(Sender: TObject);
begin
  DisassemblerForm.Visible := (not DisassemblerForm.Visible);
  actShowDisassembler.Checked := DisassemblerForm.Visible;
end;


{ MAX SPEED }

{ Normally each machine will run at a frame speed appropriate to the original
  CPU frequency, display etc, to give experience as per the original machine
  performance. As modern computers are substantially faster, this is done by
  waiting a set amount of time after each frame has been executed. This 'wait'
  can be switched off to run a 'full speed' of the emulating system. The
  resultant FPS will be displayed on the main GUI bar, and is usually just
  used to check performance changes to program architecture, etc }

procedure TMainForm.actMaxSpeedExecute(Sender: TObject);
begin
  Machine.MaxSpeed := (not Machine.MaxSpeed);
  actMaxSpeed.Checked := Machine.MaxSpeed;
end;


{ ASSEMBLER }

{ Run the assembler for the current CPU against the current source file.
  Other elements covered in Editor Form; links to ActionList therein }

procedure TMainForm.actAssembleExecute(Sender: TObject);
var
  ThisInputFile: string;
  ThisAssembler: TAssembler;
begin
  //ThisInputFile := EditorForm.SourceFilename; // Need a valid filename
  ThisInputFile := GetAppDataDirectory + 'test' + DIRECTORY_SEPARATOR + 'tanbug.asm'; (* DEBUG ONLY *)
  ThisAssembler := TAssembler.Create(Machine);
  try
    ThisAssembler.Execute(ThisInputFile);
  finally
    ThisAssembler.Free;
  end;
//  SymbolsPanel.Visible := True;
//  SymbolsSplitter.Visible := True;
//  DebugForm.EditorText.LoadFromFile(GlobalVars.DataPath + 'test' + DIRECTORY_SEPARATOR + 'tanbug.lst'); (* DEBUG ONLY *)
end;


{ UPDATE STATUS }

procedure TMainForm.UpdateStatus;
begin
  case (Machine.Info.State) of
    msRunning: StatusBar.Panels[0].Text := 'Running';
    msStopped: StatusBar.Panels[0].Text := 'Stopped';
    msStoppedOnBrkpt: StatusBar.Panels[0].Text := Format('Breakpoint $%.4x', [Machine.CPU.PC]);
  end;
  if (DeveloperMode) then
    DebugForm.Refresh;
end;


{ INIFILE READ/WRITE }

{ This procedure builds Custom Section header based on selected machine, it
  is called from uIniFile if SECT_CUSTOM is used to identify the section }

procedure TMainForm.DoCustomSection(var aSect: string);
begin
  aSect := Format('Mach%d', [CurrentMachineID]);
end;


{ READ / WRITE INI SETTINGS }

procedure TMainForm.ReadIniSettings;
begin
  CurrentMachineID := AppIni.ReadInteger(SECT_MAIN, MACHINE_ID, 0);
  Left := AppIni.ReadInteger(SECT_CUSTOM, INI_WDW_LEFT, 20);
  Top := AppIni.ReadInteger(SECT_CUSTOM, INI_WDW_TOP, 20);
  DeveloperMode := AppIni.ReadBool(SECT_CUSTOM, DEVELOPER_MODE, False);

  GlobalVars.ScreenPosition.X := AppIni.ReadInteger(SECT_CUSTOM, SCREEN + INI_WDW_LEFT, -1);
  GlobalVars.ScreenPosition.Y := AppIni.ReadInteger(SECT_CUSTOM, SCREEN + INI_WDW_TOP, -1);
  GlobalVars.ScreenSize.X := AppIni.ReadInteger(SECT_CUSTOM, SCREEN + INI_WDW_WIDTH, -1);
  GlobalVars.ScreenSize.Y := AppIni.ReadInteger(SECT_CUSTOM, SCREEN + INI_WDW_HEIGHT, -1);
end;


procedure TMainForm.WriteIniSettings;
begin
  WriteMachineID(CurrentMachineID);
  AppIni.WriteInteger(SECT_CUSTOM, INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_CUSTOM, INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_CUSTOM, INI_WDW_WIDTH, Width);
  AppIni.WriteInteger(SECT_CUSTOM, INI_WDW_HEIGHT, Height);
  AppIni.WriteBool(SECT_CUSTOM, DEVELOPER_MODE, DeveloperMode);
  AppIni.WriteInteger(SECT_CUSTOM, SCREEN + INI_WDW_LEFT, Machine.ScreenPosition.X);
  AppIni.WriteInteger(SECT_CUSTOM, SCREEN + INI_WDW_TOP, Machine.ScreenPosition.Y);
  AppIni.WriteInteger(SECT_CUSTOM, SCREEN + INI_WDW_WIDTH, Machine.ScreenSize.X);
  AppIni.WriteInteger(SECT_CUSTOM, SCREEN + INI_WDW_HEIGHT, Machine.ScreenSize.Y);

  // Cannot write these in DebugForm's or others OnDestroy events as forms
  // always appear to be invisible at that point. So write state here, but read
  // in each forms OnCreate event
  if (DeveloperMode) then
    begin
      AppIni.WriteBool(SECT_CUSTOM, 'Dbg' + INI_WDW_VIS, DebugForm.Visible);   
      AppIni.WriteBool(SECT_CUSTOM, 'Dis' + INI_WDW_VIS, DisassemblerForm.Visible);
      AppIni.WriteBool(SECT_CUSTOM, 'Compare' + INI_WDW_VIS, CompareForm.Visible);
    end;
end;


procedure TMainForm.WriteMachineID(aID: integer);
begin
  AppIni.WriteInteger(SECT_MAIN, MACHINE_ID, aID);
end;


{ TIMER EVENT }

{ Update window states every one second. Maintains track of whether a window
  is open/closed (visible) without using callbacks from each form itself.
  Since this is called every second, showing frame count each time gives FPS }

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if (DeveloperMode) then
    begin
      actShowDebug.Checked := DebugForm.Visible;      
      actShowDisassembler.Checked := DisassemblerForm.Visible;
      actShowCompare.Checked := CompareForm.Visible;
    end;
  StatusBar.Panels[3].Text := Format('%d fps', [CalculatedFPS]);
  CalculatedFPS := 0;                   // Reset each second
end;


{ AUTO STEP TIMER }

procedure TMainForm.TimerAutoStepTimer(Sender: TObject);
begin
  actStepExecute(nil);                  // Execute one 'step' on each timeout
end;


procedure TMainForm.cbAutoStepChange(Sender: TObject);
begin
  TimerAutoStep.Interval := TrackBarAutoStep.Position;
  TimerAutoStep.Enabled := cbAutoStep.Checked;
  //
  // ConvertForm.Show;  (* CANNOT REMEMBER WHY THIS HERE !! *)
end;


procedure TMainForm.TrackBarAutoStepChange(Sender: TObject);
begin
  TimerAutoStep.Interval := TrackBarAutoStep.Position;
end;


{ KEY DOWN }

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Machine.State = msRunning) then // Only get key presses when running
    ShowMessage('Ensure machine window in focus to accept key presses');
end;


{ DEBUG ??? }

procedure TMainForm.btnTestClick(Sender: TObject);
{
var
  WriteHex: TWriteHex;
  ReadHex: TReadHex;
  i: integer;
  }
begin
  {
  OpenDialog1.Execute;
  WriteHex := TWriteHex.Create(OpenDialog1.FileName + '.TMP');
  WriteHex.SetStart($FFED);
  WriteHex.WriteBytes(Machine.Memory, $FFED, $FFF3);
  WriteHex.Free;

  ReadHex := TReadHex.Create(OpenDialog1.FileName);
  for i := 0 to (ReadHex.BytesRead - 1) do
    Machine.Memory[ReadHex.StartAddress + i] := ReadHex.BytesArray[i];
  if (ReadHex.ErrorMessage <> '') then
    ShowMessage(ReadHex.ErrorMessage);
  EditorForm.Memo.Lines.Add(Format('Start address %.4x for %d bytes', [ReadHex.StartAddress, ReadHex.BytesRead]));
  ReadHex.Free;
  }
end;


{ DEBUG BUTTON }

procedure TMainForm.btnDebugClick(Sender: TObject);
begin
  //Machine.ScreenRefresh;
  case (DebugForm.TraceWidth) of
      0: DebugForm.TraceWidth := 440;
    440: DebugForm.TraceWidth := 580;
    580: DebugForm.TraceWidth := 0;
  end;
end;


end.
