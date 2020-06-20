{ ==============================================================================

  DEBUG FORM

    Provides a composite window with CPU registers, memory display, execution
    trace and breakpoints. Built from individual TFrame elements coded in
    their own units


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


unit uDebugForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Buttons, ComCtrls,
  //
  uMemoryFrame, uTraceFrame, uBreakpointsFrame, uWatchesFrame, uMachineBase,
  uRegistersFrameBase, uIniFile, uCommon;

type
  TDebugButton = (dbRun, dbStop, dbReset, dbStep, dbStepOver);
  TDebugButtonEvent = procedure(Sender: TObject; Button: TDebugButton) of object;

  { TDebugForm }

  TDebugForm = class(TForm)
    cbAutoStep: TCheckBox;
    cbBrkptsEnabled: TCheckBox;
    cbWatchesEnabled: TCheckBox;
    gbButtons: TGroupBox;
    gbMemory: TGroupBox;
    gbBreakpoints: TGroupBox;
    gbRegisters: TGroupBox;
    gbTrace: TGroupBox;
    gbWatches: TGroupBox;
    panelLeft: TPanel;
    panelRight: TPanel;
    seAutoStep: TSpinEdit;
    TimerAutoStep: TTimer;
    ToolBar1: TToolBar;
    tbRun: TToolButton;
    tbStop: TToolButton;
    tbReset: TToolButton;
    tbSep1: TToolButton;
    tbStep: TToolButton;
    tbStepOver: TToolButton;
    procedure cbAutoStepChange(Sender: TObject);
    procedure cbBrkptsEnabledChange(Sender: TObject);
    procedure cbWatchesEnabledChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure seAutoStepChange(Sender: TObject);
    procedure TimerAutoStepTimer(Sender: TObject);
  private
    fOnDebugButton: TDebugButtonEvent;
    MemoryFrame: TMemoryFrame;
    TraceFrame: TTraceFrame;
    BreakpointsFrame: TBreakpointsFrame;     
    WatchesFrame: TWatchesFrame;
    procedure DoDebugButton(button: TDebugButton);
    procedure SetStatus(aValue: string);
    procedure SetupRegistersFrame;
    procedure SetupMemoryFrame;
    procedure SetupTraceFrame;
    procedure SetupBreakpointsFrame;
    procedure SetupWatchesFrame;
  public
    procedure Refresh;
    procedure CheckBreakpointRead(aAddr: word; out IsBrkpt: boolean);
    procedure CheckBreakpointWrite(aAddr: word; out IsBrkpt: boolean);
    //
    property OnDebugButton: TDebugButtonEvent read fOnDebugButton write fOnDebugButton;
    property Status: string write SetStatus;
  end;


implementation

{$R *.lfm}

const
  SECT_DEBUG   = 'DebugForm';
  INI_PREFIX   = 'Dbg';
  INI_AUTOSTEP = 'AutoStep';
  INI_BRKPTS   = 'BrkptsEnabled';
  INI_WATCHES  = 'WatchesEnabled';

{ CREATE }

procedure TDebugForm.FormCreate(Sender: TObject);
begin
  Top := AppIni.ReadInteger(SECT_DEBUG, INI_WDW_TOP, 20);
  Left := AppIni.ReadInteger(SECT_DEBUG, INI_WDW_LEFT, 620);
  // Width is set in Mainform.MakeDebugForm dependent on which CPU selected, not saved
  Height := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_HEIGHT, 0);
  // Following is written out in MainForm.WriteIniSettings, but read here
  Visible := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_WDW_VIS, True);
  cbBrkptsEnabled.Checked := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_BRKPTS, True);
  cbWatchesEnabled.Checked := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_WATCHES, True);

  cbAutoStep.Checked := False;
  TimerAutoStep.Enabled := False;
  seAutoStep.Value := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_AUTOSTEP, 1000);

  SetupRegistersFrame;
  SetupMemoryFrame;
  SetupTraceFrame;
  SetupBreakpointsFrame;
  SetupWatchesFrame;
  Machine.BreakpointHandler := @CheckBreakpointRead;

  panelRight.Width := Machine.CPU.Info.TraceWidth;
  gbRegisters.Height := Machine.CPU.Info.RegistersHeight;
end;


{ DESTROY }

procedure TDebugForm.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_DEBUG, INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_DEBUG, INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_HEIGHT, Height);
  AppIni.WriteBool(SECT_CUSTOM, INI_PREFIX + INI_BRKPTS, cbBrkptsEnabled.Checked);
  AppIni.WriteBool(SECT_CUSTOM, INI_PREFIX + INI_WATCHES, cbWatchesEnabled.Checked);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_AUTOSTEP, seAutoStep.Value);
  //
  MemoryFrame.Parent := nil;
  MemoryFrame.Free;
  TraceFrame.Parent := nil;
  TraceFrame.Free;
  BreakpointsFrame.Parent := nil;
  BreakpointsFrame.Free;             
  WatchesFrame.Parent := nil;
  WatchesFrame.Free;
end;


procedure TDebugForm.SetStatus(aValue: string);
begin
  Caption := 'BMDS Debug [' + aValue + ']';
end;


{ SETUP DEBUG FRAMES... }

{ REGISTERS }

procedure TDebugForm.SetupRegistersFrame;
begin
  if (Machine.CPU.RegisterFrame <> nil) then
    begin                                                              
      Machine.CPU.RegisterFrame.Parent := gbRegisters;
      Machine.CPU.RegisterFrame.MemoryRef := Machine.Memory;
      Machine.CPU.RegisterFrame.Initialise;
      //
      gbRegisters.Caption := Machine.CPU.Info.Name + ' Registers';
    end
  else
    gbRegisters.Caption := 'Registers: panel not implemented';
end;


{ MEMORY }

procedure TDebugForm.SetupMemoryFrame;
var
  idx: integer;
  buttons: TStringList;
begin
  MemoryFrame := TMemoryFrame.Create(self);
  MemoryFrame.Parent := gbMemory;
  MemoryFrame.MemoryRef := Machine.Memory;
  MemoryFrame.Initialise;
  //
  gbMemory.Caption := 'Memory Display (' + IntToStr(Trunc(Length(Machine.Memory) / 1024)) + 'K)';
  buttons := TStringList.Create;
  try
    buttons.CommaText := Machine.Info.MemoryButtons;
    for idx := 0 to buttons.Count - 1 do
      MemoryFrame.AddButton(buttons.Names[idx], GetHex(buttons.ValueFromIndex[idx]));
  finally
    buttons.Free;
  end;
end;


{ TRACE }

procedure TDebugForm.SetupTraceFrame;
begin
  TraceFrame := TTraceFrame.Create(nil);
  TraceFrame.Parent := gbTrace;
  TraceFrame.CpuRef := Machine.CPU;
  TraceFrame.Initialise;
end;


{ BREAKPOINTS }

procedure TDebugForm.SetupBreakpointsFrame;
begin
  BreakpointsFrame := TBreakpointsFrame.Create(nil);
  BreakpointsFrame.Parent := gbBreakpoints;
  BreakpointsFrame.Initialise;
  cbBrkptsEnabledChange(self);

  // The breakpoints array size depends on the allocated machine memory size,
  // so that breakpoints can be set anywhere code might be
  BreakpointsFrame.ArraySize := Length(Machine.Memory);
end;


procedure TDebugForm.cbBrkptsEnabledChange(Sender: TObject);
begin
  if (BreakpointsFrame <> nil) then
    BreakpointsFrame.Enabled := cbBrkptsEnabled.Checked;
end;


{ CHECK BREAKPOINTS }

procedure TDebugForm.CheckBreakpointRead(aAddr: word; out IsBrkpt: boolean);
begin
  if (BreakpointsFrame.Enabled) then
    IsBrkpt := BreakpointsFrame.CheckRead(aAddr)
  else
    IsBrkpt := False;
end;


procedure TDebugForm.CheckBreakpointWrite(aAddr: word; out IsBrkpt: boolean);
begin
  if (BreakpointsFrame.Enabled) then
    IsBrkpt := BreakpointsFrame.CheckWrite(aAddr)
  else
    IsBrkpt := False;
end;


{ WATCHES }

procedure TDebugForm.SetupWatchesFrame;
begin
  WatchesFrame := TWatchesFrame.Create(nil);
  WatchesFrame.Parent := gbWatches;
  WatchesFrame.Initialise;
  cbWatchesEnabledChange(self);
end;


procedure TDebugForm.cbWatchesEnabledChange(Sender: TObject);
begin
  if (WatchesFrame <> nil) then
    WatchesFrame.Enabled := cbWatchesEnabled.Checked;
end;


{ REFRESH }

procedure TDebugForm.Refresh;
begin
  if (MemoryFrame <> nil) then
    MemoryFrame.Refresh;
  if (TraceFrame <> nil) then
    begin
      TraceFrame.Rows := Machine.CPU.TraceCount + 1;
      TraceFrame.Refresh;
    end;
  if (Machine.CPU.RegisterFrame <> nil) then
    Machine.CPU.RegisterFrame.Refresh;
end;


{ AUTOSTEP TIMER }

procedure TDebugForm.DoDebugButton(button: TDebugButton);
begin
  if Assigned(fOnDebugButton) then
    fOnDebugButton(self, button);
end;


procedure TDebugForm.TimerAutoStepTimer(Sender: TObject);
begin
  DoDebugButton(dbStep);
end;


procedure TDebugForm.seAutoStepChange(Sender: TObject);
begin
  TimerAutoStep.Interval := seAutoStep.Value;
end;


procedure TDebugForm.cbAutoStepChange(Sender: TObject);
begin
  TimerAutoStep.Interval := seAutoStep.Value;
  TimerAutoStep.Enabled := cbAutoStep.Checked;
end;


end.

