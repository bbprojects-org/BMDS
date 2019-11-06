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
  //
  uMemoryFrame, uTraceFrame, uBreakpointsFrame, uWatchesFrame, uMachineBase,
  uRegistersFrameBase, uIniFile, uCommon;

type

  { TDebugForm }

  TDebugForm = class(TForm)
    gbRegisters: TGroupBox;
    gbBreakpoints: TGroupBox;
    gbMemory: TGroupBox;
    gbEditor: TGroupBox;
    gbWatches: TGroupBox;
    gbTrace: TGroupBox;
    panelLeft: TPanel;
    panelMiddle: TPanel;
    panelRight: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fMachineRef: TMachineBase;
    MemoryFrame: TMemoryFrame;
    TraceFrame: TTraceFrame;
    BreakpointsFrame: TBreakpointsFrame;     
    WatchesFrame: TWatchesFrame;
    (*
    EditorFrame: TEditorFrame;
    *)
    procedure SetMachine(const aMachine: TMachineBase);
    procedure SetupRegistersFrame;
    procedure SetupMemoryFrame;
    procedure SetupTraceFrame;
    procedure SetupBreakpointsFrame;
    procedure SetupWatchesFrame;
    procedure SetupEditorFrame;
    procedure SetEditorText(const aStrings: TStrings);
    function  GetEditorText: TStrings;
    procedure SetRegistersWidth(const aValue: integer);
    procedure SetTraceWidth(const aValue: integer);
    procedure SetRegistersHeight(const aValue: integer);
    function GetRegistersHeight: integer;
    function GetRegistersWidth: integer;
    function GetTraceWidth: integer;
  public
    procedure Refresh;
    procedure CheckBreakpointRead(aAddr: word; out IsBrkpt: boolean);
    procedure CheckBreakpointWrite(aAddr: word; out IsBrkpt: boolean);
    //
    property MachineRef: TMachineBase write SetMachine;
    property EditorText: TStrings read GetEditorText write SetEditorText;
    property RegistersWidth: integer read GetRegistersWidth write SetRegistersWidth;
    property TraceWidth: integer read GetTraceWidth write SetTraceWidth;
    property RegistersHeight: integer read GetRegistersHeight write SetRegistersHeight;
  end;


implementation

{$R *.lfm}

const
  INI_PREFIX = 'Dbg';


{ CREATE }

procedure TDebugForm.FormCreate(Sender: TObject);
begin
  Top := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, 20);
  Left := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, 620);
  Width := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_WIDTH, 0);
  Height := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_HEIGHT, 0);
  Visible := AppIni.ReadBool(SECT_CUSTOM, INI_PREFIX + INI_WDW_VIS, True); // Written out in uMainForm
end;

{ DESTROY }

procedure TDebugForm.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_WIDTH, Width);
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + INI_WDW_HEIGHT, Height);
  //
  MemoryFrame.Parent := nil;
  MemoryFrame.Free;
  TraceFrame.Parent := nil;
  TraceFrame.Free;
  BreakpointsFrame.Parent := nil;
  BreakpointsFrame.Free;             
  WatchesFrame.Parent := nil;
  WatchesFrame.Free;
  (*
  EditorFrame.Parent := nil;
  EditorFrame.Free;
  *)
end;


{ SET MACHINE }

procedure TDebugForm.SetMachine(const aMachine: TMachineBase);
begin
  fMachineRef := aMachine;
  SetupRegistersFrame;
  SetupMemoryFrame;
  SetupTraceFrame;         
  SetupBreakpointsFrame;   
  SetupWatchesFrame;
  (*
  SetupEditorFrame;
  fMachineRef.BreakpointHandler := @CheckBreakpointRead;
  *)
end;


{ SET & GET EDITOR TEXT }

procedure TDebugForm.SetEditorText(const aStrings: TStrings);
begin
  (*
  if (EditorFrame <> nil) then
    EditorFrame.Lines := aStrings;
  *)
end;

function TDebugForm.GetEditorText: TStrings;
begin
  (*
  if (EditorFrame <> nil) then
    Result := EditorFrame.Lines
  else
  *)
    Result := nil;
end;


{ SETUP DEBUG FRAMES... }

{ REGISTERS }

procedure TDebugForm.SetupRegistersFrame;
begin
  if (fMachineRef.CPU.RegisterFrame <> nil) then
    begin                                                              
      fMachineRef.CPU.RegisterFrame.Parent := gbRegisters;
      fMachineRef.CPU.RegisterFrame.MemoryRef := fMachineRef.Memory;
      fMachineRef.CPU.RegisterFrame.Initialise;
      //
      gbRegisters.Caption := fMachineRef.CPU.Name + ' Registers';
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
  MemoryFrame.MemoryRef := fMachineRef.Memory;      
  MemoryFrame.Initialise;
  //
  gbMemory.Caption := 'Memory Display (' + IntToStr(Trunc(Length(fMachineRef.Memory) / 1024)) + 'K)';
  buttons := TStringList.Create;
  try
    buttons.CommaText := fMachineRef.Info.MemoryButtons;
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
  TraceFrame.CpuRef := fMachineRef.CPU; 
  TraceFrame.Initialise;
end;


{ BREAKPOINTS }

procedure TDebugForm.SetupBreakpointsFrame;
begin
  BreakpointsFrame := TBreakpointsFrame.Create(nil);
  BreakpointsFrame.Parent := gbBreakpoints;
  BreakpointsFrame.Initialise;

  // The breakpoints array size depends on the allocated machine memory size,
  // so that breakpoints can be set anywhere code might be
  BreakpointsFrame.ArraySize := Length(fMachineRef.Memory);
end;


{ WATCHES }

procedure TDebugForm.SetupWatchesFrame;
begin
  WatchesFrame := TWatchesFrame.Create(nil);
  WatchesFrame.Parent := gbWatches;            
  WatchesFrame.Initialise;
end;


{ EDITOR }

procedure TDebugForm.SetupEditorFrame;
begin
  (*
  EditorFrame := TEditorFrame.Create(nil);
  EditorFrame.Parent := gbEditor;
  EditorFrame.Initialise;
  *)
end;


{ CHECK BREAKPOINTS }

procedure TDebugForm.CheckBreakpointRead(aAddr: word; out IsBrkpt: boolean);
begin
  IsBrkpt := BreakpointsFrame.CheckRead(aAddr);
end;


procedure TDebugForm.CheckBreakpointWrite(aAddr: word; out IsBrkpt: boolean);
begin
  IsBrkpt := BreakpointsFrame.CheckWrite(aAddr);
end;


{ REFRESH }

procedure TDebugForm.Refresh;
begin
  if (TraceFrame <> nil) then
    begin
      TraceFrame.Rows := fMachineRef.CPU.TraceCount;
      TraceFrame.Refresh;
    end;
  if (fMachineRef.CPU.RegisterFrame <> nil) then
    fMachineRef.CPU.RegisterFrame.Refresh;
end;


{ GET & SET LAYOUT SIZES }

function TDebugForm.GetRegistersWidth: integer;
begin
  Result := panelLeft.Width;
end;

procedure TDebugForm.SetRegistersWidth(const aValue: integer);
begin
  panelLeft.Width := aValue;
end;


function TDebugForm.GetTraceWidth: integer;
begin
  Result := panelMiddle.Width;
end;

procedure TDebugForm.SetTraceWidth(const aValue: integer);
begin
  panelMiddle.Width := aValue;
end;


function TDebugForm.GetRegistersHeight: integer;
begin
  Result := gbRegisters.Height;
end;

procedure TDebugForm.SetRegistersHeight(const aValue: integer);
begin
  gbRegisters.Height := aValue;
end;


end.

