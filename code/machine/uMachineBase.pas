{ ==============================================================================

  MACHINE BASE

  This unit provides the base class for all Machines


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

unit uMachineBase;

{$mode objfpc}{$H+}

interface

uses
  ExtCtrls, Graphics, LCLIntf, LCLType, Classes, Forms, SysUtils, Dialogs,
  //
  uCpuBase, uMachineConfigBase, uBreakpointsFrame, uMemoryMgr, uGfxMgr, uCommon;

type
  TMachineState = (msStopped, msRunning, msStoppedOnBrkpt, msStoppedOnRead,
                   msStoppedOnWrite);

  TMachineItem = record
    Name: string;                       // Name of Machine (identifier)
    Version: string;                    // Used if machine has multiple variants
                                        // with minor variations
  end;

  TMachineInfo = record
    Year: integer;                      // Year introduced
    Name: string;
    CpuType: TCpuType;                  // Main CPU driving this machine
    CpuFreqKhz: integer;
    MemoryButtons: string;              // Optional: up to 3 buttons to put on Memory form
    MachineDefsFilename: string;        // Optional: file with standard machine defs
    State: TMachineState;
    ScreenWidthPx, ScreenHeightPx: integer;
    HasCodeToExecute: boolean;
  end;

  TBrkptHandler = procedure(Addr: word; out IsBrkpt: boolean) of object;

  { TMachineBase }

  TMachineBase = class(TObject)
  private
    function GetMemory: TMemory;
  protected  
    fConfigFrame: TMachineConfigFrame;
    fMemoryMgr: TMemoryMgr;
    fFPS: integer;
    fInfo: TMachineInfo;
    fGfx: TGfxManager;
    TexIdxScreen: integer;              // Main screen texture array index
    fMaxSpeed: boolean;
    fBkptHandler: TBrkptHandler;
    CyclesToGo: integer;
    BreakpointType: TBkptType;                  
    procedure DoBreakpoint(Sender: TObject; Value: TBkptType);
    procedure SetFPS(Value: integer); virtual;
    function  GetCPU: TCpuBase;       virtual; abstract;
    function  GetDescription: string; virtual;
    function  GetScreenPosition: TPoint; virtual;
    procedure SetScreenPosition(Value: TPoint); virtual;
    function  GetScreenSize: TPoint; virtual;
    procedure SetScreenSize(Value: TPoint); virtual;
    procedure SetScreenCaption(Value: string); virtual;
  public
    class function CreateMachine(aMachineId: integer): TMachineBase;
    class destructor Destroy;

    constructor Create;           virtual; abstract;
    procedure Reset;              virtual; abstract;
    procedure RunForOneFrame;     virtual; abstract;
    procedure Step;               virtual; abstract;
    procedure ScreenRefresh;      virtual; abstract;
    procedure SetFocus;

    property ConfigFrame: TMachineConfigFrame read fConfigFrame write fConfigFrame;
    property CPU: TCpuBase read GetCPU;
    property Info: TMachineInfo read fInfo;    
    property State: TMachineState read fInfo.State write fInfo.State;
    property Memory: TMemory read GetMemory;
    property FPS: integer read fFPS write SetFPS;                                    
    property Gfx: TGfxManager read fGfx write fGfx;
    property ScreenPosition: TPoint read GetScreenPosition write SetScreenPosition;
    property ScreenSize: TPoint read GetScreenSize write SetScreenSize;
    property ScreenCaption: string write SetScreenCaption;
    property MaxSpeed: boolean read fMaxSpeed write fMaxSpeed;
    property BreakpointHandler: TBrkptHandler read fBkptHandler write fBkptHandler;
    property Description: string read GetDescription;
  end;

const

  { DEFINE EACH MACHINE }

  MACHINES: array[0..2] of TMachineItem = (
    (Name: 'Microtan65';      Version: ''),
    (Name: 'CHIP-8';          Version: ''),
    (Name: 'Space Invaders';  Version: '') );

  MACHINE_M65   = 0;
  MACHINE_CHIP8 = 1;
  MACHINE_SI    = 2;


implementation

uses
  uMachineMicrotan, uMachineChip8, uMachineSpaceInvaders;


{ CREATE MACHINE }

class function TMachineBase.CreateMachine(aMachineId: integer): TMachineBase;
begin
  GlobalVars.MachineDataFolder := GetAppResourcesDirectory + MACHINES[aMachineID].Name + DIRECTORY_SEPARATOR;
  case (aMachineId) of
    MACHINE_M65:   Result := TMachineMicrotan.Create;
    MACHINE_CHIP8: Result := TMachineChip8.Create;
    MACHINE_SI:    Result := TMachineSpaceInvaders.Create;
  else
    Result := nil
  end;
  if (Result <> nil) then
    Result.MaxSpeed := False;
end;


{ DESTROY  }

class destructor TMachineBase.Destroy;
begin
  //
end;


{ SET FOCUS TO MACHINE WINDOW }

procedure TMachineBase.SetFocus;
begin
  Gfx.Focus;
end;


{ BREAKPOINT ACTIVATED }

procedure TMachineBase.DoBreakpoint(Sender: TObject; Value: TBkptType);
begin
  fInfo.State := msStoppedOnBrkpt;
  BreakpointType := Value;
end;


{ GET / SET = MEMORY }

function TMachineBase.GetMemory: TMemory;
begin
  Result := fMemoryMgr.Memory;
end;


{ GET / SET = FPS }

procedure TMachineBase.SetFPS(Value: integer);
begin
  if (Value < 1) then
    Value := 1;
  fFPS := Value;
end;


{ GET DESCRIPTION }

function TMachineBase.GetDescription: string;
begin
  Result := 'No description provided';
end;


{ GET / SET = SCREEN POSITION }

function TMachineBase.GetScreenPosition: TPoint;
begin
  Result := Gfx.GetWindowPosition;
end;

procedure TMachineBase.SetScreenPosition(Value: TPoint);
begin
  Gfx.SetWindowPosition(Value.X, Value.Y);
end;


{ GET / SET = SCREEN SIZE }

function TMachineBase.GetScreenSize: TPoint;
begin
  Result := Gfx.GetWindowSize;
end;

procedure TMachineBase.SetScreenSize(Value: TPoint);
begin
  Gfx.SetWindowSize(Value.X, Value.Y);
end;


{ GET / SET = SCREEN CAPTION }

procedure TMachineBase.SetScreenCaption(Value: string);
begin
  Gfx.SetCaption(Value);
end;


end.
