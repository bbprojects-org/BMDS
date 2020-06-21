{ ==============================================================================

  MACHINE BASE

    This unit provides the base class for all Machines, and also the
    machine factory for registering and creating machines


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
  Menus,
  //
  uCpuBase, uBreakpointsFrame, uMemoryMgr, uGfxMgr, uPrefsFrameBase;

type
  TMachineState = (msStopped, msRunning, msStoppedOnBrkpt, msStoppedOnRead,
                   msStoppedOnWrite);
  TMachineChangedItem = (mcFreq);

  TBrkptHandler = procedure(Addr: word; out IsBrkpt: boolean) of object;
  TConfigChange = procedure(Sender: TObject; ChangedItem: TMachineChangedItem) of object;

  // Valid file extensions supporting these machines
  TFileExt     = (feM65, feSI, feC8, feBIN, feHEX, feALL);
  TFileExtSet  = set of TFileExt;
  TFileExtList = array[TFileExt] of string;

  TMachineInfo = record
    // Name is used to register Machine class, and saved in CurrentMachineID
    Year: integer;                      // Year introduced
    CpuFreqKhz: integer;
    MemoryButtons: string;              // Optional: up to 3 buttons to put on Memory form
    MachineDefsFileName: string;        // Optional: file with standard machine defs
    State: TMachineState;
    ScreenWidthPx, ScreenHeightPx: integer;
    ScaleModifier: integer;             // Used for small screen sizes (eg CHIP-8)
    HasCodeToExecute: boolean;          // If a ROM available and loaded
    FileExts: TFileExtSet;              // Defines which file EXT valid for machine
    DefaultLoadAddr: integer;           // Default binary file load address
    HasCustomMenu: boolean;
  end;

  { TMachineBase }

  TMachineBase = class(TObject)
  private
    function GetMemory: TMemory;
  protected
    fName: string;
    fConfigFrame: TPrefsFrame;
    fMemoryMgr: TMemoryMgr;
    fGfx: TGfxManager;
    fFPS: integer;
    fInfo: TMachineInfo;
    TexIdxScreen: integer;              // Main screen texture array index
    fMaxSpeed: boolean;
    CyclesToGo: integer;
    fBkptHandler: TBrkptHandler;
    fOnConfigChange: TConfigChange;
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
    constructor Create;           virtual; abstract;
    procedure Reset;              virtual; abstract;
    procedure RunForOneFrame;     virtual; abstract;
    procedure Step;               virtual; abstract;
    procedure ScreenRefresh;      virtual; abstract;
    procedure Stop;               virtual;
    procedure LoadFromFile(FileName: string); virtual; abstract;
    procedure SaveToFile(FileName: string; FullSystem: boolean = False); virtual; abstract;
    procedure SetFocus;
    procedure SetCustomMenuItem(var item: TMenuItem); virtual;
    function  IsRAM(Addr: word): boolean;

    property Name: string read fName write fName;
    property ConfigFrame: TPrefsFrame read fConfigFrame write fConfigFrame;
    property CPU: TCpuBase read GetCPU;
    property Info: TMachineInfo read fInfo;    
    property State: TMachineState read fInfo.State write fInfo.State;
    property MemoryMgr: TMemoryMgr read fMemoryMgr write fMemoryMgr;
    property Memory: TMemory read GetMemory;
    property FPS: integer read fFPS write SetFPS;
    property Gfx: TGfxManager read fGfx write fGfx;
    property ScreenPosition: TPoint read GetScreenPosition write SetScreenPosition;
    property ScreenSize: TPoint read GetScreenSize write SetScreenSize;
    property ScreenCaption: string write SetScreenCaption;
    property MaxSpeed: boolean read fMaxSpeed write fMaxSpeed;
    property Description: string read GetDescription;
    property BreakpointHandler: TBrkptHandler read fBkptHandler write fBkptHandler;
    property OnConfigChange: TConfigChange read fOnConfigChange write fOnConfigChange;
  end;


  { TMachineFactory }

  TMachineClass = class of TMachineBase;

  TMachineRegistration = record
    ID: string;
    MachineClass: TMachineClass;
  end;


  TMachineFactory = class
  private
  public
    class procedure RegisterMachine(const aID: string; aMachineClass: TMachineClass);
    class function CreateMachineFromID(const aID: string): TMachineBase;
    class function FindMachineClassForID(const aID: string): TMachineClass;
    class function FindIdForMachineClass(aMachineClass: TMachineClass): string;
    class function FindIndexForID(const aID: string): integer;
    class function GetIdForIdx(const aIdx: integer): string;
    class function Count: integer;
  end;


var
  MachineFactory: TMachineFactory;
  MachineDataFolder: string;
  Machine: TMachineBase;


implementation

var
  RegisteredMachines: array of TMachineRegistration;


{ TMachineBase }


{ SET FOCUS TO MACHINE WINDOW }

procedure TMachineBase.SetFocus;
begin
  Gfx.Focus;
end;


{ STOP MACHINE }

procedure TMachineBase.Stop;
begin
  fInfo.State := msStopped;
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


{ SET = FPS }

procedure TMachineBase.SetFPS(Value: integer);
begin
  if (Value < 1) then
    Value := 1;
  fFPS := Value;
end;


{ GET DESCRIPTION }

function TMachineBase.GetDescription: string;
begin
  Result := 'Description';
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


{ SET = SCREEN CAPTION }

procedure TMachineBase.SetScreenCaption(Value: string);
begin
  Gfx.SetCaption(Value);
end;


{ CHECK IF ADDRESS GIVEN IS IN RAM, i.e. writeable }

function TMachineBase.IsRAM(Addr: word): boolean;
begin
  Result := fMemoryMgr.IsRAM(Addr);
end;


{ CREATE A CUSTOM MENU ITEM }

procedure TMachineBase.SetCustomMenuItem(var item: TMenuItem);
begin
  //
end;


{ TMachineFactory }


{ REGISTER MACHINE - if not already registered, add the machine to the list
                     of registered machines }

class procedure TMachineFactory.RegisterMachine(const aID: string; aMachineClass: TMachineClass);
var
  i, Len: integer;
begin
  Len := Length(RegisteredMachines);
  for i := 0 to Len - 1 do
    if (RegisteredMachines[i].ID = aID) and (RegisteredMachines[i].MachineClass = aMachineClass) then
      Exit;

  SetLength(RegisteredMachines, Len + 1);
  RegisteredMachines[Len].ID := aID;
  RegisteredMachines[Len].MachineClass := aMachineClass;
end;


{ CREATE MACHINE FROM ID - given an ID string, find in the list of registered
                           machines and create it }

class function TMachineFactory.CreateMachineFromID(const aID: string): TMachineBase;
var
  MachineClass: TMachineClass;
begin
  MachineClass :=  FindMachineClassForId(aID);
  if (MachineClass <> nil) then
    Result := MachineClass.Create
  else
    Result := nil;
end;


{ FIND MACHINE CLASS FOR GIVEN ID }

class function TMachineFactory.FindMachineClassForID(const aID: string): TMachineClass;
var
  i, Len: integer;
begin
  Result := nil;
  Len := Length(RegisteredMachines);
  for i := 0 to Len - 1 do
    if (RegisteredMachines[i].ID = aID) then
      begin
        Result := RegisteredMachines[i].MachineClass;
        break;
      end;
end;


{ FIND ID FOR GIVEN MACHINE CLASS }

class function TMachineFactory.FindIdForMachineClass(aMachineClass: TMachineClass): string;
var
  i, Len: integer;
begin
  Result := '';
  Len := Length(RegisteredMachines);
  for i := 0 to Len - 1 do
    if (RegisteredMachines[i].MachineClass = aMachineClass) then
      begin
        Result := RegisteredMachines[i].ID;
        break;
      end;
end;


{ FIND INDEX FOR GIVEN ID }

class function TMachineFactory.FindIndexForID(const aID: string): integer;
var
  i, Len: integer;
begin
  Result := -1;
  Len := Length(RegisteredMachines);
  for i := 0 to Len - 1 do
    if (RegisteredMachines[i].ID = aID) then
      begin
        Result := i;
        break;
      end;
end;


{ GET ID FOR GIVEN INDEX }

class function TMachineFactory.GetIdForIdx(const aIdx: integer): string;
begin
  Result := '';
  if (aIdx < Count) then
    Result := RegisteredMachines[aIdx].ID;
end;


{ GET COUNT - number of registered machines }

class function TMachineFactory.Count: integer;
begin
  Result := Length(RegisteredMachines);
end;


{ INITIALISE THE MACHINE FACTORY }

initialization
  MachineFactory := TMachineFactory.Create;

finalization
  MachineFactory.Free;


end.
