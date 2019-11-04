{ ==============================================================================

  PSEUDO CHIP-8 MACHINE

    Class to provide CHIP-8 code emulation


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

{ TODO : uMachineChip8 -> add 64x64 mode, and SuperChip8 128x64 mode }

{ TODO : uMachineChip8 -> STEPS_PER_FRAME is a guesstimate, need to calculate properly }

{ TODO : uMachineChip8 -> program LoadFromFile, SaveToFile }

unit uMachineChip8;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Forms, Classes, SysUtils, ExtCtrls, Graphics, Dialogs,
  //
  uMachineBase, uCpuBase, uCpuChip8, uDefsChip8, uMemoryMgr, uGfxMgr, uCommon,
  SDL2;

const
  STEPS_PER_FRAME = 10;

type

  { TMachineChip8 }

  TMachineChip8 = class(TMachineBase)
  private
    fCPU: TCpuChip8;
    fDelayTimer: byte;
    fSoundTimer: byte;    
    ScreenImage: TBitmap;                                          
    procedure CheckInput;
    procedure MemRead(Sender: TObject; Addr: word; var Value: byte);
    procedure MemWrite(Sender: TObject; Addr: word; Value: byte);
  protected
    function GetCPU: TCpuBase; override;
    function GetDescription: string; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Reset; override;
    procedure RunForOneFrame; override;
    procedure Step; override;    
    procedure ScreenRefresh; override; 
    procedure SaveToFile(strm: TStream);
    procedure LoadFromFile(Filename: string);

    property CPU: TCpuBase read GetCPU;
    property DelayTimer: byte read fDelayTimer write fDelayTimer;
    property SoundTimer: byte read fSoundTimer write fSoundTimer;
end;


implementation

{ CREATE }

constructor TMachineChip8.Create;
begin
  fConfigFrame := nil;                  // No config for CHIP8

  fInfo.Name                := MACHINES[MACHINE_CHIP8].Name;
  fInfo.Year                := 1975;
  fInfo.CpuType             := ctCHIP8;
  fInfo.ScreenWidthPx       := 64;
  fInfo.ScreenHeightPx      := 32;
  fInfo.MemoryButtons       := 'Font=0000,Start=0200'; // Hex values
  fInfo.MachineDefsFilename := '';
  fInfo.HasCodeToExecute    := False;   // Needs a program to execute

  GlobalVars.ScaleModifier := 5;        // Make 64x32 screen a bit bigger!

  fMemoryMgr := TMemoryMgr.Create(0, MEM_SIZE_4K);

  Gfx := TGfxManager.Create;            // Create screen, uses default B&W palette
  Gfx.SetWindowSize(fInfo.ScreenWidthPx, fInfo.ScreenHeightPx);
  TexIdxScreen := Gfx.GetTexture(fInfo.ScreenWidthPx, fInfo.ScreenHeightPx);

  fCPU := TCpuChip8.Create;             // Create and initialise pseudo CPU
  fCPU.Machine := self;
  fCPU.OnRead := @MemRead;
  fCPU.OnWrite := @MemWrite;
  fCPU.Reset;

  FPS := 60;                            // Matches counter rate
  fDelayTimer := 0;
  fSoundTimer := 0;
end;


{ DESTROY }

destructor TMachineChip8.Destroy;
begin
  fCPU.Free;
  ScreenImage.Free;
  Gfx.Free;
  fMemoryMgr.Free;
  inherited;
end;


{ GET / SET ROUTINES }

function TMachineChip8.GetCPU: TCpuBase;
begin
  Result := TCpuBase(fCPU);
end;


{ GET DESCRIPTION }

function TMachineChip8.GetDescription: string;
begin
  Result := 'CHIP-8' + CRLF + CRLF +
            'CHIP-8 is an interpreted programming language, developed by Joseph Weisbecker. ' +
            'It was initially used on the COSMAC VIP and RCA Telmac 1800 8-bit microcomputers in the mid-1970s. ' +
            'CHIP-8 programs are run on a CHIP-8 virtual machine. ' +
            'It was made to allow video games to be more easily programmed for these computers.'  + CRLF + CRLF +
            'The basic specification was:' + CRLF +
            '  - Sixteen 8-bit registers' + CRLF +
            '  - One 16-bit address/index register' + CRLF +
            '  - 4K RAM, inc 48 bytes for stack' + CRLF +
            '  - Two timers counting down at 60Hz; delay timer and sound timer' + CRLF +
            '  - Hex keyboard' + CRLF +
            '  - Screen 64x32 mono. Graphic sprites 8 pixels wide, 1-15 pixels high' + CRLF + CRLF +
            'A descendant, SCHIP (Super-CHIP), came out in 1990 introducing 128x64 resolution and some additional instructions.' + CRLF + CRLF +
            'Keys for this emulation:' + CRLF +
            '  8, 4, 6, 2 => up,left,right,down';
end;


{ RUNNING FOR ONE FRAME }

procedure TMachineChip8.RunForOneFrame;
var
  TimeBetweenFramesMS, LastTime, SleepMS: integer;
  NumStepsToGo: integer;
begin
  TimeBetweenFramesMS := 1000 div FPS;
  LastTime := DateTimeToTimeStamp(Now).Time;

  // Run CPU for one frame's worth instructions
  NumStepsToGo := STEPS_PER_FRAME;
  while ((fInfo.State = msRunning) and (NumStepsToGo > 0)) do
    begin
      fCPU.ExecuteInstruction;
      Dec(NumStepsToGo);
    end;

  if (fInfo.State = msRunning) then     // Do not do next if stopping
    begin
      // Decrement timers, refresh screen, etc
      if (fDelayTimer > 0) then
        Dec(fDelayTimer);
      if (fSoundTimer > 0) then
        Dec(fSoundTimer);
      CheckInput;                       // Check keyboard
      ScreenRefresh;
      Application.ProcessMessages;

      // ... then sleep until frame period has expired, if required
      SleepMS := TimeBetweenFramesMS - (DateTimeToTimeStamp(Now).Time - LastTime);
      if (SleepMS > 0) then
        Sleep(SleepMS);
    end;
end;


{ STEP }

{ Execute one CPU instruction, and decrement counters as required }

procedure TMachineChip8.Step;
begin
  fCPU.ExecuteInstruction;
  if (fDelayTimer > 0) then
    Dec(fDelayTimer);
  if (fSoundTimer > 0) then
    Dec(fSoundTimer);
  ScreenRefresh;
end;


{ RESET }

procedure TMachineChip8.Reset;
begin
  fCPU.Reset;
end;


{ CHECK INPUT }

{ Checks for any user input when machine is running }

procedure TMachineChip8.CheckInput;
var
  sdlEvent: PSDL_Event;
  thisKey: word;
begin
  New(sdlEvent);
  while (SDL_PollEvent(sdlEvent) = 1) do
    begin
      if (SdlEvent^.type_ = SDL_KEYDOWN) then
        begin
          thisKey := sdlEvent^.key.keysym.sym;
          (*
          if (Key = Chr(27)) then Exit;

          case Ord(Key) of
            $31: fCPU.Key[$1] := 1;             // 1
            $32: fCPU.Key[$2] := 1;             // 2
            $33: fCPU.Key[$3] := 1;             // 3
            $34: fCPU.Key[$C] := 1;             // 4

            $51: fCPU.Key[$4] := 1;             // Q
            $57: fCPU.Key[$5] := 1;             // W
            $45: fCPU.Key[$6] := 1;             // E
            $52: fCPU.Key[$D] := 1;             // R

            $41: fCPU.Key[$7] := 1;             // A
            $53: fCPU.Key[$8] := 1;             // S
            $44: fCPU.Key[$9] := 1;             // D
            $46: fCPU.Key[$E] := 1;             // F

            $5A: fCPU.Key[$A] := 1;             // Z
            $58: fCPU.Key[$0] := 1;             // X
            $43: fCPU.Key[$B] := 1;             // C
            $56: fCPU.Key[$F] := 1;             // V
          end;
          *)
        end;
    end;
  Dispose(sdlEvent);
end;


{ SCREEN REFRESH }

procedure TMachineChip8.ScreenRefresh;
var
  x, y: byte;
  Pixel: integer;
begin
  for y := 0 to 31 do                   // 32 rows of pixels
    for x := 0 to 63 do                 // 64 columns of pixels
      begin
        Pixel := fCPU.Pixels[x + (y * 64)];
        Gfx.SetPixel(TexIdxScreen, x, y, Gfx.Palette[Pixel]);
      end;
  Gfx.ShowPixels(TexIdxScreen);
end;


{ MEMORY READ/WRITE }

procedure TMachineChip8.MemRead(Sender: TObject; Addr: word; var Value: byte);
begin
  Value := fMemoryMgr.Memory[Addr];
end;


procedure TMachineChip8.MemWrite(Sender: TObject; Addr: word; Value: byte);
begin
  fMemoryMgr.Memory[Addr] := Value;
end;


{ LOAD / SAVE TO FILE }

procedure TMachineChip8.LoadFromFile(Filename: string);
begin
  //
end;


procedure TMachineChip8.SaveToFile(strm: TStream);
begin
  //
end;


end.
