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
{.$define machinechip8_debug}

interface

uses
  LCLIntf, LCLType, Forms, Classes, SysUtils, ExtCtrls, Graphics, Dialogs,
  //
  uMachineBase, uCpuBase, uCpuTypes, uCpuChip8, uDefsChip8, uMemoryMgr, uGfxMgr,
  uPrefsChip8, uCommon, SDL2;

const
  STEPS_PER_FRAME = 5;

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
    //
    procedure Reset; override;
    procedure RunForOneFrame; override;
    procedure Step; override;    
    procedure ScreenRefresh; override; 
    procedure SaveToFile(FileName: string; {%H-}FullSystem: boolean = False); override;
    procedure LoadFromFile(FileName: string); override;
    //
    property CPU: TCpuBase read GetCPU;
    property DelayTimer: byte read fDelayTimer write fDelayTimer;
    property SoundTimer: byte read fSoundTimer write fSoundTimer;
end;


implementation

{ CREATE }

constructor TMachineChip8.Create;
begin
  // fConfigFrame referenced by PreferencesForm for config when CHIP-8 selected
  fConfigFrame := TChip8PrefsFrame.Create(nil);
  fConfigFrame.Init;                    // Get INI settings required below

  fInfo.Year                := 1975;
  fInfo.ScreenWidthPx       := 64;
  fInfo.ScreenHeightPx      := 32;
  fInfo.ScaleModifier       := 5;       // Make 64x32 pixel screen a bit bigger!
  fInfo.MemoryButtons       := 'Font=0000,Start=0200'; // Hex values
  fInfo.MachineDefsFileName := '';
  fInfo.HasCodeToExecute    := False;   // Needs a program to execute
  fInfo.FileExts            := [feC8, feBIN];
  fInfo.DefaultLoadAddr     := $200;
  fInfo.HasCustomMenu       := False;

  fMemoryMgr := TMemoryMgr.Create(MEM_SIZE_4K);
  fMemoryMgr.AddRead($0000, $0FFF, nil, '4K RAM read');
  fMemoryMgr.AddWrite($0000, $0FFF, nil, '4K RAM write');

  Gfx := TGfxManager.Create(2);         // Create screen, uses default B&W palette
  Gfx.SetWindowSize(fInfo.ScreenWidthPx, fInfo.ScreenHeightPx);
  TexIdxScreen := Gfx.GetTexture(fInfo.ScreenWidthPx, fInfo.ScreenHeightPx);

  fCPU := TCpuChip8.Create(ctCHIP8);    // Create and initialise pseudo CPU
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
            'CHIP-8 is an interpreted programming language, developed by Joseph ' +
            'Weisbecker. It was initially used on the COSMAC VIP and RCA Telmac ' +
            '1800 8-bit microcomputers in the mid-1970s. CHIP-8 programs are run ' +
            'on a CHIP-8 virtual machine. It was made to allow video games to ' +
            'be more easily programmed for these computers.' + CRLF + CRLF +
            'The basic specification included:' + CRLF +
            '  - Sixteen 8-bit registers' + CRLF +
            '  - One 16-bit address/index register' + CRLF +
            '  - 4K RAM, and 16 word stack' + CRLF +
            '  - Two timers counting down at 60Hz; delay timer and sound timer' + CRLF +
            '  - Hex keyboard' + CRLF +
            '  - Screen 64x32 mono. Graphic sprites 8 pixels wide, 1-15 pixels high' + CRLF + CRLF +
            'A descendant, SCHIP (Super-CHIP), came out in 1990 introducing 128x64 ' +
            'resolution and some additional instructions.' + CRLF + CRLF +
            'Keys for this emulation:' + CRLF +
            '  8, 4, 6, 2 => up,left,right,down (TBD)';
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

{ Maps    1 2 3 C   to   1 2 3 4
          4 5 6 D        Q W E R
          7 8 9 E        A S D F
          A 0 B F        Z X C V }

const
  KEY_MATRIX: array[$0..$F] of byte =
    // 0 1 2 3 4 5 6 7   8 9 A B C D E F   - original keys
    // X 1 2 3 Q W E A   S D Z C 4 R F V   - mapped to
    ($58, $31, $32, $33, $51, $57, $45, $41,
     $53, $44, $5A, $43, $34, $52, $46, $56);

procedure TMachineChip8.CheckInput;
var
  sdlEvent: PSDL_Event;
  thisKey: integer;
  index: byte;
  {$ifdef machinechip8_debug}
  debug: string;
  {$endif}
begin
  New(sdlEvent);
  while (SDL_PollEvent(sdlEvent) = 1) do
    begin
      if ((SdlEvent^.type_ = SDL_KEYDOWN) or (SdlEvent^.type_ = SDL_KEYUP)) then
        begin
          thisKey := sdlEvent^.key.keysym.sym;
          if ((thisKey >= $61) and (thisKey <= $7A)) then
            thisKey := thisKey - $20;   // Convert lowercase to uppercase

          if (thisKey = $1B) then       // ESC key?
            Exit;

          for index := $0 to $F do      // Check translation table
            if (thisKey = KEY_MATRIX[index]) then
              begin                     // If valid key, indicate whether pressed
                fCPU.Key[index] := (SdlEvent^.type_ = SDL_KEYDOWN);
                break;
              end;
        end;
    end;
  Dispose(sdlEvent);

  {$ifdef machinechip8_debug}
  debug := '';
  for index := 0 to 15 do
    debug := debug + BoolToStr(fCPU.Key[index],'1','.');
  AppLog.Debug(Format('TMachineChip8.CheckInput, key $%.2x, keys %s', [thisKey, debug]));
  {$endif}
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


{ LOAD / SAVE MACHINE STATE TO FILE }

procedure TMachineChip8.LoadFromFile(FileName: string);
var
  LoadStream: TMemoryStream;
begin
  LoadStream := TMemoryStream.Create;
  try
    LoadStream.LoadFromFile(FileName);  // Sets Position = 0
    // 4K memory
    // 16B stack
    // Registers V0-VF, I, SP, PC
    // Display buffer
  finally
    LoadStream.Free;
  end;
end;


procedure TMachineChip8.SaveToFile(FileName: string; FullSystem: boolean);
var
  SaveStream: TMemoryStream;
begin
  SaveStream := TMemoryStream.Create;
  try
    // 4K memory
    // 16B stack
    // Registers V0-VF, I, SP, PC
    // Display buffer
    SaveStream.SaveToFile(FileName);
  finally
    SaveStream.Free;
  end;
end;


initialization
  MachineFactory.RegisterMachine('CHIP-8', TMachineChip8);


end.
