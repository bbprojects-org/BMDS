{ ==============================================================================

  MICROTAN 65 BOARD

    This class provides an emulation of the Microtan 65 basic board, produced
    by Tangerine Systems from 1979

    CPU:        NMOS 6502 CPU running at 750kHz
    Video:      16 rows of 32 characters (64 x 64 chunky graphics)
    Memory map: As below

      RAM (1K):
      $0000-$00ff    256B Zero Page
      $0100-$01ff    256B Stack
      $0200-$03ff    512B Screen RAM

      ROM (1K):
      $fc00-$ffff    1K Tanbug monitor program (mirrored at lower addresses)

      Special I/O via memory accesses:
      $bff0 read     Turn graphics on
      $bff0 write    Reset keyboard interrupt flag
      $bff1 write    Set delayed NMI 8-cycle countdown
      $bff2 write    Strobe keypad (not used here since have ASCII keyboard)
      $bff3 read     Read keypad / keyboard input
      $bff3 write    Turn graphics off

    Much of the code here is based on a mixture of ideas / code reworked
    into Pascal from the following:
    1. Fabrice Frances' Microtan Java emulator
    <http://www.ifrance.com/oric/microtan/microtan_java.html>
    ** Unfortunately this link is no longer valid **
    2. Daryl Rictor's 65C02 emulator written in C++
    <http://sbc.rictor.org/simulator.html>


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

  ============================================================================= }

{ TODO : uMachineMicrotan -> SaveToFile routine }

{ TODO : uMachineMicrotan -> have properties for foreground/background colours
                             which rebuild chunky characters / character set,
                             without need for reload of machine. And for
                             selected ROM too}

{ TODO : uMachineMicrotan -> have fully expanded machine with 64K }

unit uMachineMicrotan;

{$mode objfpc} {$H+}

interface

uses
  LCLIntf, LCLType, Forms, Classes, SysUtils, ExtCtrls, Graphics, Dialogs,
  //
  uMachineBase, uCpuBase, uCpu6502, uDefs6502, uFilesMgr, uMemoryMgr,
  uConfigMicrotan, uCommon, uGfxMgr, SDL2;

type
  TGraphicsBits = array[$200..$3FF] of boolean;

  { TMachineMicrotan }

  TMachineMicrotan = class(TMachineBase)
  private
    fCPU: TCpu6502;                     // 6502 CPU object
    KeyboardPort: byte;                 // Read keyboard port
    KeyboardFlipFlop: boolean;          // Keyboard flip-flop set by interrupt

    TexIdxChunkyChars: integer;         // Texture index for chunky characters
    TexIdxCharacters: integer;          // Texture index for standard characters
    GraphicsSwitch: boolean;            // Graphic switch state flag
    GraphicsBits: TGraphicsBits;        // ... and state for all character elements
    DelayedNmiCounter: integer;         // Used to support M65 delayed NMI processing
                            
    procedure DoConfigChange(idx: integer);
    procedure BuildTextures;
    procedure CheckInput;
    procedure GetROM;
    procedure SetMachineInfo;
    procedure CreateMemoryManager;
    procedure Create6502Cpu;
    procedure CreateScreenAndGraphics;
    procedure BuildChunkyCharacters;
    procedure BuildCharacterSet;
    function  ReadBFFx(Addr: word): byte;
    procedure WriteBFFx(Addr: word; Value: byte);
    procedure WriteVRAM(Addr: word; Value: byte);
  protected
    function GetCPU: TCpuBase; override;
    function GetDescription: string; override;
    procedure SetScreenSize(aValue: TPoint); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    //
    procedure Reset; override;
    procedure RunForOneFrame; override;
    procedure Step; override;   
    procedure ScreenRefresh; override;
    procedure SaveToFile(strm: TStream);
    procedure LoadFromFile(Filename: string);

    property CPU: TCpuBase read GetCPU;
end;

const
  MICROTAN_PALETTE_G: array[0..1] of TGfxColour
                      = ($000000FF, $00FF00FF); // Black, green
  MICROTAN_PALETTE_A: array[0..1] of TGfxColour
                      = ($000000FF, $FFBF00FF); // Black, amber
  MICROTAN_PALETTE_W: array[0..1] of TGfxColour
                      = ($000000FF, $FFFFFFFF); // Black, white


implementation

{ CREATE }

constructor TMachineMicrotan.Create;
begin
  fConfigFrame := TConfigMicrotan.Create(nil);
  fConfigFrame.Init(@DoConfigChange);   // Initialise callback, get config settings

  SetMachineInfo;
  CreateMemoryManager;
  Create6502Cpu;
  CreateScreenAndGraphics;

  FPS := 50;
  CyclesToGo := 0;
  KeyboardPort := 0;
  KeyboardFlipFlop := False;
  DelayedNmiCounter := 0;
end;


{ SET MACHINE INFO  }

procedure TMachineMicrotan.SetMachineInfo;
begin
  fInfo.Name := MACHINES[MACHINE_M65].Name;
  fInfo.Year := 1979;
  fInfo.CpuType := ctR6502;
  fInfo.CpuFreqKhz := 750;
  fInfo.ScreenWidthPx := 256;
  fInfo.ScreenHeightPx := 256;
  fInfo.MachineDefsFilename := 'MicrotanDefs.asm';
  fInfo.State := msStopped;
end;


{ CREATE MEMORY MANAGER }

procedure TMachineMicrotan.CreateMemoryManager;
var
  idx: integer;
begin
  fMemoryMgr := TMemoryMgr.Create(0, MEM_SIZE_64K);
  GetROM;
  case ((fConfigFrame as TConfigMicrotan).RomIndex) of

    // Basic machine, 1K RAM, 1K TANBUG
    0: begin
         for idx := 0 to $3FF do
           begin
             // Microtan had limited address decoding, so Tanbug repeated at 1K intervals
             fMemoryMgr.Memory[$F000 + idx] := fMemoryMgr.Memory[$FC00 + idx];
             fMemoryMgr.Memory[$F400 + idx] := fMemoryMgr.Memory[$FC00 + idx];
             fMemoryMgr.Memory[$F800 + idx] := fMemoryMgr.Memory[$FC00 + idx];
           end;
         // Set memory read/write accesses, 1K RAM, 1K ROM. No write to ROM area
         fMemoryMgr.AddRead($0000, $03FF, nil, '');
         // 512B work RAM + stack, 512B video RAM
         fMemoryMgr.AddRead($FC00, $FFFF, nil, '1K Tanbug ROM');
         fMemoryMgr.AddWrite($0000, $01FF, nil, 'RAM');
       end;

    // Expanded machine, 8K RAM, 16K ROM
    1: begin
         // Set memory read/write accesses, 8K RAM, 16K ROM. No write to ROM area
         fMemoryMgr.AddRead($0000, $1FFF, nil, '8K RAM');
         fMemoryMgr.AddRead($C000, $FFFF, nil, '16K ROM, XBUG + BASIC');
         fMemoryMgr.AddWrite($0000, $01FF, nil, 'Standard RAM write, lo');
         fMemoryMgr.AddWrite($0400, $1FFF, nil, 'Standard RAM write, hi');
       end;
  end;
  // Common accesses for I/O addresses, and video RAM
  fMemoryMgr.AddRead($BFF0, $BFF3, @ReadBFFx, 'Graphics on / Read keyboard');
  fMemoryMgr.AddWrite($0200, $03FF, @WriteVRAM, 'Write to Video RAM');
  fMemoryMgr.AddWrite($BFF0, $BFF3, @WriteBFFx, 'Reset keyboard, Delayed NMI, Graphics off');
end;


procedure TMachineMicrotan.GetROM;
var
  fFileMgr: TFileMgr;
begin
  fFileMgr := TFileMgr.Create;
  try
    case ((fConfigFrame as TConfigMicrotan).RomIndex) of
      0: begin
           fInfo.HasCodeToExecute := fFileMgr.LoadROM('tanbug.rom', @fMemoryMgr.Memory[$FC00], $400, $C1E45F1A);
           fInfo.MemoryButtons := 'Stack=01FF,ROM=F800';
         end;
      1: begin
           fInfo.HasCodeToExecute := fFileMgr.LoadROM('microtan.rom', @fMemoryMgr.Memory[$C000], $4000, $0E4CD26E);
           fInfo.MemoryButtons := 'Stack=01FF,ROM=C000';
         end;
    end;
  finally  
    fFileMgr.Free;
  end;
end;


{ CREATE 6502 CPU }

procedure TMachineMicrotan.Create6502Cpu;
begin
  fCPU := TCpu6502.Create;
  fCPU.OnRead := @fMemoryMgr.MemReadHandler;
  fCPU.OnWrite := @fMemoryMgr.MemWriteHandler;
  fCPU.Reset;
end;


{ CREATE SCREEN AND GRAPHICS }

procedure TMachineMicrotan.CreateScreenAndGraphics;
var
  idx: integer;
begin
  Gfx := TGfxManager.Create;
  Gfx.SetWindowSize(fInfo.ScreenWidthPx, fInfo.ScreenHeightPx);
  TexIdxScreen := Gfx.GetTexture(fInfo.ScreenWidthPx, fInfo.ScreenHeightPx);
  TexIdxChunkyChars := 0;               // No textures assigned yet
  TexIdxCharacters := 0;                                                                               
  GraphicsSwitch := False;
  for idx := $200 to $3FF do            // Randomize graphics bits
    GraphicsBits[idx] := ((Random(256) and 1) = 0);
  BuildTextures;                        // Build bitmaps for graphics chars and normal font characters
end;


{ DESTROY }

destructor TMachineMicrotan.Destroy;
begin
  Gfx.Free;
  fCPU.Free;
  fMemoryMgr.Free;
  fConfigFrame.Free;
  inherited;
end;


{ GET CPU }

function TMachineMicrotan.GetCPU: TCpuBase;
begin
  Result := TCpuBase(fCPU);
end;


{ GET DESCRIPTION }

function TMachineMicrotan.GetDescription: string;
begin
  Result := 'MICROTAN 65' + CRLF + CRLF +
            'The Tangerine Microtan 65 was a 6502 based single board microcomputer, first sold in 1979, with the following specification:' + CRLF + CRLF +
            'Main Board:' + CRLF +
            '  - NMOS 6502 running at 750 kHz' + CRLF +
            '  - 1K RAM (inc display memory)' + CRLF +
            '  - 1K ROM for TANBUG monitor' + CRLF +
            '  - screen (' + IntToStr(fInfo.ScreenWidthPx) + 'x' + IntToStr(fInfo.ScreenHeightPx) + ' pixels) 16 rows of 32 characters / 64x64 block graphics' + CRLF + CRLF +
            'Expansion Board (TANEX):' + CRLF +
            '  - add-on to TANBUG called XBUG' + CRLF +
            '  - additional 7K RAM' + CRLF +
            '  - five EPROM sockets' + CRLF +
            '  - two 6522 VIAs' + CRLF +
            '  - a 6551 UART' + CRLF + CRLF +
            fMemoryMgr.Description + CRLF + CRLF +
            'Keys for this emulation:' + CRLF +
            '  Left/Right, SpaceBar, etc';
end;


{ SET SCREEN SIZE }

procedure TMachineMicrotan.SetScreenSize(aValue: TPoint);
begin
  Gfx.SetWindowSize(aValue.X, aValue.Y);
  ScreenRefresh;                        // Refresh both display buffers
  ScreenRefresh;
end;


{ RUNNING FOR ONE FRAME }

{ Run CPU for one frame's worth of cycles... then do video update, etc }

procedure TMachineMicrotan.RunForOneFrame;
var
  TimeBetweenFramesMS, LastTime, SleepMS: integer;
  CyclesPerFrame, CyclesDone: integer;   
  IsBrkpt: boolean;
begin
  SDL_StartTextInput();
  TimeBetweenFramesMS := trunc(1000 / FPS);
  LastTime := DateTimeToTimeStamp(Now).Time; // Current time

  CyclesPerFrame := trunc((fInfo.CpuFreqKhz * 1000) / fFPS);
  Inc(CyclesToGo, CyclesPerFrame); // Accomodate any residual cycles

  // fInfo.State is set to msRunning in MainForm before call
  while ((fInfo.State = msRunning) and (CyclesToGo > 0)) do
    begin
      if Assigned(fBkptHandler) then
        begin
          fBkptHandler(fCPU.PC, IsBrkpt); // Check for Breakpoints
          if (IsBrkpt) then
            begin
              fInfo.State := msStoppedOnBrkpt;
              Break;
            end;
        end;

      CyclesDone := fCPU.ExecuteInstruction;
      Dec(CyclesToGo, CyclesDone);

      if (DelayedNmiCounter <> 0) then  // If delayed NMI active, clock downwards
        begin
          Dec(DelayedNmiCounter, CyclesDone);
          if (DelayedNmiCounter <= 0) then
            fCPU.Interrupt(NMI_IDX);    // Raise NMI
        end;
    end;

  if (fInfo.State = msRunning) then     // Only do next if not stopping
    begin             
      CheckInput;                       // Check keyboard
      ScreenRefresh;
      Application.ProcessMessages;
      if (not fMaxSpeed) then           // If not running max speed, then sleep until frame period has expired
        begin
          SleepMS := TimeBetweenFramesMS - (DateTimeToTimeStamp(Now).Time - LastTime);
          if (SleepMS > 0) then
            Sleep(SleepMS);
        end;
    end;
  SDL_StopTextInput();
end;


{ STEP }

procedure TMachineMicrotan.Step;
begin
  fCPU.ExecuteInstruction;
  ScreenRefresh;
end;


{ RESET }

procedure TMachineMicrotan.Reset;
begin
  fCPU.Reset;
end;


{ CHECK INPUT }

{ Checks for any user input when machine is running }

procedure TMachineMicrotan.CheckInput;
var
  sdlEvent: PSDL_Event;
  thisKey: longint;
  validKey: boolean;
  isCTRL: boolean;
begin
  New(sdlEvent);
  while (SDL_PollEvent(sdlEvent) = 1) do
    begin
      validKey := False;
      if (sdlEvent^.type_ = SDL_KEYDOWN) then
        // Look for special keys first (non text)
        begin
          thisKey := sdlEvent^.key.keysym.sym;
          validKey := True;
          isCTRL := (SDL_GetModState and KMOD_CTRL) > 0;
          case thisKey of
            $A, $D, $1B, $7F: ;         // LF, CR, ESC, DEL ok
            // Translate keys                                         
            SDLK_DOWN: thisKey := $A;   // Down-arrow -> LF (forward)
            SDLK_UP: thiskey := $1B;    // Up-arrow -> ESC (reverse)
            SDLK_j: if isCTRL then thisKey := $A; // Ctrl-J -> LF
            SDLK_m: if isCTRL then thisKey := $D; // Ctrl-M -> CR
          else
            validKey := False;
          end;
        end;

      if (sdlEvent^.type_ = SDL_TEXTINPUT) then
        // Else check if it is actual text input
        begin
          thisKey := ord(sdlEvent^.text.text[0]);
          if (thisKey >= $61) and (thisKey <= $7A) then
            thisKey := thisKey and $DF; // Ensure letters are uppercase
          validKey := True;
        end;

      if (validKey) then
        begin
          KeyboardPort := thisKey;
          KeyboardFlipFlop := True;
          fCPU.Interrupt(IRQ_IDX);      // Raise IRQ
        end;
    end;
  Dispose(sdlEvent);
end;


{ READ $BFFx }

function TMachineMicrotan.ReadBFFx(Addr: word): byte;
begin         
  Result := $FF;
  case (Addr) of
    // Set graphic switch on, all subsequent display writes are 'graphic' chars
    $bff0: GraphicsSwitch := True;

    // Read keyboard port. Note MSB indicates whether keyboard interrupt
    $bff3: begin
             Result := KeyboardPort and $7F;
             if (KeyboardFlipFlop) then
               Result := Result or $80;
           end;
  end;
end;


{ WRITE $BFFx }

procedure TMachineMicrotan.WriteBFFx(Addr: word; Value: byte);
begin
  case (Addr) of
    // Reset keyboard interrupt flip-flop ready for next key press activation
    $bff0: begin
             KeyboardFlipFlop := False;
             fCPU.Interrupt(IRQ_IDX, False); // Clear IRQ
           end;

    // DelayedNMI executes 8 cycles after current instruction, before triggering
    // NMI. Have additional +4 here to cater for 'STA $BFF0' just executed to get
    // us here, as decrement occurs immediately after this instruction completes
    $bff1: DelayedNmiCounter := 8 + 4;

    // Set graphic switch off, i.e. text mode
    $bff3: GraphicsSwitch := False;
  end;
end;


{ WRITE VIDEO RAM }

{ Writes to screen memory, including handling of graphic switch }

procedure TMachineMicrotan.WriteVRAM(Addr: word; Value: byte);
begin
  if ((GraphicsSwitch <> GraphicsBits[Addr]) or (fMemoryMgr.Memory[Addr] <> Value)) then
    begin
      fMemoryMgr.Memory[Addr] := Value;
      GraphicsBits[Addr] := GraphicsSwitch; // Copy current state of Graphics switch
    end;
end;


{ SCREEN REFRESH }

{ Routine checks whether GraphicsBits are set and displays appropriate
  chunky graphic, otherwise it displays a standard character, by
  copying 'sprite' from relevant Chunky Graphic / Character texture }

procedure TMachineMicrotan.ScreenRefresh;
var
  Addr: word;
  x, y: byte;
  srcRect, destRect: PGfxRect;
begin
  New(srcRect);
  New(destRect);
  srcRect^.y := 0;                      // These are common to Chunky and
  srcRect^.w := 8;                      // Normal characters
  srcRect^.h := 16;
  destRect^.w := 8;
  destRect^.h := 16;

  Addr := $200;                         // Screen memory starts at $200
  for y := 0 to 15 do                   // 16 rows
    begin
      destRect^.y := y * 16;
      for x := 0 to 31 do               // 32 columns
      begin
        destRect^.x := x * 8;
        if GraphicsBits[Addr] then
          begin
            srcRect^.x := fMemoryMgr.Memory[Addr] * 8;
            Gfx.Copy(TexIdxChunkyChars, srcRect, destRect);
          end
        else
          begin
            // Need to ensure offset into texture limited to 128 characters
            srcRect^.x := (fMemoryMgr.Memory[Addr] and $7F) * 8;
            Gfx.Copy(TexIdxCharacters, srcRect, destRect);
          end;
        Inc(Addr);
      end;
    end;
  Gfx.Show;
  Dispose(destRect);
  Dispose(srcRect);
end;


{ MICROTAN CONFIG CHANGE }

procedure TMachineMicrotan.DoConfigChange(idx: integer);
begin
  case idx of
    0: ;                                // Dynamic load of ROM not allowed
    1: BuildTextures;
  end;
end;


{ BUILD TEXTURES }

procedure TMachineMicrotan.BuildTextures;
begin
  case ((fConfigFrame as TConfigMicrotan).ColourIndex) of
    0: Gfx.Palette := MICROTAN_PALETTE_G; // Green pixels on black
    1: Gfx.Palette := MICROTAN_PALETTE_A; // Amber pixels on black
    2: Gfx.Palette := MICROTAN_PALETTE_W; // White pixels on black
  end;
  BuildChunkyCharacters;                // Build bitmaps for graphics chars
  BuildCharacterSet;                    // and normal font characters
  ScreenRefresh;
end;


{ BUILD CHUNKY CHARACTERS }

{ Creates a texture and builds 'sprites' of each of 256 chunky characters
    - Each character is 2 x 4 blocks of 4x4 pixels (8 pixels wide, 16 pixels high)
    - Texture is 256*8 wide, by 16 pixels high }

procedure TMachineMicrotan.BuildChunkyCharacters;
var
  idx, x, y, bit, offset, cx, cy: integer;
  Pixel: integer;
begin
  if (TexIdxChunkyChars = 0) then
    TexIdxChunkyChars := Gfx.GetTexture(256*8, 16);

  offset := 0;
  for idx := 0 to 255 do
    begin
      for y := 0 to 3 do                // Four blocks high
        begin
          for x := 0 to 1 do            // Two blocks wide
            begin
              bit := y * 2 + x;
              if ((idx and (1 shl bit)) <> 0) then
                Pixel := 1
              else
                Pixel := 0;
              for cy := 0 to 3 do
                for cx := 0 to 3 do
                  Gfx.SetPixel(TexIdxChunkyChars, offset+(x*4)+cx, (y*4)+cy, Gfx.Palette[Pixel]);
            end;
        end;
      Inc(offset, 8);
    end;
  Gfx.UpdateTexture(TexIdxChunkyChars);
end;


{ BUILD CHARACTER SET }

{ Creates a texture and builds 'sprites' of each of 128 screen characters
    - Each character is 8 pixels wide by 16 pixels high
    - Texture is 128*8 wide, by 16 pixels high to make selection easier }

procedure TMachineMicrotan.BuildCharacterSet;
var                              
  fFileMgr: TFileMgr;
  row, col, ch, offset: integer;
  CharByte: byte;
  CharsetStream: TMemoryStream;  
  Pixel: integer;
begin
  if (TexIdxCharacters = 0) then
    TexIdxCharacters := Gfx.GetTexture(8*128, 16);

  offset := 0;
  fFileMgr := TFileMgr.Create;
  CharsetStream := TMemoryStream.Create;
  try
    fFileMgr.OpenStream(CharsetStream, 'charset.rom', 2048, $3B3C5360);
    for ch := 0 to 127 do
      begin
        for row := 0 to 15 do
          begin
            CharsetStream.Read(CharByte, 1);
            for col := 0 to 7 do
              begin
                if (CharByte and (1 shl (7 - col)) > 0 ) then
                  Pixel := 1
                else
                  Pixel := 0;
                Gfx.SetPixel(TexIdxCharacters, offset+col, row, Gfx.Palette[Pixel]);
              end;
          end;
        Inc(offset, 8);                 // Next character
      end;

  finally
    CharsetStream.Free;
    fFileMgr.Free;
  end;
  Gfx.UpdateTexture(TexIdxCharacters);
end;


{ LOAD / SAVE MACHINE STATE TO FILE }

procedure TMachineMicrotan.LoadFromFile(Filename: string);
var
  LoadStream: TMemoryStream;
  LoadPtr: PChar;
  idx, bit, ThisByte, Addr: integer;  
  tempRegs: TRegs6502;
begin
  LoadStream := TMemoryStream.Create;
  try
    LoadStream.LoadFromFile(Filename);
    LoadPtr := LoadStream.Memory;
    if (LoadStream.Size = 8263) then    // Standard file?
    begin
      for idx := 0 to $1FFF do          // Copy 8K memory image
        fMemoryMgr.Memory[idx] := byte(LoadPtr[idx]);
      for idx := 0 to 63 do             // 64 bytes chunky graphics bits
      begin
        ThisByte := byte(LoadPtr[$2000 + idx]);
        Addr := $200 + (idx * 8);
        for bit := 0 to 7 do
          GraphicsBits[Addr + bit] := ((ThisByte and (1 shl bit)) > 0);
        ScreenRefresh;                  // Refresh display
      end;
      tempRegs.PC := byte(LoadPtr[$2040]) + (byte(LoadPtr[$2041]) shl 8);
      tempRegs.PSW := byte(LoadPtr[$2042]);
      tempRegs.A := byte(LoadPtr[$2043]);
      tempRegs.X := byte(LoadPtr[$2044]);
      tempRegs.Y := byte(LoadPtr[$2045]);
      tempRegs.SP := byte(LoadPtr[$2046]);
      fCPU.Regs := tempRegs;
    end;
  finally
    LoadStream.Free;
  end;
end;


procedure TMachineMicrotan.SaveToFile(strm: TStream);
begin
  //
end;


end.
