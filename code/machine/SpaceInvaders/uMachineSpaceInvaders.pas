{ ==============================================================================

  SPACE INVADERS HARDWARE EMULATION

    Space Invaders was created by Tomohiro Nishikado and released in 1978.
    It was manufactured and sold by Taito in Japan, and licensed to Midway in
    the USA. In addition to the 8080 CPU, Nishikado used some custom hardware
    to speed up game play.

    The basic specification was:
                '  - Intel 8080 running at 2 MHz' + CRLF +
                '  - Video screen was vertical ' + IntToStr(fInfo.ScreenWidthPx) + 'x' + IntToStr(fInfo.ScreenHeightPx) + ' pixels' + CRLF +
                '    Colours were simulated with a plastic transparent overlay

    - Intel 8080 CPU  running at 2MHz
    - Video screen was vertical 256(x) x 224(y). Colours were simulated with
      a plastic transparent overlay and a background picture
    - Sound provided by SN76477 and samples
    - Memory map:

        ROM (8K):
        $0000-$07ff    2K invaders.h
        $0800-$0fff    2K invaders.g
        $1000-$17ff    2K invaders.f
        $1800-$1fff    2K invaders.e

        RAM (8K):
        $2000-$23ff    1K Work RAM
        $2400-$3fff    7K Video RAM
        $4000-         RAM mirror (not reflected in this emulator)

    Based on information from around the internet, including:
    - Emulator101 tutorial <http://emulator101.com/welcome.html>
    - Testing 8080 emulation:
      <http://brainwagon.org/2011/09/08/more-on-my-8080-emulator/> and
      <http://www.emulator101.com/full-8080-emulation.html>
    - Space Invaders original code
      <http://computerarcheology.com/Arcade/SpaceInvaders/Code.html>
    - Intel 8080 Microcomputer Systems Users Manual (1975) by Intel
    - 8080/Z80 Assembly Language: Techniques For Improved Programming (1981)
      by Alan R. Miller


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

unit uMachineSpaceInvaders;

{$mode objfpc}{$H+}
{$R-}

interface

uses
  LCLIntf, LCLType, Forms, Classes, SysUtils, ExtCtrls, Graphics, Dialogs,
  //
  uMachineBase, uCpuBase, uCpuTypes, uCpu8080, uFilesMgr, uMemoryMgr, uGfxMgr,
  uSoundMgr, uPrefsSpaceInvaders, uCommon, SDL2;

const
  COIN_0   = $01;
  P2_START = $02;
  P1_START = $04;
  P1_FIRE  = $10;
  P1_LEFT  = $20;
  P1_RIGHT = $40;

type

  { TMachineSpaceInvaders }

  TMachineSpaceInvaders = class(TMachineBase)
  private
    fCPU: TCpu8080;
    VideoBuffer: array of byte;         // Video buffer in 'normal' pixels 
    ScreenImage: TBitmap;
    SoundMgr: TSoundMgr;
    SOUND_Coin: integer;                // Handles for sound samples...
    SOUND_BaseHit: integer;
    SOUND_InvaderHit: integer;
    SOUND_Shot: integer;
    SOUND_Ufo: integer;
    SOUND_UfoHit: integer;
    SOUND_Walk1: integer;
    SOUND_Walk2: integer;
    SOUND_Walk3: integer;
    SOUND_Walk4: integer;
    UfoActive: boolean;
    //
    Port1in: byte;                      // Various I/O ports
    Port2in: byte;
    Port2out: byte;
    Port3out: byte;
    Port4lo: byte;
    Port4hi: byte;
    Port5out: byte;
    //
    LastPC: word;
    procedure DoConfigChange(Sender: TObject; ChangedItem: integer);
    procedure CheckInput;
    procedure WriteVRAM(Addr: word; Value: byte);
    procedure PortRead(Sender: TObject; Port: byte; var Value: byte);
    procedure PortWrite(Sender: TObject; Port: byte; Value: byte);     
    procedure SetMachineInfo;        
    procedure Create8080Cpu;
    procedure CreateMemoryManager;
    procedure CreateScreen;
    procedure CreateSoundManager;
  protected
    SIPrefs: TSIPrefsFrame;
    function  GetCPU: TCpuBase; override;
    function  GetDescription: string; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    //
    procedure Reset; override;
    procedure RunForOneFrame; override;
    procedure Step; override;
    procedure ScreenRefresh; override;  
    procedure SaveToFile(strm: TStream);
    procedure LoadFromFile(FileName: string);
    //
    property CPU: TCpuBase read GetCPU;
end;

const
  SI_PALETTE_COLOUR: array[0..3] of TGfxColour // Black, white, red, green
              = ($000000FF, $FFFFFFFF, $FF0000FF, $00FF00FF);
  SI_PALETTE_BW: array[0..3] of TGfxColour // Black, white, white, white
              = ($000000FF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF);
  NUM_COLOURS = 4;


implementation

{ CREATE }

constructor TMachineSpaceInvaders.Create;
begin
  // fConfigFrame referenced by PreferencesForm for config when SI selected
  fConfigFrame := TSIPrefsFrame.Create(nil);
  fConfigFrame.Init;                    // Get INI settings required below
  fConfigFrame.OnChange := @DoConfigChange;
  SIPrefs := fConfigFrame as TSIPrefsFrame;

  SetMachineInfo;
  CreateMemoryManager;
  Create8080Cpu;
  CreateScreen;
  CreateSoundManager;

  fFPS := 60;                           // Video frame rate 60 per second
  CyclesToGo := 0;
end;


procedure TMachineSpaceInvaders.SetMachineInfo;
begin
  fInfo.Year := 1978;
  fInfo.CpuFreqKhz := 2000;
  fInfo.ScreenWidthPx := 224;
  fInfo.ScreenHeightPx := 256;
  fInfo.ScaleModifier := 1;
  fInfo.MachineDefsFileName := 'SpaceInvadersDefs.asm';
  fInfo.MemoryButtons := '';
  fInfo.State := msStopped;
  fInfo.HasCodeToExecute := False;      // Until ROMS loaded without error below
end;


procedure TMachineSpaceInvaders.CreateMemoryManager;
var
  fFileMgr: TFileMgr;
  AllOK: boolean;
begin
  fMemoryMgr := TMemoryMgr.Create(MEM_SIZE_16K);
  fMemoryMgr.AddRead($0000, $1fff, nil, '8K ROM');
  fMemoryMgr.AddRead($2000, $23ff, nil, '1K Work RAM');
  fMemoryMgr.AddRead($2400, $3fff, nil, '7K Video RAM');
  // No write to ROM area, so omitted here
  fMemoryMgr.AddWrite($2000, $23ff, nil, 'Work RAM');
  fMemoryMgr.AddWrite($2400, $3fff, @WriteVRAM, 'Write to Video RAM');

  fFileMgr := TFileMgr.Create;
  AllOK := True;
  try
    AllOK := AllOK and fFileMgr.LoadROM('invaders.h', @fMemoryMgr.Memory[$0000], $800, $734f5ad8);
    AllOK := AllOK and fFileMgr.LoadROM('invaders.g', @fMemoryMgr.Memory[$0800], $800, $6bfaca4a);
    AllOK := AllOK and fFileMgr.LoadROM('invaders.f', @fMemoryMgr.Memory[$1000], $800, $0ccead96);
    AllOK := AllOK and fFileMgr.LoadROM('invaders.e', @fMemoryMgr.Memory[$1800], $800, $14e538b0);
    fInfo.HasCodeToExecute := AllOK;
  finally
    fFileMgr.Free;
  end;
end;


procedure TMachineSpaceInvaders.Create8080Cpu;
begin
  case (SIPrefs.Asm8080Format) of
    0: fCPU := TCpu8080.Create(ct8080asmO);
    1: fCPU := TCpu8080.Create(ct8080asmZ);
  end;
  fCPU.OnRead := @fMemoryMgr.MemReadHandler;
  fCPU.OnWrite := @fMemoryMgr.MemWriteHandler;
  fCpu.OnReadPort:= @PortRead;
  fCpu.OnWritePort:= @PortWrite;
  fCPU.Reset;
end;


procedure TMachineSpaceInvaders.CreateScreen;
begin
  Gfx := TGfxManager.Create(NUM_COLOURS); // Create a display
  Gfx.SetWindowSize(fInfo.ScreenWidthPx, fInfo.ScreenHeightPx);
  TexIdxScreen := Gfx.GetTexture(fInfo.ScreenWidthPx, fInfo.ScreenHeightPx);
  SetLength(VideoBuffer, fInfo.ScreenWidthPx * fInfo.ScreenHeightPx);
end;


procedure TMachineSpaceInvaders.CreateSoundManager;
begin
  SoundMgr := TSoundMgr.Create;         // Create mgr and load sounds
  SOUND_Coin       := SoundMgr.Add('coin.wav');
  SOUND_BaseHit    := SoundMgr.Add('basehit.wav');
  SOUND_InvaderHit := SoundMgr.Add('invhit.wav');
  SOUND_Shot       := SoundMgr.Add('shot.wav');
  SOUND_Ufo        := SoundMgr.Add('ufo.wav');
  SOUND_UfoHit     := SoundMgr.Add('ufohit.wav');
  SOUND_Walk1      := SoundMgr.Add('walk1.wav');
  SOUND_Walk2      := SoundMgr.Add('walk2.wav');
  SOUND_Walk3      := SoundMgr.Add('walk3.wav');
  SOUND_Walk4      := SoundMgr.Add('walk4.wav');
  UfoActive := False;
end;


{ DESTROY }

destructor TMachineSpaceInvaders.Destroy;
begin
  SoundMgr.Free;
  ScreenImage.Free;
  Gfx.Free;
  fCPU.Free;
  fMemoryMgr.Free;
  inherited;
end;


{ GET CPU }

function TMachineSpaceInvaders.GetCPU: TCpuBase;
begin
  Result := TCpuBase(fCPU);
end;


{ GET DESCRIPTION }

function TMachineSpaceInvaders.GetDescription: string;
begin
  Result := 'SPACE INVADERS' + CRLF + CRLF +
            'Space Invaders was created by Tomohiro Nishikado and released in ' +
            '1978. It was manufactured and sold by Taito in Japan, and licensed ' +
            'to Midway in the USA. In addition to the 8080 CPU, Nishikado used ' +
            'some custom hardware to speed up game play.' + CRLF + CRLF +
            'The basic specification was:' + CRLF +
            '  - Intel 8080 running at 2 MHz' + CRLF +
            '  - Video screen was vertical ' + IntToStr(fInfo.ScreenWidthPx) + 'x' + IntToStr(fInfo.ScreenHeightPx) + ' pixels' + CRLF +
            '    Colours were simulated with a plastic transparent overlay' + CRLF + CRLF +
            fMemoryMgr.Description + CRLF + CRLF +
            'Keys for this emulation:' + CRLF +
            '  Left/Right, SpaceBar, etc';
end;


{ RUNNING FOR ONE FRAME }

procedure TMachineSpaceInvaders.RunForOneFrame;
var
  TimeBetweenFramesMS, LastTime, SleepMS: integer;
  Loop: integer;
  CyclesPerHalfFrame: integer;
  IsBrkpt: boolean;
begin
  TimeBetweenFramesMS := 1000 div fFPS;
  LastTime := DateTimeToTimeStamp(Now).Time;

  CyclesPerHalfFrame := trunc((fInfo.CpuFreqKhz * 1000) / fFPS) div 2;

  for Loop := 1 to 2 do                 // Run in to half frames
    begin
      Inc(CyclesToGo, CyclesPerHalfFrame); // Accomodate any residual cycles
      // fInfo.State is set to msRunning in MainForm before call here
      while ((fInfo.State = msRunning) and (CyclesToGo > 0)) do
        begin
          // If PC same as last time, stopped on breakpoint so skip check and run this time
          if Assigned(fBkptHandler) and (fCPU.PC <> LastPC) then
            begin
              LastPC := fCPU.PC;
              fBkptHandler(fCPU.PC, IsBrkpt); // Check for Breakpoints
              if (IsBrkpt) then
                begin
                  fInfo.State := msStoppedOnBrkpt;
                  Break;
                end;
            end;

          Dec(CyclesToGo, fCPU.ExecuteInstruction);
        end;
      if (not IsBrkpt) then
        fCPU.Interrupt(Loop);
    end;

  if (fInfo.State = msRunning) then     // Do not do next if stopping
    begin
      CheckInput;                       // Check keyboard
      ScreenRefresh;                    // Update video screen
      Application.ProcessMessages;

      if (not fMaxSpeed) then
        begin
          // ... then sleep until frame period has expired, if required
          SleepMS := TimeBetweenFramesMS - (DateTimeToTimeStamp(Now).Time - LastTime);
          if (SleepMS > 0) then
            Sleep(SleepMS);
        end;
    end;
end;


{ STEP }

procedure TMachineSpaceInvaders.Step;
begin
  fCPU.ExecuteInstruction;
  ScreenRefresh;
end;


{ RESET }

procedure TMachineSpaceInvaders.Reset;
begin
  fCPU.Reset;
  Port1in := 0;
  Port2in := 0;                         // Default 3 bases
  Port2out := 0;
  Port3out := 0;
  Port4lo := 0;
  Port4hi := 0;
  Port5out := 0;
end;


{ SCREEN REFRESH }

{ Translation from hardware memory format to an internal X,Y buffer is done
  in the MemWrite procedure. Here it is only necessary to copy the pixels
  from that buffer to the display, setting scanline colours as we go }

procedure TMachineSpaceInvaders.ScreenRefresh;
var
  VideoPtr, x, y: integer;
  Colour: TGfxColour;
begin
  if (fInfo.State <> msRunning) then    // Do nothing if stopping
    Exit;

  if (SIPrefs.ColourFilters) then
    // Set colour of scanlines as appropriate to the filters that were
    // applied over original Space Invaders white pixels on black screen
    Gfx.Palette := SI_PALETTE_COLOUR
  else
    // Set screen colours to plain old black & white
    Gfx.Palette := SI_PALETTE_BW;

  VideoPtr := 0;
  for y := 0 to (fInfo.ScreenHeightPx - 1) do
    begin
      Colour := Gfx.Palette[1];         // White
      if ((y >= 32) and (y < 64)) then
        Colour := Gfx.Palette[2];       // Red (or white in B&W mode)
      if ((y >= 184) and (y < 240)) then
        Colour := Gfx.Palette[3];       // Green (or white in B&W mode)

      for x := 0 to (fInfo.ScreenWidthPx - 1) do
        begin
          // Cycle through each pixel on a row, setting pixel colour
          // as appropriate, or to background colour black
          if (VideoBuffer[VideoPtr] = 1)
            then Gfx.SetPixel(TexIdxScreen, x, y, Colour)
          else
            Gfx.SetPixel(TexIdxScreen, x, y, Gfx.Palette[0]); // Black
          Inc(VideoPtr);
        end;
    end;

  Gfx.ShowPixels(TexIdxScreen);         // Copy pixels to rendered screen
end;


{ CHECK INPUT }

{ Checks for any user input; 5=coin, left/right movement, spacebar=fire, etc }

procedure TMachineSpaceInvaders.CheckInput;
var
  SdlEvent: PSDL_Event;
begin
  New(SdlEvent);

  while (SDL_PollEvent(SdlEvent) = 1) do
    begin
      case (SdlEvent^.type_) of

        SDL_KEYDOWN: case (SdlEvent^.key.keysym.sym) of
                       SDLK_5:     Port1in := Port1in and (not COIN_0);
                       SDLK_1:     Port1in := Port1in or P1_START;
                       SDLK_LEFT:  Port1in := Port1in or P1_LEFT;
                       SDLK_RIGHT: Port1in := Port1in or P1_RIGHT;
                       SDLK_SPACE: Port1in := Port1in or P1_FIRE;
                     end;

        SDL_KEYUP: case (SdlEvent^.key.keysym.sym) of
                     SDLK_5:     begin
                                   Port1in := Port1in or COIN_0;
                                   SoundMgr.Play(1, SOUND_Coin);
                                 end;
                     SDLK_1:     Port1in := Port1in and (not P1_START);
                     SDLK_LEFT:  Port1in := Port1in and (not P1_LEFT);
                     SDLK_RIGHT: Port1in := Port1in and (not P1_RIGHT);
                     SDLK_SPACE: Port1in := Port1in and (not P1_FIRE);
                   end;
      end;
    end;
  Dispose(SdlEvent);
end;


{ WRITE VIDEO RAM }

{ Special handling for write to Video RAM. Do conversion from hardware video
  version (rotated screen where consecutive bits correspond to vertical pixels)
  by copying pixels into a separate 'normal' X,Y oriented screen buffer.
  This will be used later, in ScreenRefresh, to write the buffer to the screen }

procedure TMachineSpaceInvaders.WriteVRAM(Addr: word; Value: byte);
var
  y, bit: integer;
begin
  fMemoryMgr.Memory[Addr] := Value;     // Normal write to video RAM
  //
  Addr := Addr - $2400;                 // Convert to 'standard' X,Y pixels
  y := trunc(((255 - ((Addr and $1F) * 8)) * 224) + (Addr / 32));
  for bit := 0 to 7 do
    begin
      VideoBuffer[y] := Value and $1;
      Value := Value shr 1;
      y := y - 224;
    end;
end;


{ PORT READ }

{ Port 1 maps the keys for space invaders
    Bit 0 = coin slot (0 when active)
    Bit 1 = two players start button
    Bit 2 = one player start button
    Bit 4 = player 1 shoot button
    Bit 5 = player 1 joystick left
    Bit 6 = player 1 joystick right

  Port 2 maps player 2 controls and dip switches
    Bit 0,1 = number of lives ($00=3, $01=4, $10=5, $11=6)
    Bit 2   = tilt 'button'
    Bit 3   = mode (1=easy, 0=hard); bonus life at 1=1000, 0=1500
    Bit 4   = player 2 shoot button
    Bit 5   = player 2 joystick left
    Bit 6   = player 2 joystick right
    Bit 7   = show or hide coin info (1=off, 0=on)

  Port 3 returns the hardware shift register result }

procedure TMachineSpaceInvaders.PortRead(Sender: TObject; Port: byte; var Value: byte);
begin
  case (Port) of
    1: begin
         Value := Port1in;
         Port1in := Port1in and $FE;    // After read, reset Coin value
       end;
    2: begin
         Value := (Port2in and $04)
                  or (Port1in and $70)  // Player1 keys used for player2
                  or (SIPrefs.NumberBases)
                  or (SIPrefs.BonusPoints shl 3);
       end;
    3: begin
         Value := ((((Port4hi << 8) or Port4lo) shl Port2out) shr 8) and $FF;
       end;
  end;
end;


{ PORT WRITE }

procedure TMachineSpaceInvaders.PortWrite(Sender: TObject; Port: byte; Value: byte);
begin
  case (Port) of
    2: begin
         // Write the shift offset for the hardware shift register
         Port2out := Value;
       end;
    3: begin
         // Ufo sound loops when activated
         if ( ((Value and $01) > 0) and ((Port3out and $01) = 0) ) then
           SoundMgr.Play(2, SOUND_Ufo, SOUND_LoopForever);
         if ( ((Value and $01) = 0) and ((Port3out and $01) > 0) ) then
           SoundMgr.Halt(2);
         // If a sound has been activated, and it is not already running, then
         // play the relevant sound
         if ( ((Value and $02) > 0) and ((Port3out and $02) = 0) ) then
           SoundMgr.Play(1, SOUND_Shot);
         if ( ((Value and $04) > 0) and ((Port3out and $04) = 0) ) then
           SoundMgr.Play(1, SOUND_BaseHit);
         if ( ((Value and $08) > 0) and ((Port3out and $08) = 0) ) then
           SoundMgr.Play(1, SOUND_InvaderHit);
         Port3out := Value;
       end;
    4: begin
         // Fill hardware shift register (shifts LS byte->MS byte, writes LS byte)
         Port4lo := Port4hi;
         Port4hi := Value;
       end;
    5: begin
      // If a sound has been activated, and it is not already running, then
      // play the relevant sound
         if ( ((Value and $01) > 0) and ((Port5out and $01) = 0) ) then
           SoundMgr.Play(1, SOUND_Walk1);
         if ( ((Value and $02) > 0) and ((Port5out and $02) = 0) ) then
           SoundMgr.Play(1, SOUND_Walk2);
         if ( ((Value and $04) > 0) and ((Port5out and $04) = 0) ) then
           SoundMgr.Play(1, SOUND_Walk3);
         if ( ((Value and $08) > 0) and ((Port5out and $08) = 0) ) then
           SoundMgr.Play(1, SOUND_Walk4);
         if ( ((Value and $10) > 0) and ((Port5out and $10) = 0) ) then
           SoundMgr.Play(1, SOUND_UfoHit);
         Port5out := Value;
       end;
  end;
end;


{ MICROTAN CONFIG CHANGE }

procedure TMachineSpaceInvaders.DoConfigChange(Sender: TObject; ChangedItem: integer);
begin
  case ChangedItem of
    0: begin                            // All
       end;
    1: {nothing};                       // Colour
    2: {nothing};                       // Bases
    3: {nothing};                       // Bonuses
    4: begin                            // Asm Format, free and rebuild CPU
         (*
         fCpu.Free;
         Create8080Cpu;
         *)
       end;
  end;
end;


{ LOAD / SAVE TO FILE }

procedure TMachineSpaceInvaders.LoadFromFile(FileName: string);
begin
  //
end;


procedure TMachineSpaceInvaders.SaveToFile(strm: TStream);
begin
  //
end;


initialization
  MachineFactory.RegisterMachine('Space Invaders', TMachineSpaceInvaders);


end.
