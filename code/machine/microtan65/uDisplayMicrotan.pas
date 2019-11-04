{ ==============================================================================

  MICROTAN DISPLAY

  Provides the machine specific display for the Microtan 65

  =============================================================================}

unit uDisplayMicrotan;

{$mode objfpc}
{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows,{$IFEND}      // Required for RT_RCDATA definition
  Classes, SysUtils, Graphics, Dialogs, lResources,
  //
  uMachineBase, uGfxEngine, uCommon;

type
  TGraphicsBits = array[$200..$3FF] of boolean;

  { TDisplayMicrotan }

  TDisplayMicrotan = class
  private
    fMachine: TMachineBase;

    fForeColour: TGfxColour;
    fBackColour: TGfxColour;
    fGraphicsSwitch: boolean;
    fGraphicsBits: TGraphicsBits;

    Gfx: TGfxEngine;
    TexIdxScreen: integer;
    TexIdxChunkyChars: integer;
    TexIdxCharacters: integer;

    procedure BuildChunkyCharacters;
    procedure BuildCharacterSet;
    function GetPosition: TPoint;
    function GetSize: TPoint;
    procedure SetCaption(AValue: string);
    procedure SetPosition(AValue: TPoint);
    procedure SetSize(AValue: TPoint);
  protected
    procedure SetForeColour(const Value: TGfxColour);
    procedure SetBackColour(const Value: TGfxColour);
  public
    constructor Create(Machine: TMachineBase);
    destructor  Destroy; override;
    procedure Refresh;
    procedure WriteScreen(Addr: word; Value: byte);
    property  ForeColour: TGfxColour read fForeColour write SetForeColour;
    property  BackColour: TGfxColour read fBackColour write SetBackColour;
    property  Position: TPoint read GetPosition write SetPosition;
    property  Size: TPoint read GetSize write SetSize;
    property  Caption: string write SetCaption;

    property  GraphicsSwitch: boolean write fGraphicsSwitch;
    property  GraphicsBits: TGraphicsBits read fGraphicsBits;
end;


implementation

{ TODO : Move this into main machine unit }

{ CREATE MICROTAN DISPLAY

  Initialise the 256x256 display, and pre-cache chunky graphics and character
  set in textures
}
constructor TDisplayMicrotan.Create(Machine: TMachineBase);
var
  idx: integer;
begin
  fMachine := Machine;
  Gfx := TGfxEngine.Create;
  { TODO : Need to set window caption too }
  { TODO : Can this window be part of main window? }
  Gfx.SetWindowSize(fMachine.Info.ScreenWidth, fMachine.Info.ScreenHeight);

  fForeColour := $00FF00FF;             // Default colours, green on black
  fBackColour := $000000FF;

  TexIdxScreen := Gfx.GetTexture(fMachine.Info.ScreenWidth, fMachine.Info.ScreenHeight);
  TexIdxChunkyChars := 0;               // No textures assigned yet
  TexIdxCharacters := 0;
  BuildChunkyCharacters;                // Build textures for characters, etc
  BuildCharacterSet;

  fGraphicsSwitch := FALSE;
  for idx := $200 to $3FF do            // Randomize graphics bits
    GraphicsBits[idx] := ((Random(256) and 1) = 0);

  Refresh;                              // and refresh screen
end;


destructor TDisplayMicrotan.Destroy;
begin
  Gfx.Free;
  inherited;
end;


{ REFRESH SCREEN DISPLAY

  Routine checks whether GraphicsBits are set and displays appropriate
  chunky graphic, otherwise it displays a standard character, by
  copying "sprite" from relevant Chunky Graphic / Character texture
}
procedure TDisplayMicrotan.Refresh;
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
      destRect^.y := (y * 16);
      for x := 0 to 31 do               // 32 columns
        begin
          destRect^.x := (x * 8);
          // If GraphicsBits then display a chunky graphics, else
          // show a normal character
          if (GraphicsBits[Addr]) then
            begin
              srcRect^.x := fMachine.Memory[Addr] * 8;
              Gfx.Copy(TexIdxChunkyChars, srcRect, destRect);
            end
          else
            begin
              // Need to ensure offset into texture limited to 128 characters
              srcRect^.x := (fMachine.Memory[Addr] and $7F) * 8;
              Gfx.Copy(TexIdxCharacters, srcRect, destRect);
            end;
          Inc(Addr);
        end;
    end;
  Gfx.Show;
  Dispose(destRect);
  Dispose(srcRect);
end;


{ WRITE TO SCREEN ADDRESS

  Writes to screen memory, including handling of graphic switch
    - Addr:  Memory address to write to
    - Value: Byte value to be written
}
procedure TDisplayMicrotan.WriteScreen(Addr: word; Value: byte);
begin
  fMachine.Memory[Addr] := Value;
  GraphicsBits[Addr] := fGraphicsSwitch; // Copy current state of Graphics switch
end;


{ BUILD CHUNKY CHARACTERS IN TEXTURE

  Creates a texture and builds "sprites" of each of 256 chunky characters
    - Each character is 2 x 4 blocks of 4x4 pixels (8 pixels wide, 16 pixels high)
    - Texture is 256*8 wide, by 16 pixels high to make selection easier
}
procedure TDisplayMicrotan.BuildChunkyCharacters;
var
  idx, x, y, bit, offset, cx, cy: integer;
  BitColour: TGfxColour;
begin
  if (TexIdxChunkyChars = 0) then
    TexIdxChunkyChars := Gfx.GetTexture(8*256, 16);
  offset := 0;
  for idx := 0 to 255 do
  begin
    for y := 0 to 3 do                  // Four blocks high
    begin
      for x := 0 to 1 do                // Two blocks wide
      begin
        bit := y * 2 + x;
        if ((idx and (1 shl bit)) <> 0) then
          BitColour := fForeColour      // Set colour according to whether bit
        else                            // is 1 (foreground) or 0 (background)
          BitColour := fBackColour;
        for cy := 0 to 3 do
          for cx := 0 to 3 do
            Gfx.SetPixel(TexIdxChunkyChars, offset+(x*4)+cx, (y*4)+cy, BitColour);
      end;
    end;
    Inc(offset, 8);                     // Next chunky character
  end;
  Gfx.UpdateTexture(TexIdxChunkyChars);
end;


{ BUILD CHARACTER SET IN TEXTURE

  Creates a texture and builds "sprites" of each of 128 screen characters
    - Each character is 8 pixels wide by 16 pixels high
    - Texture is 128*8 wide, by 16 pixels high to make selection easier
}
procedure TDisplayMicrotan.BuildCharacterSet;
var
  row, col, ch, bit, offset: integer;
  CharByte: byte;
  CharsetStream: TMemoryStream;
  colour: TGfxColour;
begin
  if (TexIdxCharacters = 0) then
    TexIdxCharacters := Gfx.GetTexture(8*128, 16);
  offset := 0;
  CharsetStream := TMemoryStream.Create;
  try
    { TODO : This needs to be a central routine which checks file is present, right length, CRC etc }
    CharsetStream.LoadFromFile(GlobalVars.MachineDataFolder + 'CharSet.rom');
    for ch := 0 to 127 do
    begin
      for row := 0 to 15 do
      begin
        CharsetStream.Read(CharByte, 1);
        for col := 0 to 7 do
        begin
          bit := CharByte and (1 shl (7-col));
          if (bit <> 0) then
            colour := fForeColour
          else
            colour := fBackColour;
          Gfx.SetPixel(TexIdxCharacters, offset+col, row, colour);
        end;
      end;
      Inc(offset, 8);                   // Next character
    end;
  finally
    CharsetStream.Free;
  end;
  Gfx.UpdateTexture(TexIdxCharacters);
end;


{ SET SCREEN COLOURS

  Sets screen colours, rebuilding textures as required
    - Value; colour of foreground/background pixels, in RGBA format
}
procedure TDisplayMicrotan.SetForeColour(const Value: TGfxColour);
begin
  if (Value <> fForeColour) then
    begin
      fForeColour := Value;
      BuildChunkyCharacters;            // Rebuild with new colour
      BuildCharacterSet;
      Refresh;                          // and refresh screen
    end;
end;


procedure TDisplayMicrotan.SetBackColour(const Value: TGfxColour);
begin
  if (Value <> fBackColour) then
    begin
      fBackColour := Value;
      BuildChunkyCharacters;
      BuildCharacterSet;
      Refresh;
    end;
end;


{ SET DISPLAY POSITION AND SIZE
}
procedure TDisplayMicrotan.SetPosition(AValue: TPoint);
begin
  Gfx.SetWindowPosition(AValue.X, AValue.Y);
end;

function TDisplayMicrotan.GetPosition: TPoint;
begin
  Result := Gfx.GetWindowPosition;
end;

procedure TDisplayMicrotan.SetSize(AValue: TPoint);
begin
  Gfx.SetWindowSize(AValue.X, AValue.Y);
end;

function TDisplayMicrotan.GetSize: TPoint;
begin
  Result := Gfx.GetWindowSize;
end;

procedure TDisplayMicrotan.SetCaption(AValue: string);
begin
  Gfx.SetCaption(AValue);
end;


end.

