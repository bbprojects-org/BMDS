{ ==============================================================================

  GRAPHICS MANAGER

    Provides interface to SDL2 graphics framework


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

{ TODO : uGfxMgr -> add 10px border around screen, use ViewPort?
         https://www.freepascal-meets-sdl.net/tag/sdl-2-0-tutorial/ }

unit uGfxMgr;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  //
  SDL2;

const
  WDW_WIDTH      = 640;                 // Default window size
  WDW_HEIGHT     = 480;

  MAX_TEXTURES   = 8;                   // Max number of textures supported

type
  PGfxRect = ^TSDL_Rect;
  TGfxRect = TSDL_Rect;

  TGfxColour = Longword;
  TPalette = array of TGfxColour;       // Dynamic array of colours

  TGfxTexture = record                  // Details about a texture:
    Texture: pSDL_Texture;              // ... texture itself
    Width: integer;                     // ... width & height
    Height: integer;
    Pixels: array of longword;          // ... pixel buffer
  end;

  { TGfxManager }

  TGfxManager = class
  private
    fPalette: TPalette;                 // Screen colours
    fCursorOn: boolean;
    fCaption: string;
    fErrorFlag: boolean;
    fErrMsg: string;
    fSdlWindow: pSDL_Window;            // Handle for SDL2 window
    fSdlRenderer: pSDL_Renderer;        // Handle for SDL2 renderer
    fSdlTextures: array [1..MAX_TEXTURES] of TGfxTexture; // Index 1 = main screen
    fSdlTextureCount: integer;
    procedure SetCursor(ShowFlag: boolean);
    procedure SetErrMsg(ErrStr: string);
    procedure SetPalette(Value: TPalette);
    procedure SetCaption(Caption: string);
  public
    constructor Create(NumColours: integer);
    destructor  Destroy; override;
    procedure SetWindowPosition(Left, Top: longint);
    procedure SetWindowSize(Width, Height: longint);
    function  GetWindowPosition: TPoint;
    function  GetWindowSize: TPoint;
    procedure SetPixel(Index, x, y: longint; aColour: longword);
    procedure UpdateTexture(Index: longint);
    function  GetTexture(Width, Height: longint; RenderQuality: integer = 0): integer;
    procedure ShowPixels(Index: integer);
    procedure Show;             
    procedure SetScale(ScaleFactor: double);
    procedure Clear;
    procedure SetDrawColour(R, G, B, A: byte);
    procedure DrawLine(x1,y1, x2,y2: longint);
    procedure DrawRect(x,y, w,h: longint);
    procedure Delay(timeMS: longword);
    function  RGB(R, G, B: byte): longword;
    procedure Copy(Index: integer; SrcRect, DestRec: PGfxRect);
    function  GetVersion: string;
    procedure Focus;

    property Palette: TPalette read fPalette write SetPalette;
    property ShowCursor: boolean read fCursorOn write SetCursor;
    property Caption: string read fCaption write SetCaption;
end;


implementation

{ CREATE - Initialise SDL
         - Create initial window
         - Create renderer }

constructor TGfxManager.Create(NumColours: integer);
begin
  fErrorFlag := False;                  // No errors at this time
  fSdlTextureCount := 0;                // No textures in use
  SetLength(fPalette, NumColours);
  fPalette[0] := $000000FF;             // Black and white
  fPalette[1] := $FFFFFFFF;

  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
    begin
      SetErrMsg('unable to initialize SDL');
      Exit;
    end;

  // Create default window
  fSdlWindow := SDL_CreateWindow('Gfx', 0, 0, WDW_WIDTH, WDW_HEIGHT, SDL_WINDOW_RESIZABLE);
  if (fSdlWindow = nil) then            // Failure returns nil
    begin
      SetErrMsg('unable to create window');
      Exit;
    end;

  { TODO : uGfxMgr -> add 'Set Min/Max' as separate methods }
  //SDL_SetWindowMinimumSize(fSdlWindow, WDW_WIDTH, WDW_HEIGHT);
  //SDL_SetWindowGrab(fSdlWindow, longbool(SDL_TRUE));

  // Creates the renderer; it's what we do all our drawing on
  fSdlRenderer := SDL_CreateRenderer(fSdlWindow, -1, 0);
  if (fSdlRenderer = nil) then          // Failure returns nil
    begin
      SetErrMsg('unable to create render');
      Exit;
    end;
  SDL_ClearError;                       // Remove spurious 'Invalid render'
                                        // created by SDL_CreateRenderer
end;


{ DESTROY }

destructor TGfxManager.Destroy;
var
  i: integer;
begin
  if (fSdlTextureCount > 0) then
    for i := 1 to fSdlTextureCount do
      begin
        SDL_DestroyTexture(fSdlTextures[i].Texture);
        SetLength(fSdlTextures[i].Pixels, 0);
      end;
  SDL_DestroyRenderer(fSdlRenderer);
  SDL_DestroyWindow(fSdlWindow);
  SDL_Quit;
  inherited;
end;


{ SET ERROR MESSAGE }

procedure TGfxManager.SetErrMsg(ErrStr: string);
begin
  fErrorFlag := True;
  fErrMsg := Format('GfxMgr error; %s. SDL reports [%s]', [ErrStr, SDL_GetError]);
end;


{ SET PALETTE }

procedure TGfxManager.SetPalette(Value: TPalette);
begin
  SetLength(fPalette, Length(Value));
  fPalette := Value;
end;


{ SET / GET WINDOW POSITION }

procedure TGfxManager.SetWindowPosition(Left, Top: longint);
begin
  SDL_SetWindowPosition(fSdlWindow, Left, Top);
end;


function TGfxManager.GetWindowPosition: TPoint;
begin
  SDL_GetWindowPosition(fSdlWindow, @Result.X, @Result.Y);
end;


{ SET / GET WINDOW SIZE }

procedure TGfxManager.SetWindowSize(Width, Height: longint);
begin
  SDL_SetWindowSize(fSdlWindow, Width, Height);
end;


function TGfxManager.GetWindowSize: TPoint;
begin
  SDL_GetWindowSize(fSdlWindow, @Result.X, @Result.Y);
end;


{ SET CAPTION }

procedure TGfxManager.SetCaption(Caption: string);
begin
  fCaption := Caption;
  SDL_SetWindowTitle(fSdlWindow, pointer(Caption));
end;


{ SET FOCUS }

procedure TGfxManager.Focus;
begin
  SDL_RaiseWindow(fSdlWindow);
end;


{ GET A TEXTURE }

{ Create a texture of passed width, height and quality, and return index
  into texture array}

function TGfxManager.GetTexture(Width, Height: longint; RenderQuality: integer): integer;
begin
  if (fSdlTextureCount = MAX_TEXTURES) then // Any more textures permitted?
    Exit;

  case (RenderQuality) of               // Check quality, default = 0
    0: SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '0');
    1: SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1');
    2: SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '2');
  end;
  Inc(fSdlTextureCount);                // Next texture index
  Result := fSdlTextureCount;
  fSdlTextures[fSdlTextureCount].Texture :=
      SDL_CreateTexture(fSdlRenderer,
                        SDL_PIXELFORMAT_RGBA8888,
                        SDL_TEXTUREACCESS_STREAMING,
                        Width, Height );
  if (fSdlTextures[fSdlTextureCount].Texture = nil) then
    begin
      SetErrMsg('unable to create texture');
      Dec(fSdlTextureCount);
      Result := 0;
      Exit;
    end;
  // Create pixels cache array
  SetLength(fSdlTextures[fSdlTextureCount].Pixels, Width * Height);
  fSdlTextures[fSdlTextureCount].Width := Width;
  fSdlTextures[fSdlTextureCount].Height := Height;
end;


{ SET PIXEL }

procedure TGfxManager.SetPixel(Index, x, y: longint; aColour: longword);
begin
  fSdlTextures[Index].Pixels[x + (y * fSdlTextures[Index].Width)] := aColour;
end;


{ SET CURSOR ON/OFF }

procedure TGfxManager.SetCursor(ShowFlag: boolean);
begin
  if (fCursorOn = ShowFlag) then Exit;

  fCursorOn := ShowFlag;
  if (fCursorOn) then
    SDL_ShowCursor(SDL_ENABLE)
  else
    SDL_ShowCursor(SDL_DISABLE);
end;


{ GET VERSION }

function TGfxManager.GetVersion: string;
var
  ver: TSDL_Version;
begin
  SDL_Version(@ver);
  Result := Format('SDL Version %d.%d.%d', [ver.major, ver.minor, ver.patch]);
end;


{ SHOW PIXELS }

{ Updates the indexed texture and renders to the display window. The index
  is position of the texture in textures array (as returned by GetTexture) }

procedure TGfxManager.ShowPixels(Index: integer);
begin
  SDL_UpdateTexture(fSdlTextures[Index].Texture,
                    nil,
                    pointer(fSdlTextures[Index].Pixels),
                    fSdlTextures[Index].Width * 4 );
  SDL_RenderCopy(fSdlRenderer, fSdlTextures[Index].Texture, nil, nil);
  SDL_RenderPresent(fSdlRenderer);
end;


{ SHOW }

procedure TGfxManager.Show;
var
  w, h: integer;
begin
  { TODO : uGfxMgr -> move resize to a Resize event, so only do when necessary
           Need to figure better way of using [1] for screen texture? }
  SDL_GetWindowSize(fSdlWindow, @w, @h);
  SDL_RenderSetScale(fSdlRenderer, w / fSdlTextures[1].Width, h / fSdlTextures[1].Height);
  SDL_RenderPresent(fSdlRenderer);
end;


{ SET SCALE }

procedure TGfxManager.SetScale(ScaleFactor: double);
begin
  SDL_RenderSetScale(fSdlRenderer, ScaleFactor, ScaleFactor);
  SDL_RenderPresent(fSdlRenderer);
end;


{ SET DRAW COLOUR }

{ Sets colour to be used in subsequent operations. Alpha 255=opaque, 0=transparent }

procedure TGfxManager.SetDrawColour(R, G, B, A: byte);
begin
  SDL_SetRenderDrawColor(fSdlRenderer, R, G, B, A);
end;


{ CLEAR }

{ Clear the rendered image, using drawing colour set }

procedure TGfxManager.Clear;
begin
  SDL_RenderClear(fSdlRenderer);
end;


{ DRAW LINE }

{ Draw a line using draw colour set. Top, Left, Bottom, Right }

procedure TGfxManager.DrawLine(x1, y1, x2, y2: longint);
begin
  SDL_RenderDrawLine(fSdlRenderer, x1,y1, x2,y2);
end;


{ DRAW RECTANGLE }

{ Draw a rectangle using draw colour set. Top, Left, Bottom, Right }

procedure TGfxManager.DrawRect(x, y,  w, h: longint);
var
  sdlRect: pSDL_Rect;
begin
  New(sdlRect);
  sdlRect^.x := x;
  sdlRect^.y := y;
  sdlRect^.w := w;
  sdlRect^.h := h;
  SDL_RenderDrawRect(fSdlRenderer, sdlRect);
  Dispose(sdlRect);
end;


{ DELAY }

procedure TGfxManager.Delay(timeMS: longword);
begin
  SDL_Delay(timeMS);
end;


{ RGB }

{ Get a RGBA colour value based on separate Red/Green/Blue values, with
  alpha value set to fully opaque }

function TGfxManager.RGB(R, G, B: byte): longword;
begin
  Result := (R shl 24) + (G shl 16) + (B shl 8) + 255;
end;


{ COPY }

{ Copy portion of the indexed texture to the renderer, using rectangles passed
  where NIL = whole texture. The texture source will be stretched to fill
  destination rectangle in renderer }

procedure TGfxManager.Copy(Index: integer; SrcRect, DestRec: PGfxRect);
begin
  SDL_RenderCopy(fSdlRenderer, fSdlTextures[Index].Texture, SrcRect, DestRec);
end;


{ UPDATE TEXTURE }

{ Update the indexed texture with new pixel data }

procedure TGfxManager.UpdateTexture(Index: longint);
begin
  SDL_UpdateTexture(
    fSdlTextures[Index].Texture,        // Selected texture
    nil,                                // NIL to update the entire texture
    pointer(fSdlTextures[Index].Pixels), // Pixels buffer
    fSdlTextures[Index].Width * 4       // Four bytes per pixel (RGBA)
    );
end;


end.

