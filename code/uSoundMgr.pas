{ ==============================================================================

  SOUND MANAGER

  Handles interface to SDL2 for sound related activities

  Sounds files are added to an array, and an index/handle returned. These are
  then used to play relevant sound files


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

unit uSoundMgr;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  //
  SDL2, SDL2_mixer, uFilesMgr;

const
  SOUND_LoopForever = -1;

type
  TSound = record
    FileName: string;
    SdlHandle: PMix_Chunk;
  end;

  { TSoundMgr }

  TSoundMgr = class
  private
    aSoundsList: array of TSound;
  protected
  public
    constructor Create(Freq: integer = 44100; Channels: integer = 2);
    destructor  Destroy; override;

    function  Add(FileName: string): integer;
    procedure Play(Channel, Index: integer; Loops: integer = 0);
    procedure Halt(Channel: integer = -1);
  end;


implementation

{ CREATE }

{ If optional parameters omitted, sample freq = 44100, channels = 2 }

constructor TSoundMgr.Create(Freq: integer; Channels: integer);
begin
  if (SDL_InitSubSystem(SDL_INIT_AUDIO) < 0) then
    begin
      MessageDlg('Error in sound manager', 'Unable to initialize SDL for audio', mtError, [mbOK], 0);
      Exit;
    end;

  //Initialize SDL_mixer
  if (Mix_OpenAudio(Freq, MIX_DEFAULT_FORMAT, Channels, 2048) < 0) then
    begin
      MessageDlg('Error in sound manager', 'SDL_mixer could not initialize', mtError, [mbOK], 0);
      Exit;
    end;

  SetLength(aSoundsList, 0);            // Initialise soundfiles array
end;


{ DESTROY }

destructor TSoundMgr.Destroy;
var
  n, idx: integer;
begin
  n := Length(aSoundsList);             // If any sounds used
  if (n > 0) then
    for idx := 0 to (n - 1) do          // ... free their resources
      begin
        Mix_FreeChunk(aSoundsList[idx].SdlHandle);
        aSoundsList[idx].SdlHandle := nil;
      end;
  SetLength(aSoundsList, 0);            // Reset sound list to empty
  Mix_Quit;
  inherited Destroy;
end;


{ ADD }

function TSoundMgr.Add(FileName: string): integer;
var
  n: integer;
begin
  n := Length(aSoundsList);
  SetLength(aSoundsList, n + 1);
  aSoundsList[n].FileName := FileName;
  aSoundsList[n].SdlHandle := Mix_LoadWAV(PChar(GetFullFileName(FileName)));
  if (aSoundsList[n].SdlHandle = nil) then
    begin
      MessageDlg('Error in sound manager', Format('Unable to load sound [%s]', [GetFullFileName(FileName)]), mtError, [mbOK], 0);
      SetLength(aSoundsList, n);        // Reset array
      Result := 0;
    end
  else
    Result := n;
end;


{ PLAY }

procedure TSoundMgr.Play(Channel, Index: integer; Loops: integer);
begin
  Mix_PlayChannel(Channel,
                  aSoundsList[Index].SdlHandle,
                  Loops                 // Default = 0 play once, -1 = loop forever
                 );
end;


{ HALT }

{ Stop sound on selected channel, default -1 = all }

procedure TSoundMgr.Halt(Channel: integer);
begin
  Mix_HaltChannel(Channel);
end;


end.

