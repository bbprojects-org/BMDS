{ ==============================================================================

  PREFERENCES FRAMES BASE CLASS

    This is the base frame that is inherited by all preferences frames


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

unit uPrefsFrameBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type         
  TConfigChange = procedure(Sender: TObject) of object;
  TDebugLogEvent = procedure(Sender: TObject; Msg: string) of object;

  { TPrefsFrame }

  TPrefsFrame = class(TFrame)
    btnResetDefault: TButton;
  protected
    fFrame: TPrefsFrame;
    fChangedItem: integer;
    fChanged: boolean;
    fOnChange: TConfigChange;
    fOnDebug: TDebugLogEvent;
  public
    procedure Init; virtual;
    destructor Destroy; override;
    procedure SaveChanges; virtual; abstract;
    procedure CancelChanges; virtual; abstract;
    //
    property Frame: TPrefsFrame read fFrame write fFrame;
    property OnChange: TConfigChange read fOnChange write fOnChange;
    property OnDebug: TDebugLogEvent read fOnDebug write fOnDebug;
  end;


implementation

{$R *.lfm}

{ TPrefsFrame }

procedure TPrefsFrame.Init;
begin
  fChanged := False;
end;


destructor TPrefsFrame.Destroy;
begin
  //
  inherited Destroy;
end;


end.

