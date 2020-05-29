{ ==============================================================================

  PREFERENCES FRAME FOR CHIP-8

    No current settings.
    Saves settings in the application's INI file


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

{ TODO : uPrefsChip8 -> when add SCHIP, select preference here }

unit uPrefsChip8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uPrefsFrameBase;

type
  
  { TChip8PrefsFrame }

  TChip8PrefsFrame = class(TPrefsFrame)
    Label1: TLabel;
  private
  public
    procedure Init; override;
    destructor Destroy; override;
    procedure SaveChanges; override;
    procedure CancelChanges; override;
  end;

var
  Chip8Prefs: TChip8PrefsFrame;


implementation

{$R *.lfm}

{ TChip8PrefsFrame }

procedure TChip8PrefsFrame.Init;
begin
  inherited;
  btnResetDefault.Enabled := False;
  //ReadIniItems;
end;


destructor TChip8PrefsFrame.Destroy;
begin
  inherited;
end;


procedure TChip8PrefsFrame.SaveChanges;
begin
  //if (fChanged) then
  //WriteIniItems;
end;


procedure TChip8PrefsFrame.CancelChanges;
begin
  //if (fChanged) then
  //ReadIniItems;
end;


end.

