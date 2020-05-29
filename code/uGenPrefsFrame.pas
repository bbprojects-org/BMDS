{ ==============================================================================

  GENERAL PREFERENCES FRAME

    Provides facility to amend general options


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

{ TODO : uGenPrefsFrame -> add button to browse to folder }
{ TODO : uGenPrefsFrame -> add read/write INI items }

unit uGenPrefsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uPrefsFrameBase;

type
  
  { TGenPrefsFrame }

  TGenPrefsFrame = class(TPrefsFrame)
    edDataPath: TLabeledEdit;
    GroupBox1: TGroupBox;
  private
  public
    procedure Init; override;
    destructor Destroy; override;
    procedure SaveChanges; override;
    procedure CancelChanges; override;
  end;

var
  GenPrefs: TGenPrefsFrame;


implementation

{$R *.lfm}

{ TGenPrefsFrame }

procedure TGenPrefsFrame.Init;
begin
  inherited Init;
  btnResetDefault.Enabled := False;
  //edDataPath.Text := GlobalVars.MachineDataPath;
  //ReadIniItems;
end;


destructor TGenPrefsFrame.Destroy;
begin
  inherited Destroy;
end;


procedure TGenPrefsFrame.SaveChanges;
begin
  //if (fChanged) then
  //WriteIniItems;
end;


procedure TGenPrefsFrame.CancelChanges;
begin
  //if (fChanged) then
  //ReadIniItems;
end;

end.

