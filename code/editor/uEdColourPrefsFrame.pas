{ ==============================================================================

  EDITOR COLOUR PREFERENCES FRAME

    Provides facility to amend editor colour options
    Saves settings to application's INI file


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

unit uEdColourPrefsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ColorBox,
  SynEdit, uPrefsFrameBase;

type
  TEdColourPrefs = record
    //
  end;

  { TEdColourPrefsFrame }

  TEdColourPrefsFrame = class(TPrefsFrame)
    btnRevertF: TButton;
    btnRevertF1: TButton;
    cbForeground: TCheckBox;
    cbBackground: TCheckBox;
    colorBackground: TColorBox;
    colorForeground: TColorBox;
    comboAttribute: TComboBox;
    gbColours: TGroupBox;
    Label11: TLabel;
    SampleEditor: TSynEdit;
    procedure btnResetDefaultsClick(Sender: TObject);
  private
    procedure UpdateEditor;
  public
    procedure Init; override;
    destructor Destroy; override;
    procedure SaveChanges; override;
    procedure CancelChanges; override;
  end;

var
  EdColourPrefs: TEdColourPrefsFrame;


implementation

{$R *.lfm}

{ INIT }

procedure TEdColourPrefsFrame.Init;
begin
  inherited Init;
end;


{ DESTROY }

destructor TEdColourPrefsFrame.Destroy;
begin
  inherited Destroy;
end;


{ UPDATE EDITOR - show changes visually }

procedure TEdColourPrefsFrame.UpdateEditor;
begin
  if Assigned(fOnChange) then
    fOnChange(self);
end;


{ SAVE CHANGES / CANCEL CHANGES / RESET DEFAULTS }

procedure TEdColourPrefsFrame.SaveChanges;
begin
  //if (fChanged) then
  //  WriteIniItems;
end;


procedure TEdColourPrefsFrame.CancelChanges;
begin
  //if (fChanged) then
  //  ReadIniItems;
end;


procedure TEdColourPrefsFrame.btnResetDefaultsClick(Sender: TObject);
begin
  //
end;


end.

