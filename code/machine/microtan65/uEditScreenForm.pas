{ ==============================================================================

  EDIT SCREEN FORM (Microtan 65)

    This form allows simple editing of the text on the screen of the
    Microtan 65. This can be used prior to saving the machine state, including
    any program, to a file in order to provide 'on screen' instructions

    OnScreenWrite: event handler to update the actual screen after changes
                   made to its memory here

    MemoryRef: assign machine memory reference for video access. This
               copies the original screen memory to a temporary array

    Whenever the memo is changed, its lines are written to the screen memory
    and the OnScreenWrite event triggered

    Save button: exits leaving M65 screen amended as required
    Cancel button: restores the original screen memory, then exits


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

unit uEditScreenForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  //
  uMemoryMgr, uIniFile;

type
  TOnScreenWrite = procedure of object;

  TEditScreenForm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    fMemoryRef: TMemory;
    fOnScreenWrite: TOnScreenWrite;
    fMemTemp: array[$200..$3FF] of byte;
    procedure SetMemoryRef(mem: TMemory);
  public
    property MemoryRef: TMemory write SetMemoryRef;
    property OnScreenWrite: TOnScreenWrite read fOnScreenWrite write fOnScreenWrite;
  end;


implementation

{$R *.lfm}

const
  SECT_ESF = 'EditScreenForm';

{ CREATE / DESTROY }

procedure TEditScreenForm.FormCreate(Sender: TObject);
begin
  Left := AppIni.ReadInteger(SECT_ESF, INI_WDW_LEFT, 50);
  Top := AppIni.ReadInteger(SECT_ESF, INI_WDW_TOP, 50);
end;


procedure TEditScreenForm.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_ESF, INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_ESF, INI_WDW_TOP, Top);
  inherited;
end;


{ GET / SET ROUTINES }

procedure TEditScreenForm.SetMemoryRef(mem: TMemory);
var
  i, row, col: integer;
  line: string;
  ch: char;
begin
  if (fMemoryRef = mem) then
    Exit;

  fMemoryRef := mem;
  for i := $200 to $3FF do
    fMemTemp[i] := fMemoryRef[i];       // Save current screen contents

  Memo1.Lines.Clear;
  for row := 0 to 15 do                 // Read screen into Memo
    begin
      line := '';
      for col := 0 to 31 do
        begin
          ch := chr(fMemoryRef[$200 + (row * 32) + col] and $7F);
          if (ch in [#32..#126]) then   // Displayable character ?
            line := line + ch
          else
            line := line + '.';
        end;
      Memo1.Lines.Add(line);
    end;
  Memo1Change(nil);
end;


{ ON MEMO CHANGE }

procedure TEditScreenForm.Memo1Change(Sender: TObject);
var
  row, col: integer;
  line: string;
begin
  if (fMemoryRef = nil) then
    Exit;

  for row := 0 to 15 do
    begin
      line := Memo1.Lines[row] + '                                ';
      for col := 1 to 32 do
        fMemoryRef[$1FF + (row * 32) + col] := ord(line[col]);
    end;
  if Assigned(fOnScreenWrite) then
    fOnScreenWrite;
end;


{ BUTTONS }

procedure TEditScreenForm.btnOKClick(Sender: TObject);
begin
  Close;
end;


procedure TEditScreenForm.btnCancelClick(Sender: TObject);
var
  i: integer;
begin
  for i := $200 to $3FF do
    fMemoryRef[i] := fMemTemp[i];       // Restore original screen contents
  if Assigned(fOnScreenWrite) then
    fOnScreenWrite;
  Close;
end;


end.

