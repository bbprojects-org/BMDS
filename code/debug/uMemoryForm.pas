{ ==============================================================================

  MEMORY FRAME

    Displays the machine memory in a bytes list, with ASCII interpretation
    where relevant. Has buttons to jump to specific locations


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

{ TODO : uMemoryFrame

  . option width 8/16/24 bytes per row?

  . highlight any bytes on the stack / point to SP position

}

unit uMemoryForm;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids,
  //
  uGetAddrForm, uMemoryMgr, uCommon;

type

  { TMemoryForm }

  TMemoryForm = class(TForm)
    DrawGrid1: TDrawGrid;
    btnSetAddress: TButton;
    btnSpare1: TButton;
    btnSpare2: TButton;
    btnSpare3: TButton;
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure btnSetAddressClick(Sender: TObject);
    procedure btnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fMemory: TMemory;
    MaxAddr: integer;
    BtnIndex: integer;
    procedure SetMemory(aMemory: TMemory);
  public
    procedure AddButton(aCaption: string; aAddress: word); // Max three buttons 
    //procedure Refresh;
    //
    property Memory: TMemory write SetMemory;
  end;


implementation

{$R *.lfm}


procedure TMemoryForm.FormCreate(Sender: TObject);
begin
  BtnIndex := 1;                        // Set first button for 'AddButton'
  btnSpare1.Visible := False;
  btnSpare2.Visible := False;
  btnSpare3.Visible := False;
end;


{ SET MEMORY }

procedure TMemoryForm.SetMemory(aMemory: TMemory);
begin
  fMemory := aMemory;
  MaxAddr := Length(aMemory) - $08;
  DrawGrid1.RowCount := Length(aMemory) div 8;
end;


{ BUTTONS ROUTINES }

procedure TMemoryForm.AddButton(aCaption: string; aAddress: word);
var
  ThisButton: TButton;
begin
  case BtnIndex of
    1: ThisButton := btnSpare1;
    2: ThisButton := btnSpare2;
    3: ThisButton := btnSpare3;
  else
    Exit;
  end;
  ThisButton.Caption := aCaption;
  ThisButton.Tag := aAddress;
  ThisButton.OnClick := @btnClick;
  ThisButton.Visible := True;
  Inc(BtnIndex);
end;


procedure TMemoryForm.btnSetAddressClick(Sender: TObject);
var
  GetAddrBox: TGetAddressBox;
begin
  GetAddrBox := TGetAddressBox.Create(nil);
  try
    GetAddrBox.Address := DrawGrid1.Row * 8;
    if (GetAddrBox.ShowModal = mrOk) then
      DrawGrid1.Row := (GetAddrBox.Address and MaxAddr) div 8;
  finally
    GetAddrBox.Free;
  end;
end;


procedure TMemoryForm.btnClick(Sender: TObject);
begin
  DrawGrid1.Row := 0;                   // Force bottom, then selected address
  DrawGrid1.Row := ((Sender as TButton).Tag) div 8;
end;


{ DRAW DETAILS ON THE GRID }

procedure TMemoryForm.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  MemPointer, ctr: integer;
  ch: char;
  AsciiChars, Line: string;
begin
  MemPointer := aRow * 8;               // 8 bytes per row
  AsciiChars := '';
  for ctr := MemPointer to (MemPointer + 7) do
    begin
      ch := chr(fMemory[ctr]);
      if (ch in [#32..#126]) then       // Displayable character ?
        AsciiChars := AsciiChars + ch
      else
        AsciiChars := AsciiChars + '.';
    end;
  Line := (Format('%.4x  %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x  %s',
    [MemPointer,
    fMemory[MemPointer],
    fMemory[MemPointer + 1],
    fMemory[MemPointer + 2],
    fMemory[MemPointer + 3],
    fMemory[MemPointer + 4],
    fMemory[MemPointer + 5],
    fMemory[MemPointer + 6],
    fMemory[MemPointer + 7],
    AsciiChars]));

  DrawGrid1.Canvas.TextOut(aRect.Left, aRect.Top, Line);
end;


end.
