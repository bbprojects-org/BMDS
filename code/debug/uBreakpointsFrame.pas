
{ ==============================================================================

  BREAKPOINTS FRAME

    Allows user to select addresses where execution can be stopped
    to see status, single step, or other debug activity as required


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

{ TODO : uBreakpointsForm -> Memory R/W = value, Register = value }

{ TODO : uBreakpointsForm -> program types btEqual, btNotEqual }

unit uBreakpointsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  //
  uIniFile, uCommon;

type

  TBkptType = (	btNone,	                // No breakpoint set
                btTemp,	                // Temporary breakpoint, will be cleared after breakpoint event
                btReadWrite,	        // Standard breakpoint, stop on read or write
	        btRead,	                // Stop on read only
	        btWrite,	        // Stop on write only

	        btEqual,                // Value at location = parameter set
	        btNotEqual);	        // Value at location <> parameter set

  TBkptData = record
    BkptType: TBkptType;
    Parameter: Word;
  end;

  TBkptArray = array of TBkptData;      // Length will be set at runtime

  { TBreakpointsFrame }

  TBreakpointsFrame = class(TFrame)
    edAddress: TLabeledEdit;
    lbBreakpoints: TListBox;
    btnAddBkpt: TButton;
    btnClearBkpts: TButton;
    btnDeleteBkpt: TButton;
    procedure btnAddBkptClick(Sender: TObject);
    procedure btnClearBkptsClick(Sender: TObject);
    procedure btnDeleteBkptClick(Sender: TObject);
    procedure edAddressExit(Sender: TObject);
    procedure lbBreakpointsClick(Sender: TObject);
  private
    fBkptArraySize: integer;
    BkptArray: TBkptArray;
    BkptAddress: word;
    procedure SetArraySize(aSize: integer);
    procedure UpdateBkpts;
  public
    procedure Initialise;        
    destructor Destroy; override;
    function CheckRead(aAddr: word): boolean;
    function CheckWrite(aAddr: word): boolean;
    procedure SetBreakpoint(aBkptAddr: word);
    //
    property ArraySize: integer write SetArraySize;
  end;


implementation

{$R *.lfm}
const
  INI_PREFIX = 'Bkpt';
  NUM_BKPTS  = 'Count';
  ARRAY_SIZE = 'Size';


{ INIT }

procedure TBreakpointsFrame.Initialise;
var
  Count, idx: integer;
  Addr: word;
begin
  SetArraySize(AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + ARRAY_SIZE, 1));
  Count := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + NUM_BKPTS, 0);
  for idx := 1 to Count do
    begin
      Addr := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + IntToStr(idx), $FFFF);
      BkptArray[Addr].BkptType := btReadWrite;
    end;
  BkptAddress := 0;
  UpdateBkpts;
end;


{ DESTROY }

destructor TBreakpointsFrame.Destroy;
var
  idx, Count: integer;
begin
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + ARRAY_SIZE, fBkptArraySize);
  Count := 0;
  (* ERROR HERE
  for idx := 0 to (fBkptArraySize - 1) do
    if (BkptArray[idx].BkptType <> btNone) then
      begin
        Inc(Count);
        AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + IntToStr(Count), idx);
      end;
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + NUM_BKPTS, Count);
  *)
  SetLength(BkptArray, 0);
  inherited Destroy;
end;


{ SET ARRAY SIZE }

procedure TBreakpointsFrame.SetArraySize(aSize: integer);
var
  idx: integer;
begin
  if (aSize <> fBkptArraySize) then
    begin
      fBkptArraySize := aSize;
      SetLength(BkptArray, fBkptArraySize);
      for idx := 0 to (fBkptArraySize - 1) do // Clear all breakpoints
        BkptArray[idx].BkptType := btNone;
    end;
end;


{ SET BREAKPOINT }

procedure TBreakpointsFrame.SetBreakpoint(aBkptAddr: word);
begin
  if (aBkptAddr < fBkptArraySize) then
    begin
      BkptArray[aBkptAddr].BkptType := btReadWrite;
      BkptArray[aBkptAddr].Parameter := 0;
    end;
  UpdateBkpts;
end;


{ CHECK READ }

function TBreakpointsFrame.CheckRead(aAddr: word): boolean;
begin
  if ((aAddr < fBkptArraySize) and (BkptArray[aAddr].BkptType <> btNone)) then
    Result := (BkptArray[aAddr].BkptType = btReadWrite)
           or (BkptArray[aAddr].BkptType = btRead)
  else
    Result := False;
end;


{ CHECK WRITE }

function TBreakpointsFrame.CheckWrite(aAddr: word): boolean;
begin
  if ((aAddr < fBkptArraySize) and (BkptArray[aAddr].BkptType <> btNone)) then
    Result := (BkptArray[aAddr].BkptType = btReadWrite)
           or (BkptArray[aAddr].BkptType = btWrite)
  else
    Result := False;
end;


{ ADDRESS EXIT }

{ On editing hex edit boxes, check values, and for watch calculate decimal value }

procedure TBreakpointsFrame.edAddressExit(Sender: TObject);
var
  Value: integer;
begin
  Value := GetHex(edAddress.Text);
  edAddress.Text := Format('%.4x', [Value]);
  if ((Value < 0) or (Value > fBkptArraySize)) then
    begin
      (*
      MessageDlg(Format('Value must be between $0000 and $%.4x', [fBkptArraySize-1]),
        mtWarning, [mbOK], 0);
      *)
      edAddress.SetFocus;
      Exit;
    end;
  BkptAddress := Value;
end;


{ ADD BREAKPOINT }

procedure TBreakpointsFrame.btnAddBkptClick(Sender: TObject);
begin
  btnAddBkpt.SetFocus;                  // Force exit of edit box
  BkptArray[BkptAddress].BkptType := btReadWrite;
  btnClearBkpts.Enabled := True;
  UpdateBkpts;
end;


{ DELETE BREAKPOINT }

procedure TBreakpointsFrame.btnDeleteBkptClick(Sender: TObject);
var
  i, j: integer;
begin
  for i := (lbBreakpoints.Items.Count - 1) downto 0 do
    if (lbBreakpoints.Selected[i]) then
      begin
        j := GetHex(lbBreakpoints.Items[i]);
        BkptArray[j].BkptType := btNone;
      end;
  btnDeleteBkpt.Enabled := False;
  UpdateBkpts;
end;


{ CLEAR BREAKPOINTS }

procedure TBreakpointsFrame.btnClearBkptsClick(Sender: TObject);
var
  idx: integer;
begin
  lbBreakpoints.Items.Clear;
  for idx := 0 to (fBkptArraySize - 1) do
    BkptArray[idx].BkptType := btNone;
  btnClearBkpts.Enabled := False;
end;


{ UPDATE BREAKPOINTS }

{ Update the list of breakpoints in address order, enabling the Clear button
  if there are breakpoints in the list }

procedure TBreakpointsFrame.UpdateBkpts;
var
  idx: integer;
begin
  lbBreakpoints.Items.Clear;
  for idx := 0 to (fBkptArraySize - 1) do
    if (BkptArray[idx].BkptType <> btNone) then
      lbBreakpoints.Items.Add(Format('$%.4x', [idx]));
  btnClearBkpts.Enabled := (lbBreakpoints.Items.Count > 0);
end;


{ BREAKPOINTS CLICK }

{ If user clicks on a breakpoint then enable delete key }

procedure TBreakpointsFrame.lbBreakpointsClick(Sender: TObject);
begin
  btnDeleteBkpt.Enabled := (lbBreakpoints.SelCount > 0);
end;


end.

