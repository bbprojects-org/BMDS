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
{ TODO : uBreakpointsForm -> do we use OnBreakpoint event ? }

unit uBreakpointsForm;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  //
  uCommon;

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

  TWatchType = ( wtEquals,	        // Register = parameter set
 	         wtDiffers );	        // Register <> parameter set

  TWatchData = record
    WatchType: TWatchType;
    Parameter: Word;
  end;

  TWatchArray = array of TWatchData;    // Length will be set at runtime

  TOnBreakpointEvent = procedure(Sender: TObject; Value: TBkptType) of object;

  { TBreakpointsForm }

  TBreakpointsForm = class(TForm)
    btnAddBkpt: TButton;
    btnAddWatch: TButton;
    btnClearWatches: TButton;
    btnDeleteBkpt: TButton;
    btnDeleteWatch: TButton;
    btnClearBkpts: TButton;
    ComboBox1: TComboBox;
    edAddress: TLabeledEdit;
    edValueHex: TLabeledEdit;
    edValueDecimal: TLabeledEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblDebug: TLabel;
    lbBreakpoints: TListBox;
    lbDataAddresses1: TListBox;
    rbEquals: TRadioButton;
    rbNotEqual: TRadioButton;
    procedure btnAddBkptClick(Sender: TObject);
    procedure btnClearBkptsClick(Sender: TObject);
    procedure btnDeleteBkptClick(Sender: TObject);
    procedure edAddressExit(Sender: TObject);
    procedure edValueDecimalExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbBreakpointsClick(Sender: TObject);
  private
    fOnBreakpoint: TOnBreakpointEvent;
    fBkptArraySize: integer;
    BkptArray: TBkptArray;
    BkptAddress: word;
    WatchValue: word;
    procedure SetArraySize(Value: integer);
    procedure UpdateBkpts;
  public
    property ArraySize: integer write SetArraySize;
    property OnBreakpoint: TOnBreakpointEvent read fOnBreakpoint write fOnBreakpoint;
    function CheckRead(Value: word): boolean;
    function CheckWrite(Value: word): boolean;
    procedure SetBreakpoint(Value: word);
  end;

var
  BreakpointsForm: TBreakpointsForm;


implementation

{$R *.lfm}

const
  INI_PREFIX = 'Bkpt';
  NUM_BKPTS  = 'Count';
  ARRAY_SIZE = 'Size';

{ CREATE }

procedure TBreakpointsForm.FormCreate(Sender: TObject);
var
  IniSect: string;
  Count, idx: integer;
  Addr: word;
begin
(*
  IniSect := GetIniSect;
  Top := IniReadInteger(IniSect, INI_PREFIX + WDW_TOP, 330);
  Left := IniReadInteger(IniSect, INI_PREFIX + WDW_LEFT, 280);
  SetArraySize(IniReadInteger(IniSect, INI_PREFIX + ARRAY_SIZE, 1));
  Count := IniReadInteger(IniSect, INI_PREFIX + NUM_BKPTS, 0);
  for idx := 1 to Count do
    begin
      Addr := IniReadInteger(IniSect, INI_PREFIX + IntToStr(idx), $FFFF);
      BkptArray[Addr].BkptType := btReadWrite;
    end;
*)
  BkptAddress := 0;
  WatchValue := 0;
  UpdateBkpts;
end;


{ DESTROY }

procedure TBreakpointsForm.FormDestroy(Sender: TObject);
var
  IniSect: string;
  idx, Count: integer;
begin
(*
  IniSect := GetIniSect;
  IniWriteInteger(IniSect, INI_PREFIX + WDW_TOP, Top);
  IniWriteInteger(IniSect, INI_PREFIX + WDW_LEFT, Left);
  IniWriteInteger(IniSect, INI_PREFIX + ARRAY_SIZE, fBkptArraySize);
  Count := 0;
  for idx := 0 to (fBkptArraySize - 1) do
    if (BkptArray[idx].BkptType <> btNone) then
      begin
        Inc(Count);
        IniWriteInteger(IniSect, INI_PREFIX + IntToStr(Count), idx);
      end;
  IniWriteInteger(IniSect, INI_PREFIX + NUM_BKPTS, Count);
*)
end;


{ SET ARRAY SIZE }

procedure TBreakpointsForm.SetArraySize(Value: integer);
var
  idx: integer;
begin
  if (Value <> fBkptArraySize) then
    begin
      fBkptArraySize := Value;
      SetLength(BkptArray, fBkptArraySize);
      for idx := 0 to (fBkptArraySize - 1) do // Clear all breakpoints
        BkptArray[idx].BkptType := btNone;
    end;
end;


{ SET BREAKPOINT }

procedure TBreakpointsForm.SetBreakpoint(Value: word);
begin
  if (Value < fBkptArraySize) then
    begin
      BkptArray[Value].BkptType := btReadWrite;
      BkptArray[Value].Parameter := 0;
    end;
  UpdateBkpts;
end;


{ CHECK READ }

function TBreakpointsForm.CheckRead(
           Value: word):                // Program address to check
           boolean;                     // Returns: true if address is in breakpoint list
begin
  if ((Value < fBkptArraySize) and (BkptArray[Value].BkptType <> btNone)) then
    Result := (BkptArray[Value].BkptType = btReadWrite)
           or (BkptArray[Value].BkptType = btRead)
  else
    Result := False;
end;


{ CHECK WRITE }

function TBreakpointsForm.CheckWrite(
           Value: word):                // Program address to check
           boolean;                     // Returns: true if address is in breakpoint list
begin
  if ((Value < fBkptArraySize) and (BkptArray[Value].BkptType <> btNone)) then
    Result := (BkptArray[Value].BkptType = btReadWrite)
           or (BkptArray[Value].BkptType = btWrite)
  else
    Result := False;
end;


{ ADDRESS EXIT }

{ On editing hex edit boxes, check values, and for watch calculate decimal value }

procedure TBreakpointsForm.edAddressExit(Sender: TObject);
var
  Value: integer;
  TempEdit: TLabeledEdit;
begin
  TempEdit := Sender as TLabeledEdit;
  Value := GetHex(TempEdit.Text);
  TempEdit.Text := Format('%.4x', [Value]);
  if ((Value < 0) or (Value > fBkptArraySize)) then
    begin
      MessageDlg(Format('Value must be between $0000 and $%.4x', [fBkptArraySize-1]),
        mtWarning, [mbOK], 0);
      TempEdit.SetFocus;
      Exit;
    end;
  case TempEdit.Tag of
    100: BkptAddress := Value;
    200: begin
           WatchValue := Value;
           edValueDecimal.Text := Format('%d', [Value]);
         end;
  end;
end;


{ VALUE DECIMAL EXIT }

{ On editing in the decimal edit box, check value and calculate the hex value }

procedure TBreakpointsForm.edValueDecimalExit(Sender: TObject);
var
  Value: integer;
begin
  Value := StrToInt(edValueDecimal.Text);
  if ((Value < 0) or (Value > fBkptArraySize)) then
    begin
      MessageDlg(Format('Value must be between 0 and %d', [fBkptArraySize-1]),
        mtWarning, [mbOK], 0);
      edValueDecimal.SetFocus;
      Exit;
    end;
  WatchValue := Value;
  edValueHex.Text := Format('%.4x', [Value]);
end;


{ ADD BREAKPOINT }

procedure TBreakpointsForm.btnAddBkptClick(Sender: TObject);
begin
  btnAddBkpt.SetFocus;                  // Force exit of edit box
  BkptArray[BkptAddress].BkptType := btReadWrite;
  btnClearBkpts.Enabled := True;
  UpdateBkpts;
end;


{ DELETE BREAKPOINT }

procedure TBreakpointsForm.btnDeleteBkptClick(Sender: TObject);
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

procedure TBreakpointsForm.btnClearBkptsClick(Sender: TObject);
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

procedure TBreakpointsForm.UpdateBkpts;
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

procedure TBreakpointsForm.lbBreakpointsClick(Sender: TObject);
begin
  btnDeleteBkpt.Enabled := (lbBreakpoints.SelCount > 0);
end;


end.

