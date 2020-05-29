{ ==============================================================================

  WATCHES FRAME

    Allows user to select watches where execution can be stopped
    to see status, single step, or other debug activity as required

    Saves watch list in application's INI file, for each machine


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

{ TODO : uWatchesFrame -> allow check for Memory R/W = value, Register = value }

unit uWatchesFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  //
  uCommon;

type
  TWatchType = ( wtEquals,	        // Register = parameter set
       	         wtDiffers );	        // Register <> parameter set

  TWatchData = record
    WatchType: TWatchType;
    Parameter: Word;
  end;

  TWatchArray = array of TWatchData;    // Length set at runtime

  { TWatchesFrame }

  TWatchesFrame = class(TFrame)
    btnAddWatch: TButton;
    btnClearWatches: TButton;
    btnDeleteWatch: TButton;
    ComboBox1: TComboBox;
    edValueDecimal: TLabeledEdit;
    edValueHex: TLabeledEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbDataAddresses1: TListBox;
    rbEquals: TRadioButton;
    rbNotEqual: TRadioButton;
    procedure btnAddWatchClick(Sender: TObject);
    procedure btnClearWatchesClick(Sender: TObject);
    procedure btnDeleteWatchClick(Sender: TObject);
    procedure edValueDecimalExit(Sender: TObject);
    procedure edValueHexExit(Sender: TObject);
  private              
    fEnabled: boolean;
    WatchValue: word;
    procedure SetEnabled(aFlag: boolean);
  public         
    procedure Initialise;
    destructor Destroy; override;
    //
    property Enabled: boolean read fEnabled write SetEnabled;
  end;


implementation

{$R *.lfm}

const
  INI_PREFIX = 'Watches';
  NUM_BKPTS  = 'Count';
  ARRAY_SIZE = 'Size';


{ INITIALISE }

procedure TWatchesFrame.Initialise;
begin
  WatchValue := 0;
end;


{ DESTROY }

destructor TWatchesFrame.Destroy;
begin
  //
  inherited;
end;


{SET ENABLED STATE }

procedure TWatchesFrame.SetEnabled(aFlag: boolean);
var
  i: integer;
begin
  fEnabled := aFlag;
  for i := 0 to self.ControlCount-1 do
    self.Controls[i].Enabled := fEnabled;
end;


{ VALUE HEX EDIT EXIT }

{ On editing hex edit box, check value and calculate decimal value }

procedure TWatchesFrame.edValueHexExit(Sender: TObject);
var
  Value: integer;
begin
  Value := GetHex(edValueHex.Text);
  edValueHex.Text := Format('%.4x', [Value]);
(*
  if ((Value < 0) or (Value > fBkptArraySize)) then
    begin
      MessageWarning(Format('Value must be between $0000 and $%.4x', [fBkptArraySize-1]));
      edValueHex.SetFocus;
      Exit;
    end;
*)
  WatchValue := Value;
  edValueDecimal.Text := Format('%d', [Value]);
end;


{ VALUE DECIMAL EDIT EXIT }   

{ On editing in the decimal edit box, check value and calculate the hex value }

procedure TWatchesFrame.edValueDecimalExit(Sender: TObject);
var
  Value: integer;
begin
  Value := StrToInt(edValueDecimal.Text);
  (*
  if ((Value < 0) or (Value > fBkptArraySize)) then
    begin
      {
      MessageWarning(Format('Value must be between 0 and %d', [fBkptArraySize-1]));
      }
      edValueDecimal.SetFocus;
      Exit;
    end;
  *)
  WatchValue := Value;
  edValueHex.Text := Format('%.4x', [Value]);
end;


{ ADD WATCH }

procedure TWatchesFrame.btnAddWatchClick(Sender: TObject);
begin
  //
end;


{ DELETE WATCH }

procedure TWatchesFrame.btnDeleteWatchClick(Sender: TObject);
begin
  //
end;


{ CLEAR WATCHES }

procedure TWatchesFrame.btnClearWatchesClick(Sender: TObject);
begin
  //
end;


end.

