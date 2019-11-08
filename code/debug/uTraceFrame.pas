{ ==============================================================================

  TRACE FRAME

    Displays an execution trace from the last run of code


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

unit uTraceFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Grids, StdCtrls, Types, ExtCtrls, Math,
  //
  uCpuBase, uCommon;

type

  { TTraceFrame }

  TTraceFrame = class(TFrame)
    btnClear: TButton;
    DrawGrid1: TDrawGrid;
    procedure btnClearClick(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; {%H-}aState: TGridDrawState);
  private
    fCpuRef: TCpuBase;
    procedure SetCPU(aValue: TCpuBase);
    procedure SetRows(aValue: integer);
  public           
    procedure Initialise;
    //
    property CpuRef: TCpuBase write SetCPU;
    property Rows: integer write SetRows;
  end;

implementation

{$R *.lfm}


{ INIT }

procedure TTraceFrame.Initialise;
begin                      
  DrawGrid1.RowCount := 1;              // Header row
end;


{ SET CPU }

{ Sets grid column parameters to reflect specific CPU in use }

procedure TTraceFrame.SetCPU(aValue: TCpuBase);
var
  i: integer;
  col: TGridColumn;
  ColumnsArray: TTraceColArray;
begin
  fCpuRef := aValue;                    // Reference to CPU
  ColumnsArray := fCpuRef.TraceColumns; // Get specific column data for CPU
  DrawGrid1.Columns.Clear;
  for i := 0 to Length(ColumnsArray)-1 do
  begin
    col := DrawGrid1.Columns.Add;       // and create columns
    col.Title.Caption := ColumnsArray[i].Title;
    col.Width := ColumnsArray[i].Width;
    col.Alignment := ColumnsArray[i].Align;
  end;
  btnClear.Enabled := True;
end;


{ SET ROWS }

procedure TTraceFrame.SetRows(aValue: integer);
begin
  DrawGrid1.RowCount := Max(aValue, DrawGrid1.FixedRows);
end;


{ DISPLAY RESULTS }

{ Usually when machine execution stopped }

procedure TTraceFrame.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Data: TDisassembledData;
  idx: integer;
begin
  if (fCpuRef = nil) then Exit;
  if (aRow = 0) then Exit;

  // Get disassembly data for this row from CPU's trace buffer
  Data := fCpuRef.GetTrace(aRow-1);
  if (aRow > 0) then                    // Row 0 = heading row
    case aCol of                        // Update each column as required
      0:  DrawGrid1.Canvas.TextOut(aRect.Left+2, aRect.Top, Format('%.4x', [Data.Addr]));
      1:  DrawGrid1.Canvas.TextOut(aRect.Left+2, aRect.Top, Data.BytesStr);
      2:  DrawGrid1.Canvas.TextOut(aRect.Left+2, aRect.Top, '=');
      3:  DrawGrid1.Canvas.TextOut(aRect.Left+2, aRect.Top, Data.MnemStr);
      4:  DrawGrid1.Canvas.TextOut(aRect.Left+2, aRect.Top,
              Format(Data.AddrModeStr, [Data.OperandStr]));
    else
      begin
        idx := aCol - 5;                // or get register data to show
        if (idx < Length(Data.RegStr)) then
          DrawGrid1.Canvas.TextOut(aRect.Left+2, aRect.Top, Data.RegStr[idx]);
      end;
    end;
end;


{ BUTTON CLEAR CLICK  }

procedure TTraceFrame.btnClearClick(Sender: TObject);
begin
  DrawGrid1.Clear;
  fCpuRef.ResetTrace;
end;


end.

