{ ==============================================================================

  TRACE FORM

  Provides buffering for execution trace when running, followed by display
  when stopped. Also provides immediate trace for single stepping

  Saves form position and size in application's INI file, for each machine

  =============================================================================}

unit uTraceForm;
                                                                                { TODO : uTraceForm -> ensure always see bottom line }
{$mode objfpc}
{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, ExtCtrls, Grids, Dialogs, StdCtrls,
  //
  uRegistersFormBase, uCpuBase, uIniFile, uCommon;

type

  { TTraceForm }

  TTraceForm = class(TForm)
    btnClear: TButton;
    DrawGrid1: TDrawGrid;
    procedure btnClearClick(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fCPU: TCpuBase;
    procedure SetCPU(AValue: TCpuBase);
    procedure SetRows(AValue: integer);
  public
    property CPU: TCpuBase write SetCPU;
    property Rows: integer write SetRows;
  end;

var
  TraceForm: TTraceForm;


implementation

uses
  uMainForm;

{$R *.lfm}

const
  INI_PREFIX = 'Trace';


procedure TTraceForm.FormCreate(Sender: TObject);
var
  IniSect: string;
begin
(*
  IniSect := GetIniSect;
  Top := IniReadInteger(IniSect, INI_PREFIX + WDW_TOP,
         MainForm.Machine.ScreenPosition.Y - TOP_BAR_HEIGHT);
  Left := IniReadInteger(IniSect, INI_PREFIX + WDW_LEFT,
          RegistersForm.Left + RegistersForm.Width);
  Width := IniReadInteger(IniSect, INI_PREFIX + WDW_WIDTH, 424);
  Height := IniReadInteger(IniSect, INI_PREFIX + WDW_HEIGHT,
            MainForm.Machine.ScreenSize.Y);
*)
  DrawGrid1.RowCount := 0;
end;


procedure TTraceForm.FormDestroy(Sender: TObject);
var
  IniSect: string;
begin
(*
  IniSect := GetIniSect;
  IniWriteInteger(IniSect, INI_PREFIX + WDW_TOP, Top);
  IniWriteInteger(IniSect, INI_PREFIX + WDW_LEFT, Left);
  IniWriteInteger(IniSect, INI_PREFIX + WDW_WIDTH, Width);
  IniWriteInteger(IniSect, INI_PREFIX + WDW_HEIGHT, Height);
*)
end;


{ SET CPU }

{ Sets grid column parameters to reflect specific CPU in use }

procedure TTraceForm.SetCPU(AValue: TCpuBase);
var
  i: integer;
  col: TGridColumn;
  ColumnsArray: TTraceColArray;
begin
  fCPU := AValue;                       // Reference to CPU
  ColumnsArray := fCPU.TraceColumns;    // Get specific column data for CPU
  DrawGrid1.Columns.Clear;
  for i := 0 to Length(ColumnsArray)-1 do
  begin
    col := DrawGrid1.Columns.Add;       // and create columns
    col.Title.Caption := ColumnsArray[i].Title;
    col.Width := ColumnsArray[i].Width;
    col.Alignment := ColumnsArray[i].Align;
  end;
end;


{ SET ROWS }

procedure TTraceForm.SetRows(AValue: integer);
begin
  DrawGrid1.RowCount := AValue;
end;


{ DISPLAY RESULTS }

{ Usually when machine execution stopped }

procedure TTraceForm.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Data: TDisassembledData;
  idx: integer;
begin
  // Get disassembly data for this row from CPU's trace buffer
  Data := fCPU.GetTrace(aRow-1);
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


{ CLEAR TRACE }

procedure TTraceForm.btnClearClick(Sender: TObject);
begin
  DrawGrid1.Clear;
  fCPU.ResetTrace;
end;


end.
