///////////////////////////////////////////////////////////////////////////////
//
// DEBUGGER
//
// From: \Microtan65\src\Debugger\
//
// Breakpoints manager, etc
//
///////////////////////////////////////////////////////////////////////////////

unit uDebugger;

interface

uses
  Classes, uAssembler;

type
  TDebuggerState = (dsStopped, dsRunning, dsPaused);

  TBreakpointChangeEvent = procedure(Sender: TObject; Line: integer) of object;
  TDebuggerStateChangeEvent = procedure(Sender: TObject; OldState, NewState: TDebuggerState) of object;

  TDebugger = class
  private
    fDebuggerState: TDebuggerState;
    fWantedState: TDebuggerState;
    fLinesInfo: TArrayLineInfo;
    fCurrentLine: integer;
    fLineToStop: integer;
    fBreakpoints: TList;
    //
    fOnBreakpointChange: TBreakpointChangeEvent;
    fOnStateChange: TDebuggerStateChangeEvent;
    fOnCurrentLineChange: TNotifyEvent;
    procedure DoCurrentLineChanged;
    procedure DoOnBreakpointChanged(Line: integer);
  public
    constructor Create;
    destructor  Destroy; override;

    // DEBUG ACTIVITIES
    function  CanGotoCursor(Line: integer): boolean;
    function  CanPause: boolean;
    function  CanRun: boolean;
    function  CanStep: boolean;
    function  CanStop: boolean;
    procedure Pause;
    procedure Run;
    procedure Step;
    procedure Stop;
    
    //
    procedure DoStateChange;
    function  IsExecutableLine(Line: integer): boolean;
    property  LineInfo: TArrayLineInfo read fLinesInfo write fLinesInfo;

    // BREAKPOINTS
    procedure ToggleBreakpoint(Line: integer);
    procedure ClearAllBreakpoints;
    function  HasBreakpoints: boolean;
    function  IsBreakpointLine(Line: integer): boolean;
    //
    property  OnBreakpointChange: TBreakpointChangeEvent read fOnBreakpointChange write fOnBreakpointChange;
    property  OnStateChange: TDebuggerStateChangeEvent read fOnStateChange write fOnStateChange;
    property  OnCurrentLineChange: TNotifyEvent read fOnCurrentLineChange write fOnCurrentLineChange;
  end;


implementation

constructor TDebugger.Create;
begin
  fDebuggerState := dsStopped;
  fBreakpoints := TList.Create;
  fCurrentLine := -1;
end;


destructor TDebugger.Destroy;
begin
  fBreakpoints.Free;
  inherited;
end;



///////////////////////////////////////////////////////////////////////////////
//
// DEBUG OPERATIONS
//
///////////////////////////////////////////////////////////////////////////////

function TDebugger.CanGotoCursor(Line: integer): boolean;
begin
  Result := ((fDebuggerState <> dsRunning) and IsExecutableLine(Line));
end;


function TDebugger.CanPause: boolean;
begin
  Result := (fDebuggerState = dsRunning);
end;


function TDebugger.CanRun: boolean;
begin
  Result := (fDebuggerState <> dsRunning);
end;


function TDebugger.CanStep: boolean;
begin
  Result := (fDebuggerState <> dsRunning);
end;


function TDebugger.CanStop: boolean;
begin
  Result := (fDebuggerState <> dsStopped);
end;


procedure TDebugger.Pause;
begin
  if (fDebuggerState = dsRunning) then
    fWantedState := dsPaused;
end;


procedure TDebugger.Run;
//var
//  dwTime: DWORD;
begin
  fWantedState := dsRunning;
  DoStateChange;
{
  dwTime := GetTickCount + 100;
  repeat
    if GetTickCount >= dwTime then begin
      DoYield;
      dwTime := GetTickCount + 100;
    end;
    Step;
    if fWantedState <> fDebuggerState then
      DoStateChange;
  until fDebuggerState <> dsRunning;
  fLineToStop := -1;
}
end;


procedure TDebugger.Step;
begin
{
  if (fDebuggerState = dsStopped) then
    begin
      fNextInstruction := Low(SampleCode);
      fCurrentLine := SampleCode[fNextInstruction].Line;
      fWantedState := dsPaused;
      DoStateChange;
    end
  else
    begin
      Sleep(50);
      fNextInstruction := fNextInstruction + SampleCode[fNextInstruction].Delta;
      fCurrentLine := SampleCode[fNextInstruction].Line;
      case fDebuggerState of
        dsRunning:
          begin
            if CurrentLineIsBreakpoint then
              fWantedState := dsPaused;
          end;
      else
        DoCurrentLineChanged;
      end;
    end;
}
end;


procedure TDebugger.Stop;
begin
  fWantedState := dsStopped;
  DoStateChange;
end;


procedure TDebugger.DoStateChange;
begin
  if (fDebuggerState <> fWantedState) then
    begin
      if (fWantedState = dsStopped) then
        fCurrentLine := -1;
      if Assigned(fOnStateChange) then
        fOnStateChange(Self, fDebuggerState, fWantedState);
      fDebuggerState := fWantedState;
      if (fWantedState <> dsRunning) then
        fLineToStop := -1;
      DoCurrentLineChanged;
    end;
end;


///////////////////////////////////////////////////////////////////////////////


function TDebugger.IsExecutableLine(Line: integer): boolean;
var
  idx: integer;
begin
  Result := FALSE;
  for idx := 0 to Length(fLinesInfo) - 1 do
    begin
      // NEED TO CHECK SourceFileIndex AS WELL
      if (fLinesInfo[idx].LineNumber = Line) then
        begin
          Result := TRUE;
          break;
        end;
    end;
end;


///////////////////////////////////////////////////////////////////////////////
//
// MANAGE BREAKPOINTS
//
///////////////////////////////////////////////////////////////////////////////

procedure TDebugger.ToggleBreakpoint(Line: integer);
var
  SetBreakpoint: boolean;
  idx: integer;
begin
  if ((Line > 0) and IsExecutableLine(Line)) then
    begin
      SetBreakpoint := TRUE;
      for idx := 0 to fBreakpoints.Count - 1 do
        begin
          if (integer(fBreakpoints[idx]) = Line) then
            begin
              fBreakpoints.Delete(idx);
              SetBreakpoint := FALSE;
              break;
            end
          else
            if (integer(fBreakpoints[idx]) > Line) then
              begin
              fBreakpoints.Insert(idx, pointer(Line));
              SetBreakpoint := FALSE;
              break;
            end;
        end;
      if (SetBreakpoint) then
        fBreakpoints.Add(pointer(Line));
      DoOnBreakpointChanged(Line);
    end;
end;


procedure TDebugger.ClearAllBreakpoints;
begin
  if (fBreakpoints.Count > 0) then
    begin
      fBreakpoints.Clear;
      DoOnBreakpointChanged(-1);
    end;
end;


function TDebugger.IsBreakpointLine(Line: integer): boolean;
var
  idx: integer;
begin
  Result := FALSE;
  if (Line > 0) then
    begin
      idx := fBreakpoints.Count - 1;
      while idx >= 0 do
        begin
          if (integer(fBreakpoints[idx]) = Line) then
            begin
              Result := TRUE;
              break;
            end;
          Dec(idx);
        end;
    end;
end;


function TDebugger.HasBreakpoints: boolean;
begin
  Result := (fBreakpoints.Count > 0);
end;


procedure TDebugger.DoOnBreakpointChanged(Line: integer);
begin
  if Assigned(fOnBreakpointChange) then
    fOnBreakpointChange(Self, Line);
end;


procedure TDebugger.DoCurrentLineChanged;
begin
  if Assigned(fOnCurrentLineChange) then
    fOnCurrentLineChange(Self);
end;

end.
