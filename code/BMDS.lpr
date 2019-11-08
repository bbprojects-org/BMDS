program BMDS;

{$mode objfpc}{$H+}

{$DEFINE debug}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF debug}
    SysUtils,
  {$ENDIF}
  Interfaces,                           // this includes the LCL widgetset
  Forms, uMainForm;

{$R *.res}

begin
  {$IFDEF DEBUG}
    // Set up -gh output for the Leakview package:
    if FileExists('/developer/heap.trc') then
      DeleteFile('/developer/heap.trc');
    SetHeapTraceOutput('/developer/heap.trc');
  {$ENDIF DEBUG}

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

