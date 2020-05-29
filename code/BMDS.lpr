program BMDS;

{$mode objfpc}{$H+}

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
  Forms, uMainForm, uMachineMicrotan, uMachineSpaceInvaders, uMachineChip8,
  uCommon;

{$R *.res}

{$IFDEF debug}
var
  FileName: string;
{$ENDIF}

begin
  {$IFDEF debug}
    // Set up -gh output for the Leakview package:
    FileName := GetAppDataDirectory + 'heap.trc';
    if FileExists(FileName) then
      DeleteFile(FileName);
    SetHeapTraceOutput(FileName);
  {$ENDIF}

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

