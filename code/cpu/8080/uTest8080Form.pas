{ =============================================================================

  TEST 8080 CODE

    Builds a simple local machine to run the 8080 test code in

    Tests based on information at:
    http://www.emulator101.com/full-8080-emulation.html

  ============================================================================}

unit uTest8080Form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  //
  uCpu8080, uCpuTypes, uIniFile, uCommon;

type

  { TTest8080Form }

  TTest8080Form = class(TForm)
    btnExit: TButton;
    btnRun: TButton;
    Image1: TImage;
    memoLog: TMemo;
    Panel1: TPanel;
    procedure btnExitClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    CPU: TCpu8080;
    Memory: PByte;
    Message: string;
    procedure Load(FileName: string);
    procedure MemWrite(Sender: TObject; Addr: word; Value: byte);
    procedure MemRead(Sender: TObject; Addr: word; var Value: byte);
    procedure RunTest;
    procedure ProcessBDOS;
    procedure Log(logtext: string);
  public
  end;

var
  Test8080Form: TTest8080Form;


implementation

{$R *.lfm}

const
  SECT_PREFS     = 'Test8080';

{ CREATE }

procedure TTest8080Form.FormCreate(Sender: TObject);
begin
  Left := AppIni.ReadInteger(SECT_PREFS, INI_WDW_LEFT, 100);
  Top := AppIni.ReadInteger(SECT_PREFS, INI_WDW_TOP, 60);
  //
  CPU := TCpu8080.Create(ct8080asmO);
  CPU.OnRead := @MemRead;
  CPU.OnWrite := @MemWrite;
  GetMem(Memory, 65536);                // 64K
  Memory[$0005] := $C9;                 // Force RET after processing BDOS call
end;


{ DESTROY }

procedure TTest8080Form.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_PREFS, INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_PREFS, INI_WDW_LEFT, Left);
  //
  FreeMem(Memory);
  CPU.Free;
  inherited;
end;


{ BUTTON - RUN TEST }

procedure TTest8080Form.btnRunClick(Sender: TObject);
begin
  try
    // Load test binary file
    Load('cpudiag.bin');                // Loads to $100

    // Put JMP $100, where binary assembled, into location $0000 (reset vector)
    Memory[0] := $C3;
    Memory[1] := $00;
    Memory[2] := $01;

    // Currently skipping DAA test by jumping past it
    Memory[$059C] := $C3;
    Memory[$059D] := $C2;
    Memory[$059E] := $05;

    // ... and run test
    RunTest;
  except
    on E: Exception do
      memoLog.Lines.Add(E.ClassName + ': ' + E.Message);
  end;
end;


{ BUTTON - EXIT }

procedure TTest8080Form.btnExitClick(Sender: TObject);
begin
  Close;
end;


{ LOAD TEST FILE }

procedure TTest8080Form.Load(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(GetAppResourcesDirectory + '8080' + DIRECTORY_SEPARATOR + Filename, fmOpenRead);
  try
    Stream.Read(Memory[$100], Stream.Size);
  finally
    Stream.Free;
  end;
end;


{ RUN TEST }

procedure TTest8080Form.RunTest;
var
  LastPC: Word;
  StartTime: TTimeStamp;
  Duration: longint;
begin
  CPU.Reset;                           // Jump to $0000

  Message := '';
  StartTime := DateTimeToTimeStamp(Now);
  repeat
    if (CPU.Regs.PC = $0005) then
      ProcessBDOS;
      // Then fall through with PC = $0005 = RET

    LastPC := CPU.Regs.PC;
    CPU.ExecuteInstruction;
    Application.ProcessMessages;
    until (CPU.Regs.PC = 0)             // WBOOT
       or (CPU.Regs.PC = LastPC);       // Stuck in a loop?

  Duration := DateTimeToTimeStamp(Now).Time - StartTime.Time;

  Log('');
  Log('Test code output message is:');
  Log(Message);
  Log('');
  Log(Format('Test completed in %d ms', [Duration]));
  Log('Last address $' + Format('%.4x', [CPU.Regs.PC]));
end;


{ MEMORY READ / WRITE }

procedure TTest8080Form.MemRead(Sender: TObject; Addr: word; var Value: byte);
begin
  Value := Memory[Addr];
end;


procedure TTest8080Form.MemWrite(Sender: TObject; Addr: word; Value: byte);
begin
  Memory[Addr] := Value;
end;


{ PROCESS BDOS }

procedure TTest8080Form.ProcessBDOS;
var
  DE: word;
begin
  case CPU.Regs.C of
    2: begin
         Message := Message + char(CPU.Regs.A);
       end;
    9: begin
         DE := (CPU.Regs.D shl 8) + CPU.Regs.E;
         DE := DE + 3;                  // Skip first 3 bytes (CLS/CR/LF)
         while (Memory[DE] <> Ord('$')) do
           begin
             Message := Message + char(Memory[DE]);
             Inc(DE);
           end;
       end
  end;
end;


{ LOG IN MEMO }

procedure TTest8080Form.Log(logtext: string);
begin
  memoLog.Lines.Add(logtext);
end;


end.

