{ =============================================================================

  TEST 6502 CODE

  Runs test code from Klaus Dormann to verify operation of the 6502 processor
  emulation code. Code version 04-dec-2017, refer to:
  https://github.com/Klaus2m5/6502_65C02_functional_tests

  Builds a simple local machine to run the test code in
  based on code from https://github.com/Dennis1000/mos6502-delphi

  ============================================================================}

unit uTest6502Form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  //
  uCpu6502, uCpuTypes, uIniFile, uCommon;

type
  { TTest6502Form }

  TTest6502Form = class(TForm)
    btnRun: TButton;
    btnExit: TButton;
    btnStop: TButton;
    Image1: TImage;
    lblRunning: TLabel;
    memoLog: TMemo;
    Panel1: TPanel;
    Timer1: TTimer;
    procedure btnExitClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    CPU: TCpu6502;
    Memory: PByte;
    StopFlag: boolean;
    AliveCount: integer;
    procedure Load(FileName: string);
    procedure MemWrite(Sender: TObject; Addr: word; Value: byte);
    procedure MemRead(Sender: TObject; Addr: word; var Value: byte);
    procedure RunTest(EndAddress: word);
    procedure Log(logtext: string);
    procedure SetButtons(State: boolean);
  public
  end;

var
  Test6502Form: TForm;


implementation

{$R *.lfm}

const
  SECT_PREFS     = 'Test6502';
  ALIVECOUNT_MAX = 10;

{ CREATE}

procedure TTest6502Form.FormCreate(Sender: TObject);
begin
  Left := AppIni.ReadInteger(SECT_PREFS, INI_WDW_LEFT, 100);
  Top := AppIni.ReadInteger(SECT_PREFS, INI_WDW_TOP, 60);
  SetButtons(False);
  //
  CPU := TCpu6502.Create(ct6502);
  CPU.OnRead := @MemRead;
  CPU.OnWrite := @MemWrite;
  GetMem(Memory, 65536);
end;


{ DESTROY }

procedure TTest6502Form.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_PREFS, INI_WDW_TOP, Top);
  AppIni.WriteInteger(SECT_PREFS, INI_WDW_LEFT, Left);
  //
  FreeMem(Memory);
  CPU.Free;
end;


{ BUTTON - RUN TEST }

procedure TTest6502Form.btnRunClick(Sender: TObject);
begin
  try
    // Load test binary file
    Load('6502_functional_test.bin');

    // Set reset vectors to $0400
    Memory[$FFFC] := $00;
    Memory[$FFFD] := $04;

    // ... and run test suite, if PC reaches $3469 then test is successful
    RunTest($3469);
  except
    on E: Exception do
      memoLog.Lines.Add(E.ClassName + ': ' + E.Message);
  end;
end;


{ BUTTON - STOP }

procedure TTest6502Form.btnStopClick(Sender: TObject);
begin
  StopFlag := True;
end;


{ BUTTON - EXIT }

procedure TTest6502Form.btnExitClick(Sender: TObject);
begin
  Close;
end;


{ LOAD TEST FILE }

procedure TTest6502Form.Load(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(GetAppResourcesDirectory + '6502' + DIRECTORY_SEPARATOR + Filename, fmOpenRead);
  try
    Stream.Read(Memory[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;


{ RUN TEST }

procedure TTest6502Form.RunTest(EndAddress: Word);
var
  LastPC: Word;
  LastTest: Byte;
  StartTime: TTimeStamp;
  Duration: longint;
begin
  AliveCount := 0;
  SetButtons(True);
  Timer1.Enabled := True;

  StopFlag := False;
  CPU.Reset;                            // Jump to $400

  Log('');
  LastTest := $FF;
  StartTime := DateTimeToTimeStamp(Now);
  repeat
    LastPC := CPU.Regs.PC;
    if (Memory[$200] <> LastTest) then
      begin
        if (LastTest <> $FF) then
          begin
            duration := DateTimeToTimeStamp(Now).Time - StartTime.Time;
            if (duration > 1000) then
              Log(Format('  completed in %d seconds', [Round(duration/1000)]));
          end;
        StartTime := DateTimeToTimeStamp(Now);
        LastTest := Memory[$200];
        Log('test case ' + LastTest.ToString + ' at $' + IntToHex(CPU.Regs.PC, 4));
      end;
    Cpu.ExecuteInstruction;
    Application.ProcessMessages;
  until (CPU.InvalidOpcode)             // Oops, bad opcode
     or (CPU.Regs.PC = LastPC)          // Locked up
     or (CPU.Regs.PC = EndAddress)      // Finished
     or (StopFlag);                     // User stopped

  Timer1.Enabled := False;
  SetButtons(False);

  Log('');
  if (CPU.Regs.PC = EndAddress) then
    Log('Test successful!')
  else if (StopFlag) then
    Log('Stopped by user. PC = $' + Format('%.4x', [CPU.Regs.PC]))
  else
    Log('Failed at $' + Format('%.4x', [CPU.Regs.PC]));
end;


{ MEMORY READ / WRITE }

procedure TTest6502Form.MemRead(Sender: TObject; Addr: word; var Value: byte);
begin
  Value := Memory[Addr];
end;


procedure TTest6502Form.MemWrite(Sender: TObject; Addr: word; Value: byte);
begin
  Memory[Addr] := Value;
end;


{ LOG IN MEMO }

procedure TTest6502Form.Log(logtext: string);
begin
  memoLog.Lines.Add(logtext);
end;


{ SET BUTTON STATES - State = True = running }

procedure TTest6502Form.SetButtons(State: boolean);
begin
  btnRun.Enabled := not State;
  btnStop.Enabled := State;
  btnExit.Enabled := not State;
  if (State) then
    lblRunning.Caption := 'Running'
  else
    lblRunning.Caption := '';
end;


{ STILL ALIVE TIMER }

procedure TTest6502Form.Timer1Timer(Sender: TObject);
begin
  if (AliveCount = ALIVECOUNT_MAX) then
    begin
      AliveCount := 0;
      lblRunning.Caption := 'Running';
    end;
  lblRunning.Caption := lblRunning.Caption + '.';
  Inc(AliveCount);
end;


end.

