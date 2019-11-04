program Test6502;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces,
  Test6502U in 'Test6502U.pas' {Test6502Form},
  u6502defs in '..\u6502defs.pas',
  u6502emu in '..\u6502emu.pas',
  uCpuBase in '..\..\uCpuBase.pas';

begin
  Application.Initialize;
  Application.CreateForm(TTest6502Form, Test6502Form);
  Application.Run;
end.
