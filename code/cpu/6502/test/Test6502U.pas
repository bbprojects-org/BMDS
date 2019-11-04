///////////////////////////////////////////////////////////////////////////////
//
//  TEST 6502 - Runs a number of tests against the 6502 emulator
//
//  The tests here are based on the JavaScript test code for David Caldwell's
//  Apple II emulator (http://porkrind.org/a2/)
//
//  { TODO : Need to expand to test all opcodes, not just few here }
//
///////////////////////////////////////////////////////////////////////////////

unit Test6502U;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  uCpuBase, u6502defs, u6502emu;

type
  TCreateCpu = function: TCpuBase;

  TRegIdx = (rA, rX, rY, rSP, rPC);
  TFlagIdx = (fC, fZ, fI, fD, fB, fV, fN);

  TReg = record
    Value: word;
    Checked: boolean;
  end;

  TFlag = record
    Value: boolean;
    Checked: boolean;
  end;

  TTest6502Form = class(TForm)
    panelRight: TPanel;
    panelLeft: TPanel;
    btnTest: TButton;
    memoResults: TMemo;
    btnClose: TButton;
    lblCPU: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    fCPU: TCpuBase;
    fMemory: array[0..$FFFF] of byte;
    SaveRegs: array[rA..rPC] of TReg;
    SaveFlags: array [fC..fN] of TFlag;
    sTestCode: string;
    sResult: string;
    procedure MemRead(Sender: TObject; Addr: word; var Value: byte);
    procedure MemWrite(Sender: TObject; Addr: word; Value: byte);
    procedure Init;
    procedure T(value: string);
    procedure SetA(value: byte);
    procedure SetX(value: byte);
    procedure SetY(value: byte);
    procedure SetPC(value: word);
    procedure SetC(value: byte);
    procedure SetD(value: byte);
    procedure SetM(addr: word; value: array of byte);
    procedure SetOp(op: array of byte);
    procedure Test;
    procedure CheckA(value: byte);
    procedure CheckSP(value: byte);
    procedure CheckPC(value: word);
    procedure CheckM(addr: word; value: array of byte);
    procedure Error2(Item: string; Expected, Got: byte);
    procedure Error4(Item: string; Expected, Got: word);
    procedure CheckSR(value: string);
    procedure TestUncheckedRegs;
    procedure TestUncheckedFlags;
    procedure SetN(value: byte);
    procedure SetZ(value: byte);
    procedure SetV(value: byte);
    procedure SetI(value: byte);
    procedure SetSP(value: byte);
    procedure SetB(value: byte);
  public
    //
  end;

var
  Test6502Form: TTest6502Form;

const
  ERR_MSG = '*** ERROR:';
  CHECK_STATE = 'PSW:';


implementation

{$R *.lfm}

procedure TTest6502Form.FormCreate(Sender: TObject);
var
  testInfo: TCpuInfo;
begin
  FillChar(fMemory, SizeOf(fMemory), Random(256));  // Randomize memory
  //
  fCPU := T6502.Create;
  //
  testInfo := fCPU.Info;
  lblCPU.Caption := testInfo.Name;
  //
  fCPU.OnRead := @MemRead;
  fCPU.OnWrite := @MemWrite;
  fCPU.Reset;                           // Initialise CPU state
end;

procedure TTest6502Form.FormDestroy(Sender: TObject);
begin
  fCPU.Free;
end;


//
// TEST SEQUENCES
//

procedure TTest6502Form.Init;
var
  i: TRegIdx;
  j: TFlagIdx;
  PSW: byte;
begin
  fCPU.RegisterValue[RegA] := Random(256);
  fCPU.RegisterValue[RegX] := Random(256);
  fCPU.RegisterValue[RegY] := Random(256);
  fCPU.RegisterValue[RegSP] := $E3;     // Arbitrary, want to ensure there is a stack
  fCPU.RegisterValue[RegPC] := $400;    // Arbitrary
  sResult := ERR_MSG;
  //
  for i := rA to rPC do                 // Save register state for checking later
    SaveRegs[i].Checked := FALSE;
  SaveRegs[rA].Value := fCPU.RegisterValue[RegA];
  SaveRegs[rX].Value := fCPU.RegisterValue[RegX];
  SaveRegs[rY].Value := fCPU.RegisterValue[RegY];
  SaveRegs[rSP].Value := fCPU.RegisterValue[RegSP];
  for j := fC to fN do                  // Save flag states for checking later
    SaveFlags[j].Checked := FALSE;
  PSW := fCPU.RegisterValue[RegPSW];
  SaveFlags[fC].Value := (PSW and P_CARRY) > 0;
  SaveFlags[fZ].Value := (PSW and P_ZERO) > 0;
  SaveFlags[fI].Value := (PSW and P_IRQ) > 0;
  SaveFlags[fD].Value := (PSW and P_DECIMAL) > 0;
  SaveFlags[fB].Value := (PSW and P_BRK) > 0;
  SaveFlags[fV].Value := (PSW and P_OVERFLOW) > 0;
  SaveFlags[fN].Value := (PSW and P_NEGATIVE) > 0;
end;


procedure TTest6502Form.btnTestClick(Sender: TObject);
begin
  memoResults.Lines.Clear;
  memoResults.Lines.Add('Tests started');

  T('adc imm');         Init; SetA($01); SetD(0); SetC(0); SetOp([$69, $02]); Test; CheckA($03); CheckSR('nzcv');
  T('adc imm+C');       Init; SetA($01); SetD(0); SetC(1); SetOp([$69, $02]); Test; CheckA($04); CheckSR('nzcv');
  T('adc imm:C');       Init; SetA($f0); SetD(0); SetC(0); SetOp([$69, $20]); Test; CheckA($10); CheckSR('nzCv');
  T('adc imm+C:Z');     Init; SetA($f0); SetD(0); SetC(1); SetOp([$69, $0f]); Test; CheckA($00); CheckSR('nZCv');
  T('adc imm:V');       Init; SetA($60); SetD(0); SetC(1); SetOp([$69, $40]); Test; CheckA($a1); CheckSR('NzcV');
  T('adc imm:V.2');     Init; SetA($40); SetD(0); SetC(1); SetOp([$69, $60]); Test; CheckA($a1); CheckSR('NzcV');
  T('adc imm:V.3');     Init; SetA($f0); SetD(0); SetC(0); SetOp([$69, $60]); Test; CheckA($50); CheckSR('nzCv');
  T('adc imm:V.4');     Init; SetA($60); SetD(0); SetC(0); SetOp([$69, $f0]); Test; CheckA($50); CheckSR('nzCv');

  T('adc imm+D');       Init; SetA($01); SetD(1); SetC(0); SetOp([$69, $02]); Test; CheckA($03); CheckSR('zc'); // Does Dec mode set oVerflow or
  T('adc imm+D+10');    Init; SetA($04); SetD(1); SetC(0); SetOp([$69, $08]); Test; CheckA($12); CheckSR('zc'); // Negative?
  T('adc imm+D');       Init; SetA($01); SetD(1); SetC(0); SetOp([$69, $0a]); Test; CheckA($11); CheckSR('zc');
  T('adc imm+D+C');     Init; SetA($01); SetD(1); SetC(1); SetOp([$69, $02]); Test; CheckA($04); CheckSR('zc');
  T('adc imm+D:C');     Init; SetA($90); SetD(1); SetC(0); SetOp([$69, $20]); Test; CheckA($10); CheckSR('zC');
  T('adc imm+D+C:Z');   Init; SetA($90); SetD(1); SetC(1); SetOp([$69, $09]); Test; CheckA($00); CheckSR('ZC');

  T('adc zp');          Init; SetA($01); SetD(0); SetC(0); SetM($50,[$02]); SetOp([$65, $50]); Test; CheckA($03); CheckSR('nzcv');
  T('adc zp+C');        Init; SetA($01); SetD(0); SetC(1); SetM($50,[$02]); SetOp([$65, $50]); Test; CheckA($04); CheckSR('nzcv');
  T('adc zp:C');        Init; SetA($f0); SetD(0); SetC(0); SetM($50,[$20]); SetOp([$65, $50]); Test; CheckA($10); CheckSR('nzCv');
  T('adc zp+C:Z');      Init; SetA($f0); SetD(0); SetC(1); SetM($50,[$0f]); SetOp([$65, $50]); Test; CheckA($00); CheckSR('nZCv');
  T('adc zp:V');        Init; SetA($60); SetD(0); SetC(1); SetM($50,[$40]); SetOp([$65, $50]); Test; CheckA($a1); CheckSR('NzcV');

  T('adc zp,x');        Init; SetA($01); SetD(0); SetC(0); SetX($01); SetM($51,[$02]); SetOp([$75, $50]); Test; CheckA($03); CheckSR('nzcv');
  T('adc zp,x+C');      Init; SetA($01); SetD(0); SetC(1); SetX($80); SetM($20,[$02]); SetOp([$75, $a0]); Test; CheckA($04); CheckSR('nzcv');
  T('adc zp,x:C');      Init; SetA($f0); SetD(0); SetC(0); SetX($00); SetM($50,[$20]); SetOp([$75, $50]); Test; CheckA($10); CheckSR('nzCv');
  T('adc zp,x+C:Z');    Init; SetA($f0); SetD(0); SetC(1); SetX($ff); SetM($4f,[$0f]); SetOp([$75, $50]); Test; CheckA($00); CheckSR('nZCv');
  T('adc zp,x:V');      Init; SetA($60); SetD(0); SetC(1); SetX($10); SetM($60,[$40]); SetOp([$75, $50]); Test; CheckA($a1); CheckSR('NzcV');

  T('adc abs');         Init; SetA($01); SetD(0); SetC(0); SetM($1234,[$02]); SetOp([$6d, $34, $12]); Test; CheckA($03); CheckSR('nzcv');
  T('adc abs+C');       Init; SetA($01); SetD(0); SetC(1); SetM($0050,[$02]); SetOp([$6d, $50, $00]); Test; CheckA($04); CheckSR('nzcv');
  T('adc abs:C');       Init; SetA($f0); SetD(0); SetC(0); SetM($55aa,[$20]); SetOp([$6d, $aa, $55]); Test; CheckA($10); CheckSR('nzCv');
  T('adc abs+C:Z');     Init; SetA($f0); SetD(0); SetC(1); SetM($aa55,[$0f]); SetOp([$6d, $55, $aa]); Test; CheckA($00); CheckSR('nZCv');
  T('adc abs:V');       Init; SetA($60); SetD(0); SetC(1); SetM($3254,[$40]); SetOp([$6d, $54, $32]); Test; CheckA($a1); CheckSR('NzcV');

  T('adc abs,x');       Init; SetA($01); SetD(0); SetC(0); SetX($01); SetM($1235,[$02]); SetOp([$7d, $34, $12]); Test; CheckA($03); CheckSR('nzcv');
  T('adc abs,x+C');     Init; SetA($01); SetD(0); SetC(1); SetX($80); SetM($0120,[$02]); SetOp([$7d, $a0, $00]); Test; CheckA($04); CheckSR('nzcv');
  T('adc abs,x:C');     Init; SetA($f0); SetD(0); SetC(0); SetX($00); SetM($55aa,[$20]); SetOp([$7d, $aa, $55]); Test; CheckA($10); CheckSR('nzCv');
  T('adc abs,x+C:Z');   Init; SetA($f0); SetD(0); SetC(1); SetX($ff); SetM($ab54,[$0f]); SetOp([$7d, $55, $aa]); Test; CheckA($00); CheckSR('nZCv');
  T('adc abs,x:V');     Init; SetA($60); SetD(0); SetC(1); SetX($10); SetM($3264,[$40]); SetOp([$7d, $54, $32]); Test; CheckA($a1); CheckSR('NzcV');

  T('adc abs,y');       Init; SetA($01); SetD(0); SetC(0); SetY($01); SetM($1235,[$02]); SetOp([$79, $34, $12]); Test; CheckA($03); CheckSR('nzcv');
  T('adc abs,y+C');     Init; SetA($01); SetD(0); SetC(1); SetY($80); SetM($0120,[$02]); SetOp([$79, $a0, $00]); Test; CheckA($04); CheckSR('nzcv');
  T('adc abs,y:C');     Init; SetA($f0); SetD(0); SetC(0); SetY($00); SetM($55aa,[$20]); SetOp([$79, $aa, $55]); Test; CheckA($10); CheckSR('nzCv');
  T('adc abs,y+C:Z');   Init; SetA($f0); SetD(0); SetC(1); SetY($ff); SetM($ab54,[$0f]); SetOp([$79, $55, $aa]); Test; CheckA($00); CheckSR('nZCv');
  T('adc abs,y:V');     Init; SetA($60); SetD(0); SetC(1); SetY($10); SetM($3264,[$40]); SetOp([$79, $54, $32]); Test; CheckA($a1); CheckSR('NzcV');

  T('adc (ind,x)');     Init; SetA($01); SetD(0); SetC(0); SetX($01); SetM($35,[$12,$10]); SetM($1012,[$02]); SetOp([$61, $34]); Test; CheckA($03); CheckSR('nzcv');
  T('adc (ind,x)+C');   Init; SetA($01); SetD(0); SetC(1); SetX($80); SetM($20,[$34,$20]); SetM($2034,[$02]); SetOp([$61, $a0]); Test; CheckA($04); CheckSR('nzcv');
  T('adc (ind,x):C');   Init; SetA($f0); SetD(0); SetC(0); SetX($00); SetM($aa,[$56,$30]); SetM($3056,[$20]); SetOp([$61, $aa]); Test; CheckA($10); CheckSR('nzCv');
  T('adc (ind,x)+C:Z'); Init; SetA($f0); SetD(0); SetC(1); SetX($ff); SetM($54,[$78,$40]); SetM($4078,[$0f]); SetOp([$61, $55]); Test; CheckA($00); CheckSR('nZCv');
  T('adc (ind,x):V');   Init; SetA($60); SetD(0); SetC(1); SetX($10); SetM($64,[$9a,$50]); SetM($509a,[$40]); SetOp([$61, $54]); Test; CheckA($a1); CheckSR('NzcV');

  T('adc (ind,y)');     Init; SetA($01); SetD(0); SetC(0); SetY($01); SetM($34,[$12,$10]); SetM($1013,[$02]); SetOp([$71, $34]); Test; CheckA($03); CheckSR('nzcv');
  T('adc (ind,y)+C');   Init; SetA($01); SetD(0); SetC(1); SetY($80); SetM($50,[$34,$20]); SetM($20b4,[$02]); SetOp([$71, $50]); Test; CheckA($04); CheckSR('nzcv');
  T('adc (ind,y):C');   Init; SetA($f0); SetD(0); SetC(0); SetY($00); SetM($aa,[$56,$30]); SetM($3056,[$20]); SetOp([$71, $aa]); Test; CheckA($10); CheckSR('nzCv');
  T('adc (ind,y)+C:Z'); Init; SetA($f0); SetD(0); SetC(1); SetY($ff); SetM($55,[$78,$40]); SetM($4177,[$0f]); SetOp([$71, $55]); Test; CheckA($00); CheckSR('nZCv');
  T('adc (ind,y):V');   Init; SetA($60); SetD(0); SetC(1); SetY($10); SetM($54,[$9a,$50]); SetM($50aa,[$40]); SetOp([$71, $54]); Test; CheckA($a1); CheckSR('NzcV');

  T('and imm');         Init; SetA($f0); SetOp([$29, $42]); Test; CheckA($40); CheckSR('nz');
  T('and imm+C');       Init; SetA($0f); SetOp([$29, $5a]); Test; CheckA($0a); CheckSR('nz');
  T('and imm:Z');       Init; SetA($f0); SetOp([$29, $0f]); Test; CheckA($00); CheckSR('nZ');
  T('and imm:N');       Init; SetA($dd); SetOp([$29, $c0]); Test; CheckA($c0); CheckSR('Nz');

  T('and zp');          Init; SetA($f0); SetM($50,[$42]); SetOp([$25, $50]); Test; CheckA($40); CheckSR('nz');
  T('and zp+C');        Init; SetA($0f); SetM($50,[$5a]); SetOp([$25, $50]); Test; CheckA($0a); CheckSR('nz');
  T('and zp.2');        Init; SetA($f0); SetM($50,[$20]); SetOp([$25, $50]); Test; CheckA($20); CheckSR('nz');
  T('and zp:Z');        Init; SetA($f0); SetM($50,[$0f]); SetOp([$25, $50]); Test; CheckA($00); CheckSR('nZ');
  T('and zp:N');        Init; SetA($dd); SetM($50,[$c0]); SetOp([$25, $50]); Test; CheckA($c0); CheckSR('Nz');

  T('and zp,x');        Init; SetA($f0); SetX($01); SetM($51,[$42]);          SetOp([$35, $50]); Test; CheckA($40); CheckSR('nz');
  T('and zp,x+C');      Init; SetA($0f); SetX($80); SetM($20,[$5a]); SetC(1); SetOp([$35, $a0]); Test; CheckA($0a); CheckSR('nz');
  T('and zp,x.2');      Init; SetA($f0); SetX($00); SetM($50,[$20]);          SetOp([$35, $50]); Test; CheckA($20); CheckSR('nz');
  T('and zp,x:Z');      Init; SetA($f0); SetX($ff); SetM($4f,[$0f]);          SetOp([$35, $50]); Test; CheckA($00); CheckSR('nZ');
  T('and zp,x:N');      Init; SetA($dd); SetX($10); SetM($60,[$c0]);          SetOp([$35, $50]); Test; CheckA($c0); CheckSR('Nz');

  T('and abs');         Init; SetA($f0); SetM($1234,[$42]);          SetOp([$2d, $34, $12]); Test; CheckA($40); CheckSR('nz');
  T('and abs+C');       Init; SetA($0f); SetM($0050,[$5a]); SetC(1); SetOp([$2d, $50, $00]); Test; CheckA($0a); CheckSR('nz');
  T('and abs.2');       Init; SetA($f0); SetM($55aa,[$20]);          SetOp([$2d, $aa, $55]); Test; CheckA($20); CheckSR('nz');
  T('and abs:Z');       Init; SetA($f0); SetM($aa55,[$0f]);          SetOp([$2d, $55, $aa]); Test; CheckA($00); CheckSR('nZ');
  T('and abs:N');       Init; SetA($dd); SetM($3254,[$c0]);          SetOp([$2d, $54, $32]); Test; CheckA($c0); CheckSR('Nz');

  T('and abs,x');       Init; SetA($f0); SetX($01); SetM($1235,[$42]);          SetOp([$3d, $34, $12]); Test; CheckA($40); CheckSR('nz');
  T('and abs,x+C');     Init; SetA($0f); SetX($80); SetM($0120,[$5a]); SetC(1); SetOp([$3d, $a0, $00]); Test; CheckA($0a); CheckSR('nz');
  T('and abs,x.2');     Init; SetA($f0); SetX($00); SetM($55aa,[$20]);          SetOp([$3d, $aa, $55]); Test; CheckA($20); CheckSR('nz');
  T('and abs,x:Z');     Init; SetA($f0); SetX($ff); SetM($ab54,[$0f]);          SetOp([$3d, $55, $aa]); Test; CheckA($00); CheckSR('nZ');
  T('and abs,x:N');     Init; SetA($dd); SetX($10); SetM($3264,[$c0]);          SetOp([$3d, $54, $32]); Test; CheckA($c0); CheckSR('Nz');

  T('and abs,y');       Init; SetA($f0); SetY($01); SetM($1235,[$42]);          SetOp([$39, $34, $12]); Test; CheckA($40); CheckSR('nz');
  T('and abs,y+C');     Init; SetA($0f); SetY($80); SetM($0120,[$5a]); SetC(1); SetOp([$39, $a0, $00]); Test; CheckA($0a); CheckSR('nz');
  T('and abs,y.2');     Init; SetA($f0); SetY($00); SetM($55aa,[$20]);          SetOp([$39, $aa, $55]); Test; CheckA($20); CheckSR('nz');
  T('and abs,y:Z');     Init; SetA($f0); SetY($ff); SetM($ab54,[$0f]);          SetOp([$39, $55, $aa]); Test; CheckA($00); CheckSR('nZ');
  T('and abs,y:N');     Init; SetA($dd); SetY($10); SetM($3264,[$c0]);          SetOp([$39, $54, $32]); Test; CheckA($c0); CheckSR('Nz');

  T('and (ind,x)');     Init; SetA($f0); SetX($01); SetM($35,[$12,$10]); SetM($1012,[$42]);          SetOp([$21, $34]); Test; CheckA($40); CheckSR('nz');
  T('and (ind,x)+C');   Init; SetA($0f); SetX($80); SetM($20,[$34,$20]); SetM($2034,[$5a]); SetC(1); SetOp([$21, $a0]); Test; CheckA($0a); CheckSR('nz');
  T('and (ind,x).2');   Init; SetA($f0); SetX($00); SetM($aa,[$56,$30]); SetM($3056,[$20]);          SetOp([$21, $aa]); Test; CheckA($20); CheckSR('nz');
  T('and (ind,x):Z');   Init; SetA($f0); SetX($ff); SetM($54,[$78,$40]); SetM($4078,[$0f]);          SetOp([$21, $55]); Test; CheckA($00); CheckSR('nZ');
  T('and (ind,x):N');   Init; SetA($dd); SetX($10); SetM($64,[$9a,$50]); SetM($509a,[$c0]);          SetOp([$21, $54]); Test; CheckA($c0); CheckSR('Nz');

  T('and (ind),y');     Init; SetA($f0); SetY($01); SetM($34,[$12,$10]); SetM($1013,[$42]);          SetOp([$31, $34]); Test; CheckA($40); CheckSR('nz');
  T('and (ind),y+C');   Init; SetA($0f); SetY($80); SetM($50,[$34,$20]); SetM($20b4,[$5a]); SetC(1); SetOp([$31, $50]); Test; CheckA($0a); CheckSR('nz');
  T('and (ind),y.2');   Init; SetA($f0); SetY($00); SetM($aa,[$56,$30]); SetM($3056,[$20]);          SetOp([$31, $aa]); Test; CheckA($20); CheckSR('nz');
  T('and (ind),y:Z');   Init; SetA($f0); SetY($ff); SetM($55,[$78,$40]); SetM($4177,[$0f]);          SetOp([$31, $55]); Test; CheckA($00); CheckSR('nZ');
  T('and (ind),y:N');   Init; SetA($dd); SetY($10); SetM($54,[$9a,$50]); SetM($50aa,[$c0]);          SetOp([$31, $54]); Test; CheckA($c0); CheckSR('Nz');

  T('asl imm');         Init; SetA($05); SetC(0); SetOp([$0a]); Test; CheckA($0a); CheckSR('nzc');
  T('asl imm:N');       Init; SetA($55); SetC(0); SetOp([$0a]); Test; CheckA($aa); CheckSR('Nzc');
  T('asl imm:Z');       Init; SetA($00); SetC(0); SetOp([$0a]); Test; CheckA($00); CheckSR('nZc');
  T('asl imm:C');       Init; SetA($aa); SetC(0); SetOp([$0a]); Test; CheckA($54); CheckSR('nzC');
  T('asl imm+C');       Init; SetA($00); SetC(1); SetOp([$0a]); Test; CheckA($00); CheckSR('nZc');
  T('asl imm+C:C');     Init; SetA($80); SetC(1); SetOp([$0a]); Test; CheckA($00); CheckSR('nZC');

  T('asl zp');          Init; SetM($12,[$05]); SetC(0); SetOp([$06, $12]); Test; CheckM($12,[$0a]); CheckSR('nzc');
  T('asl zp:N');        Init; SetM($34,[$55]); SetC(0); SetOp([$06, $34]); Test; CheckM($34,[$aa]); CheckSR('Nzc');
  T('asl zp:Z');        Init; SetM($56,[$00]); SetC(0); SetOp([$06, $56]); Test; CheckM($56,[$00]); CheckSR('nZc');
  T('asl zp:C');        Init; SetM($78,[$aa]); SetC(0); SetOp([$06, $78]); Test; CheckM($78,[$54]); CheckSR('nzC');
  T('asl zp+C');        Init; SetM($ab,[$00]); SetC(1); SetOp([$06, $ab]); Test; CheckM($ab,[$00]); CheckSR('nZc');
  T('asl zp+C:C');      Init; SetM($cd,[$80]); SetC(1); SetOp([$06, $cd]); Test; CheckM($cd,[$00]); CheckSR('nZC');

  T('asl zp,x');        Init; SetX($fd); SetM($0f,[$05]); SetC(0); SetOp([$16,$12]); Test; CheckM($0f,[$0a]); CheckSR('nzc');
  T('asl zp,x:N');      Init; SetX($ec); SetM($20,[$55]); SetC(0); SetOp([$16,$34]); Test; CheckM($20,[$aa]); CheckSR('Nzc');
  T('asl zp,x:Z');      Init; SetX($ba); SetM($10,[$00]); SetC(0); SetOp([$16,$56]); Test; CheckM($10,[$00]); CheckSR('nZc');
  T('asl zp,x:C');      Init; SetX($54); SetM($cc,[$aa]); SetC(0); SetOp([$16,$78]); Test; CheckM($cc,[$54]); CheckSR('nzC');
  T('asl zp,x+C');      Init; SetX($21); SetM($cc,[$00]); SetC(1); SetOp([$16,$ab]); Test; CheckM($cc,[$00]); CheckSR('nZc');
  T('asl zp+C:C');      Init; SetX($01); SetM($df,[$80]); SetC(1); SetOp([$16,$de]); Test; CheckM($df,[$00]); CheckSR('nZC');

  T('asl abs');         Init; SetM($0112,[$05]); SetC(0); SetOp([$0e,$12,$01]); Test; CheckM($0112,[$0a]); CheckSR('nzc');
  T('asl abs:N');       Init; SetM($9234,[$55]); SetC(0); SetOp([$0e,$34,$92]); Test; CheckM($9234,[$aa]); CheckSR('Nzc');
  T('asl abs:Z');       Init; SetM($8356,[$00]); SetC(0); SetOp([$0e,$56,$83]); Test; CheckM($8356,[$00]); CheckSR('nZc');
  T('asl abs:C');       Init; SetM($7478,[$aa]); SetC(0); SetOp([$0e,$78,$74]); Test; CheckM($7478,[$54]); CheckSR('nzC');
  T('asl abs+C');       Init; SetM($65ab,[$00]); SetC(1); SetOp([$0e,$ab,$65]); Test; CheckM($65ab,[$00]); CheckSR('nZc');
  T('asl abs+C:C');     Init; SetM($56cd,[$80]); SetC(1); SetOp([$0e,$cd,$56]); Test; CheckM($56cd,[$00]); CheckSR('nZC');

  T('asl abs,x');       Init; SetX($fd); SetM($020f,[$05]); SetC(0); SetOp([$1e,$12,$01]); Test; CheckM($020f,[$0a]); CheckSR('nzc');
  T('asl abs,x:N');     Init; SetX($ec); SetM($9320,[$55]); SetC(0); SetOp([$1e,$34,$92]); Test; CheckM($9320,[$aa]); CheckSR('Nzc');
  T('asl abs,x:Z');     Init; SetX($ba); SetM($8410,[$00]); SetC(0); SetOp([$1e,$56,$83]); Test; CheckM($8410,[$00]); CheckSR('nZc');
  T('asl abs,x:C');     Init; SetX($54); SetM($74cc,[$aa]); SetC(0); SetOp([$1e,$78,$74]); Test; CheckM($74cc,[$54]); CheckSR('nzC');
  T('asl abs,x+C');     Init; SetX($21); SetM($65cc,[$00]); SetC(1); SetOp([$1e,$ab,$65]); Test; CheckM($65cc,[$00]); CheckSR('nZc');
  T('asl abs+C:C');     Init; SetX($01); SetM($56df,[$80]); SetC(1); SetOp([$1e,$de,$56]); Test; CheckM($56df,[$00]); CheckSR('nZC');

  T('bcc++C');          Init; SetPC($1000); SetC(1); SetOp([$90, $10]); Test; CheckPC($1002); CheckSR('');
  T('bcc-+C');          Init; SetPC($1000); SetC(1); SetOp([$90, $f0]); Test; CheckPC($1002); CheckSR('');
  T('bcc+');            Init; SetPC($1000); SetC(0); SetOp([$90, $10]); Test; CheckPC($1012); CheckSR('');
  T('bcc-');            Init; SetPC($1000); SetC(0); SetOp([$90, $f0]); Test; CheckPC($0ff2); CheckSR('');

  T('bcs++C');          Init; SetPC($1000); SetC(1); SetOp([$b0,$10]); Test; CheckPC($1012); CheckSR('');
  T('bcs-+C');          Init; SetPC($1000); SetC(1); SetOp([$b0,$f0]); Test; CheckPC($0ff2); CheckSR('');
  T('bcs+');            Init; SetPC($1000); SetC(0); SetOp([$b0,$10]); Test; CheckPC($1002); CheckSR('');
  T('bcs-');            Init; SetPC($1000); SetC(0); SetOp([$b0,$f0]); Test; CheckPC($1002); CheckSR('');

  T('beq++Z');          Init; SetPC($1000); SetZ(1); SetOp([$f0,$10]); Test; CheckPC($1012); CheckSR('');
  T('beq-+Z');          Init; SetPC($1000); SetZ(1); SetOp([$f0,$f0]); Test; CheckPC($0ff2); CheckSR('');
  T('beq+');            Init; SetPC($1000); SetZ(0); SetOp([$f0,$10]); Test; CheckPC($1002); CheckSR('');
  T('beq-');            Init; SetPC($1000); SetZ(0); SetOp([$f0,$f0]); Test; CheckPC($1002); CheckSR('');

  T('bne++C');          Init; SetPC($1000); SetZ(1); SetOp([$d0,$10]); Test; CheckPC($1002); CheckSR('');
  T('bne-+C');          Init; SetPC($1000); SetZ(1); SetOp([$d0,$f0]); Test; CheckPC($1002); CheckSR('');
  T('bne+');            Init; SetPC($1000); SetZ(0); SetOp([$d0,$10]); Test; CheckPC($1012); CheckSR('');
  T('bne-');            Init; SetPC($1000); SetZ(0); SetOp([$d0,$f0]); Test; CheckPC($0ff2); CheckSR('');

  T('bmi++C');          Init; SetPC($1000); SetN(1); SetOp([$30,$10]); Test; CheckPC($1012); CheckSR('');
  T('bmi-+C');          Init; SetPC($1000); SetN(1); SetOp([$30,$f0]); Test; CheckPC($0ff2); CheckSR('');
  T('bmi+');            Init; SetPC($1000); SetN(0); SetOp([$30,$10]); Test; CheckPC($1002); CheckSR('');
  T('bmi-');            Init; SetPC($1000); SetN(0); SetOp([$30,$f0]); Test; CheckPC($1002); CheckSR('');

  T('bpl++C');          Init; SetPC($1000); SetN(1); SetOp([$10,$10]); Test; CheckPC($1002); CheckSR('');
  T('bpl-+C');          Init; SetPC($1000); SetN(1); SetOp([$10,$f0]); Test; CheckPC($1002); CheckSR('');
  T('bpl+');            Init; SetPC($1000); SetN(0); SetOp([$10,$10]); Test; CheckPC($1012); CheckSR('');
  T('bpl-');            Init; SetPC($1000); SetN(0); SetOp([$10,$f0]); Test; CheckPC($0ff2); CheckSR('');

  T('bit zp');          Init; SetA($0f); SetM($24,[$17]); SetOp([$24,$24]); Test; CheckA($0f); CheckM($24,[$17]); CheckSR('nvz');
  T('bit zp:N');        Init; SetA($20); SetM($35,[$aa]); SetOp([$24,$35]); Test; CheckA($20); CheckM($35,[$aa]); CheckSR('Nvz');
  T('bit zp:V');        Init; SetA($0f); SetM($46,[$55]); SetOp([$24,$46]); Test; CheckA($0f); CheckM($46,[$55]); CheckSR('nVz');
  T('bit zp:NV');       Init; SetA($77); SetM($57,[$cc]); SetOp([$24,$57]); Test; CheckA($77); CheckM($57,[$cc]); CheckSR('NVz');
  T('bit zp:Z');        Init; SetA($c0); SetM($ab,[$17]); SetOp([$24,$ab]); Test; CheckA($c0); CheckM($ab,[$17]); CheckSR('nvZ');
  T('bit zp:ZN');       Init; SetA($55); SetM($35,[$aa]); SetOp([$24,$35]); Test; CheckA($55); CheckM($35,[$aa]); CheckSR('NvZ');
  T('bit zp:ZV');       Init; SetA($aa); SetM($46,[$55]); SetOp([$24,$46]); Test; CheckA($aa); CheckM($46,[$55]); CheckSR('nVZ');
  T('bit zp:ZNV');      Init; SetA($11); SetM($57,[$cc]); SetOp([$24,$57]); Test; CheckA($11); CheckM($57,[$cc]); CheckSR('NVZ');

  T('bit abs');         Init; SetA($0f); SetM($ab24,[$17]); SetOp([$2c,$24,$ab]); Test; CheckA($0f); CheckM($ab24,[$17]); CheckSR('nvz');
  T('bit abs:N');       Init; SetA($20); SetM($cd35,[$aa]); SetOp([$2c,$35,$cd]); Test; CheckA($20); CheckM($cd35,[$aa]); CheckSR('Nvz');
  T('bit abs:V');       Init; SetA($0f); SetM($ef46,[$55]); SetOp([$2c,$46,$ef]); Test; CheckA($0f); CheckM($ef46,[$55]); CheckSR('nVz');
  T('bit abs:NV');      Init; SetA($77); SetM($0157,[$cc]); SetOp([$2c,$57,$01]); Test; CheckA($77); CheckM($0157,[$cc]); CheckSR('NVz');
  T('bit abs:Z');       Init; SetA($c0); SetM($ab24,[$17]); SetOp([$2c,$24,$ab]); Test; CheckA($c0); CheckM($ab24,[$17]); CheckSR('nvZ');
  T('bit abs:ZN');      Init; SetA($55); SetM($cd35,[$aa]); SetOp([$2c,$35,$cd]); Test; CheckA($55); CheckM($cd35,[$aa]); CheckSR('NvZ');
  T('bit abs:ZV');      Init; SetA($aa); SetM($ef46,[$55]); SetOp([$2c,$46,$ef]); Test; CheckA($aa); CheckM($ef46,[$55]); CheckSR('nVZ');
  T('bit abs:ZNV');     Init; SetA($11); SetM($0157,[$cc]); SetOp([$2c,$57,$01]); Test; CheckA($11); CheckM($0157,[$cc]); CheckSR('NVZ');

  T('bvc++C');          Init; SetPC($1000); SetV(1); SetOp([$50,$10]); Test; CheckPC($1002); CheckSR('');
  T('bvc-+C');          Init; SetPC($1000); SetV(1); SetOp([$50,$f0]); Test; CheckPC($1002); CheckSR('');
  T('bvc+');            Init; SetPC($1000); SetV(0); SetOp([$50,$10]); Test; CheckPC($1012); CheckSR('');
  T('bvc-');            Init; SetPC($1000); SetV(0); SetOp([$50,$f0]); Test; CheckPC($0ff2); CheckSR('');

  T('bvs++C');          Init; SetPC($1000); SetV(1); SetOp([$70,$10]); Test; CheckPC($1012); CheckSR('');
  T('bvs-+C');          Init; SetPC($1000); SetV(1); SetOp([$70,$f0]); Test; CheckPC($0ff2); CheckSR('');
  T('bvs+');            Init; SetPC($1000); SetV(0); SetOp([$70,$10]); Test; CheckPC($1002); CheckSR('');
  T('bvs-');            Init; SetPC($1000); SetV(0); SetOp([$70,$f0]); Test; CheckPC($1002); CheckSR('');

  T('clc');             Init; SetC(0); SetOp([$18]); Test; CheckSR('c');
  T('clc+C');           Init; SetC(1); SetOp([$18]); Test; CheckSR('c');

  T('cld');             Init; SetD(0); SetOp([$d8]); Test; CheckSR('d');
  T('cld+D');           Init; SetD(1); SetOp([$d8]); Test; CheckSR('d');

  T('cli');             Init; SetI(0); SetOp([$58]); Test; CheckSR('i');
  T('cli+I');           Init; SetI(1); SetOp([$58]); Test; CheckSR('i');

  T('clv');             Init; SetV(0); SetOp([$b8]); Test; CheckSR('v');
  T('clv+V');           Init; SetV(1); SetOp([$b8]); Test; CheckSR('v');

  T('cmp imm<');        Init; SetA($44); SetOp([$c9,$ff]); Test; CheckA($44); CheckSR('nzc');
  T('cmp imm>');        Init; SetA($79); SetOp([$c9,$3f]); Test; CheckA($79); CheckSR('nzC');
  T('cmp imm=');        Init; SetA($55); SetOp([$c9,$55]); Test; CheckA($55); CheckSR('nZC');
  T('cmp imm<:N');      Init; SetA($44); SetOp([$c9,$50]); Test; CheckA($44); CheckSR('Nzc');
  T('cmp imm>:N');      Init; SetA($ee); SetOp([$c9,$33]); Test; CheckA($ee); CheckSR('NzC');

  T('cmp zp<');         Init; SetA($44); SetM($50,[$ff]); SetOp([$c5,$50]); Test; CheckA($44); CheckSR('nzc');
  T('cmp zp>');         Init; SetA($79); SetM($50,[$3f]); SetOp([$c5,$50]); Test; CheckA($79); CheckSR('nzC');
  T('cmp zp=');         Init; SetA($55); SetM($50,[$55]); SetOp([$c5,$50]); Test; CheckA($55); CheckSR('nZC');
  T('cmp zp<:N');       Init; SetA($44); SetM($50,[$50]); SetOp([$c5,$50]); Test; CheckA($44); CheckSR('Nzc');
  T('cmp zp>:N');       Init; SetA($ee); SetM($50,[$33]); SetOp([$c5,$50]); Test; CheckA($ee); CheckSR('NzC');

  T('cmp zp<');         Init; SetA($44); SetX($01); SetM($51,[$ff]); SetOp([$d5,$50]); Test; CheckA($44); CheckSR('nzc');
  T('cmp zp>');         Init; SetA($79); SetX($80); SetM($20,[$3f]); SetOp([$d5,$a0]); Test; CheckA($79); CheckSR('nzC');
  T('cmp zp=');         Init; SetA($55); SetX($00); SetM($50,[$55]); SetOp([$d5,$50]); Test; CheckA($55); CheckSR('nZC');
  T('cmp zp<:N');       Init; SetA($44); SetX($ff); SetM($4f,[$50]); SetOp([$d5,$50]); Test; CheckA($44); CheckSR('Nzc');
  T('cmp zp>:N');       Init; SetA($ee); SetX($10); SetM($60,[$33]); SetOp([$d5,$50]); Test; CheckA($ee); CheckSR('NzC');

  T('cmp abs<');        Init; SetA($44); SetM($1234,[$ff]); SetOp([$cd,$34,$12]); Test; CheckA($44); CheckSR('nzc');
  T('cmp abs>');        Init; SetA($79); SetM($0050,[$3f]); SetOp([$cd,$50,$00]); Test; CheckA($79); CheckSR('nzC');
  T('cmp abs=');        Init; SetA($55); SetM($55aa,[$55]); SetOp([$cd,$aa,$55]); Test; CheckA($55); CheckSR('nZC');
  T('cmp abs<:N');      Init; SetA($44); SetM($aa55,[$50]); SetOp([$cd,$55,$aa]); Test; CheckA($44); CheckSR('Nzc');
  T('cmp abs>:N');      Init; SetA($ee); SetM($3254,[$33]); SetOp([$cd,$54,$32]); Test; CheckA($ee); CheckSR('NzC');

  T('cmp abs<');        Init; SetA($44); SetX($01); SetM($1235,[$ff]); SetOp([$dd,$34,$12]); Test; CheckA($44); CheckSR('nzc');
  T('cmp abs>');        Init; SetA($79); SetX($80); SetM($0120,[$3f]); SetOp([$dd,$a0,$00]); Test; CheckA($79); CheckSR('nzC');
  T('cmp abs=');        Init; SetA($55); SetX($00); SetM($55aa,[$55]); SetOp([$dd,$aa,$55]); Test; CheckA($55); CheckSR('nZC');
  T('cmp abs<:N');      Init; SetA($44); SetX($ff); SetM($ab54,[$50]); SetOp([$dd,$55,$aa]); Test; CheckA($44); CheckSR('Nzc');
  T('cmp abs>:N');      Init; SetA($ee); SetX($10); SetM($3264,[$33]); SetOp([$dd,$54,$32]); Test; CheckA($ee); CheckSR('NzC');

  T('cmp abs<');        Init; SetA($44); SetY($01); SetM($1235,[$ff]); SetOp([$d9,$34,$12]); Test; CheckA($44); CheckSR('nzc');
  T('cmp abs>');        Init; SetA($79); SetY($80); SetM($0120,[$3f]); SetOp([$d9,$a0,$00]); Test; CheckA($79); CheckSR('nzC');
  T('cmp abs=');        Init; SetA($55); SetY($00); SetM($55aa,[$55]); SetOp([$d9,$aa,$55]); Test; CheckA($55); CheckSR('nZC');
  T('cmp abs<:N');      Init; SetA($44); SetY($ff); SetM($ab54,[$50]); SetOp([$d9,$55,$aa]); Test; CheckA($44); CheckSR('Nzc');
  T('cmp abs>:N');      Init; SetA($ee); SetY($10); SetM($3264,[$33]); SetOp([$d9,$54,$32]); Test; CheckA($ee); CheckSR('NzC');

  T('cmp (ind,x)<');    Init; SetA($44); SetX($01); SetM($35,[$12,$10]); SetM($1012,[$ff]); SetOp([$c1,$34]); Test; CheckA($44); CheckSR('nzc');
  T('cmp (ind,x)>');    Init; SetA($79); SetX($80); SetM($20,[$34,$20]); SetM($2034,[$3f]); SetOp([$c1,$a0]); Test; CheckA($79); CheckSR('nzC');
  T('cmp (ind,x)=');    Init; SetA($55); SetX($00); SetM($aa,[$56,$30]); SetM($3056,[$55]); SetOp([$c1,$aa]); Test; CheckA($55); CheckSR('nZC');
  T('cmp (ind,x)<:N');  Init; SetA($44); SetX($ff); SetM($54,[$78,$40]); SetM($4078,[$50]); SetOp([$c1,$55]); Test; CheckA($44); CheckSR('Nzc');
  T('cmp (ind,x)>:N');  Init; SetA($ee); SetX($10); SetM($64,[$9a,$50]); SetM($509a,[$33]); SetOp([$c1,$54]); Test; CheckA($ee); CheckSR('NzC');

  T('cmp (ind),y<');    Init; SetA($44); SetY($01); SetM($34,[$12,$10]); SetM($1013,[$ff]); SetOp([$d1,$34]); Test; CheckA($44); CheckSR('nzc');
  T('cmp (ind),y>');    Init; SetA($79); SetY($80); SetM($50,[$34,$20]); SetM($20b4,[$3f]); SetOp([$d1,$50]); Test; CheckA($79); CheckSR('nzC');
  T('cmp (ind),y=');    Init; SetA($55); SetY($00); SetM($aa,[$56,$30]); SetM($3056,[$55]); SetOp([$d1,$aa]); Test; CheckA($55); CheckSR('nZC');
  T('cmp (ind),y<:N');  Init; SetA($44); SetY($ff); SetM($55,[$78,$40]); SetM($4177,[$50]); SetOp([$d1,$55]); Test; CheckA($44); CheckSR('Nzc');
  T('cmp (ind),y>:N');  Init; SetA($ee); SetY($10); SetM($54,[$9a,$50]); SetM($50aa,[$33]); SetOp([$d1,$54]); Test; CheckA($ee); CheckSR('NzC');

  T('eor imm.1');       Init; SetA($44); SetOp([$49,$ff]); Test; CheckA($bb); CheckSR('Nz');
  T('eor imm.2');       Init; SetA($79); SetOp([$49,$3f]); Test; CheckA($46); CheckSR('nz');
  T('eor imm.3');       Init; SetA($55); SetOp([$49,$55]); Test; CheckA($00); CheckSR('nZ');
  T('eor imm.4');       Init; SetA($44); SetOp([$49,$50]); Test; CheckA($14); CheckSR('nz');
  T('eor imm.5');       Init; SetA($ee); SetOp([$49,$33]); Test; CheckA($dd); CheckSR('Nz');

  T('eor zp.1');        Init; SetA($44); SetM($50,[$ff]); SetOp([$45,$50]); Test; CheckA($bb); CheckSR('Nz');
  T('eor zp.2');        Init; SetA($79); SetM($50,[$3f]); SetOp([$45,$50]); Test; CheckA($46); CheckSR('nz');
  T('eor zp.3');        Init; SetA($55); SetM($50,[$55]); SetOp([$45,$50]); Test; CheckA($00); CheckSR('nZ');
  T('eor zp.4');        Init; SetA($44); SetM($50,[$50]); SetOp([$45,$50]); Test; CheckA($14); CheckSR('nz');
  T('eor zp.5');        Init; SetA($ee); SetM($50,[$33]); SetOp([$45,$50]); Test; CheckA($dd); CheckSR('Nz');

  T('eor zp.1');        Init; SetA($44); SetX($01); SetM($51,[$ff]); SetOp([$55,$50]); Test; CheckA($bb); CheckSR('Nz');
  T('eor zp.2');        Init; SetA($79); SetX($80); SetM($20,[$3f]); SetOp([$55,$a0]); Test; CheckA($46); CheckSR('nz');
  T('eor zp.3');        Init; SetA($55); SetX($00); SetM($50,[$55]); SetOp([$55,$50]); Test; CheckA($00); CheckSR('nZ');
  T('eor zp.4');        Init; SetA($44); SetX($ff); SetM($4f,[$50]); SetOp([$55,$50]); Test; CheckA($14); CheckSR('nz');
  T('eor zp.5');        Init; SetA($ee); SetX($10); SetM($60,[$33]); SetOp([$55,$50]); Test; CheckA($dd); CheckSR('Nz');

  T('eor abs.1');       Init; SetA($44); SetM($1234,[$ff]); SetOp([$4d,$34,$12]); Test; CheckA($bb); CheckSR('Nz');
  T('eor abs.2');       Init; SetA($79); SetM($0050,[$3f]); SetOp([$4d,$50,$00]); Test; CheckA($46); CheckSR('nz');
  T('eor abs.3');       Init; SetA($55); SetM($55aa,[$55]); SetOp([$4d,$aa,$55]); Test; CheckA($00); CheckSR('nZ');
  T('eor abs.4');       Init; SetA($44); SetM($aa55,[$50]); SetOp([$4d,$55,$aa]); Test; CheckA($14); CheckSR('nz');
  T('eor abs.5');       Init; SetA($ee); SetM($3254,[$33]); SetOp([$4d,$54,$32]); Test; CheckA($dd); CheckSR('Nz');

  T('eor abs.1');       Init; SetA($44); SetX($01); SetM($1235,[$ff]); SetOp([$5d,$34,$12]); Test; CheckA($bb); CheckSR('Nz');
  T('eor abs.2');       Init; SetA($79); SetX($80); SetM($0120,[$3f]); SetOp([$5d,$a0,$00]); Test; CheckA($46); CheckSR('nz');
  T('eor abs.3');       Init; SetA($55); SetX($00); SetM($55aa,[$55]); SetOp([$5d,$aa,$55]); Test; CheckA($00); CheckSR('nZ');
  T('eor abs.4');       Init; SetA($44); SetX($ff); SetM($ab54,[$50]); SetOp([$5d,$55,$aa]); Test; CheckA($14); CheckSR('nz');
  T('eor abs.5');       Init; SetA($ee); SetX($10); SetM($3264,[$33]); SetOp([$5d,$54,$32]); Test; CheckA($dd); CheckSR('Nz');

  T('eor abs.1');       Init; SetA($44); SetY($01); SetM($1235,[$ff]); SetOp([$59,$34,$12]); Test; CheckA($bb); CheckSR('Nz');
  T('eor abs.2');       Init; SetA($79); SetY($80); SetM($0120,[$3f]); SetOp([$59,$a0,$00]); Test; CheckA($46); CheckSR('nz');
  T('eor abs.3');       Init; SetA($55); SetY($00); SetM($55aa,[$55]); SetOp([$59,$aa,$55]); Test; CheckA($00); CheckSR('nZ');
  T('eor abs.4');       Init; SetA($44); SetY($ff); SetM($ab54,[$50]); SetOp([$59,$55,$aa]); Test; CheckA($14); CheckSR('nz');
  T('eor abs.5');       Init; SetA($ee); SetY($10); SetM($3264,[$33]); SetOp([$59,$54,$32]); Test; CheckA($dd); CheckSR('Nz');

  T('eor (ind,x).1');   Init; SetA($44); SetX($01); SetM($35,[$12,$10]); SetM($1012,[$ff]); SetOp([$41,$34]); Test; CheckA($bb); CheckSR('Nz');
  T('eor (ind,x).2');   Init; SetA($79); SetX($80); SetM($20,[$34,$20]); SetM($2034,[$3f]); SetOp([$41,$a0]); Test; CheckA($46); CheckSR('nz');
  T('eor (ind,x).3');   Init; SetA($55); SetX($00); SetM($aa,[$56,$30]); SetM($3056,[$55]); SetOp([$41,$aa]); Test; CheckA($00); CheckSR('nZ');
  T('eor (ind,x).4');   Init; SetA($44); SetX($ff); SetM($54,[$78,$40]); SetM($4078,[$50]); SetOp([$41,$55]); Test; CheckA($14); CheckSR('nz');
  T('eor (ind,x).5');   Init; SetA($ee); SetX($10); SetM($64,[$9a,$50]); SetM($509a,[$33]); SetOp([$41,$54]); Test; CheckA($dd); CheckSR('Nz');

  T('eor (ind),y.1');   Init; SetA($44); SetY($01); SetM($34,[$12,$10]); SetM($1013,[$ff]); SetOp([$51,$34]); Test; CheckA($bb); CheckSR('Nz');
  T('eor (ind),y.2');   Init; SetA($79); SetY($80); SetM($50,[$34,$20]); SetM($20b4,[$3f]); SetOp([$51,$50]); Test; CheckA($46); CheckSR('nz');
  T('eor (ind),y.3');   Init; SetA($55); SetY($00); SetM($aa,[$56,$30]); SetM($3056,[$55]); SetOp([$51,$aa]); Test; CheckA($00); CheckSR('nZ');
  T('eor (ind),y.4');   Init; SetA($44); SetY($ff); SetM($55,[$78,$40]); SetM($4177,[$50]); SetOp([$51,$55]); Test; CheckA($14); CheckSR('nz');
  T('eor (ind),y.5');   Init; SetA($ee); SetY($10); SetM($54,[$9a,$50]); SetM($50aa,[$33]); SetOp([$51,$54]); Test; CheckA($dd); CheckSR('Nz');

  T('jmp abs');         Init; SetPC($0100); SetOp([$4c,$ed,$fd]); Test; CheckPC($fded); CheckSR('');
  T('jmp ind');         Init; SetPC($0100); SetM($1234,[$ed,$fd]); SetOp([$6c,$34,$12]); Test; CheckPC($fded); CheckSR('');

  T('jsr');             Init; SetSP($ff); SetPC($0100); SetOp([$20,$ed,$fd]); Test; CheckSP($fd); CheckPC($fded); CheckM($1fe,[$02,$01]); CheckSR('');

  T('php+C');           Init; SetSP($80); SetN(0); SetV(0); SetB(0); SetD(0); SetI(0); SetZ(0); SetC(1); SetOp([$08]); Test; CheckM($0180,[$21]); CheckSP($7f); CheckSR('');
  T('php+Z');           Init; SetSP($80); SetN(0); SetV(0); SetB(0); SetD(0); SetI(0); SetZ(1); SetC(0); SetOp([$08]); Test; CheckM($0180,[$22]); CheckSP($7f); CheckSR('');
  T('php+I');           Init; SetSP($80); SetN(0); SetV(0); SetB(0); SetD(0); SetI(1); SetZ(0); SetC(0); SetOp([$08]); Test; CheckM($0180,[$24]); CheckSP($7f); CheckSR('');
  T('php+D');           Init; SetSP($80); SetN(0); SetV(0); SetB(0); SetD(1); SetI(0); SetZ(0); SetC(0); SetOp([$08]); Test; CheckM($0180,[$28]); CheckSP($7f); CheckSR('');
  T('php+B');           Init; SetSP($80); SetN(0); SetV(0); SetB(1); SetD(0); SetI(0); SetZ(0); SetC(0); SetOp([$08]); Test; CheckM($0180,[$30]); CheckSP($7f); CheckSR('');
  T('php+V');           Init; SetSP($80); SetN(0); SetV(1); SetB(0); SetD(0); SetI(0); SetZ(0); SetC(0); SetOp([$08]); Test; CheckM($0180,[$60]); CheckSP($7f); CheckSR('');
  T('php+N');           Init; SetSP($80); SetN(1); SetV(0); SetB(0); SetD(0); SetI(0); SetZ(0); SetC(0); SetOp([$08]); Test; CheckM($0180,[$a0]); CheckSP($7f); CheckSR('');

  T('plp+C');           Init; SetSP($7f); SetM($0180,[$01]); SetOp([$28]); Test; CheckSP($80); CheckSR('nvbdizC');
  T('plp+Z');           Init; SetSP($7f); SetM($0180,[$02]); SetOp([$28]); Test; CheckSP($80); CheckSR('nvbdiZc');
  T('plp+I');           Init; SetSP($7f); SetM($0180,[$04]); SetOp([$28]); Test; CheckSP($80); CheckSR('nvbdIzc');
  T('plp+D');           Init; SetSP($7f); SetM($0180,[$08]); SetOp([$28]); Test; CheckSP($80); CheckSR('nvbDizc');
  T('plp+B');           Init; SetSP($7f); SetM($0180,[$10]); SetOp([$28]); Test; CheckSP($80); CheckSR('nvBdizc');
  T('plp+V');           Init; SetSP($7f); SetM($0180,[$40]); SetOp([$28]); Test; CheckSP($80); CheckSR('nVbdizc');
  T('plp+N');           Init; SetSP($7f); SetM($0180,[$80]); SetOp([$28]); Test; CheckSP($80); CheckSR('Nvbdizc');

  T('rol A');           Init; SetA($01); SetC(0); SetOp([$2a]); Test; CheckA($02); CheckSR('nzc');
  T('rol A+C');         Init; SetA($01); SetC(1); SetOp([$2a]); Test; CheckA($03); CheckSR('nzc');
  T('rol A:Z');         Init; SetA($00); SetC(0); SetOp([$2a]); Test; CheckA($00); CheckSR('nZc');
  T('rol A:N');         Init; SetA($40); SetC(0); SetOp([$2a]); Test; CheckA($80); CheckSR('Nzc');
  T('rol A:C');         Init; SetA($81); SetC(0); SetOp([$2a]); Test; CheckA($02); CheckSR('nzC');
  T('rol A+C:C');       Init; SetA($80); SetC(1); SetOp([$2a]); Test; CheckA($01); CheckSR('nzC');
  T('rol A+C:CN');      Init; SetA($F0); SetC(1); SetOp([$2a]); Test; CheckA($E1); CheckSR('NzC');
  T('rol A:CZ');        Init; SetA($80); SetC(0); SetOp([$2a]); Test; CheckA($00); CheckSR('nZC');

  T('rol zp');          Init; SetM($10,[$01]); SetC(0); SetOp([$26,$10]); Test; CheckM($10,[$02]); CheckSR('nzc');
  T('rol zp+C');        Init; SetM($29,[$01]); SetC(1); SetOp([$26,$29]); Test; CheckM($29,[$03]); CheckSR('nzc');
  T('rol zp:Z');        Init; SetM($38,[$00]); SetC(0); SetOp([$26,$38]); Test; CheckM($38,[$00]); CheckSR('nZc');
  T('rol zp:N');        Init; SetM($47,[$40]); SetC(0); SetOp([$26,$47]); Test; CheckM($47,[$80]); CheckSR('Nzc');
  T('rol zp:C');        Init; SetM($56,[$81]); SetC(0); SetOp([$26,$56]); Test; CheckM($56,[$02]); CheckSR('nzC');
  T('rol zp+C:C');      Init; SetM($fa,[$80]); SetC(1); SetOp([$26,$fa]); Test; CheckM($fa,[$01]); CheckSR('nzC');
  T('rol zp+C:CN');     Init; SetM($db,[$F0]); SetC(1); SetOp([$26,$db]); Test; CheckM($db,[$E1]); CheckSR('NzC');
  T('rol zp:CZ');       Init; SetM($ec,[$80]); SetC(0); SetOp([$26,$ec]); Test; CheckM($ec,[$00]); CheckSR('nZC');

  T('rol zp,x');        Init; SetM($22,[$01]); SetC(0); SetX($12); SetOp([$36,$10]); Test; CheckM($22,[$02]); CheckSR('nzc');
  T('rol zp,x+C');      Init; SetM($4c,[$01]); SetC(1); SetX($23); SetOp([$36,$29]); Test; CheckM($4c,[$03]); CheckSR('nzc');
  T('rol zp,x:Z');      Init; SetM($6c,[$00]); SetC(0); SetX($34); SetOp([$36,$38]); Test; CheckM($6c,[$00]); CheckSR('nZc');
  T('rol zp,x:N');      Init; SetM($8c,[$40]); SetC(0); SetX($45); SetOp([$36,$47]); Test; CheckM($8c,[$80]); CheckSR('Nzc');
  T('rol zp,x:C');      Init; SetM($ac,[$81]); SetC(0); SetX($56); SetOp([$36,$56]); Test; CheckM($ac,[$02]); CheckSR('nzC');
  T('rol zp,x+C:C');    Init; SetM($61,[$80]); SetC(1); SetX($67); SetOp([$36,$fa]); Test; CheckM($61,[$01]); CheckSR('nzC');
  T('rol zp,x+C:CN');   Init; SetM($53,[$F0]); SetC(1); SetX($78); SetOp([$36,$db]); Test; CheckM($53,[$E1]); CheckSR('NzC');
  T('rol zp,x:CZ');     Init; SetM($ea,[$80]); SetC(0); SetX($fe); SetOp([$36,$ec]); Test; CheckM($ea,[$00]); CheckSR('nZC');

  T('rol abs');         Init; SetM($1110,[$01]); SetC(0); SetOp([$2e,$10,$11]); Test; CheckM($1110,[$02]); CheckSR('nzc');
  T('rol abs+C');       Init; SetM($2229,[$01]); SetC(1); SetOp([$2e,$29,$22]); Test; CheckM($2229,[$03]); CheckSR('nzc');
  T('rol abs:Z');       Init; SetM($3338,[$00]); SetC(0); SetOp([$2e,$38,$33]); Test; CheckM($3338,[$00]); CheckSR('nZc');
  T('rol abs:N');       Init; SetM($4447,[$40]); SetC(0); SetOp([$2e,$47,$44]); Test; CheckM($4447,[$80]); CheckSR('Nzc');
  T('rol abs:C');       Init; SetM($9956,[$81]); SetC(0); SetOp([$2e,$56,$99]); Test; CheckM($9956,[$02]); CheckSR('nzC');
  T('rol abs+C:C');     Init; SetM($88fa,[$80]); SetC(1); SetOp([$2e,$fa,$88]); Test; CheckM($88fa,[$01]); CheckSR('nzC');
  T('rol abs+C:CN');    Init; SetM($77db,[$F0]); SetC(1); SetOp([$2e,$db,$77]); Test; CheckM($77db,[$E1]); CheckSR('NzC');
  T('rol abs:CZ');      Init; SetM($ddec,[$80]); SetC(0); SetOp([$2e,$ec,$dd]); Test; CheckM($ddec,[$00]); CheckSR('nZC');

  T('rol abs,x');       Init; SetM($1122,[$01]); SetC(0); SetX($12); SetOp([$3e,$10,$11]); Test; CheckM($1122,[$02]); CheckSR('nzc');
  T('rol abs,x+C');     Init; SetM($224c,[$01]); SetC(1); SetX($23); SetOp([$3e,$29,$22]); Test; CheckM($224c,[$03]); CheckSR('nzc');
  T('rol abs,x:Z');     Init; SetM($336c,[$00]); SetC(0); SetX($34); SetOp([$3e,$38,$33]); Test; CheckM($336c,[$00]); CheckSR('nZc');
  T('rol abs,x:N');     Init; SetM($448c,[$40]); SetC(0); SetX($45); SetOp([$3e,$47,$44]); Test; CheckM($448c,[$80]); CheckSR('Nzc');
  T('rol abs,x:C');     Init; SetM($99ac,[$81]); SetC(0); SetX($56); SetOp([$3e,$56,$99]); Test; CheckM($99ac,[$02]); CheckSR('nzC');
  T('rol abs,x+C:C');   Init; SetM($8961,[$80]); SetC(1); SetX($67); SetOp([$3e,$fa,$88]); Test; CheckM($8961,[$01]); CheckSR('nzC');
  T('rol abs,x+C:CN');  Init; SetM($7853,[$F0]); SetC(1); SetX($78); SetOp([$3e,$db,$77]); Test; CheckM($7853,[$E1]); CheckSR('NzC');
  T('rol abs,x:CZ');    Init; SetM($deea,[$80]); SetC(0); SetX($fe); SetOp([$3e,$ec,$dd]); Test; CheckM($deea,[$00]); CheckSR('nZC');

  T('ror A');           Init; SetA($10); SetC(0); SetOp([$6a]); Test; CheckA($08); CheckSR('nzc');
  T('ror A+C:N');       Init; SetA($10); SetC(1); SetOp([$6a]); Test; CheckA($88); CheckSR('Nzc');
  T('ror A:Z');         Init; SetA($00); SetC(0); SetOp([$6a]); Test; CheckA($00); CheckSR('nZc');
  T('ror A:C');         Init; SetA($81); SetC(0); SetOp([$6a]); Test; CheckA($40); CheckSR('nzC');
  T('ror A+C:CN');      Init; SetA($01); SetC(1); SetOp([$6a]); Test; CheckA($80); CheckSR('NzC');
  T('ror A:CZ');        Init; SetA($01); SetC(0); SetOp([$6a]); Test; CheckA($00); CheckSR('nZC');

  T('ror zp');          Init; SetM($10,[$10]); SetC(0); SetOp([$66,$10]); Test; CheckM($10,[$08]); CheckSR('nzc');
  T('ror zp+C:N');      Init; SetM($29,[$10]); SetC(1); SetOp([$66,$29]); Test; CheckM($29,[$88]); CheckSR('Nzc');
  T('ror zp:Z');        Init; SetM($38,[$00]); SetC(0); SetOp([$66,$38]); Test; CheckM($38,[$00]); CheckSR('nZc');
  T('ror zp:C');        Init; SetM($56,[$81]); SetC(0); SetOp([$66,$56]); Test; CheckM($56,[$40]); CheckSR('nzC');
  T('ror zp+C:CN');     Init; SetM($db,[$01]); SetC(1); SetOp([$66,$db]); Test; CheckM($db,[$80]); CheckSR('NzC');
  T('ror zp:CZ');       Init; SetM($ec,[$01]); SetC(0); SetOp([$66,$ec]); Test; CheckM($ec,[$00]); CheckSR('nZC');

  T('ror zp,x');        Init; SetM($22,[$10]); SetC(0); SetX($12); SetOp([$76,$10]); Test; CheckM($22,[$08]); CheckSR('nzc');
  T('ror zp,x+C:N');    Init; SetM($4c,[$10]); SetC(1); SetX($23); SetOp([$76,$29]); Test; CheckM($4c,[$88]); CheckSR('Nzc');
  T('ror zp,x:Z');      Init; SetM($6c,[$00]); SetC(0); SetX($34); SetOp([$76,$38]); Test; CheckM($6c,[$00]); CheckSR('nZc');
  T('ror zp,x:C');      Init; SetM($ac,[$81]); SetC(0); SetX($56); SetOp([$76,$56]); Test; CheckM($ac,[$40]); CheckSR('nzC');
  T('ror zp,x+C:CN');   Init; SetM($53,[$01]); SetC(1); SetX($78); SetOp([$76,$db]); Test; CheckM($53,[$80]); CheckSR('NzC');
  T('ror zp,x:CZ');     Init; SetM($ea,[$01]); SetC(0); SetX($fe); SetOp([$76,$ec]); Test; CheckM($ea,[$00]); CheckSR('nZC'); 

  T('ror abs');         Init; SetM($1110,[$10]); SetC(0); SetOp([$6e,$10,$11]); Test; CheckM($1110,[$08]); CheckSR('nzc');
  T('ror abs+C:N');     Init; SetM($2229,[$10]); SetC(1); SetOp([$6e,$29,$22]); Test; CheckM($2229,[$88]); CheckSR('Nzc');
  T('ror abs:Z');       Init; SetM($3338,[$00]); SetC(0); SetOp([$6e,$38,$33]); Test; CheckM($3338,[$00]); CheckSR('nZc');
  T('ror abs:C');       Init; SetM($9956,[$81]); SetC(0); SetOp([$6e,$56,$99]); Test; CheckM($9956,[$40]); CheckSR('nzC');
  T('ror abs+C:CN');    Init; SetM($77db,[$01]); SetC(1); SetOp([$6e,$db,$77]); Test; CheckM($77db,[$80]); CheckSR('NzC');
  T('ror abs:CZ');      Init; SetM($ddec,[$01]); SetC(0); SetOp([$6e,$ec,$dd]); Test; CheckM($ddec,[$00]); CheckSR('nZC');

  T('ror abs,x');       Init; SetM($1122,[$10]); SetC(0); SetX($12); SetOp([$7e,$10,$11]); Test; CheckM($1122,[$08]); CheckSR('nzc');
  T('ror abs,x+C:N');   Init; SetM($224c,[$10]); SetC(1); SetX($23); SetOp([$7e,$29,$22]); Test; CheckM($224c,[$88]); CheckSR('Nzc');
  T('ror abs,x:Z');     Init; SetM($336c,[$00]); SetC(0); SetX($34); SetOp([$7e,$38,$33]); Test; CheckM($336c,[$00]); CheckSR('nZc');
  T('ror abs,x:C');     Init; SetM($99ac,[$81]); SetC(0); SetX($56); SetOp([$7e,$56,$99]); Test; CheckM($99ac,[$40]); CheckSR('nzC');
  T('ror abs,x+C:CN');  Init; SetM($7853,[$01]); SetC(1); SetX($78); SetOp([$7e,$db,$77]); Test; CheckM($7853,[$80]); CheckSR('NzC');
  T('ror abs,x:CZ');    Init; SetM($deea,[$01]); SetC(0); SetX($fe); SetOp([$7e,$ec,$dd]); Test; CheckM($deea,[$00]); CheckSR('nZC');

  memoResults.Lines.Add('Tests completed');
end;


//
// SET CODES
//

procedure TTest6502Form.T(value: string);
begin
  sTestCode := value;
end;

procedure TTest6502Form.SetA(value: byte);
begin
  fCPU.RegisterValue[RegA] := value;
  SaveRegs[rA].Value := value;
end;

procedure TTest6502Form.SetX(value: byte);
begin
  fCPU.RegisterValue[RegX] := value;
  SaveRegs[rX].Value := value;
end;

procedure TTest6502Form.SetY(value: byte);
begin
  fCPU.RegisterValue[RegY] := value;
  SaveRegs[rY].Value := value;
end;

procedure TTest6502Form.SetSP(value: byte);
begin
  fCPU.RegisterValue[RegSP] := value;
  SaveRegs[rSP].Value := value;
end;

procedure TTest6502Form.SetPC(value: word);
begin
  fCPU.RegisterValue[RegPC] := value;
  SaveRegs[rPC].Value := value;
end;

procedure TTest6502Form.SetC(value: byte);
begin
  if (value = 0) then
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] and (not P_CARRY)
  else
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] or P_CARRY;
  SaveFlags[fC].Value := (value = 1);
end;

procedure TTest6502Form.SetZ(value: byte);
begin
  if (value = 0) then
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] and (not P_ZERO)
  else
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] or P_ZERO;
  SaveFlags[fZ].Value := (value = 1);
end;

procedure TTest6502Form.SetI(value: byte);
begin
  if (value = 0) then
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] and (not P_IRQ)
  else
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] or P_IRQ;
  SaveFlags[fI].Value := (value = 1);
end;

procedure TTest6502Form.SetD(value: byte);
begin
  if (value = 0) then
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] and (not P_DECIMAL)
  else
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] or P_DECIMAL;
  SaveFlags[fD].Value := (value = 1);
end;

procedure TTest6502Form.SetB(value: byte);
begin
  if (value = 0) then
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] and (not P_BRK)
  else
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] or P_BRK;
  SaveFlags[fB].Value := (value = 1);
end;

procedure TTest6502Form.SetV(value: byte);
begin
  if (value = 0) then
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] and (not P_OVERFLOW)
  else
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] or P_OVERFLOW;
  SaveFlags[fV].Value := (value = 1);
end;

procedure TTest6502Form.SetN(value: byte);
begin
  if (value = 0) then
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] and (not P_NEGATIVE)
  else
    fCPU.RegisterValue[RegPSW] := fCPU.RegisterValue[RegPSW] or P_NEGATIVE;
  SaveFlags[fN].Value := (value = 1);
end;

procedure TTest6502Form.SetM(addr:word; value: array of byte);
var
  i,j: integer;
begin
  j := addr;
  for i := 0 to length(value)-1 do
    begin
      fMemory[j] := value[i];
      inc(j);
    end;
end;

procedure TTest6502Form.SetOp(op: array of byte);
var
  i,j: integer;
begin
  j := fCPU.RegisterValue[RegPC];
  for i := 0 to length(op) do
    begin
      fMemory[j] := op[i];
      inc(j);
    end;
  SaveRegs[rPC].Value := j - 1;
end;


//
// EXECUTE THE TEST
//

procedure TTest6502Form.Test;
begin
  // UpdateRegisters;
  // ShowMessage('Ready for test [' + sTestCode + ']');
  fCPU.Step;
end;


//
// CHECK RESULTS
//

procedure TTest6502Form.CheckA(value: byte);
begin
  SaveRegs[rA].Checked := TRUE;
  if (fCPU.RegisterValue[RegA] <> value) then
    Error2('A', value, fCPU.RegisterValue[RegA]);
end;

procedure TTest6502Form.CheckSP(value: byte);
begin
  SaveRegs[rSP].Checked := TRUE;
  if (fCPU.RegisterValue[RegSP] <> value) then
    Error2('SP', value, fCPU.RegisterValue[RegSP]);
end;

procedure TTest6502Form.CheckPC(value: word);
begin
  SaveRegs[rPC].Checked := TRUE;
  if (fCPU.RegisterValue[RegPC] <> value) then
    Error4('PC', value, fCPU.RegisterValue[RegPC]);
end;

procedure TTest6502Form.CheckM(addr:word; value: array of byte);
var
  i,j: integer;
  sCheck: string;
begin
  sCheck := '';
  j := addr;
  for i := 0 to length(value)-1 do
    begin
      if fMemory[j] <> value[i] then
        sCheck := sCheck + Format('%.2x->%.2x ', [value[i], fMemory[j]]);
      inc(j);
    end;
  if (sCheck <> '') then
    sResult := sResult + Format('  Mem@%.4x: %s', [addr, sCheck]);
end;

procedure TTest6502Form.CheckSR(value: string);
var
  i: integer;
  ch: char;
  state: string;
  PSW: byte;
begin
    state := CHECK_STATE;
    PSW := fCPU.RegisterValue[RegPSW];
    for i := 1 to length(value) do
    begin
      ch := value[i];
      case ch of
        'C': if (PSW and P_CARRY) = 0 then
               state := state + ' C->c';
        'c': if (PSW and P_CARRY) <> 0 then
               state := state + ' c->C';
        'Z': if (PSW and P_ZERO) = 0 then
               state := state + ' Z->z';
        'z': if (PSW and P_ZERO) <> 0 then
               state := state + ' z->Z';
        'I': if (PSW and P_IRQ) = 0 then
               state := state + ' I->i';
        'i': if (PSW and P_IRQ) <> 0 then
               state := state + ' i->I';
        'D': if (PSW and P_DECIMAL) = 0 then
               state := state + ' D->d';
        'd': if (PSW and P_DECIMAL) <> 0 then
               state := state + ' d->D';
        // BRK flag not tested
        'V': if (PSW and P_OVERFLOW) = 0 then
               state := state + ' V->v';
        'v': if (PSW and P_OVERFLOW) <> 0 then
               state := state + ' v->V';
        'N': if (PSW and P_NEGATIVE) = 0 then
               state := state + ' N->n';
        'n': if (PSW and P_NEGATIVE) <> 0 then
               state := state + ' n->N';
      end;
      case UpCase(ch) of
        'C': SaveFlags[fC].Checked := TRUE;
        'Z': SaveFlags[fZ].Checked := TRUE;
        'I': SaveFlags[fI].Checked := TRUE;
        'D': SaveFlags[fD].Checked := TRUE;
        'V': SaveFlags[fV].Checked := TRUE;
        'N': SaveFlags[fN].Checked := TRUE;
      end;
    end;
  if (state <> CHECK_STATE) then
    sResult := sResult + ' ' + state;
  TestUncheckedRegs;
  TestUncheckedFlags;
  //
  if sResult = ERR_MSG then            // Errors?
    sResult := 'OK';
//  if sResult <> ERR_MSG then            // Errors?
  memoResults.Lines.Add('Test [' + sTestCode + ']: '+ sResult);
  //
  // UpdateRegisters;
  // ShowMessage('Test complete');
end;

procedure TTest6502Form.TestUncheckedRegs;
var
  i: TRegIdx;
begin
  for i := rA to rPC do
    begin
      if not SaveRegs[i].Checked then
        case i of
          rA: if (fCPU.RegisterValue[RegA] <> SaveRegs[i].Value) then
                Error2('A', SaveRegs[i].Value, fCPU.RegisterValue[RegA]);
          rX: if (fCPU.RegisterValue[RegX] <> SaveRegs[i].Value) then
                Error2('X', SaveRegs[i].Value, fCPU.RegisterValue[RegX]);
          rY: if (fCPU.RegisterValue[RegY] <> SaveRegs[i].Value) then
                Error2('Y', SaveRegs[i].Value, fCPU.RegisterValue[RegY]);
          rSP: if (fCPU.RegisterValue[RegSP] <> SaveRegs[i].Value) then
                Error2('SP', SaveRegs[i].Value, fCPU.RegisterValue[RegSP]);
          rPC: if (fCPU.RegisterValue[RegPC] <> SaveRegs[i].Value) then
                Error4('PC', SaveRegs[i].Value, fCPU.RegisterValue[RegPC]);
        end;
    end;
end;

procedure TTest6502Form.TestUncheckedFlags;
var
  i: TFlagIdx;
  state: string;
  PSW: byte;
begin
  state := CHECK_STATE;
  PSW := fCPU.RegisterValue[RegPSW];
  for i := fC to fN do
    begin
      if (not SaveFlags[i].Checked) then
        case i of
          fC: if (PSW and P_CARRY <> 0) <> SaveFlags[i].Value then
                state := state + ' Cflag';
          fZ: if (PSW and P_ZERO <> 0) <> SaveFlags[i].Value then
                state := state + ' Zflag';
          fI: if (PSW and P_IRQ <> 0) <> SaveFlags[i].Value then
                state := state + ' Iflag';
          fD: if (PSW and P_DECIMAL <> 0) <> SaveFlags[i].Value then
                state := state + ' Dflag';
          fV: if (PSW and P_OVERFLOW <> 0) <> SaveFlags[i].Value then
                state := state + ' Vflag';
          fN: if (PSW and P_NEGATIVE <> 0) <> SaveFlags[i].Value then
                state := state + ' Nflag';
        end;
    end;
  if (state <> CHECK_STATE) then
    sResult := sResult + ' ' + state;
end;


//
// REPORT ERRORS
//

procedure TTest6502Form.Error2(Item: string; Expected, Got: byte);
begin
  sResult := sResult + Format('  %s:%.2x->%.2x', [Item, Expected, Got]);
end;

procedure TTest6502Form.Error4(Item: string; Expected, Got: word);
begin
  sResult := sResult + Format('  %s:%.4x->%.4x', [Item, Expected, Got]);
end;


//
// MEMORY PROCESSES
//

procedure TTest6502Form.MemRead(Sender: TObject; Addr: word; var Value: byte);
begin
  Value := fMemory[Addr];
end;

procedure TTest6502Form.MemWrite(Sender: TObject; Addr: word; Value: byte);
begin
  fMemory[Addr] := Value;
end;


procedure TTest6502Form.btnCloseClick(Sender: TObject);
begin
  Close;
end;


end.
