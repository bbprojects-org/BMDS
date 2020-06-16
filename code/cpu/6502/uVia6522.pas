{ ==============================================================================

  6522 VIA

    This class emulates elements of the 6522 Versatile Interface Adapter (VIA)
    including:
    - Timer 1 and 2 only

    From info in:
    1. http://archive.6502.org/datasheets/rockwell_r6522_via.pdf


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

unit uVia6522;

{$mode objfpc}{$H+}
{.$define via_debug}


interface

uses
  Classes, SysUtils {$ifdef via_debug}, uCommon{$endif} ;

type

  TOnInterruptEvent = TNotifyEvent;

  TRegs6522 = record
    DDRB: byte;                         // Data Direction Registers
    DDRA: byte;
    SR: byte;                           // Shift register
    ACR: byte;                          // Auxiliary control register
    PCR: byte;                          // Peripheral Control Register
    IFR: byte;                          // Interrupt Flag Register
    IER: byte;                          // Interrupt Enabl Register
  end;

  { TVia6522 }

  TVia6522 = class(TObject)
  private
    fRegs: TRegs6522;
    fOnInterrupt: TOnInterruptEvent;
    T1_Counter: integer;
    T1_Latch: word;
    T1_PB7: boolean;                    // Timer 1, output toggle bit
    T2_Counter: integer;
    T2_Latch: word;
  public
    constructor Create;
    destructor  Destroy; override;
    //
    procedure Reset;
    function  Read(addr: word): byte;
    procedure Write(addr: word; value: byte);
    procedure DoCycles(Cycles: integer);
    //
    property Regs: TRegs6522 read fRegs write fRegs;
    property OnInterrupt: TOnInterruptEvent read fOnInterrupt write fOnInterrupt;
  end;


implementation

const
  INTERRUPT_IRQ  = %10000000;           // Any interrupt in the 6522
  INTERRUPT_T1   = %01000000;           // Time-out of Timer 1
  INTERRUPT_T2   = %00100000;           // Time-out of Timer 2
  {
  INTERRUPT_CB1  = %00010000;           // Active transition on CB1 pin
  INTERRUPT_CB2  = %00001000;           // Active transition on CB2 pin
  INTERRUPT_SR   = %00000100;           // Completion of 8 shifts
  INTERRUPT_CA1  = %00000010;           // Active transition on CA1 pin
  INTERRUPT_CA2  = %00000001;           // Active transition on CA2 pin
  }
  ACR_T1PB7_EN   = %10000000;           // 1=PB7 enabled, 0=PB7 disabled
  ACR_T1_FREERUN = %01000000;           // 1=continuous interrupts, 0=one-shot
  ACR_T2_PULSES  = %00100000;           // 1=countdown with pulses on PB6, 0=one-shot
  {
  ACR_SHIFT_IO   = %00010000;           // 1=output mode, 0=input
  ACR_SHIFT_M1   = %00001000;           // Two bits give source of clock pulses:
  ACR_SHIFT_M0   = %00000100;           // 00=disabled, 01=T2, 10=sysclock, 11=ext clock
  }

{ CREATE / DESTROY }

constructor TVia6522.Create;
begin
  Reset;
end;


destructor TVia6522.Destroy;
begin
  //
  inherited Destroy;
end;


{ RESET }

procedure TVia6522.Reset;
begin
  // Ref#1 says "Reset clears all internal registers (except T1, T2 and SR)"
  fRegs.DDRB := 0;
  fRegs.DDRA := 0;
  fRegs.ACR := 0;
  fRegs.PCR := 0;
  fRegs.IFR := 0;
  fRegs.IER := 0;
end;


{ WRITE }

procedure TVia6522.Write(addr: word; value: byte);
begin
  case (addr and $0F) of
    0: begin                            // ORB
         // Nothing implemented
       end;

    1: begin                            // ORA
         // Nothing further implemented
       end;

    2: begin                            // DDRB
         fRegs.DDRB := value;
         // Nothing further implemented
       end;

    3: begin                            // DDRA
         fRegs.DDRA := value;
         // Nothing further implemented
       end;

    4,                                  // T1CL / T1LL (same action)
    6: begin
         T1_Latch := (T1_Latch and $FF00) or value;
       end;

    5: begin                            // T1CH
         T1_Latch := (T1_Latch and $FF) or (value shl 8);
         T1_Counter := T1_Latch;
         if ((fRegs.ACR and ACR_T1PB7_EN) > 0) then
           // If PB7 enabled, set it low. Goes high when times out
           T1_PB7 := False;
         fRegs.IFR := fRegs.IFR and (not INTERRUPT_T1);
       end;

    7: begin                            // T1LH
         T1_Latch := (T1_Latch and $FF) or (value shl 8);
       end;

    8: begin                            // T2CL
         T2_Latch := (T2_Latch and $FF00) or value;
       end;

    9: begin                            // T2CH
         T2_Latch := (T2_Latch and $FF) or (value shl 8);
         T2_Counter := T2_Latch;
         fRegs.IFR := fRegs.IFR and (not INTERRUPT_T2);
       end;

    10: begin                           // SR
          fRegs.SR := value;
          // Nothing further implemented
        end;

    11: begin                           // ACR
          fRegs.ACR := value;
        end;

    12: begin                           // PCR
          fRegs.PCR := value;
          // Nothing further implemented
        end;

    13: begin                           // IFR
          // Read only
        end;

    // If bit 7 = 0, each 1 in bits 6 thru 0 clears corresponding bit in IER
    // If bit 7 = 1, each 1 in bits 6 thru 0 sets corresponding bit in IER
    14: begin                           // IER
          if ((value and $80) > 0) then
            fRegs.IER := fRegs.IER or (value and $7F)
          else
            fRegs.IER := fRegs.IER and (not (value and $7F));
        end;

    15: begin                           // ORA, no effect on handshake
          // Nothing implemented
        end;
  end;
end;


{ READ }

function TVia6522.Read(addr: word): byte;
begin
  case (addr and $0F) of
    0: begin                            // IRB
         Result := $FF;                 // Nothing to read, not implemented
       end;

    1: begin                            // IRA
         Result := $FF;                 // Nothing to read, not implemented
       end;

    2: begin                            // DDRB
         Result := fRegs.DDRB
       end;

    3: begin                            // DDRA
         Result := fRegs.DDRA
       end;

    4: begin                            // T1CL, also clears interrupt flag
         fRegs.IFR := fRegs.IFR and (not INTERRUPT_T1);
         Result := T1_Counter and $FF;
       end;

    5: begin                            // T1CH
         Result := (T1_Counter shr 8) and $FF;
       end;

    6: begin                            // T1LL
         Result := T1_Latch and $FF;
       end;

    7: begin                            // T1LH
         Result := (T1_Latch shr 8) and $FF;
       end;

    8: begin                            // T2CL, also clears interrupt flag
         fRegs.IFR := fRegs.IFR and (not INTERRUPT_T2);
         Result := T2_Counter and $FF;
       end;

    9: begin                            // T2CH
         Result := (T2_Counter shr 8) and $FF;
       end;

    10: begin                           // SR
          Result := fRegs.SR;
        end;

    11: begin                           // ACR
          Result := fRegs.ACR;
        end;

    12: begin                           // PCR
          Result := fRegs.PCR;
        end;

    13: begin                           // IFR
          if ((fRegs.IFR and fRegs.IER and $7F) > 0) then
            Result := fRegs.IFR or INTERRUPT_IRQ         // Set bit7 if *any* interrupt in 6522
          else
            Result := fRegs.IFR and (not INTERRUPT_IRQ); // else make sure it's clear
        end;

    14: begin                           // IER
          Result := fRegs.IER or $80 ;  // Bit 7 reads as 1
        end;

    15: begin                           // IRA, no effect on handshake
          Result := $FF;                // Nothing to read, not implemented
        end;
  end;
end;


{ DO CYCLES }

procedure TVia6522.DoCycles(Cycles: integer);
var
  IsInterrupt: boolean;
begin
  IsInterrupt := False;

  if (T1_Counter > 0) then
    Dec(T1_Counter, Cycles);
  if (T1_Counter <= 0) then
    begin
      if ((fRegs.IER and INTERRUPT_T1) > 0) then
        begin
          fRegs.IFR := fRegs.IFR or INTERRUPT_T1;
          IsInterrupt := True;
        end;
      if ((fRegs.ACR and ACR_T1_FREERUN) > 0) then
        // If free-run, reset counter from latch
        T1_Counter := T1_Latch
      else
        // Else just roll around in 16-bits
        T1_Counter := $FFFF;
      if ((fRegs.ACR and ACR_T1PB7_EN) > 0) then
        // If PB7 enabled, toggle it
        T1_PB7 := not T1_PB7;
    end;

  if ((fRegs.ACR and ACR_T2_PULSES) = 0) then // If timer 2 in one-shot...
    begin
      Dec(T2_Counter, Cycles);
      if ((T2_Counter < 0) and ((fRegs.IER and INTERRUPT_T2) > 0)) then
        begin
          fRegs.IFR := fRegs.IFR or INTERRUPT_T2;
          IsInterrupt := True;
        end;
    end;

  {$ifdef via_debug}
  if (fRegs.IER > 0) then
    AppLog.Debug('TVia6522.DoCycles, T1C=%d, interrupt=%s', [T1_Counter, BoolToStr(IsInterrupt,'T','F')]);
  {$endif}

  if ((IsInterrupt) and Assigned(fOnInterrupt)) then
    fOnInterrupt(self);
end;


end.

