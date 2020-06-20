//
// Test the 6522 VIA timer
//
// Set up timer to generate an interrupt every 20ms. In the ISR increment
// a HH:MM:SS timer which is displayed and updated every second. The program
// also updates the LEDs with the seconds value
// Pressing SPACE bar exits the timer and returns to TANBUG
//
// Use G400 to execute
//

// I/O Defines

LEDS          equ   $B000               // Emulated LEDs address

T1CL          equ   $BFC4               // VIA Timer 1 Counter Lo/Hi
T1CH          equ   $BFC5
ACR           equ   $BFCB               // VIA Aux Control Register
IFR           equ   $BFCD               // VIA Interrupt Flag Register
IER           equ   $BFCE               // VIA Interrupt Enable Register

// TANBUG defines

ICHAR         equ   $0001               // Keyboard input character
VDUIND        equ   $0003               // Display index
ICURS         equ   $000A               // Cursor index
INTSL         equ   $0010               // Slow interrupt jump link

OPCHR         equ   $FE75               // Output a characters, saves XY
RETMON        equ   $F823               // Return to TANBUG


              org   $80,RAM

// Zero page variables for this program

TempINTSL     rmb   3                  // Temporary store
Count20ms     rmb   1                  // Count every 20ms
Seconds       rmb   1                  // Timer bytes
Minutes       rmb   1
Hours         rmb   1


              org   $400,CODE

// Program start, use G400 to execute

Start         ldx   #2
SaveIt        lda   INTSL,X            // Save current INTSL vector (normally
              sta   TempINTSL,X        // just an RTI instruction)
              dex
              bne   SaveIt

              lda   #$4C               // Setup IRQ to jump to our ISR
              sta   INTSL
              lda   #<ISR
              sta   INTSL+1
              lda   #>ISR
              sta   INTSL+2

              stx   LEDS               // Initialise LEDs all off
              stx   Hours              // and Timer = 00:00:00
              stx   Minutes
              stx   Seconds

Timer         stx   Count20ms          // Initialise interrupt counter
              jsr   SetupVIA           // and setup the VIA Timer 1

              inc   VDUIND             // Make space for timer output
              inc   VDUIND

PrtTimer      ldy   VDUIND             // Get timer screen position
              lda   Hours
              jsr   Print2Dec          // and output the timer
              lda   #':'
              sta   (ICURS),Y
              iny
              lda   Minutes
              jsr   Print2Dec
              lda   #':'
              sta   (ICURS),Y
              iny
              lda   Seconds
              sta   LEDS               // and set the LEDs too
              jsr   Print2Dec
              ldx   Seconds

TimerLoop     cpx   Seconds            // Wait for seconds to change
              bne   PrtTimer
CheckKey      lda   ICHAR              // or SPACE key pressed
              cmp   #32
              bne   TimerLoop

              ldx   #2                 // Stop, put things back to how they were
RestoreIt     lda   TempINTSL,X
              sta   INTSL,X
              dex
              bne   RestoreIt
              jsr   T1Off              // Disable Timer 1

              lda   #15
              sta   VDUIND             // Move cursor after timer and
              ldx   #0                 // let user know all done
OutMsg        lda   Msg,X
              beq   Finish
              jsr   OPCHR
              inx
              bne   OutMsg             // Unconditional

Finish        jmp   RETMON             // Exit back to TANBUG

Msg           fcb   'Stopped', 13, 0


// Setup the VIA for an interrupt every 20ms

SetupVIA      lda   #$98               // $3A98 = 15000 = 20ms @ 750kHz
              sta   T1CL
              lda   #$3A
              sta   T1CH               // This write starts timer
              lda   ACR
              and   #%0111.1111        // Disable toggle PB7 output
              ora   #%0100.0000        // Interrupt every time-out (free-run)
              sta   ACR
T1On          lda   #%1100.0000        // Enable Timer 1 interrupt
              bne   SetIER             // bit7=set, bit6=T1

T1Off         lda   #%0100.0000        // Disable Timer 1, bit7=clear, bit6=T1
SetIER        sta   IER
              rts


// Print ACC as two decimal digits (ACC <= 99)
// On entry Y points to current screen position

Print2Dec     ldx   #$FF
              sec                      // Prepare for subtraction
PrDec10s      inx
              sbc   #10
              bcs   PrDec10s           // Count how many 10s
              adc   #10
              jsr   PrDecDigit         // Print the 10s

PrDec1s       tax                      // Pass 1s into X

PrDecDigit    pha
              txa
              ora   #'0'               // Convert to char and write to screen
              sta   (ICURS),Y
              iny
              pla
              rts


// This is the Interrupt Service Routine
// Increment the 20ms counter until reach 50 (=1sec) then IncSecs, IncMins, ...

ISR           pha
              lda   T1CL               // Clear Timer 1 interrupt
              inc   Count20ms
              lda   Count20ms
              cmp   #50                // Counted 50 x 20ms = 1s ?
              bmi   EndISR
              lda   #0                 // Reset count for next 50
              sta   Count20ms

IncSecs       inc   Seconds            // Count the seconds
              lda   Seconds
              cmp   #60
              bne   EndISR
              lda   #0                 // Reset seconds, and...
              sta   Seconds

IncMins       inc   Minutes            // Increment the minutes
              lda   Minutes
              cmp   #60
              bne   EndISR
              lda   #0                 // Reset minutes, and ...
              sta   Minutes

IncHrs        inc   Hours              // Increment the hours
              lda   Hours
              cmp   #24
              bne   EndISR
              lda   #0                 // Reset hours
              sta   Hours

EndISR        pla
              rti


              end
