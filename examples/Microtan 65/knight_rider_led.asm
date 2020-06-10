//
// Testing LEDs with a Knight Rider like sequence
//
// Assemble with 'Write code to memory' set
// Then in TANBUG use G400 to run
// Stop with Reset button
//

leds          equ   $B000

              org   $400,CODE          // Start of program code

start         ldx   #0
              lda   #0
loopLeft      ora   ledOn,X            // LED x on
              sta   leds
              jsr   delay
              ora   ledOn+1,x          // then LED x+1 on
              sta   leds
              jsr   delay
              and   ledOff,X           // LED x off, leaving x+1 on
              sta   leds
              jsr   delay
              jsr   delay
              inx
              cpx   #7                 // Cycle through all LEDs
              bne   loopLeft

              lda   #0
loopRight     ora   ledOn,X            // Then do in reverse
              sta   leds
              jsr   delay
              ora   ledOn-1,X
              sta   leds
              jsr   delay
              and   ledOff,X
              sta   leds
              jsr   delay
              jsr   delay
              dex
              bne   loopRight
              jmp   start              // And around again

//
// Delay, set by trial
//
delay         pha                      // Save index and current LEDs state
              txa
              pha
              ldx   #20                // Outer loop controlled via X
loop1         ldy   #0
loop2         dey                      // Inner loop 256 times via Y
              bne   loop2
              dex
              bne   loop1
              pla                      // Restore index and LEDs state
              tax
              pla
              rts

// Table of bits for switching each LED on via OR, or off via AND operation

ledOn         fcb   %0000.0001         // On
              fcb   %0000.0010
              fcb   %0000.0100
              fcb   %0000.1000
              fcb   %0001.0000
              fcb   %0010.0000
              fcb   %0100.0000
              fcb   %1000.0000

ledOff        fcb   %1111.1110         // Off
              fcb   %1111.1101
              fcb   %1111.1011
              fcb   %1111.0111
              fcb   %1110.1111
              fcb   %1101.1111
              fcb   %1011.1111
              fcb   %0111.1111

              end
