;===============================================================================
;
;  Code for TAITO Space Invaders from 1978
;
;  Based on http://www.computerarcheology.com/Arcade/SpaceInvaders/Code.html
;
;===============================================================================

; It is never completely finished:
;  * TODO Look at the various versions of the ROMs and see what's different
;  * TODO Check all X/Y references and make !Xr/Yr or !Xn/Yn

; A word on coordinates. Sometimes the code is easier to understand in the
; context of the screen in standard (no rotated) position. The comments will
; refer to Xn (X not-rotated) and Yn. In other instances the code is easier
; to understand in the context of the rotated screen. The comments will refer
; to Xr (X rotated) and Yr.

;
; DEFINE SYSTEM VARIABLES
;

                   ORG   $2000,RAM     ; Variables in RAM start at $2000

WaitOnDraw         DS    2             ; Cleared by alien-draw and set by next-alien. This
                                       ; ensures no alien gets missed while drawing
AlienIsExploding   DS    1             ; Not 0 if an alien is exploding, 0 if not exploding
ExpAlienTimer      DS    1             ; Time (ISR ticks) left in alien-explosion
AlienRow           DS    1             ; Row number of current alien (cursor)
AlienFrame         DS    1             ; Animation frame number (0 or 1) for current alien (cursor)
AlienCurIndex      DS    1             ; Alien cursor index (from 0 to 54)
RefAlienDYr        DS    1             ; Reference alien delta Yr
RefAlienDXr        DS    1             ; Reference alien deltaXr
RefAlienYr         DS    1             ; Reference alien Yr coordinate
RefAlienXr         DS    1             ; Reference alien Xr coordinate
AlienPos           DS    2             ; Alien cursor bit pos
RackDirection      DS    1             ; 0 if rack moving right, 1 if moving left
RackDownDelta      DS    2             ; Constant value of alien rack dropping after
                                       ; bumping screen edge

; GameObject0 (Move/draw the player)

Obj0Timer          DS    2             ; Wait 128 interrupts (~2s) before player task starts
Obj0TimerExtra     DS    1
Obj0Handler        DS    2             ; Player handler code at 028E
PlayerAlive        DS    1             ; Player is alive (FF=alive). Toggles between 0 and 1
                                       ; for blow-up images
ExpAnimateTimer    DS    1             ; Time till next blow-up sprite change (reloaded to 5)
ExpAnimateCnt      DS    1             ; Number of changes left in blow-up sequence
PlayerSprPic       DS    2             ; Player sprite descriptor... picture
PlayerYr           DS    1             ; Player sprite descriptor... location
PlayerXr           DS    1             ; Player sprite descriptor... location
PlyrSprSiz         DS    1             ; Player sprite descriptor... size of sprite
NextDemoCmd        DS    1             ; Next movement command for demo
HidMessSeq         DS    1             ; Set to 1 after 1st of 2 sequences are entered for
                                       ; hidden-message display

                   DS    1             ; Unused?

; GameObject1 (Move/draw the player shot)

Obj1Timer          DS    2
Obj1TimerExtra     DS    1             ; All 0's... run immediately
Obj1Handler        DS    2             ; Shot handler code at 03BB
PlyrShotStatus     DS    1             ; 0 if available,
                                       ; 1 if just initiated,
                                       ; 2 moving normally,
                                       ; 3 hit something besides alien,
                                       ; 4 if alien has exploded (remove from active duty)
                                       ; 5 if alien explosion is in progress
BlowUpTimer        DS    1             ; Sprite blow-up timer
Obj1Image          DS    2             ; Sprite image at 1C90 (just one byte)
Obj1CoorYr         DS    1             ; Player shot Y coordinate
Obj1CoorXr         DS    1             ; Player shot X coordinate
Obj1ImageSize      DS    1             ; Size of shot image (just one byte)
ShotDeltaX         DS    1             ; Shot's delta X
FireBounce         DS    1             ; 1 if button has been handled but remains down

                   DS    2             ; Unused?

; GameObject2 (Alien rolling-shot)

Obj2Timer          DS    2
Obj2TimerExtra     DS    1             ; GO-3 runs when this is 1. GO-4 runs when this
                                       ; is 2. (copied to 2080 in game loop)
Obj2Handler        DS    2             ; Handler code at 0476
RolShotStatus      DS    1
RolShotStepCnt     DS    1
RolShotTrack       DS    1             ; A 0 means this shot tracks the player
RolShotColFirTbl   DS    2             ; Pointer to column-firing table (not used for targeting)
RolShotBlowCnt     DS    1
RolShotImage       DS    2
RolShotYr          DS    1
RolShotXr          DS    1
RolShotSize        DS    1

; GameObject3 (Alien plunger-shot)

Obj3Timer          DS    2
Obj3TimerExtra     DS    1
Obj3Handler        DS    2             ; Handler code at 04B6
PluShotStatus      DS    1
PluShotStepCnt     DS    1
PluShotTrack       DS    1             ; A 1 means this shot does not track the player
PluShotColFirTbl   DS    2             ; Pointer to column-firing table
PluShotBlowCnt     DS    1
PluShotImage       DS    2
PluShotYr          DS    1
PluSHotXr          DS    1
PluShotSize        DS    1

; GameObject4 (Flying saucer OR alien squiggly shot)

Obj4Timer          DS    2
Obj4TimerExtra     DS    1
Obj4Handler        DS    2             ; Handler code at 0682
SquShotStatus      DS    1
SquShotStepCnt     DS    1
SquShotTrack       DS    1             ; A 1 means this shot does not track the player
SquShotColFirTbl   DS    2             ; Pointer to column-firing table
SquShotBlowCnt     DS    1
SquShotImage       DS    2
SquShotYr          DS    1
SquShotXr          DS    1
SquShotSize        DS    1

EndOfTasks         DS    1             ; FF marks the end of the tasks list
Collision          DS    1             ; Set to 1 if sprite-draw detects collision
ExpAlien           DS    2             ; Exploding alien picture 1CC0
ExpAlienYr         DS    1             ; Y coordinate of exploding alien
ExpAlienXr         DS    1             ; X coordinate of exploding alien
ExpAlienSize       DS    1             ; Size of exploding alien sprite (16 bytes)
PlayerDataMSB      DS    1             ; Current player's data-pointer MSB (21xx or 22xx)
PlayerOK           DS    1             ; 1 means OK, 0 means blowing up
EnableAlienFire    DS    1             ; 1 means aliens can fire, 0 means not
AlienFireDelay     DS    1             ; Count down till aliens can fire (2069 flag is then set)
OneAlien           DS    1             ; 1 when only one alien is on screen
Temp206C           DS    1             ; Holds the value ten... number of characters in each
                                       ; '=xx POINTS' string but gets set to 18 in mem
                                       ; copy before game
Invaded            DS    1             ; Set to 1 when player blows up because rack has
                                       ; reached bottom
SkipPlunger        DS    1             ; When there is only one alien left this goes to 1
                                       ; to disable the plunger-shot when it ends

                   DS    1             ; Unused?

OtherShot1         DS    1             ; When processing a shot, this holds one of the
                                       ; other shot's info
OtherShot2         DS    1             ; Ditto
VBlankStatus       DS    1             ; 80=screen is being drawn (don't touch),
                                       ; 0=blanking in progress (ok to change)

; Alien shot information (copied from the 3 actual structures when processed)

AlienShotStatus    DS    1             ; Bit 0 set if shot is blowing up,
                                       ; bit 7 set if active
AlienShotStepCnt   DS    1             ; Count of steps made by shot (used for fire
                                       ; reload rate)
AlienShotTrack     DS    1             ; 0 if shot tracks player or 1 if uses the column-fire table
AlienShotColFirTbl DS    2             ; Pointer to column-firing table
AlienShotBlowCnt   DS    1             ; Alien shot blow up counter. At 3 the explosion is
                                       ; drawn. At 0 it is done
AlienShotImage     DS    2             ; Alien shot image
AlienShotYr        DS    1             ; Alien shot delta Y
AlienShotXr        DS    1             ; Alien shot delta X
AlienShotSize      DS    1             ; Alien shot size
AlienShotDelta     DS    1             ; Alien shot speed. Normally -1 but set to -4 with
                                       ; less than 9 aliens
ShotPicEnd         DS    1             ; The last picture in the current alien shot animation
ShotSync           DS    1             ; All 3 shots are synchronised to the GO-2 timer,
                                       ; this is copied from timer in the game loop
Tmp2081            DS    1             ; Used to hold the remember/restore flag in
                                       ; shield-copy routine
NumAliens          DS    1             ; Number of aliens on screen

SaucerStart        DS    1             ; Flag to start saucer (set to 1 when 2091:2092 counts down to 0)
SaucerActive       DS    1             ; Saucer is on screen (1 means yes)
SaucerHit          DS    1             ; Saucer has been hit (1 means draw it but don't move it)
SaucerHitTime      DS    1             ; Hit-sequence timer (explosion drawn at 1F, score drawn at 18)

SaucerPriLoc       DS    2             ; Mystery ship print descriptor... coordinate
SaucerPriPicLSB    DS    1             ; Mystery ship print descriptor... message
SaucerPriPicMSB    DS    1
SaucerPriSize      DS    1             ; Mystery ship print descriptor ... number of characters
SaucerDeltaY       DS    1             ; Mystery ship delta Y
SaucerScore        DS    2             ; Pointer into mystery-ship score table
ShotCount          DS    2             ; Bumped every shot-removal. Saucer's direction
                                       ; is bit 0 (0=2/29, 1=-2/E0)
TillSaucer         DS    2             ; Count down every game loop. When it reaches
                                       ; 0 saucer is triggered. Reset to 600.
WaitStartLoop      DS    1             ; 1=in wait-for-start loop, 0=in splash screens

SoundPort3         DS    1             ; Current status of sound port (out $03)
ChangeFleetSnd     DS    1             ; Set to 1 in ISR if time to change the fleet sound
FleetSndCnt        DS    1             ; Delay until next fleet movement tone
FleetSndReload     DS    1             ; Reload value for fleet sound counter
SoundPort5         DS    1             ; Current status of sound port (out $05)
ExtraHold          DS    1             ; Duration counter for extra-ship sound
Tilt               DS    1             ; 1 if tilt handling is in progress
FleetSndHold       DS    1             ; Time to hold fleet-sound at each change

                   DS    5             ; Unused?

; In the ROM mirror copied to RAM this is the image of the alien sprite pulling the upside down Y.
; The code expects it to be 0030 below the second animation picture at $1BD0. This RAM area must be
; unused. The copy is wasted

                   DS    31

; End of initialisation copy from ROM mirror
; These are copied once from ROM at startup ($01E6)

IsrDelay           DS    1             ; Delay counter decremented in ISR
IsrSplashTask      DS    1             ; 1=In demo, 2=Little-alien and Y, 4=shooting extra 'C'

; Splash screen animation structure

SplashAnForm       DS    1             ; Image form (increments each draw)
SplashDeltaX       DS    1             ; Delta X
SplashDeltaY       DS    1             ; Delta Y
SplashYr           DS    1             ; Y coordinate
SplashXr           DS    1             ; X coordinate
SplashImage        DS    2             ; Base image
SplashImageSize    DS    1             ; Size of image (16 bytes)
SplashTargetY      DS    1             ; Target Y coordinate
SplashReached      DS    1             ; Reached target Y flag (1 when reached)
SplashImageRest    DS    2             ; Base image for restore

TwoPlayers         DS    1             ; 1 for two players, 0 for 1 player
AlienShotRelRate   DS    1             ; Based on the MSB of the player's score...
                                       ; how fast the aliens reload their shots

                   DS    21            ; This is where the alien-sprite-carrying-Y lives in ROM

Player1Ex          DS    1             ; Extra ship has been awarded = 0
Player2Ex          DS    1             ; ditto
Player1Alive       DS    1             ; 1 if player is alive, 0 if dead (after last man)
Player2Alive       DS    1             ; ditto
SuspendPlay        DS    1             ; 1=game things moving, 0=game things suspended
CoinSwitch         DS    1             ; 1=switch down, 0=switch up (used to debounce switch)
NumCoins           DS    1             ; number of coin credits in BCD format (99 max)
SplashAnimate      DS    1             ; 0 for animation during splash and 1 for not,
                                       ; this alternates after every cycle
DemoCmdPtr         DS    2             ; pointer to demo commands 1663
GameMode           DS    1             ; 1=game running, 0=demo or splash screens

                   DS    1             ; Unused?

AdjustScore        DS    1             ; Set to 1 if score needs adjusting
ScoreDelta         DS    2             ; Score adjustment
HiScore            DS    2             ; Hi-score descriptor... value
HiScoreLoc         DS    2             ; Hi-score descriptor... location
P1Score            DS    2             ; Hi-score descriptor... value
P1ScoreLoc         DS    2             ; Hi-score descriptor... location
P2Score            DS    2             ; Hi-score descriptor... value
P2ScoreLoc         DS    2             ; Hi-score descriptor... location

; Player 1 specific data

P1_Data            DS    55            ; Player 1 alien ship indicators (0=dead) 11*5 = 55

                   DS    11            ; Unused 11 bytes (room for another row of aliens?)

P1_Shields         DS    176           ; Player 1 shields remembered between rounds
                                       ; 44 bytes * 4 shields ($B0 bytes)

                   DS    9             ; Unused?

P1RefAlienDX       DS    1             ; Player 1 reference-alien delta X
P1RefAlienY        DS    1             ; Player 1 reference-alien Y coordinate
P1RefAlienX        DS    1             ; Player 1 reference-alien X coordinate
P1RackCnt          DS    1             ; Player 1 rack-count (starts at 0 but gets
                                       ; incremented to 1-8)
P1ShipsRem         DS    1             ; Ships remaining after current dies

; Player 2 specific data

P2_Data            DS    55            ; Player 2 alien ship indicators (0=dead) 11*5 = 55

                   DS    11            ; Unused 11 bytes (room for another row of aliens?)

P2_Shields         DS    176           ; Player 2 shields remembered between rounds
                                       ; 44 bytes * 4 shields ($B0 bytes)

                   DS    9             ; Unused?

P2RefAlienDX       DS    1             ; Player 2 reference-alien delta X
P2RefAlienYr       DS    1             ; Player 2 reference-alien Y coordinate
P2RefAlienXr       DS    1             ; Player 2 reference-alien X coordinate
P2RackCnt          DS    1             ; Player 2 rack-count (starts at 0 but gets
                                       ; incremented to 1-8)
P2ShipsRem         DS    1             ; Ships remaining after current dies

;
; DEFINE SYSTEM CONSTANTS
;

INP0               EQU    0            ; Read, mapped in hardware but never used by the code
INP1               EQU    1            ; Read
INP2               EQU    2            ; Read
SHFT_IN            EQU    3            ; Read

SHFT_AMNT          EQU    2            ; Write
SOUND1             EQU    3            ; Write
SHFT_DATA          EQU    4            ; Write
SOUND2             EQU    5            ; Write
WATCHDOG           EQU    6            ; Write

STACK_TOP          EQU    $2400        ; 2300:23DD unused, 23DE:23FF in the emulator the
                                       ; stack consumes this area (roughly 16 levels)


;===============================================================================
; START OF PROGRAM
;===============================================================================

                   ORG $0000,CODE                ; Code resides from $0000 upwards

; Execution begins here on power-up and reset

Reset              NOP                           ; This provides a slot ...
                   NOP                           ; ... to put in a JP for ...
                   NOP                           ; ... development
                   JP    Initialise              ; Continue startup at 18D4
                   NOP                           ; Padding before next fixed ISR address
                   NOP

; Interrupt brings us here when the beam is *near* the middle of the screen.
; The real middle would be 224/2 = 112. The code pretends this interrupt
; happens at line 128

ScanLine96         PUSH  AF                      ; Save ...
                   PUSH  BC                      ; ...
                   PUSH  DE                      ; ...
                   PUSH  HL                      ; ... everything
                   JP    ContScanLine96          ; Continue ISR at $008C
                   NOP                           ; Padding before fixed ISR address

; Interrupt brings us here when the beam is at the end of the screen (line 224) when the
; VBLANK begins

ScanLine224        PUSH  AF                      ; Save ...
                   PUSH  BC                      ; ...
                   PUSH  DE                      ; ...
                   PUSH  HL                      ; ... everything
                   LD    A,$80                   ; Flag that tells objects on the lower ...
                   LD    (VBlankStatus),A        ; ... half of the screen to draw/move
                   LD    HL,IsrDelay             ; Decrement the general ...
                   DEC   (HL)                    ; ... countdown (used for pauses)
                   CALL  CheckHandleTilt         ; Check and handle TILT
                   IN    A,(INP1)                ; Read coin switch
                   RRCA                          ; Has a coin been deposited (bit 0)?
                   JP    C,CoinDeposited         ; Yes... note that switch is closed and
                                                 ; continue with A=1
                   LD    A,(CoinSwitch)          ; Switch is now open. Was it ...
                   AND   A                       ; ... closed last time?
                   JP    Z,Temp_0042             ; No ... skip registering the credit

; Handle bumping credit count

                   LD    A,(NumCoins)            ; Number of credits in BCD
                   CP    $99                     ; 99 credits already?
                   JP    Z,Already99             ; Yes ... ignore this (better than rolling over to 00)
                   ADD   A,$01                   ; Bump number of credits
                   DAA                           ; Make it binary coded decimal
                   LD    (NumCoins),A            ; New number of credits
                   CALL  DrawNumCredits          ; Draw credits on screen
Already99          XOR   A                       ; Credit switch ...
Temp_003F          LD    (CoinSwitch),A          ; ... has opened

Temp_0042          LD    A,(SuspendPlay)         ; Are we moving ...
                   AND   A                       ; ... game objects?
                   JP    Z,Temp_0082             ; No ... restore registers and out
                   LD    A,(GameMode)            ; Are we in ...
                   AND   A                       ; ... game mode?
                   JP    NZ,MainGameLoop         ; Yes ... go process game-play things and out
                   LD    A,(NumCoins)            ; Number of credits
                   AND   A                       ; Are there any credits (player standing there)?
                   JP    NZ,Temp_005D            ; Yes ... skip any ISR animations for the splash screens
                   CALL  ISRSplTasks             ; Process ISR tasks for splash screens
                   JP    $0082                   ; Restore registers and out

; At this point no game is going and there are credits

Temp_005D          LD    A,(WaitStartLoop)       ; Are we in the ...
                   AND   A                       ; ... 'press start' loop?
                   JP    NZ,Temp_0082            ; Yes ... restore registers and out
                   JP    WaitForStart            ; Start the 'press start' loop

; Mark credit as needing registering

CoinDeposited      LD    A,$01                   ; Remember switch ...
                   LD    (CoinSwitch),A          ; ... state for debounce
                   JP    Temp_003F               ; Continue

; Main game-play timing loop

MainGameLoop       CALL  TimeFleetSound          ; Time down fleet sound and sets flag if needs new
                   LD    A,(Obj2TimerExtra)      ; Use rolling shot's timer to sync ...
                   LD    (ShotSync),A            ; ... other two shots
                   CALL  DrawAlien               ; Draw the current alien (or exploding alien)
                   CALL  RunGameObjs             ; Process game objects (including player object)
                   CALL  TimeToSaucer            ; Count down time to saucer

                   NOP

Temp_0082          POP   HL                      ; Restore ...
                   POP   DE                      ; ...
                   POP   BC                      ; ...
                   POP   AF                      ; ... everything
                   EI                            ; Enable interrupts
                   RET                           ; Done

                   NOP
                   NOP
                   NOP
                   NOP

; Continues here from scanline 96 ISR

ContScanLine96     XOR   A                       ; Flag that tells ...
                   LD    (VBlankStatus),A        ; ... objects on the upper half of screen to draw/move
                   LD    A,(SuspendPlay)         ; Are we moving ...
                   AND   A                       ; ... game objects?
                   JP    Z,$0082                 ; No ... restore and return
                   LD    A,(GameMode)            ; Are we in ...
                   AND   A                       ; ... game mode?
                   JP    NZ,$00A5                ; Yes .... process game objects and out
                   LD    A,(IsrSplashTask)       ; Splash-animation tasks
                   RRCA                          ; If we are in demo-mode then we'll process the tasks anyway
                   JP    NC,$0082                ; Not in demo mode ... done
;
                   LD    HL,Obj1Timer            ; Game object table (skip player-object at 2010)
                   CALL  $024B                   ; Process all game objects (except player object)
                   CALL  CursorNextAlien         ; Advance cursor to next alien (move the alien if it is last one)
                   JP    $0082                   ; Restore and return
;
; Initialize the player's rack of aliens. Copy the reference-location and deltas from the player's data bank
;
InitRack           CALL  GetAlRefPtr             ; 2xFC Get current player's ref-alien position pointer
                   PUSH  HL                      ; Hold pointer
                   LD    A,(HL)                  ; Get player's ...
                   INC   HL                      ; ... ref-alien ...
                   LD    H,(HL)                  ; ...
                   LD    L,A                     ; ... coordinates
                   LD    (RefAlienYr),HL         ; Set game's reference alien's X,Y
                   LD    (AlienPos),HL           ; Set game's alien cursor bit position
                   POP   HL                      ; Restore pointer
                   DEC   HL                      ; 21FB or 22FB ref alien's delta (left or right)
                   LD    A,(HL)                  ; Get ref alien's delta X
                   CP    $03                     ; If there is one alien it will move right at 3
                   JP    NZ,$00C8                ; Not 3 ... keep it
                   DEC   A                       ; If it is 3, back it down to 2 until it switches again
                   LD    (RefAlienDXr),A         ; Store alien deltaY
                   CP    $00FE                   ; Moving left?
                   LD    A,$0000                 ; Value of 0 for rack-moving-right (not XOR so flags are unaffected)
                   JP    NZ,$00D3                ; Not FE ... keep the value 0 for right
                   INC   A                       ; It IS FE ... use 1 for left
                   LD    (RackDirection),A       ; Store rack direction
                   RET                           ; Done

                   LD    A,$02                   ; Set ...
                   LD    (P1RefAlienDX),A        ; ... player 1 and 2 ...
                   LD    (P2RefAlienDX),A        ; ... alien delta to 2 (right 2 pixels)
                   JP    $08E4                   ;

                   DS    30, 0                   ; Unused space?

; This is heavily patched from a previous version of the code. There was a test here to
; jump to a self-test routine on startup (based on a dip switch). Even the original code
; padded with zeros to make the next function begin at 0100. Room for expansion?

; 2006 holds the index into the alien flag data grid. 2067 holds the MSB of the pointer
; (21xx or 22xx). If there is an alien exploding time it down. Otherwise draw the alien
; if it alive (or skip if it isn't). If an alien is drawn (or blank) then the 2000
; alien-drawing flag is cleared

DrawAlien          LD    HL,AlienIsExploding     ; Is there an ...
                   LD    A,(HL)                  ; ... alien ...
                   AND   A                       ; ... exploding?
                   JP    NZ,AlienExplodeTime     ; Yes ... go time it down and out

                   PUSH  HL                      ; 2002 on the stack
                   LD    A,(AlienCurIndex)       ; Get alien index ...
                   LD    L,A                     ; ... for the 21xx or 22xx pointer
                   LD    A,(PlayerDataMSB)       ; Get MSB...
                   LD    H,A                     ; ... of data area (21xx or 22xx)
                   LD    A,(HL)                  ; Get alien status flag
                   AND   A                       ; Is the alien alive?
                   POP   HL                      ; HL=2002
                   JP    Z,$0136                 ; No alien ... skip drawing alien sprite (but flag done)
                   INC   HL                      ; HL=2003 Bump descriptor
                   INC   HL                      ; HL=2004 Point to alien's row
                   LD    A,(HL)                  ; Get alien type
                   INC   HL                      ; HL=2005 Bump descriptor
                   LD    B,(HL)                  ; Get animation number
                   AND   $FE                     ; Translate row to type offset as follows: ...
                   RLCA                          ; ... 0,1 -> 32 (type 1) ...
                   RLCA                          ; ... 2,3 -> 16 (type 2) ...
                   RLCA                          ; ...   4 -> 32 (type 3) on top row
                   LD    E,A                     ; Sprite offset LSB
                   LD    D,$00                   ; MSB is 0
                   LD    HL,AlienSprAPos0        ; Position 0 alien sprites
                   ADD   HL,DE                   ; Offset to sprite type
                   EX    DE,HL                   ; Sprite offset to DE
                   LD    A,B                     ; Animation frame number
                   AND   A                       ; Is it position 0?
                   CALL  NZ,$013B                ; No ... add 30 and use position 1 alien sprites
                   LD    HL,(AlienPos)           ; Pixel position
                   LD    B,$10                   ; 16 rows in alien sprites
                   CALL  DrawSprite              ; Draw shifted sprite

                   XOR   A                       ; Let the ISR routine ...
                   LD    (WaitOnDraw),A          ; ... advance the cursor to the next alien
                   RET                           ; Done

                   LD    HL,$0030                ; Offset sprite pointer ...
                   ADD   HL,DE                   ; ... to animation frame 1 sprites
                   EX    DE,HL                   ; Back to DE
                   RET                           ; Done

; This is called from the mid-screen ISR to set the cursor for the next alien to draw.
; When the cursor moves over all aliens then it is reset to the beginning and the reference
; alien is moved to its next position

; The flag at 2000 keeps this in sync with the alien-draw routine called from the end-screen ISR.
; When the cursor is moved here then the flag at 2000 is set to 1. This routine will not change
; the cursor until the alien-draw routine at 100 clears the flag. Thus no alien is skipped

CursorNextAlien    LD    A,(PlayerOK)            ; Is the player ...
                   AND   A                       ; ... blowing up?
                   RET   Z                       ; Yes ... ignore the aliens
                   LD    A,(WaitOnDraw)          ; Still waiting on ...
                   AND   A                       ; ... this alien to be drawn?
                   RET   NZ                      ; Yes ... leave cursor in place
                   LD    A,(PlayerDataMSB)       ; Load alien-data ...
                   LD    H,A                     ; ... MSB (either 21xx or 22xx)
                   LD    A,(AlienCurIndex)       ; Load the xx part of the alien flag pointer
                   LD    D,$02                   ; When all are gone this triggers 1A1 to return from this stack frame
                   INC   A                       ; Have we drawn all aliens ...
                   CP    $37                     ; ... at last position?
                   CALL  Z,MoveRefAlien          ; Yes ... move the bottom/right alien and reset index to 0
                   LD    L,A                     ; HL now points to alien flag
                   LD    B,(HL)                  ; Is alien ...
                   DEC   B                       ; ... alive?
                   JP    NZ,$0154                ; No ... skip to next alien
                   LD    (AlienCurIndex),A       ; New alien index
                   CALL  GetAlienCoords          ; Calculate bit position and type for index
                   LD    H,C                     ; The calculation returns the MSB in C
                   LD    (AlienPos),HL           ; Store new bit position
                   LD    A,L                     ; Has this alien ...
                   CP    $28                     ; ... reached the end of screen?
                   JP    C,$1971                 ; Yes ... kill the player
                   LD    A,D                     ; This alien's ...
                   LD    (AlienRow),A            ; ... row index
                   LD    A,$01                   ; Set the wait-flag for the ...
                   LD    (WaitOnDraw),A          ; ... draw-alien routine to clear
                   RET                           ; Done

; Convert alien index in L to screen bit position in C,L
; Return alien row index (converts to type) in D

GetAlienCoords     LD    D,$00                   ; Row 0
                   LD    A,L                     ; Hold onto alien index
                   LD    HL,RefAlienYr           ; Get alien Y ...
                   LD    B,(HL)                  ; ... to B
                   INC   HL                      ; Get alien X ...
                   LD    C,(HL)                  ; ... to C
                   CP    $0B                     ; Can we take a full row off of index?
                   JP    M,$0194                 ; No ... we have the row
                   SBC   A,$0B                   ; Subtract off 11 (one whole row)
                   LD    E,A                     ; Hold the new index
                   LD    A,B                     ; Add ...
                   ADD   A,$10                   ; ... 16 to bit ...
                   LD    B,A                     ; ... position Y (1 row in rack)
                   LD    A,E                     ; Restore tallied index
                   INC   D                       ; Next row
                   JP    $0183                   ; Keep skipping whole rows

                   LD    L,B                     ; We have the LSB (the row)
                   AND   A                       ; Are we in the right column?
                   RET   Z                       ; Yes ... X and Y are right
                   LD    E,A                     ; Hold index
                   LD    A,C                     ; Add ...
                   ADD   A,$10                   ; ... 16 to bit ...
                   LD    C,A                     ; ... position X (1 column in rack)
                   LD    A,E                     ; Restore index
                   DEC   A                       ; We adjusted for 1 column
                   JP    $0195                   ; Keep moving over column

; The 'reference alien' is the bottom left. All other aliens are drawn relative to this
; reference. This routine moves the reference alien (the delta is set elsewhere) and toggles
; the animation frame number between 0 and 1

MoveRefAlien       DEC   D                       ; This decrements with each call to move
                   JP    Z,ReturnTwo             ; Return out of TWO call frames (only used if no aliens left)
                   LD    HL,AlienCurIndex        ; Set current alien index ...
                   LD    (HL),$00                ; ... to 0
                   INC   HL                      ; Point to DeltaX
                   LD    C,(HL)                  ; Load DX into C
                   LD    (HL),$00                ; Set DX to 0
                   CALL  AddDelta                ; Move alien
                   LD    HL,AlienFrame           ; Alien animation frame number
                   LD    A,(HL)                  ; Toggle ...
                   INC   A                       ; ... animation ...
                   AND   $01                     ; ... number between ...
                   LD    (HL),A                  ; ... 0 and 1
                   XOR   A                       ; Alien index in A is now 0
                   LD    HL,PlayerDataMSB        ; Restore H ...
                   LD    H,(HL)                  ; ... to player data MSB (21 or 22)
                   RET                           ; Done

                   NOP

; Initialize the 55 aliens from last to first. 1 means alive

InitAliens         LD    HL,$2100                ; Start of alien structures (this is the last alien)
                   LD    B,$37                   ; Count to 55 (that's five rows of 11 aliens)
                   LD    (HL),$01                ; Bring alien to live
                   INC   HL                      ; Next alien
                   DEC   B                       ; All done?
                   JP    NZ,$01C5                ; No ... keep looping
                   RET                           ; Done

; If there are no aliens left on the screen then MoveDrawAlien comes here which returns from the
; caller's stack frame

ReturnTwo          POP   HL                      ; Drop return to caller
                   RET                           ; Return to caller's caller

; Draw a 1px line across the player's stash at the bottom of the screen

DrawBottomLine     LD    A,$01                   ; Bit 1 set ... going to draw a 1-pixel stripe down left side
                   LD    B,$E0                   ; All the way down the screen
                   LD    HL,$2402                ; Screen coordinates (3rd byte from upper left)
                   JP    $14CC                   ; Draw line down left side

; HL points to descriptor: DX DY XX YY except DX is already loaded in C
; ** Why the 'already loaded' part? Why not just load it here?

AddDelta           INC   HL                      ; We loaded delta-x already ... skip over it
                   LD    B,(HL)                  ; Get delta-y
                   INC   HL                      ; Skip over it
                   LD    A,C                     ; Add delta-x ...
                   ADD   A,(HL)                  ; ... to x
                   LD    (HL),A                  ; Store new x
                   INC   HL                      ; Skip to y
                   LD    A,B                     ; Add delta-y ...
                   ADD   A,(HL)                  ; ... to y
                   LD    (HL),A                  ; Store new y
                   RET                           ; Done

; Block copy ROM mirror 1B00-1BBF to initialise RAM at 2000-20BF

CopyRamMirror      LD    B,$C0                   ; Number of bytes
                   LD    DE,Data_CopyToRam       ; RAM mirror in ROM
                   LD    HL,$2000                ; Start of RAM
                   JP    BlockCopy               ; Copy [DE]->[HL] and return

; Draw the shields for player 1 (draws it in the buffer in the player's data area)

DrawShieldPl1      LD    HL,P1_Shields           ; Player 1 shield buffer (remember between games in multi-player)
                   JP    $01F8                   ; Common draw point

; Draw the shields for player 2 (draws it in the buffer in the player's data area)

DrawShieldPl2      LD    HL,P2_Shields           ; Player 2 shield buffer (remember between games in multi-player)
                   LD    C,$04                   ; Going to draw 4 shields
                   LD    DE,$1D20                ; Shield pixel pattern
                   PUSH  DE                      ; Hold the start for the next shield
                   LD    B,$2C                   ; 44 bytes to copy
                   CALL  BlockCopy               ; Block copy DE to HL (B bytes)
                   POP   DE                      ; Restore start of shield pattern
                   DEC   C                       ; Drawn all shields?
                   JP    NZ,$01FD                ; No ... go draw them all
                   RET                           ; Done

; Copy shields on the screen to player 1's data area

RememberShields1   LD    A,$01                   ; Not zero means remember
                   JP    $021B                   ; Shuffle-shields player 1

; Copy shields on the screen to player 2's data area

RememberShields2   LD    A,$01                   ; Not zero means remember
                   JP    $0214                   ; Shuffle-shields player 2

; Copy shields from player 2's data area to screen

RestoreShields2    XOR   A                       ; Zero means restore
                   LD    DE,P2_Shields           ; Player 2 shield buffer (remember between games in multi-player)
                   JP    CopyShields             ; Shuffle-shields player 2

; Copy shields from player 1's data area to screen

RestoreShieldsP1   XOR   A                       ; Zero means restore
                   LD    DE,P1_Shields           ; Player 1 shield buffer (remember between games in multi-player)

; A is 1 for screen-to-buffer, 0 for to buffer-to-screen
; HL is screen coordinates of first shield. There are 23 rows between shields
; DE is sprite buffer in memory

CopyShields        LD    (Tmp2081),A             ; Remember copy/restore flag
                   LD    BC,$1602                ; 22 rows, 2 bytes/row (for 1 shield pattern)
                   LD    HL,$2806                ; Screen coordinates
                   LD    A,$04                   ; Four shields to move
                   PUSH  AF                      ; Hold shield count
                   PUSH  BC                      ; Hold sprite-size
                   LD    A,(Tmp2081)             ; Get back copy/restore flag
                   AND   A                       ; Not zero ...
                   JP    NZ,$0242                ; ... means remember shields
                   CALL  RestoreShields          ; Restore player's shields
                   POP   BC                      ; Get back sprite-size
                   POP   AF                      ; Get back shield count
                   DEC   A                       ; Have we moved all shields?
                   RET   Z                       ; Yes ... out
                   PUSH  DE                      ; Hold sprite buffer
                   LD    DE,$02E0                ; Add 2E0 (23 rows) to get to ...
                   ADD   HL,DE                   ; ... next shield on screen
                   POP   DE                      ; restore sprite buffer
                   JP    $0229                   ; Go back and do all

                   CALL  RememberShields         ; Remember player's shields
                   JP    $0235                   ; Continue with next shield

; Process game objects. Each game object has a 16 byte structure. The handler routine for the object
; is at xx03 and xx04 of the structure. The pointer to xx04 is pushed onto the stack before calling
; the handler

; All game objects (except task 0 ... the player) are called at the mid-screen and end-screen renderings.
; Each object decides when to run based on its Y (not rotated) coordinate. If an object is on the lower
; half of the screen then it does its work when the beam is at the top of the screen. If an object is
; on the top of the screen then it does its work when the beam is at the bottom. This keeps the
; object from updating while it is being drawn which would result in an ugly flicker

; The player is only processed at the mid-screen interrupt. I am not sure why

; The first three bytes of the structure are used for status and timers

; If the first byte is FF then the end of the game-task list has been reached
; If the first byte is FE then the object is skipped

; If the first-two bytes are non-zero then they are treated like a two-byte counter
; and decremented as such. The 2nd byte is the LSB (moves the fastest)

; If the first-two bytes are zero then the third byte is treated as an additional counter. It
; is decremented as such

; When all three bytes reach zero the task is executed

; The third-byte-counter was used as a speed-governor for the player's object, but evidently even the slowest
; setting was too slow. It got changed to 0 (fastest possible)

RunGameObjs        LD    HL,Obj0Timer            ; First game object (active player)
                   LD    A,(HL)                  ; Have we reached the ...
                   CP    $FF                     ; ... end of the object list?
                   RET   Z                       ; Yes ... done
                   CP    $FE                     ; Is object active?
                   JP    Z,$0281                 ; No ... skip it
                   INC   HL                      ; xx01
                   LD    B,(HL)                  ; First byte to B
                   LD    C,A                     ; Hold 1st byte
                   OR    B                       ; OR 1st and 2nd byte
                   LD    A,C                     ; Restore 1st byte
                   JP    NZ,$0277                ; If word at xx00,xx02 is non zero then decrement it

                   INC   HL                      ; xx02
                   LD    A,(HL)                  ; Get byte counter
                   AND   A                       ; Is it 0?
                   JP    NZ,$0288                ; No ... decrement byte counter at xx02
                   INC   HL                      ; xx03
                   LD    E,(HL)                  ; Get handler address LSB
                   INC   HL                      ; xx04
                   LD    D,(HL)                  ; Get handler address MSB
                   PUSH  HL                      ; Remember pointer to MSB
                   EX    DE,HL                   ; Handler address to HL
                   PUSH  HL                      ; Now to stack (making room for indirect call)
                   LD    HL,$026F                ; Return address to 026F
                   EX    (SP),HL                 ; Return address (026F) now on stack. Handler in HL.
                   PUSH  DE                      ; Push pointer to data struct (xx04) for handler to use
                   JP    (HL)                    ; Run object's code (will return to next line)
                   POP   HL                      ; Restore pointer to xx04
                   LD    DE,$000C                ; Offset to next ...
                   ADD   HL,DE                   ; ... game task (C+4=10)
                   JP    $024B                   ; Do next game task

; Word at xx00 and xx01 is non-zero. Decrement it and move to next task

                   DEC   B                       ; Decrement ...
                   INC   B                       ; ... two ...
                   JP    NZ,$027D                ; ... byte ...
                   DEC   A                       ; ... value ...
                   DEC   B                       ; ... at ...
                   LD    (HL),B                  ; ... xx00 ...
                   DEC   HL                      ; ... and ...
                   LD    (HL),A                  ; ... xx01

                   LD    DE,$0010                ; Next ...
                   ADD   HL,DE                   ; ... object descriptor
                   JP    $024B                   ; Keep processing game objects

; Word at xx00 and xx01 is zero and byte at xx02 is non-zero. Decrement xx02 and
; move to next task

                   DEC   (HL)                    ; Decrement the xx02 counter
                   DEC   HL                      ; Back up to ...
                   DEC   HL                      ; ... start of game task
                   JP    $0281                   ; Next game task

; Game object 0: Move/draw the player

; This task is only called at the mid-screen ISR. It ALWAYS does its work here, even though
; the player can be on the top or bottom of the screen (not rotated)

GameObj0           POP   HL                      ; Get player object structure 2014
                   INC   HL                      ; Point to blow-up status
                   LD    A,(HL)                  ; Get player blow-up status
                   CP    $FF                     ; Player is blowing up?
                   JP    Z,$033B                 ; No ... go do normal movement

; Handle blowing up player

                   INC   HL                      ; Point to blow-up delay count
                   DEC   (HL)                    ; Decrement the blow-up delay
                   RET   NZ                      ; Not time for a new blow-up sprite ... out
                   LD    B,A                     ; Hold sprite image number
                   XOR   A                       ; 0
                   LD    (PlayerOK),A            ; Player is NOT OK ... player is blowing up
                   LD    (EnableAlienFire),A     ; Alien fire is disabled
                   LD    A,$30                   ; Reset count ...
                   LD    (AlienFireDelay),A      ; ... till alien shots are enabled
                   LD    A,B                     ; Restore sprite image number (used if we go to 39B)
                   LD    (HL),$05                ; Reload time between blow-up changes
                   INC   HL                      ; Point to number of blow-up changes
                   DEC   (HL)                    ; Count down blow-up changes
                   JP    NZ,DrawPlayerDie        ; Still blowing up ... go draw next sprite

; Blow up finished

                   LD    HL,(PlayerYr)           ; Player's coordinates
                   LD    B,$10                   ; 16 Bytes
                   CALL  EraseSimpleSprite       ; Erase simple sprite (the player)
                   LD    HL,Obj0Timer            ; Restore player ...
                   LD    DE,$1B10                ; ... structure ...
                   LD    B,$10                   ; ... from ...
                   CALL  BlockCopy               ; ... ROM mirror
                   LD    B,$00                   ; Turn off ...
                   CALL  SoundBits3Off           ; ... all sounds
                   LD    A,(Invaded)             ; Has rack reached ...
                   AND   A                       ; ... the bottom of the screen?
                   RET   NZ                      ; Yes ... done here
                   LD    A,(GameMode)            ; Are we in ...
                   AND   A                       ; ... game mode?
                   RET   Z                       ; No ... return to splash screens
                   LD    SP,STACK_TOP            ; We aren't going to return
                   EI                            ; Enable interrupts (we just dropped the ISR context)
                   CALL  DisableGameTasks        ; Disable game tasks
                   CALL  $092E                   ; Get number of ships for active player
                   AND   A                       ; Any left?
                   JP    Z,$166D                 ; No ... handle game over for player
                   CALL  $18E7                   ; Get player-alive status pointer
                   LD    A,(HL)                  ; Is player ...
                   AND   A                       ; ... alive?
                   JP    Z,$032C                 ; Yes ... remove a ship from player's stash and reenter game loop
                   LD    A,(TwoPlayers)          ; Multi-player game
                   AND   A                       ; Only one player?
                   JP    Z,$032C                 ; Yes ... remove a ship from player's stash and reenter game loop
                   LD    A,(PlayerDataMSB)       ; Player data MSB
                   PUSH  AF                      ; Hold the MSB
                   RRCA                          ; Player 1 is active player?
                   JP    C,$0332                 ; Yes ... go store player 1 shields and come back to 02F8
                   CALL  RememberShields2        ; No ... go store player 2 shields
                   CALL  $0878                   ; Get ref-alien info and pointer to storage
                   LD    (HL),E                  ; Hold the ...
                   INC   HL                      ; ... ref-alien ...
                   LD    (HL),D                  ; ... screen coordinates
                   DEC   HL                      ; Back up ...
                   DEC   HL                      ; .. to delta storage
                   LD    (HL),B                  ; Store ref-alien's delta (direction)

                   NOP

                   CALL  CopyRamMirror           ; Copy RAM mirror (getting ready to switch players)
                   POP   AF                      ; Restore active player MSB
                   RRCA                          ; Player 1?
                   LD    A,$21                   ; Player 1 data pointer
                   LD    B,$00                   ; Cocktail bit=0 (player 1)
                   JP    NC,$0312                ; It was player one ... keep data for player 2
                   LD    B,$20                   ; Cocktail bit=1 (player 2)
                   LD    A,$22                   ; Player 2 data pointer
                   LD    (PlayerDataMSB),A       ; Change players
                   CALL  TwoSecDelay             ; Two second delay
                   XOR   A                       ; Clear the player-object ...
                   LD    (Obj0Timer + 1),A       ; ... timer (player can move instantly after switching players)
                   LD    A,B                     ; Cocktail bit to A
                   OUT   (SOUND2),A              ; Set the cocktail mode
                   INC   A                       ; Fleet sound 1 (first tone)
                   LD    (SoundPort5),A          ; Set the port 5 hold
                   CALL  ClearPlayField          ; Clear centre window
                   CALL  RemoveShip              ; Remove a ship and update indicators
                   JP    $07F9                   ; Tell the players that the switch has been made

                   CALL  RemoveShip              ; Remove a ship and update indicators
                   JP    $0817                   ; Continue into game loop

                   CALL  RememberShields1        ; Remember the shields for player 1
                   JP    $02F8                   ; Back to switching-players above

                   NOP
                   NOP
                   NOP

; Player not blowing up ... handle inputs

                   LD    HL,PlayerOK             ; Player OK flag
                   LD    (HL),$01                ; Flag 1 ... player is OK
                   INC   HL                      ; 2069
                   LD    A,(HL)                  ; Alien shots enabled?
                   AND   A                       ; Set flags
                   JP    $03B0                   ; Continue

                   NOP

                   DEC   HL                      ; 2069
                   LD    (HL),$01                ; Enable alien fire

                   LD    A,(PlayerXr)            ; Current player coordinates
                   LD    B,A                     ; Hold it
                   LD    A,(GameMode)            ; Are we in ...
                   AND   A                       ; ... game mode?
                   JP    NZ,$0363                ; Yes ... use switches as player controls

                   LD    A,(NextDemoCmd)         ; Get demo command
                   RRCA                          ; Is it right?
                   JP    C,MovePlayerRight       ; Yes ... do right
                   RRCA                          ; Is it left?
                   JP    C,$038E                 ; Yes ... do left
                   JP    $036F                   ; Skip over movement (draw player and out)

; Player is in control

                   CALL  ReadInputs              ; Read active player controls
                   RLCA                          ; Test for ...
                   RLCA                          ; ... right button
                   JP    C,MovePlayerRight       ; Yes ... handle move right
                   RLCA                          ; Test for left button
                   JP    C,$038E                 ; Yes ... handle move left

; Draw player sprite

                   LD    HL,PlayerSprPic         ; Active player descriptor
                   CALL  ReadDesc                ; Load 5 byte sprite descriptor in order: EDLHB
                   CALL  ConvToScr               ; Convert HL to screen coordinates
                   CALL  DrawSimpleSprite        ; Draw player
                   LD    A,$00                   ; Clear the task timer. Nobody changes this but it could have ...
                   LD    (Obj0TimerExtra),A      ; ... been speed set for the player with a value other than 0 (not XORA)
                   RET                           ; Done

; Handle player moving right

MovePlayerRight    LD    A,B                     ; Player coordinate
                   CP    $D9                     ; At right edge?
                   JP    Z,$036F                 ; Yes ... ignore this
                   INC   A                       ; Bump X coordinate
                   LD    (PlayerXr),A            ; New X coordinate
                   JP    $036F                   ; Draw player and out

; Handle player moving left

MovePlayerLeft     LD    A,B                     ; Player coordinate
                   CP    $30                     ; At left edge
                   JP    Z,$036F                 ; Yes ... ignore this
                   DEC   A                       ; Bump X coordinate
                   LD    (PlayerXr),A            ; New X coordinate
                   JP    $036F                   ; Draw player and out

; Toggle the player's blowing-up sprite between two pictures and draw it

DrawPlayerDie      INC   A                       ; Toggle blowing-up ...
                   AND   $01                     ; ... player sprite (0,1,0,1)
                   LD    (PlayerAlive),A         ; Hold current state
                   RLCA                          ; *2
                   RLCA                          ; *4
                   RLCA                          ; *8
                   RLCA                          ; *16
                   LD    HL,$1C70                ; Base blow-up sprite location
                   ADD   A,L                     ; Offset sprite ...
                   LD    L,A                     ; ... pointer
                   LD    (PlayerSprPic),HL       ; New blow-up sprite picture
                   JP    $036F                   ; Draw new blow-up sprite and out

                   JP    NZ,$034A                ; Alien shots enabled ... move player's ship, draw it, and out
                   INC   HL                      ; To 206A
                   DEC   (HL)                    ; Time until aliens can fire
                   JP    NZ,$034A                ; Not time to enable ... move player's ship, draw it, and out
                   JP    $0346                   ; Enable alien fire ... move player's ship, draw it, and out

; Game object 1: Move/draw the player shot

; This task executes at either mid-screen ISR (if it is on the top half of the non-rotated screen) or
; at the end-screen ISR (if it is on the bottom half of the screen).

GameObj1           LD    DE,Obj1CoorXr           ; Object's Xr coordinate
                   CALL  CompYToBeam             ; Compare to screen-update location
                   POP   HL                      ; Pointer to task data
                   RET   NC                      ; Make sure we are in the right ISR

                   INC   HL                      ; Point to 2025 ... the shot status
                   LD    A,(HL)                  ; Get shot status
                   AND   A                       ; Return if ...
                   RET   Z                       ; ... no shot is active

                   CP    $01                     ; Shot just starting (requested elsewhere)?
                   JP    Z,InitPlyShot           ; Yes ... go initiate shot

                   CP    $02                     ; Progressing normally?
                   JP    Z,MovePlyShot           ; Yes ... go move it

                   INC   HL                      ; 2026
                   CP    $03                     ; Shot blowing up (not because of alien)?
                   JP    NZ,$042A                ; No ... try other options

; Shot blowing up because it left the playfield, hit a shield, or hit another bullet

                   DEC   (HL)                    ; Decrement the timer
                   JP    Z,EndOfBlowup           ; If done then
                   LD    A,(HL)                  ; Get timer value
                   CP    $0F                     ; Starts at 10 ... first decrement brings us here
                   RET   NZ                      ; Not the first time ... explosion has been drawn

; Draw explosion first pass through timer loop

                   PUSH  HL                      ; Hold pointer to data
                   CALL  ReadPlyShot             ; Read shot descriptor
                   CALL  EraseShifted            ; Erase the sprite
                   POP   HL                      ; 2026 (timer flag)
                   INC   HL                      ; 2027 point to sprite LSB
                   INC   (HL)                    ; Change 1C90 to 1C91
                   INC   HL                      ; 2028
                   INC   HL                      ; 2029
                   DEC   (HL)                    ; Drop X coordinate ...
                   DEC   (HL)                    ; ... by 2
                   INC   HL                      ; 202A
                   DEC   (HL)                    ; Drop Y ...
                   DEC   (HL)                    ; ... coordinate ...
                   DEC   (HL)                    ; ... by ...
                   INC   HL                      ; ... 3
                   LD    (HL),$08                ; 202B 8 bytes in size of sprite
                   CALL  ReadPlyShot             ; Read player shot structure
                   JP    DrawShiftedSprite       ; Draw sprite and out

InitPlyShot        INC   A                       ; Type is now ...
                   LD    (HL),A                  ; ... 2 (in progress)
                   LD    A,(PlayerXr)            ; Players Y coordinate
                   ADD   A,$08                   ; To centre of player
                   LD    (Obj1CoorXr),A          ; Shot's Y coordinate
                   CALL  ReadPlyShot             ; Read 5 byte structure
                   JP    DrawShiftedSprite       ; Draw sprite and out

MovePlyShot        CALL  ReadPlyShot             ; Read the shot structure
                   PUSH  DE                      ; Hold pointer to sprite image
                   PUSH  HL                      ; Hold sprite coordinates
                   PUSH  BC                      ; Hold sprite size (in B)
                   CALL  EraseShifted            ; Erase the sprite from the screen
                   POP   BC                      ; Restore size
                   POP   HL                      ; Restore coords
                   POP   DE                      ; Restore pointer to sprite image
                   LD    A,(ShotDeltaX)          ; DeltaX for shot
                   ADD   A,L                     ; Move the shot ...
                   LD    L,A                     ; ... up the screen
                   LD    (Obj1CoorYr),A          ; Store shot's new X coordinate
                   CALL  DrawSprCollision        ; Draw sprite with collision detection
                   LD    A,(Collision)           ; Test for ...
                   AND   A                       ; ... collision
                   RET   Z                       ; No collision ... out

; Collision with alien detected

                   LD    (AlienIsExploding),A    ; Set to not-0 indicating ...
                   RET                           ; ... an alien is blowing up

; Other shot-status options

                   CP    $05                     ; Alien explosion in progress?
                   RET   Z                       ; Yes ... nothing to do
                   JP    EndOfBlowup             ; Anything else erases the shot and removes it from duty

ReadPlyShot        LD    HL,Obj1Image            ; Read 5 byte sprite structure for ...
                   JP    ReadDesc                ; ... player shot

EndOfBlowup        CALL  ReadPlyShot             ; Read the shot structure
                   CALL  EraseShifted            ; Erase the player's shot
                   LD    HL,PlyrShotStatus       ; Reinit ...
                   LD    DE,$1B25                ; ... shot structure ...
                   LD    B,$07                   ; ... from ...
                   CALL  BlockCopy               ; ... ROM mirror
                   LD    HL,(SaucerScore)        ; Get pointer to saucer-score table
                   INC   L                       ; Every shot explosion advances it one
                   LD    A,L                     ; Have we passed ...
                   CP    $63                     ; ... the end at 1D63 (bug! this should be $64 to cover all 16 values)
                   JP    C,$0453                 ; No .... keep it
                   LD    L,$54                   ; Wrap back around to 1D54
                   LD    (SaucerScore),HL        ; New score pointer
                   LD    HL,(ShotCount)          ; Increments with every shot ...
                   INC   L                       ; ... but only LSB ** ...
                   LD    (ShotCount),HL          ; ... used for saucer direction
                   LD    A,(SaucerActive)        ; Is saucer ...
                   AND   A                       ; ... on screen?
                   RET   NZ                      ; Yes ... don't reset it

; Setup saucer direction for next trip

                   LD    A,(HL)                  ; Shot counter
                   AND   $01                     ; Lowest bit set?
                   LD    BC,$0229                ; Xr delta of 2 starting at Xr=29
                   JP    NZ,$046E                ; Yes ... use 2/29
                   LD    BC,$FEE0                ; No ... Xr delta of -2 starting at Xr=E0
                   LD    HL,SaucerPriPicMSB      ; Saucer descriptor
                   LD    (HL),C                  ; Store Xr coordinate
                   INC   HL                      ; Point to ...
                   INC   HL                      ; ... delta Xr
                   LD    (HL),B                  ; Store delta Xr
                   RET                           ; Done

; Game object 2: Allien rolling-shot (targets player specifically)

; The 2-byte value at 2038 is where the firing-column-table-pointer would be (see other
; shots ... next game objects). This shot doesn't use that table. It targets the player
; specifically. Instead the value is used as a flag to have the shot skip its first
; attempt at firing every time it is reinitialised (when it blows up)

; The task-timer at 2032 is copied to 2080 in the game loop. The flag is used as a
; synchronisation flag to keep all the shots processed on separate interrupt ticks. This
; has the main effect of slowing the shots down

; When the timer is 2 the squiggly-shot/saucer (object 4) runs
; When the timer is 1 the plunger-shot (object 3) runs
; When the timer is 0 this object, the rolling-shot, runs

GameObj2           POP   HL                      ; Game object data
                   LD    A,($1B32)               ; Restore delay from ...
                   LD    (Obj2TimerExtra),A      ; ... ROM mirror (value 2)
                   LD    HL,(RolShotColFirTbl)   ; Get pointer to ...
                   LD    A,L                     ; ... column-firing table.
                   OR    H                       ; All zeros?
                   JP    NZ,$048A                ; No ... must be a valid column. Go fire.
                   DEC   HL                      ; Decrement the counter
                   LD    (RolShotColFirTbl),HL   ; Store new counter value (run the shot next time)
                   RET                           ; Done

                   LD    DE,RolShotStatus        ; Rolling-shot data structure
                   LD    A,$F9                   ; Last picture of 'rolling' alien shot
                   CALL  ToShotStruct            ; Set code to handle rolling-shot
                   LD    A,(PluShotStepCnt)      ; Get the plunger-shot step count
                   LD    (OtherShot1),A          ; Hold it
                   LD    A,(SquShotStepCnt)      ; Get the squiggly-shot step count
                   LD    (OtherShot2),A          ; Hold it
                   CALL  HandleAlienShot         ; Handle active shot structure
                   LD    A,(AlienShotBlowCnt)    ; Blow up counter
                   AND   A                       ; Test if shot has cycled through blowing up
                   LD    HL,RolShotStatus        ; Rolling-shot data structure
                   JP    NZ,FromShotStruct       ; If shot is still running, copy the updated data and out

; The rolling-shot has blown up. Reset the data structure

ResetShot          LD    DE,$1B30                ; Reload ...
                   LD    HL,Obj2Timer            ; ... object ...
                   LD    B,16                    ; ... structure ...
                   JP    BlockCopy               ; ... from ROM mirror and out

; Game object 3: Alien plunger-shot

; This is skipped if there is only one alien left on the screen

GameObj3           POP   HL                      ; Game object data
                   LD    A,(SkipPlunger)         ; One alien left? Skip plunger shot?
                   AND   A                       ; Check
                   RET   NZ                      ; Yes. Only one alien. Skip this shot.
                   LD    A,(ShotSync)            ; Sync flag (copied from GO-2's timer value)
                   CP    $01                     ; GO-2 and GO-4 are idle?
                   RET   NZ                      ; No ... only one shot at a time

                   LD    DE,PluShotStatus        ; Plunger alien shot data structure
                   LD    A,$ED                   ; Last picture of 'plunger' alien shot
                   CALL  ToShotStruct            ; Copy the plunger alien to the active structure
                   LD    A,(RolShotStepCnt)      ; Step count from rolling-shot
                   LD    (OtherShot1),A          ; Hold it
                   LD    A,(SquShotStepCnt)      ; Step count from squiggly shot
                   LD    (OtherShot2),A          ; Hold it
                   CALL  HandleAlienShot         ; Handle active shot structure
                   LD    A,(AlienShotColFirTbl)  ; LSB of column-firing table
                   CP    $10                     ; Been through all entries in the table?
                   JP    C,$04E7                 ; Not yet ... table is OK
                   LD    A,($1B48)               ; Been through all ..
                   LD    (AlienShotColFirTbl),A  ; ... so reset pointer into firing-column table
                   LD    A,(AlienShotBlowCnt)    ; Get the blow up timer
                   AND   A                       ; Zero means shot is done
                   LD    HL,PluShotStatus        ; Plunger shot data
                   JP    NZ,FromShotStruct       ; If shot is still running, go copy the updated data and out

                   LD    DE,$1B40                ; Reload ...
                   LD    HL,Obj3Timer            ; ... object ...
                   LD    B,16                    ; ... structure ...
                   CALL  BlockCopy               ; ... from mirror

                   LD    A,(NumAliens)           ; Number of aliens on screen
                   DEC   A                       ; Is there only one left?
                   JP    NZ,$0508                ; No ... move on
                   LD    A,$01                   ; Disable plunger shot ...
                   LD    (SkipPlunger),A         ; ... when only one alien remains
                   LD    HL,(AlienShotColFirTbl) ; Set the plunger shot's ...
                   JP    $067E                   ; ... column-firing pointer data

; Game task 4 when splash screen alien is shooting extra 'C' with a squiggly shot

                   POP   HL                      ; Ignore the task data pointer passed on stack

; GameObject 4 comes here if processing a squiggly shot

                   LD    DE,SquShotStatus        ; Squiggly shot data structure
                   LD    A,$DB                   ; LSB of last byte of picture
                   CALL  ToShotStruct            ; Copy squiggly shot to
                   LD    A,(PluShotStepCnt)      ; Get plunger ...
                   LD    (OtherShot1),A          ; ... step count
                   LD    A,(RolShotStepCnt)      ; Get rolling ...
                   LD    (OtherShot2),A          ; ... step count
                   CALL  HandleAlienShot         ; Handle active shot structure
                   LD    A,(AlienShotColFirTbl)  ; LSB of column-firing table pointer
                   CP    $15                     ; Have we processed all entries?
                   JP    C,$0534                 ; No ... don't reset it
                   LD    A,($1B58)               ; Reset the pointer ...
                   LD    (AlienShotColFirTbl),A  ; ... back to the start of the table
                   LD    A,(AlienShotBlowCnt)    ; Check to see if squiggly shot is done
                   AND   A                       ; 0 means blow-up timer expired
                   LD    HL,SquShotStatus        ; Squiggly shot data structure
                   JP    NZ,FromShotStruct       ; If shot is still running, go copy the updated data and out

; Shot explosion is over. Remove the shot

                   LD    DE,$1B50                ; Reload
                   LD    HL,Obj4Timer            ; ... object ...
                   LD    B,16                    ; ... structure ...
                   CALL  BlockCopy               ; ... from mirror
                   LD    HL,(AlienShotColFirTbl) ; Copy pointer to column-firing table ...
                   LD    (SquShotColFirTbl),HL   ; ... back to data structure (for next shot)
                   RET                           ; Done

ToShotStruct       LD    (ShotPicEnd),A          ; LSB of last byte of last picture in sprite
                   LD    HL,AlienShotStatus      ; Destination is the shot-structure
                   LD    B,11                    ; 11 bytes
                   JP    BlockCopy               ; Block copy and out

FromShotStruct     LD    DE,AlienShotStatus      ; Source is the shot-structure
                   LD    B,$0B                   ; 11 bytes
                   JP    BlockCopy               ; Block copy and out

; Each of the 3 shots copy their data to the 2073 structure (0B bytes) and call this.
; Then they copy back if the shot is still active. Otherwise they copy from the mirror

; The alien 'fire rate' is based on the number of steps the other two shots on the screen
; have made. The smallest number-of-steps is compared to the reload-rate. If it is too
; soon then no shot is made. The reload-rate is based on the player's score. The MSB
; is looked up in a table to get the reload-rate. The smaller the rate the faster the
; aliens fire. Setting rate this way keeps shots from walking on each other

HandleAlienShot    LD    HL,AlienShotStatus      ; Start of active shot structure
                   LD    A,(HL)                  ; Get the shot status
                   AND   $80                     ; Is the shot active?
                   JP    NZ,$05C1                ; Yes ... go move it

                   LD    A,(IsrSplashTask)       ; ISR splash task
                   CP    $04                     ; Shooting the 'C' ?
                   LD    A,(EnableAlienFire)     ; Alien fire enabled flag
                   JP    Z,$05B7                 ; We are shooting the extra 'C' ... just flag it active and out
                   AND   A                       ; Is alien fire enabled?
                   RET   Z                       ; No ... don't start a new shot

                   INC   HL                      ; 2074 step count of current shot
                   LD    (HL),$0000              ; clear the step count

; Make sure it isn't too soon to fire another shot

                   LD    A,(OtherShot1)          ; Get the step count of the 1st 'other shot'
                   AND   A                       ; Any steps made?
                   JP    Z,$0589                 ; No ... ignore this count
                   LD    B,A                     ; Shuffle off step count
                   LD    A,(AlienShotRelRate)    ; Get the reload rate (based on MSB of score)
                   CP    B                       ; Too soon to fire again?
                   RET   NC                      ; Yes ... don't fire
                   LD    A,(OtherShot2)          ; Get the step count of the 2nd 'other shot'
                   AND   A                       ; Any steps made?
                   JP    Z,$0596                 ; No steps on any shot ... we are clear to fire
                   LD    B,A                     ; Shuffle off step count
                   LD    A,(AlienShotRelRate)    ; Get the reload rate (based on MSB of score)
                   CP    B                       ; Too soon to fire again?
                   RET   NC                      ; Yes ... don't fire
                   INC   HL                      ; 2075
                   LD    A,(HL)                  ; Get tracking flag
                   AND   A                       ; Does this shot track the player?
                   JP    Z,$061B                 ; Yes ... go make a tracking shot;
                   LD    HL,(AlienShotColFirTbl) ; Column-firing table
                   LD    C,(HL)                  ; Get next column to fire from
                   INC   HL                      ; Bump the ...

                   NOP

                   LD    (AlienShotColFirTbl),HL ; ... pointer into column table
                   CALL  FindInColumn            ; Find alien in target column
                   RET   NC                      ; No alien is alive in target column ... out

                   CALL  GetAlienCoords          ; Get coordinates of alien (lowest alien in firing column)
                   LD    A,C                     ; Offset ...
                   ADD   A,7                     ; ... Y by 7
                   LD    H,A                     ; To H
                   LD    A,L                     ; Offset ...
                   SUB   10                      ; ... X down 10
                   LD    L,A                     ; To L
                   LD    (AlienShotYr),HL        ; Set shot coordinates below alien

                   LD    HL,AlienShotStatus      ; Alien shot status
                   LD    A,(HL)                  ; Get the status
                   OR    $80                     ; Mark this shot ...
                   LD    (HL),A                  ; ... as actively running
                   INC   HL                      ; 2074 step count
                   INC   (HL)                    ; Give this shot 1 step (it just started)
                   RET                           ; Done

; Move the alien shot

                   LD    DE,AlienShotXr          ; Alien-shot Y coordinate
                   CALL  CompYToBeam             ; Compare to beam position
                   RET   NC                      ; Not the right ISR for this shot

                   INC   HL                      ; 2073 status
                   LD    A,(HL)                  ; Get shot status
                   AND   $01                     ; Bit 0 is 1 if blowing up
                   JP    NZ,ShotBlowingUp        ; Go do shot-is-blowing-up sequence
                   INC   HL                      ; 2074 step count
                   INC   (HL)                    ; Count the steps (used for fire rate)
                   CALL  $0675                   ; Erase shot
                   LD    A,(AlienShotImage)      ; Get LSB of the image pointer
                   ADD   A,$03                   ; Next set of images
                   LD    HL,ShotPicEnd           ; End of image
                   CP    (HL)                    ; Have we reached the end of the set?
                   JP    C,$05E2                 ; No ... keep it
                   SUB   $0C                     ; Back up to the 1st image in the set
                   LD    (AlienShotImage),A      ; New LSB image pointer
                   LD    A,(AlienShotYr)         ; Get shot's Y coordinate
                   LD    B,A                     ; Hold it
                   LD    A,(AlienShotDelta)      ; Get alien shot delta
                   ADD   A,B                     ; Add to shots coordinate
                   LD    (AlienShotYr),A         ; New shot Y coordinate
                   CALL  $066C                   ; Draw the alien shot
                   LD    A,(AlienShotYr)         ; Shot's Y coordinate
                   CP    $15                     ; Still in the active play field?
                   JP    C,$0612                 ; No ... end it
                   LD    A,(Collision)           ; Did shot collide ...
                   AND   A                       ; ... with something?
                   RET   Z                       ; No ... we are done here
                   LD    A,(AlienShotYr)         ; Shot's Y coordinate
                   CP    $1E                     ; Is it below player's area?
                   JP    C,$0612                 ; Yes ... end it
                   CP    $27                     ; Is it above player's area?

                   NOP

                   JP    NC,$0612                ; Yes ... end it
                   SUB   A                       ; Flag that player ...
                   LD    (PlayerAlive),A         ; ... has been struck
                   LD    A,(AlienShotStatus)     ; Flag to ...
                   OR    $01                     ; ... start shot ...
                   LD    (AlienShotStatus),A     ; ... blowing up
                   RET                           ; Done

; Start a shot right over the player

                   LD    A,(PlayerXr)            ; Player's X coordinate
                   ADD   A,$08                   ; Centre of player
                   LD    H,A                     ; To H for routine
                   CALL  FindColumn              ; Find the column
                   LD    A,C                     ; Get the column right over player
                   CP    $0C                     ; Is it a valid column?
                   JP    C,$05A5                 ; Yes ... use what we found
                   LD    C,$0B                   ; Else use ...
                   JP    $05A5                   ; ... as far over as we can

; C contains the target column. Look for a live alien in the column starting with
; the lowest position. Return C=1 if found ... HL points to found slot

FindInColumn       DEC   C                       ; Column that is firing
                   LD    A,(PlayerDataMSB)       ; Player's MSB (21xx or 22xx)
                   LD    H,A                     ; To MSB of HL
                   LD    L,C                     ; Column to L
                   LD    D,$05                   ; 5 rows of aliens
                   LD    A,(HL)                  ; Get alien's status
                   AND   A                       ; 0 means dead
                   SCF                           ; In case not 0
                   RET   NZ                      ; Alien is alive? Yes ... return
                   LD    A,L                     ; Get the flag pointer LSB
                   ADD   A,11                    ; Jump to same column on next row of rack (+11 aliens per row)
                   LD    L,A                     ; New alien index
                   DEC   D                       ; Tested all rows?
                   JP    NZ,$0637                ; No ... keep looking for a live alien up the rack
                   RET                           ; Didn't find a live alien. Return with C=0.

; Alien shot is blowing up

ShotBlowingUp      LD    HL,AlienShotBlowCnt     ; Blow up timer
                   DEC   (HL)                    ; Decrement the value
                   LD    A,(HL)                  ; Get the value
                   CP    $03                     ; First tick, 4, we draw the explosion
                   JP    NZ,$0667                ; After that just wait
                   CALL  $0675                   ; Erase the shot
                   LD    HL,$1CDC                ; Alien shot ...
                   LD    (AlienShotImage),HL     ; ... explosion sprite
                   LD    HL,AlienShotXr          ; Alien shot Y
                   DEC   (HL)                    ; Left two for ...
                   DEC   (HL)                    ; ... explosion
                   DEC   HL                      ; Point alien shot X
                   DEC   (HL)                    ; Up two for ...
                   DEC   (HL)                    ; ... explosion
                   LD    A,$06                   ; Alien shot descriptor ...
                   LD    (AlienShotSize),A       ; ... size 6
                   JP    $066C                   ; Draw alien shot explosion

                   AND   A                       ; Have we reached 0?
                   RET   NZ                      ; No ... keep waiting
                   JP    $0675                   ; Erase the explosion and out

                   LD    HL,AlienShotImage       ; Alien shot descriptor
                   CALL  ReadDesc                ; Read 5 byte structure
                   JP    DrawSprCollision        ; Draw shot and out

                   LD    HL,AlienShotImage       ; Alien shot descriptor
                   CALL  ReadDesc                ; Read 5 byte structure
                   JP    EraseShifted            ; Erase the shot and out

                   LD    (PluShotColFirTbl),HL   ; From 50B, update ...
                   RET                           ; ... column-firing table pointer and out


; Game object 4: Flying Saucer OR squiggly shot

; This task is shared by the squiggly-shot and the flying saucer. The saucer waits until the
; squiggly-shot is over before it begins

GameObj4           POP   HL                      ; Pull data pointer from the stack (not going to use it)
                   LD    A,(ShotSync)            ; Sync flag (copied from GO-2's timer value)
                   CP    $02                     ; Are GO-2 and GO-3 idle?
                   RET   NZ                      ; No ... only one at a time
                   LD    HL,SaucerStart          ; Time-till-saucer flag
                   LD    A,(HL)                  ; Is it time ...
                   AND   A                       ; ... for a saucer?
                   JP    Z,$050F                 ; No ... go process squiggly shot
                   LD    A,(SquShotStepCnt)      ; Is there a ...
                   AND   A                       ; ... squiggly shot going?
                   JP    NZ,$050F                ; Yes ... go handle squiggly shot

                   INC   HL                      ; Saucer on screen flag
                   LD    A,(HL)                  ; (2084) Is the saucer ...
                   AND   A                       ; ... already on the screen?
                   JP    NZ,$06AB                ; Yes ... go handle it
                   LD    A,(NumAliens)           ; Number of aliens remaining
                   CP    $08                     ; Less than ...
                   JP    C,$050F                 ; ... 8 ... no saucer
                   LD    (HL),$01                ; (2084) The saucer is on the screen
                   CALL  $073C                   ; Draw the flying saucer

                   LD    DE,SaucerPriPicMSB      ; Saucer's Y coordinate (CHECK)
                   CALL  CompYToBeam             ; Compare to beam position
                   RET   NC                      ; Not the right ISR for moving saucer

                   LD    HL,SaucerHit            ; Saucer hit flag
                   LD    A,(HL)                  ; Has saucer ...
                   AND   A                       ; ... been hit?
                   JP    NZ,$06D6                ; Yes ... don't move it

                   LD    HL,SaucerPriPicMSB      ; Saucer's structure
                   LD    A,(HL)                  ; Get saucer's Y coordinate
                   INC   HL                      ; Bump to ...
                   INC   HL                      ; ... delta Y
                   ADD   A,(HL)                  ; Move saucer
                   LD    (SaucerPriPicMSB),A     ; New coordinate
                   CALL  $073C                   ; Draw the flying saucer
                   LD    HL,SaucerPriPicMSB      ; Saucer's structure
                   LD    A,(HL)                  ; Y coordinate
                   CP    $28                     ; Too low? End of screen?
                   JP    C,$06F9                 ; Yes ... remove from play
                   CP    $E1                     ; Too high? End of screen?
                   JP    NC,$06F9                ; Yes ... remove from play
                   RET                           ; Done

                   LD    B,$FE                   ; Turn off ...
                   CALL  SoundBits3Off           ; ... flying saucer sound
                   INC   HL                      ; (2086) show-hit timer
                   DEC   (HL)                    ; Count down show-hit timer
                   LD    A,(HL)                  ; Get current value
                   CP    $1F                     ; Starts at 20 ... is this the first tick of show-hit timer?
                   JP    Z,$074B                 ; Yes ... go show the explosion
                   CP    $18                     ; A little later ...
                   JP    Z,$070C                 ; ... show the score besides the saucer and add it
                   AND   A                       ; Has timer expired?
                   RET   NZ                      ; No ... let it run
                   LD    B,$00EF                 ; 1110_1111 (mask off saucer hit sound)
                   LD    HL,SoundPort5           ; Get current ...
                   LD    A,(HL)                  ; ... value of port 5 sound
                   AND   B                       ; Mask off the saucer-hit sound
                   LD    (HL),A                  ; Set the new value
                   AND   $20                     ; All sound off but ...
                   OUT   (SOUND2),A              ; ... cocktail cabinet bit

                   NOP
                   NOP
                   NOP

                   CALL  $0742                   ; Covert pixel pos from descriptor to HL screen and shift
                   CALL  ClearSmallSprite        ; Clear a one byte sprite at HL
                   LD    HL,SaucerStart          ; Saucer structure
                   LD    B,$0A                   ; 10 bytes in saucer structure
                   CALL  $075F                   ; Reinitialise saucer structure

                   LD    B,$FE                   ; Turn off UFO ...
                   JP    SoundBits3Off           ; ... sound and out

                   LD    A,$01                   ; Flag the score ...
                   LD    (AdjustScore),A         ; ... needs updating
                   LD    HL,(SaucerScore)        ; Saucer score table
                   LD    B,(HL)                  ; Get score for this saucer
                   LD    C,$04                   ; There are only 4 possibilities
                   LD    HL,$1D50                ; Possible scores table
                   LD    DE,$1D4C                ; Print strings for each score
                   LD    A,(DE)                  ; Find ...
                   CP    B                       ; ... the ...
                   JP    Z,$0728                 ; ... print ...
                   INC   HL                      ; ... string ...
                   INC   DE                      ; ... for ...
                   DEC   C                       ; ... the ...
                   JP    NZ,$071D                ; ... score
                   LD    A,(HL)                  ; Get LSB of message (MSB is 2088 which is 1D)
                   LD    (SaucerPriLoc),A        ; Message's LSB (_50=1D94 100=1D97 150=1D9A 300=1D9D)
                   LD    H,$00                   ; MSB = 0 ...
                   LD    L,B                     ; HL = B
                   ADD   HL,HL                   ; *2
                   ADD   HL,HL                   ; *4
                   ADD   HL,HL                   ; *8
                   ADD   HL,HL                   ; *16
                   LD    (ScoreDelta),HL         ; Add score for hitting saucer (015 becomes 150 in BCD).
                   CALL  $0742                   ; Get the flying saucer score descriptor
                   JP    $08F1                   ; Print the three-byte score and out

                   CALL  $0742                   ; Draw the ...
                   JP    DrawSimpleSprite        ; ... flying saucer

                   LD    HL,SaucerPriLoc         ; Read flying saucer ...
                   CALL  ReadDesc                ; ... structure
                   JP    ConvToScr               ; Convert pixel number to screen and shift and out

                   LD    B,$10                   ; Saucer hit sound bit
                   LD    HL,SoundPort5           ; Current state of sounds
                   LD    A,(HL)                  ; OR ...
                   OR    B                       ; ... in ...
                   LD    (HL),A                  ; ... saucer-hit sound
                   CALL  $1770                   ; Turn off fleet sound and start saucer-hit
                   LD    HL,$1D7C                ; Sprite for saucer blowing up
                   LD    (SaucerPriLoc),HL       ; Store it in structure
                   JP    $073C                   ; Draw the flying saucer

                   LD    DE,$1B83                ; Data for saucer (702 sets count to 0A)
                   JP    BlockCopy               ; Reset saucer object data

; Wait for player 1 start button press

WaitForStart       LD    A,$01                   ; Tell ISR that we ...
                   LD    (WaitStartLoop),A       ; ... have started to wait
                   LD    SP,STACK_TOP            ; Reset stack
                   EI                            ; Enable interrupts
                   CALL  $1979                   ; Suspend game tasks
                   CALL  ClearPlayField          ; Clear centre window
                   LD    HL,$3013                ; Screen coordinates
                   LD    DE,Msg_Push             ; Point to string 'PUSH'
                   LD    C,4                     ; Message length
                   CALL  PrintMessage            ; Print it
                   LD    A,(NumCoins)            ; Number of credits
                   DEC   A                       ; Set flags
                   LD    HL,$2810                ; Screen coordinates
                   LD    C,20                    ; Message length
                   JP    NZ,$0857                ; Take 1 or 2 player start
                   LD    DE,Msg_Only1Player      ; Point to string 'ONLY 1PLAYER BUTTON '
                   CALL  PrintMessage            ; Print message
                   IN    A,(INP1)                ; Read player controls
                   AND   $04                     ; 1Player start button?
                   JP    Z,$077F                 ; No ... wait for button or credit


;===============================================================================
; START NEW GAME
;===============================================================================

; 1 Player start

NewGame            LD    B,$99                   ; Essentially a -1 for DAA
                   XOR   A                       ; Clear two player flag

; 2 player start sequence enters here with a=1 and B=98 (-2)

                   LD    (TwoPlayers),A          ; Set flag for 1 or 2 players
                   LD    A,(NumCoins)            ; Number of credits
                   ADD   A,B                     ; Take away credits
                   DAA                           ; Convert back to DAA
                   LD    (NumCoins),A            ; New credit count
                   CALL  DrawNumCredits          ; Display number of credits
                   LD    HL,$0000                ; Score of 0000
                   LD    (P1Score),HL            ; Clear player-1 score
                   LD    (P2Score),HL            ; Clear player-2 score
                   CALL  $1925                   ; Print player-1 score
                   CALL  $192B                   ; Print player-2 score
                   CALL  DisableGameTasks        ; Disable game tasks
                   LD    HL,$0101                ; Two bytes 1, 1
                   LD    A,H                     ; 1 to A
                   LD    (GameMode),A            ; 20EF=1 ... game mode
                   LD    (Player1Alive),HL       ; 20E7 and 20E8 both one ... players 1 and 2 are alive
                   LD    (Player1Ex),HL          ; Extra-ship is available for player-1 and player-2
                   CALL  DrawStatus              ; Print scores and credits
                   CALL  DrawShieldPl1           ; Draw shields for player-1
                   CALL  DrawShieldPl2           ; Draw shields for player-2
                   CALL  GetShipsPerCred         ; Get number of ships from DIP settings
                   LD    (P1ShipsRem),A          ; Player-1 ships
                   LD    (P2ShipsRem),A          ; Player-2 ships
                   CALL  $00D7                   ; Set player-1 and player-2 alien racks going right
                   XOR   A                       ; Make a 0
                   LD    (P1RackCnt),A           ; Player 1 is on first rack of aliens
                   LD    (P2RackCnt),A           ; Player 2 is on first rack of aliens
                   CALL  InitAliens              ; Initialise 55 aliens for player 1
                   CALL  InitAliensP2            ; Initialise 55 aliens for player 2
                   LD    HL,$3878                ; Screen coordinates for lower-left alien
                   LD    (P1RefAlienY),HL        ; Initialise reference alien for player 1
                   LD    (P2RefAlienYr),HL       ; Initialise reference alien for player 2
                   CALL  CopyRamMirror           ; Copy ROM mirror to RAM (2000 - 20C0)
                   CALL  RemoveShip              ; Initialise ship hold indicator

                   CALL  PromptPlayer            ; Prompt with 'PLAY PLAYER '
                   CALL  ClearPlayField          ; Clear the play field

                   NOP

                   XOR   A                       ; Make a 0
                   LD    (IsrSplashTask),A       ; Disable ISR splash-task animation
                   CALL  DrawBottomLine          ; Draw line across screen under player
                   LD    A,(PlayerDataMSB)       ; Current player
                   RRCA                          ; Right bit tells all
                   JP    C,$0872                 ; Go do player 1

                   CALL  RestoreShields2         ; Restore shields for player 2
                   CALL  DrawBottomLine          ; Draw line across screen under player
                   CALL  InitRack                ; Initialise alien rack for current player
                   CALL  EnableGameTasks         ; Enable game tasks in ISR
                   LD    B,$20                   ; Enable ...
                   CALL  SoundBits3On            ; ... sound amplifier

; GAME LOOP

                   CALL  PlrFireOrDemo           ; Initiate player shot if button pressed
                   CALL  PlyrShotAndBump         ; Collision detect player's shot and rack-bump
                   CALL  CountAliens             ; Count aliens (count to 2082)
                   CALL  AdjustScore1            ; Adjust score (and print) if there is an adjustment
                   LD    A,(NumAliens)           ; Number of live aliens
                   AND   A                       ; All aliens gone?
                   JP    Z,$09EF                 ; Yes ... end of turn
                   CALL  UpdateReloadRate        ; Update alien-shot-rate based on player's score
                   CALL  $0935                   ; Check (and handle) extra ship award
                   CALL  SpeedShots              ; Adjust alien shot speed
                   CALL  ShotSound               ; Shot sound on or off with 2025
                   CALL  $0A59                   ; Check if player is hit
                   JP    Z,$0849                 ; No hit ... jump handler
                   LD    B,$04                   ; Player hit sound
                   CALL  SoundBits3On            ; Make explosion sound
                   CALL  FleetDelayExShip        ; Extra-ship sound timer, set fleet-delay, play fleet movement sound
                   OUT   (WATCHDOG),A            ; Feed the watchdog
                   CALL  CtrlSaucerSound         ; Control saucer sound
                   JP    $081F                   ; Continue game loop

                   NOP
                   NOP
                   NOP

; Test for 1 or 2 player start button press

                   LD    DE,Msg_1or2Players      ; '1 OR 2PLAYERS BUTTON'
                   CALL  PrintMessage            ; Print message
                   LD    B,$98                   ; -2 (take away 2 credits)
                   IN    A,(INP1)                ; Read player controls
                   RRCA                          ; Test ...
                   RRCA                          ; ... bit 2
                   JP    C,$086D                 ; 2 player button pressed ... do it
                   RRCA                          ; Test bit 3
                   JP    C,NewGame               ; One player start ... do it
                   JP    $077F                   ; Keep waiting on credit or button

; 2 PLAYER START

                   LD    A,$01                   ; Flag 2 player game
                   JP    $079B                   ; Continue normal startup

                   CALL  RestoreShieldsP1        ; Restore shields for player 1
                   JP    $0814                   ; Continue in game loop

                   LD    A,(RefAlienDXr)         ; Alien deltaY
                   LD    B,A                     ; Hold it
                   LD    HL,(RefAlienYr)         ; Alien coordinates
                   EX    DE,HL                   ; Coordinates to DE
                   JP    GetAlRefPtr             ; HL is 21FC or 22FC and out

                   NOP
                   NOP
                   NOP

; Get pointer to player's alien ref coordinates

GetAlRefPtr        LD    A,(PlayerDataMSB)       ; Player data MSB (21 or 22)
                   LD    H,A                     ; To H
                   LD    L,$FC                   ; 21FC or 22FC ... alien coordinates
                   RET                           ; Done

; Print 'PLAY PLAYER ' and blink score for 2 seconds

PromptPlayer       LD    HL,$2B11                ; Screen coordinates
                   LD    DE,Msg_Player1          ; Point to string 'PLAY PLAYER<1>'
                   LD    C,14                    ; 14 bytes in message
                   CALL  PrintMessage            ; Print the message
                   LD    A,(PlayerDataMSB)       ; Get the player number
                   RRCA                          ; C will be set for player 1
                   LD    A,$1C                   ; The '2' character
                   LD    HL,$3711                ; Replace the '<1>' with '<2>'
                   CALL  NC,DrawChar             ; If player 2 ... change the message
                   LD    A,$B0                   ; Delay of 176 (roughly 2 seconds)
                   LD    (IsrDelay),A            ; Set the ISR delay value
                   LD    A,(IsrDelay)            ; Get the ISR delay value
                   AND   A                       ; Has the 2 second delay expired?
                   RET   Z                       ; Yes ... done
                   AND   $04                     ; Every 4 ISRs ...
                   JP    NZ,$08BC                ; ... flash the player's score
                   CALL  $09CA                   ; Get the score descriptor for the active player
                   CALL  DrawScore               ; Draw the score
                   JP    $08A9                   ; Back to the top of the wait loop

                   LD    B,32                    ; 32 rows (4 characters * 8 bytes each)
                   LD    HL,$271C                ; Player-1 score on the screen
                   LD    A,(PlayerDataMSB)       ; Get the player number
                   RRCA                          ; C will be set for player 1
                   JP    C,$08CB                 ; We have the right score coordinates
                   LD    HL,$391C                ; Use coordinates for player-2's score
                   CALL  ClearSmallSprite        ; Clear a one byte sprite at HL
                   JP    $08A9                   ; Back to the top of the wait loop


; Get number of ships from DIP settings

GetShipsPerCred    IN    A,(INP2)                ; DIP settings
                   AND   $03                     ; Get number of ships
                   ADD   A,$03                   ; From 3-6
                   RET                           ; Done

; With less than 9 aliens on the screen the alien shots get a tad bit faster. Probably
; because the advancing rack can catch them

SpeedShots         LD    A,(NumAliens)           ; Number of aliens on screen
                   CP    $09                     ; More than 8?
                   RET   NC                      ; Yes ... leave shot speed alone
                   LD    A,$FB                   ; Normally FF (-1) ... now FB (-4)
                   LD    (AlienShotDelta),A      ; Speed up alien shots
                   RET                           ; Done

                   LD    A,(TwoPlayers)          ; Number of players
                   AND   A                       ; Skip if ...
                   RET   NZ                      ; ... two player
                   LD    HL,$391C                ; Player 2's score
                   LD    B,32                    ; 32 rows is 4 digits * 8 rows each
                   JP    ClearSmallSprite        ; Clear a one byte sprite (32 rows long) at HL

                   LD    C,$03                   ; Length of saucer-score message ... fall into print

; Print a message on the screen
; HL = coordinates, DE = message buffer, C = length

PrintMessage       LD    A,(DE)                  ; Get character
                   PUSH  DE                      ; Preserve
                   CALL  DrawChar                ; Print character
                   POP   DE                      ; Restore
                   INC   DE                      ; Next character
                   DEC   C                       ; All done?
                   JP    NZ,PrintMessage         ; Print all of message
                   RET                           ; Done

;===============================================================================

; Get pointer to 8 byte sprite number in A and draw sprite on screen at HL

DrawChar           LD    DE,$1E00                ; Character set
                   PUSH  HL                      ; Preserve
                   LD    H,$00                   ; MSB=0
                   LD    L,A                     ; Character number to L
                   ADD   HL,HL                   ; HL = HL *2
                   ADD   HL,HL                   ; *4
                   ADD   HL,HL                   ; *8 (8 bytes each)
                   ADD   HL,DE                   ; Get pointer to sprite
                   EX    DE,HL                   ; Now into DE
                   POP   HL                      ; Restore HL
                   LD    B,8                     ; 8 bytes each
                   OUT   (WATCHDOG),A            ; Feed watchdog
                   JP    DrawSimpleSprite        ; To screen

TimeToSaucer       LD    A,(RefAlienYr)          ; Reference alien's X coordinate
                   CP    $78                     ; Don't process saucer timer ... ($78 is 1st rack Yr)
                   RET   NC                      ; ... unless aliens are closer to bottom
                   LD    HL,(TillSaucer)         ; Time to saucer
                   LD    A,L                     ; Is it time ...
                   OR    H                       ; ... for a saucer
                   JP    NZ,$0929                ; No ... skip flagging
                   LD    HL,$0600                ; Reset timer to 600 game loops
                   LD    A,$01                   ; Flag a ...
                   LD    (SaucerStart),A         ; ... saucer sequence
                   DEC   HL                      ; Decrement the ...
                   LD    (TillSaucer),HL         ; ... time-to-saucer
                   RET                           ; Done

;===============================================================================

; Get number of ships for active player

                   CALL  GetPlayerDataPtr        ; HL points to player data
                   LD    L,$FF                   ; Last byte = number of ships
                   LD    A,(HL)                  ; Get number of ships
                   RET                           ; Done

;===============================================================================

; Award extra ship if score has reached ceiling

                   CALL  CurPlyAlive             ; Get descriptor of sorts
                   DEC   HL                      ; Back up ...
                   DEC   HL                      ; ... two bytes
                   LD    A,(HL)                  ; Has extra ship ...
                   AND   A                       ; already been awarded?
                   RET   Z                       ; Yes ... ignore
                   LD    B,$15                   ; Default 1500
                   IN    A,(INP2)                ; Read DIP settings
                   AND   $08                     ; Extra ship at 1000 or 1500
                   JP    Z,$0948                 ; 0=1500
                   LD    B,$10                   ; Awarded at 1000
                   CALL  $09CA                   ; Get score descriptor for active player
                   INC   HL                      ; MSB of score ...
                   LD    A,(HL)                  ; ... to accumulator
                   CP    B                       ; Time for an extra ship?
                   RET   C                       ; No ... out
                   CALL  $092E                   ; Get pointer to number of ships
                   INC   (HL)                    ; Bump number of ships
                   LD    A,(HL)                  ; Get the new total
                   PUSH  AF                      ; Hang onto it for a bit
                   LD    HL,$2501                ; Screen coords for ship hold
                   INC   H                       ; Bump to ...
                   INC   H                       ; ... next
                   DEC   A                       ; ... spot
                   JP    NZ,$0958                ; Find spot for new ship
                   LD    B,16                    ; 16 byte sprite
                   LD    DE,PlayerSprite         ; Player sprite
                   CALL  DrawSimpleSprite        ; Draw the sprite
                   POP   AF                      ; Restore the count
                   INC   A                       ; +1
                   CALL  $1A8B                   ; Print the number of ships
                   CALL  CurPlyAlive             ; Get descriptor for active player of some sort
                   DEC   HL                      ; Back up ...
                   DEC   HL                      ; ... two bytes
                   LD    (HL),$00                ; Flag extra ship has been awarded
                   LD    A,$FF                   ; Set timer ...
                   LD    (ExtraHold),A           ; ... for extra-ship sound
                   LD    B,$10                   ; Make sound ...
                   JP    SoundBits3On            ; ... for extra man

AlienScoreValue    LD    HL,AlienScores          ; Table for scores for hitting alien
                   CP    $02                     ; 0 or 1 (lower two rows) ...
                   RET   C                       ; ... return HL points to value 10
                   INC   HL                      ; next value
                   CP    $04                     ; 2 or 3 (middle two rows) ...
                   RET   C                       ; ... return HL points to value 20
                   INC   HL                      ; Top row ...
                   RET                           ; ... return HL points to value 30

; Adjust the score for the active player. 20F1 is 1 if there is a new value to add.
; The adjustment is in 20F2,20F3. Then print the score

AdjustScore1       CALL  $09CA                   ; Get score structure for active player
                   LD    A,(AdjustScore)         ; Does the score ...
                   AND   A                       ; ... need increasing?
                   RET   Z                       ; No ... done
                   XOR   A                       ; Mark score ...
                   LD    (AdjustScore),A         ; ... as adjusted
                   PUSH  HL                      ; Hold the pointer to the structure
                   LD    HL,(ScoreDelta)         ; Get requested adjustment
                   EX    DE,HL                   ; Adjustment to DE
                   POP   HL                      ; Get back pointer to structure
                   LD    A,(HL)                  ; Add adjustment ...
                   ADD   A,E                     ; ... first byte
                   DAA                           ; Adjust it for BCD
                   LD    (HL),A                  ; Store new LSB
                   LD    E,A                     ; Add adjustment ...
                   INC   HL                      ; ... to ...
                   LD    A,(HL)                  ; ... second ...
                   ADC   A,D                     ; ... byte
                   DAA                           ; Adjust for BCD (cary gets dropped)
                   LD    (HL),A                  ; Store second byte
                   LD    D,A                     ; Second byte to D (first byte still in E)
                   INC   HL                      ; Load ...
                   LD    A,(HL)                  ; ... the ...
                   INC   HL                      ; ... screen ...
                   LD    H,(HL)                  ; ... coordinates ...
                   LD    L,A                     ; ... to HL
                   JP    Print4Digits            ; ** Usually a good idea, but wasted here

; Print 4 digits in DE

Print4Digits       LD    A,D                     ; Get first 2 digits of BCD or hex
                   CALL  DrawHexByte             ; Print them
                   LD    A,E                     ; Get second 2 digits of BCD or hex (fall into print)

; Display 2 digits in A to screen at HL

DrawHexByte        PUSH  DE                      ; Preserve
                   PUSH  AF                      ; Save for later
                   RRCA                          ; Get ...
                   RRCA                          ; ...
                   RRCA                          ; ...
                   RRCA                          ; ... left digit
                   AND   $0F                     ; Mask out lower digit's bits
                   CALL  $09C5                   ; To screen at HL
                   POP   AF                      ; Restore digit
                   AND   $0F                     ; Mask out upper digit
                   CALL  $09C5                   ; To screen
                   POP   DE                      ; Restore
                   RET                           ; Done

                   ADD   A,$1A                   ; Bump to number characters
                   JP    DrawChar                ; Continue ...

; Get score descriptor for active player

                   LD    A,(PlayerDataMSB)       ; Get active player
                   RRCA                          ; Test for player
                   LD    HL,P1Score              ; Player 1 score descriptor
                   RET   C                       ; Keep it if player 1 is active
                   LD    HL,P2Score              ; Else get player 2 descriptor
                   RET                           ; Done

; Clear center window of screen

ClearPlayField     LD    HL,$2402                ; Third from left, top of screen
                   LD    (HL),$00                ; Clear screen byte
                   INC   HL                      ; Next in row
                   LD    A,L                     ; Get X ...
                   AND   $1F                     ; ... coordinate
                   CP    $1C                     ; Edge minus a buffer?
                   JP    C,$09E8                 ; No ... keep going
                   LD    DE,$0006                ; Else ... bump to
                   ADD   HL,DE                   ; ... next edge + buffer
                   LD    A,H                     ; Get Y coordinate
                   CP    $40                     ; Reached bottom?
                   JP    C,$09D9                 ; No ... keep going
                   RET                           ; Done

                   CALL  $0A3C                   ;
                   XOR   A                       ; Suspend ...
                   LD    (SuspendPlay),A         ; ... ISR game tasks
                   CALL  ClearPlayField          ; Clear play field
                   LD    A,(PlayerDataMSB)       ; Hold current player number ...
                   PUSH  AF                      ; ... on stack
                   CALL  CopyRamMirror           ; Block copy RAM mirror from ROM
                   POP   AF                      ; Restore ...
                   LD    (PlayerDataMSB),A       ; ... current player number
                   LD    A,(PlayerDataMSB)       ; ** Why load this again? Nobody ever jumps to 0A04?
                   LD    H,A                     ; To H
                   PUSH  HL                      ; Hold player-data pointer
                   LD    L,$FE                   ; 2xFE ... rack count
                   LD    A,(HL)                  ; Get the number of racks the player has beaten
                   AND   $07                     ; 0-7
                   INC   A                       ; Now 1-8
                   LD    (HL),A                  ; Update count since player just beat a rack
                   LD    HL,AlienScores + 2      ; Starting coordinate of alien table
                   INC   HL                      ; Find the ...
                   DEC   A                       ; ... right entry ...
                   JP    NZ,$0A13                ; ... in the table
                   LD    A,(HL)                  ; Get the starting Y coordinate
                   POP   HL                      ; Restore player's pointer
                   LD    L,$FC                   ; 2xFC ...
                   LD    (HL),A                  ; Set rack's starting Y coordinate
                   INC   HL                      ; Point to X
                   LD    (HL),$38                ; Set rack's starting X coordinate to 38
                   LD    A,H                     ; Player ...
                   RRCA                          ; ... number to carry
                   JP    C,$0A33                 ; 2nd player stuff
                   LD    A,$21                   ; Start fleet with ...
                   LD    (SoundPort5),A          ; ... first sound
                   CALL  DrawShieldPl2           ; Draw shields for player 2
                   CALL  InitAliensP2            ; Initialise aliens for player 2
                   JP    $0804                   ; Continue at top of game loop

                   CALL  DrawShieldPl1           ; Draw shields for player 1
                   CALL  InitAliens              ; Initialise aliens for player 1
                   JP    $0804                   ; Continue at top of game loop

                   CALL  $0A59                   ; Check player collision
                   JP    NZ,$0A52                ; Player is not alive ... skip delay
                   LD    A,$30                   ; Half second delay
                   LD    (IsrDelay),A            ; Set ISR timer
                   LD    A,(IsrDelay)            ; Has timer expired?
                   AND   A                       ; Check expire
                   RET   Z                       ; Out if done
                   CALL  $0A59                   ; Check player collision
                   JP    Z,$0A47                 ; No collision ... wait on timer
                   CALL  $0A59                   ; Wait for ...
                   JP    NZ,$0A52                ; ... collision to end
                   RET                           ; Done

; Check to see if player is hit

                   LD    A,(PlayerAlive)         ; Active player hit flag
                   CP    $00FF                   ; All FFs means player is OK
                   RET                           ; Done

; Start the hit-alien sound and flag the adjustment for the score
; B contains the row, which determines the score value

ScoreForAlien      LD    A,(GameMode)            ; Are we in ...
                   AND   A                       ; ... game mode?
                   JP    Z,$0A7C                 ; No ... skip scoring in demo
                   LD    C,B                     ; Hold row number
                   LD    B,$08                   ; Alien hit sound
                   CALL  SoundBits3On            ; Enable sound
                   LD    B,C                     ; Restore row number
                   LD    A,B                     ; Into A
                   CALL  AlienScoreValue         ; Look up the score for the alien
                   LD    A,(HL)                  ; Get the score value
                   LD    HL,ScoreDelta + 1       ; Pointer to score delta
                   LD    (HL),$00                ; Upper byte of score delta is '00'
                   DEC   HL                      ; Point to score delta LSB
                   LD    (HL),A                  ; Set score for hitting alien
                   DEC   HL                      ; Point to adjust-score-flag
                   LD    (HL),$01                ; The score will get changed elsewhere
                   LD    HL,ExpAlien             ; Return exploding-alien descriptor
                   RET                           ; Done

; Start the ISR moving the sprite. Return when done

Animate            LD    A,$02                   ; Start simple linear ...
                   LD    (IsrSplashTask),A       ; ... sprite animation (splash)
                   OUT   (WATCHDOG),A            ; Feed watchdog
                   LD    A,(SplashReached)       ; Has the ...
                   AND   A                       ; ... sprite reached target?
                   JP    Z,$0A85                 ; No ... wait
                   XOR   A                       ; Stop ...
                   LD    (IsrSplashTask),A       ; ... ISR animation
                   RET                           ; Done

; Print message from DE to screen at HL (length in B) with a
; delay between letters

PrintMessageDel    PUSH  DE                      ; Preserve
                   LD    A,(DE)                  ; Get character
                   CALL  DrawChar                ; Draw character on screen
                   POP   DE                      ; Preserve
                   LD    A,$07                   ; Delay between letters
                   LD    (IsrDelay),A            ; Set counter
                   LD    A,(IsrDelay)            ; Get counter
                   DEC   A                       ; Is it 1?
                   JP    NZ,$0A9E                ; No ... wait on it
                   INC   DE                      ; Next in message
                   DEC   C                       ; All done?
                   JP    NZ,PrintMessageDel      ; No ... do all
                   RET                           ; Done

SplashSquiggly     LD    HL,Obj4Timer            ; Pointer to game-object 4 timer
                   JP    $024B                   ; Process squiggly-shot in demo mode

; Delay 64 interrupts

OneSecDelay        LD    A,$40                   ; Delay of 64 (tad over 1 sec)
                   JP    $0AD7                   ; Do delay

; Delay 128 interrupts

TwoSecDelay        LD    A,$0080                 ; Delay of 80 (tad over 2 sec)
                   JP    $0AD7                   ; Do delay

SplashDemo         POP   HL                      ; Drop the call to ABF and ...
                   JP    $0072                   ; ... do a demo game loop without sound

; Different types of splash tasks managed by ISR in splash screens. The ISR
; calls this if in splash-mode. These may have been bit flags to allow all 3
; at the same time. Maybe it is just easier to do a switch with a rotate-to-carry

ISRSplTasks        LD    A,(IsrSplashTask)       ; Get the ISR task number
                   RRCA                          ; In demo play mode?
                   JP    C,SplashDemo            ; 1: Yes ... go do game play (without sound)
                   RRCA                          ; Moving little alien from point A to B?
                   JP    C,SplashSprite          ; 2: Yes ... go move little alien from point A to B
                   RRCA                          ; Shooting extra 'C' with squiggly shot?
                   JP    C,SplashSquiggly        ; 4: Yes ... go shoot extra 'C' in splash
                   RET                           ; No task to do

; Message to center of screen
; Only used in one place for 'SPACE  INVADERS'

                   LD    HL,$2B14                ; Near centre of screen
                   LD    C,15                    ; 15 bytes in message
                   JP    PrintMessageDel         ; Print and out

; Wait on ISR counter to reach 0

WaitOnDelay        LD    (IsrDelay),A            ; Delay counter
                   LD    A,(IsrDelay)            ; Get current delay
                   AND   A                       ; Zero yet?
                   JP    NZ,$0ADA                ; No ... wait on it
                   RET                           ; Done

; Init the splash-animation block

IniSplashAni       LD    HL,SplashAnForm         ; The splash-animation descriptor
                   LD    B,12                    ; 12 bytes
                   JP    BlockCopy               ; Block copy DE to descriptor

;===============================================================================

; After initialization ... splash screens

                   XOR   A                       ; Make a 0
                   OUT   (SOUND1),A              ; Turn off sound
                   OUT   (SOUND2),A              ; Turn off sound
                   CALL  $1982                   ; Turn off ISR splash-task
                   EI                            ; Enable interrupts (using them for delays)
                   CALL  OneSecDelay             ; One second delay
                   LD    A,(SplashAnimate)       ; Splash screen type
                   AND   A                       ; Set flags based on type
                   LD    HL,$3017                ; Screen coordinates (middle near top)
                   LD    C,4                     ; 4 characters in 'PLAY'
                   JP    NZ,$0BE8                ; Not 0 ... do 'normal' PLAY
                   LD    DE,Msg_PlayUY           ; Point to string 'PLAy' with upside down 'Y'
                   CALL  PrintMessageDel         ; Print the 'PLAy'
                   LD    DE,Msg_Invaders         ; Point to string 'SPACE  INVADERS'
                   CALL  $0ACF                   ; Print to middle-ish of screen
                   CALL  OneSecDelay             ; One second delay
                   CALL  DrawAdvTable            ; Draw 'SCORE ADVANCE TABLE' with print delay
                   CALL  TwoSecDelay             ; Two second delay
                   LD    A,(SplashAnimate)       ; Do splash ...
                   AND   A                       ; ... animations?
                   JP    NZ,$0B4A                ; Not 0 ... no animations

; Animate small alien replacing upside-down Y with correct Y

                   LD    DE,SplashScrAnimStr1    ; Animate sprite from Y=FE to Y=9E step -1
                   CALL  IniSplashAni            ; Copy to splash-animate structure
                   CALL  Animate                 ; Wait for ISR to move sprite (small alien)
                   LD    DE,Temp_1BB0            ; Animate sprite from Y=98 to Y=FF step 1
                   CALL  IniSplashAni            ; Copy to splash-animate structure
                   CALL  Animate                 ; Wait for ISR to move sprite (alien pulling upside down Y)
                   CALL  OneSecDelay             ; One second delay
                   LD    DE,SplashScrAnimStr3    ; Animate sprite from Y=FF to Y=97 step 1
                   CALL  IniSplashAni            ; Copy to splash-animate structure
                   CALL  Animate                 ; Wait for ISR to move sprite (alien pushing Y)
                   CALL  OneSecDelay             ; One second delay
                   LD    HL,$33B7                ; Where the splash alien ends up
                   LD    B,10                    ; 10 rows
                   CALL  ClearSmallSprite        ; Clear a one byte sprite at HL
                   CALL  TwoSecDelay             ; Two second delay

; Play demo

                   CALL  ClearPlayField          ; Clear play field
                   LD    A,(P1ShipsRem)          ; Number of ships for player-1
                   AND   A                       ; If non zero ...
                   JP    NZ,$0B5D                ; ... keep it (counts down between demos)
                   CALL  GetShipsPerCred         ; Get number of ships from DIP settings
                   LD    (P1ShipsRem),A          ; Reset number of ships for player-1
                   CALL  RemoveShip              ; Remove a ship from stash and update indicators

                   CALL  CopyRamMirror           ; Block copy ROM mirror to initialise RAM
                   CALL  InitAliens              ; Initialise all player 1 aliens
                   CALL  DrawShieldPl1           ; Draw shields for player 1 (to buffer)
                   CALL  RestoreShieldsP1        ; Restore shields for player 1 (to screen)
                   LD    A,$01                   ; ISR splash-task ...
                   LD    (IsrSplashTask),A       ; ... playing demo
                   CALL  DrawBottomLine          ; Draw play field line

                   CALL  PlrFireOrDemo           ; In demo ... process demo movement and always fire
                   CALL  $0BF1                   ; Check player shot and aliens bumping edges of screen and hidden message
                   OUT   (WATCHDOG),A            ; Feed watchdog
                   CALL  $0A59                   ; Has demo player been hit?
                   JP    Z,$0B71                 ; No ... continue game
                   XOR   A                       ; Remove player shot ...
                   LD    (PlyrShotStatus),A      ; ... from activity
                   CALL  $0A59                   ; Wait for demo player ...
                   JP    NZ,$0B83                ; ... to stop exploding

; Credit information

                   XOR   A                       ; Turn off ...
                   LD    (IsrSplashTask),A       ; ... splash animation
                   CALL  OneSecDelay             ; One second delay
                   CALL  $1988                   ; ** Something else at one time? Jump straight to clear-play-field
                   LD    C,12                   ; Message size
                   LD    HL,$2C11                ; Screen coordinates
                   LD    DE,$1F90                ; 'INSERT  COIN'
                   CALL  PrintMessage            ; Print message
                   LD    A,(SplashAnimate)       ; Do splash ...
                   CP    $00                     ; ... animations?
                   JP    NZ,$0BAE                ; Not 0 ... not on this screen
                   LD    HL,$3311                ; Screen coordinates
                   LD    A,$02                   ; Character 'C'
                   CALL  DrawChar                ; Put an extra 'C' for 'CCOIN' on the screen
                   LD    BC,$1F9C                ; '<1 OR 2 PLAYERS>  '
                   CALL  ReadPriStruct           ; Load the screen pointer
                   CALL  $184C                   ; Print the message
                   IN    A,(INP2)                ; Display coin info (bit 7) ...
                   RLCA                          ; ... on demo screen?
                   JP    C,$0BC3                 ; 1 means no ... skip it
                   LD    BC,$1FA0                ; '*1 PLAYER  1 COIN '
                   CALL  $183A                   ; Load the descriptor
                   CALL  TwoSecDelay             ; Print TWO descriptors worth
                   LD    A,(SplashAnimate)       ; Doing splash ...
                   CP    $00                     ; ... animation?
                   JP    NZ,$0BDA                ; Not 0 ... not on this screen
                   LD    DE,SplashScrAnimStr4    ; Animation for small alien to line up with extra 'C'
                   CALL  IniSplashAni            ; Copy the animation block
                   CALL  Animate                 ; Wait for the animation to complete
                   CALL  $189E                   ; Animate alien shot to extra 'C'
                   LD    HL,SplashAnimate        ; Toggle ...
                   LD    A,(HL)                  ; ... the ...
                   INC   A                       ; ... splash screen ...
                   AND   $01                     ; ... animation for ...
                   LD    (HL),A                  ; ... next time
                   CALL  ClearPlayField          ; Clear play field
                   JP    $18DF                   ; Keep splashing

                   LD    DE,Msg_PlayY            ;'PLAY' with normal 'Y'
                   CALL  PrintMessageDel         ; Print it
                   JP    $0B0B                   ; Continue with splash (HL will be pointing to next message)

                   CALL  PlyrShotAndBump         ; Check if player is shot and aliens bumping the edge of screen
                   JP    CheckHiddenMes          ; Check for hidden-message display sequence

Msg_TaitoCop       DB    $13, $00, $08, $13      ; 'TAITO COP' (no R)
                   DB    $0E ,$26 ,$02, $0E
                   DB    $0F

;===============================================================================

; DIAGNOSTICS ROUTINE (not included)
;
; The very centre 2K of the code map is an expansion area. It originally contained
; a 1K diagnostics routine beginning at 1000. The original code would check bit 0
; of port 0 (wired to DIP4) and jump to this routine if the switch was flipped.
; The routine was removed in this Midway version of the code. And it was removed
; in later versions of the TAITO code line

                   DS    2048,0                  ; Write zeros to fill space

;===============================================================================

; The only differences between this and EraseSimpleSprite is two CPL instructions in the latter and
; the use of AND instead of OR. NOP takes the same amount of time/space as CPL. So the two NOPs
; here make these two parallel routines the same size and speed

DrawShiftedSprite  NOP                           ; Time/size pad to match CPL in EraseShiftedSprite
                   CALL  CnvtPixNumber           ; Convert pixel number to coord and shift
                   NOP                           ; Time/size pad to match CPL in EraseShiftedSprite
                   PUSH  BC                      ; Hold count
                   PUSH  HL                      ; Hold start coordinate
                   LD    A,(DE)                  ; Get the picture bits
                   OUT   (SHFT_DATA),A           ; Store in shift register
                   IN    A,(SHFT_IN)             ; Read the shifted pixels
                   OR    (HL)                    ; OR them onto the screen
                   LD    (HL),A                  ; Store them back to screen
                   INC   HL                      ; Next column on screen
                   INC   DE                      ; Next in picture
                   XOR   A                       ; Shift over ...
                   OUT   (SHFT_DATA),A           ; ... to next byte in register (shift in 0)
                   IN    A,(SHFT_IN)             ; Read the shifted pixels
                   OR    (HL)                    ; OR them onto the screen
                   LD    (HL),A                  ; Store them back to screen
                   POP   HL                      ; Restore starting coordinate
                   LD    BC,32                   ; Add 32 ...
                   ADD   HL,BC                   ; ... to coordinate (move to next row)
                   POP   BC                      ; Restore count
                   DEC   B                       ; All done?
                   JP    NZ,$1405                ; No ... go do all rows
                   RET                           ; Done

                   NOP
                   NOP

; Clear a sprite from the screen (standard pixel number descriptor)
; ** We clear 2 bytes even though the draw-simple only draws one

EraseSimpleSprite  CALL  CnvtPixNumber           ; Convert pixel number in HL
                   PUSH  BC                      ; Hold
                   PUSH  HL                      ; Hold
                   XOR   A                       ; 0
                   LD    (HL),A                  ; Clear screen byte
                   INC   HL                      ; Next byte
                   LD    (HL),A                  ; Clear byte
                   INC   HL                      ; ** Is this to mimic timing? We increment then pop
                   POP   HL                      ; Restore screen coordinate
                   LD    BC,32                   ; Add 1 row of 32 ...
                   ADD   HL,BC                   ; ... to screen coordinate
                   POP   BC                      ; Restore counter
                   DEC   B                       ; All rows done?
                   JP    NZ,$1427                ; Do all rows
                   RET                           ; Done

; Display character to screen
; HL = screen coordinates, DE = character data, B = number of rows

DrawSimpleSprite   PUSH  BC                      ; Preserve counter
                   LD    A,(DE)                  ; From character set ...
                   LD    (HL),A                  ; ... to screen
                   INC   DE                      ; Next in character set
                   LD    BC,32                   ; Next row of 32 ...
                   ADD   HL,BC                   ; ... on screen
                   POP   BC                      ; Restore counter
                   DEC   B                       ; Decrement counter
                   JP    NZ,DrawSimpleSprite     ; Do all
                   RET                           ; Done

                   DS    11, 0                    ; Unused space?

; Erases a shifted sprite from screen (like for player's explosion)

EraseShifted       CALL  CnvtPixNumber           ; Convert pixel number in HL to coordinates with shift
                   PUSH  BC                      ; Hold BC
                   PUSH  HL                      ; Hold coordinate
                   LD    A,(DE)                  ; Get picture value
                   OUT   (SHFT_DATA),A           ; Value into shift register
                   IN    A,(SHFT_IN)             ; Read shifted sprite picture
                   CPL                           ; Reverse it (erasing bits)
                   AND   (HL)                    ; Erase the bits from the screen
                   LD    (HL),A                  ; Store the erased pattern back
                   INC   HL                      ; Next column on screen
                   INC   DE                      ; Next in image
                   XOR   A                       ; Shift register over ...
                   OUT   (SHFT_DATA),A           ; ... 8 bits (shift in 0)
                   IN    A,(SHFT_IN)             ; Read 2nd byte of image
                   CPL                           ; Reverse it (erasing bits)
                   AND   (HL)                    ; Erase the bits from the screen
                   LD    (HL),A                  ; Store the erased pattern back
                   POP   HL                      ; Restore starting coordinate
                   LD    BC,32                   ; Add 32 ...
                   ADD   HL,BC                   ; ... to next row
                   POP   BC                      ; Restore BC (count)
                   DEC   B                       ; All rows done?
                   JP    NZ,$1455                ; No ... erase all
                   RET                           ; Done

; Convert pixel number in HL to screen coordinate and shift amount
; HL gets screen coordinate
; Hardware shift-register gets amount

CnvtPixNumber      LD    A,L                     ; Get X coordinate
                   AND   $07                     ; Shift by pixel position
                   OUT   (SHFT_AMNT),A           ; Write shift amount to hardware
                   JP    ConvToScr               ; HL = HL/8 + 2000 (screen coordinate)

; In a multi-player game the player's shields are block-copied to and from RAM between turns
; HL = screen pointer, DE = memory buffer, B = number of rows, C = number of columns

RememberShields    PUSH  BC                      ; Hold counter
                   PUSH  HL                      ; Hold start
                   LD    A,(HL)                  ; From sprite ... (should be DE)
                   LD    (DE),A                  ; ... to screen ... (should be HL)
                   INC   DE                      ; Next in sprite
                   INC   HL                      ; Next on screen
                   DEC   C                       ; All columns done?
                   JP    NZ,$147E                ; No ... do multi columns
                   POP   HL                      ; Restore screen start
                   LD    BC,32                   ; Add 32 ...
                   ADD   HL,BC                   ; ... to get to next row
                   POP   BC                      ; Pop the counters
                   DEC   B                       ; All rows done?
                   JP    NZ,RememberShields      ; No ... do multi rows
                   RET                           ; Done

DrawSprCollision   CALL  CnvtPixNumber           ; Convert pixel number to coord and shift
                   XOR   A                       ; Clear the ...
                   LD    (Collision),A           ; ... collision-detection flag
                   PUSH  BC                      ; Hold count
                   PUSH  HL                      ; Hold screen
                   LD    A,(DE)                  ; Get byte
                   OUT   (SHFT_DATA),A           ; Write first byte to shift register
                   IN    A,(SHFT_IN)             ; Read shifted pattern
                   PUSH  AF                      ; Hold the pattern
                   AND   (HL)                    ; Any bits from pixel collide with bits on screen?
                   JP    Z,$14A9                 ; No ... leave flag alone
                   LD    A,$01                   ; Yes ... set ...
                   LD    (Collision),A           ; ... collision flag
                   POP   AF                      ; Restore the pixel pattern
                   OR    (HL)                    ; OR it onto the screen
                   LD    (HL),A                  ; Store new screen value
                   INC   HL                      ; Next byte on screen
                   INC   DE                      ; Next in pixel pattern
                   XOR   A                       ; Write zero ...
                   OUT   (SHFT_DATA),A           ; ... to shift register
                   IN    A,(SHFT_IN)             ; Read 2nd half of shifted sprite
                   PUSH  AF                      ; Hold pattern
                   AND   (HL)                    ; Any bits from pixel collide with bits on screen?
                   JP    Z,$14BD                 ; No ... leave flag alone
                   LD    A,$01                   ; Yes ... set ...
                   LD    (Collision),A           ; ... collision flag
                   POP   AF                      ; Restore the pixel pattern
                   OR    (HL)                    ; OR it onto the screen
                   LD    (HL),A                  ; Store new screen pattern
                   POP   HL                      ; Starting screen coordinate
                   LD    BC,32                   ; Add 32 ...
                   ADD   HL,BC                   ; ... to get to next row
                   POP   BC                      ; Restore count
                   DEC   B                       ; All done?
                   JP    NZ,$1498                ; No ... do all rows
                   RET                           ; Done

; Clear a one byte sprite at HL. B=number of rows

ClearSmallSprite   XOR   A                       ; 0
                   PUSH  BC                      ; Preserve BC
                   LD    (HL),A                  ; Clear screen byte
                   LD    BC,32                   ; Bump HL ...
                   ADD   HL,BC                   ; ... one screen row of 32
                   POP   BC                      ; Restore
                   DEC   B                       ; All done?
                   JP    NZ,$14CC                ; No ... clear all
                   RET                           ; Done

; The player's shot hit something (or is being removed from play)

PlayerShotHit      LD    A,(PlyrShotStatus)      ; Player shot flag
                   CP    $05                     ; Alien explosion in progress?
                   RET   Z                       ; Yes ... ignore this function
                   CP    $02                     ; Normal movement?
                   RET   NZ                      ; No ... out

                   LD    A,(Obj1CoorYr)          ; Get Yr coordinate of player shot
                   CP    $D8                     ; Compare to 216 (40 from Top-rotated)
                   LD    B,A                     ; Hold value for later
                   JP    NC,$1530                ; Yr is within 40 from top initiate miss-explosion (shot flag 3)
                   LD    A,(AlienIsExploding)    ; Is an alien ...
                   AND   A                       ; ... blowing up?
                   RET   Z                       ; No ... out

                   LD    A,B                     ; Get original Yr coordinate back to A
                   CP    $CE                     ; Compare to 206 (50 from rotated top)
                   JP    NC,$1579                ; Yr is within 50 from top? Yes ... saucer must be hit
                   ADD   A,$06                   ; Offset to coordinate for wider 'explosion' picture
                   LD    B,A                     ; Hold that
                   LD    A,(RefAlienYr)          ; Ref alien Y coordinate

; If the lower 4 rows are all empty then the reference alien's Y coordinate will wrap around from 0 to F8.
; At this point the top row of aliens is in the shields and we will assume that everything is within
; the rack

                   CP    $90                     ; This is true if ...
                   JP    NC,CodeBug1             ; ... aliens are down in the shields
                   CP    B                       ; Compare to shot's coordinate
                   JP    NC,$1530                ; Outside the rack-square ... do miss explosion

; We get here if the player's shot hit something within the rack area (a shot or an alien).
; Find the alien that is (or would be) where the shot hit. If there is no alien alive at the row/column
; thn the player hit an alien missile. If there is an alien then explode the alien.
;
; There is a code bug here, but it is extremely subtle. The algorithm for finding the row/column in the
; rack works by adding 16 to the reference coordinates (X for column, Y for row) until it passes or equals
; the target coordinates. This works great as long as the target point is within the alien's rack area.
; If the reference point is far to the right, the column number will be greater than 11, which messes
; up the column/row-to-pointer math.
;
; The entire rack of aliens is based on the lower left alien. Imagine all aliens are dead except the
; upper left. It wiggles down the screen and enters the players shields on the lower left where it begins
; to eat them. Imagine the player is under his own shields on the right side of the screen and fires a
; shot into his own shield.
;
; The alien is in the rack on row 4 (rows are numbered from bottom up starting with 0). The shot hits
; the shields below the alien's Y coordinate and gets correctly assigned to row 3. The alien is in the rack
; at column 0 (columns are numbered from left to right starting with 0). The shot hits the shields far to
; the right of the alien's X coordinate. The algorithm says it is in column 11. But 0-10 are the only
; correct values.
;
; The column/row-to-pointer math works by multiplying the row by 11 and adding the column. For the alien
; that is 11*4 + 0 = 44. For the shot that is 11*3 +11 = 44. The game thinks the shot hit the alien.

CodeBug1           LD    L,B                     ; L now holds the shot coordinate (adjusted)
                   CALL  FindRow                 ; Look up row number to B
                   LD    A,(Obj1CoorXr)          ; Player's shot's Xr coordinate ...
                   LD    H,A                     ; ... to H
                   CALL  FindColumn              ; Get alien's coordinate
                   LD    (ExpAlienYr),HL         ; Put it in the exploding-alien descriptor
                   LD    A,$05                   ; Flag alien explosion ...
                   LD    (PlyrShotStatus),A      ; ... in progress
                   CALL  GetAlienStatPtr         ; Get descriptor for alien
                   LD    A,(HL)                  ; Is alien ...
                   AND   A                       ; ... alive
                   JP    Z,$1530                 ; No ... must have been an alien shot

                   LD    (HL),$00                ; Make alien invader dead
                   CALL  ScoreForAlien           ; Makes alien explosion sound and adjust score
                   CALL  ReadDesc                ; Load 5 byte sprite descriptor
                   CALL  DrawSprite              ; Draw explosion sprite on screen
                   LD    A,$10                   ; Initiate alien-explosion
                   LD    (ExpAlienTimer),A       ; ... timer to 16
                   RET                           ; Done

; Player shot leaving playfield, hitting shield, or hitting an alien shot

                   LD    A,$03                   ; Mark ...
                   LD    (PlyrShotStatus),A      ; ... player shot hit something other than alien
                   JP    $154A                   ; Finish up

; Time down the alien explosion. Remove when done

AlienExplodeTime   LD    HL,ExpAlienTimer        ; Decrement alien explosion ...
                   DEC   (HL)                    ; ... timer
                   RET   NZ                      ; Not done  ... out
                   LD    HL,(ExpAlienYr)         ; Pixel pointer for exploding alien
                   LD    B,16                    ; 16 row pixel
                   CALL  EraseSimpleSprite       ; Clear the explosion sprite from the screen
                   LD    A,4                     ; 4 means that ...
                   LD    (PlyrShotStatus),A      ; ... alien has exploded (remove from active duty)

                   XOR   A                       ; Turn off ...
                   LD    (AlienIsExploding),A    ; ... alien-is-blowing-up flag
                   LD    B,$F7                   ; Turn off ...
                   JP    SoundBits3Off           ; ... alien exploding sound

                   NOP

; Count number of 16s needed to bring reference (in A) up to target (in H).
; If the reference starts out beyond the target then we add 16s as long as
; the reference has a signed bit. But these aren't signed quantities. This
; doesn't make any sense. This counting algorithm produces questionable
; results if the reference is beyond the target

Cnt16s             LD    C,$00                   ; Count of 16s
                   CP    H                       ; Compare reference coordinate to target
                   CALL  NC,WrapRef              ; If reference is greater or equal then do something questionable ... see below
                   CP    H                       ; Compare reference coordinate to target
                   RET   NC                      ; If reference is greater or equal then done
                   ADD   A,16                    ; Add 16 to reference
                   INC   C                       ; Bump 16s count
                   JP    $155A                   ; Keep testing

; L contains a Yr coordinate. Find the row number within the rack that corresponds
; to the Yr coordinate. Return the row coordinate in L and the row number in C

FindRow            LD    A,(RefAlienYr)          ; Reference alien Yr coordinate
                   LD    H,L                     ; Target Yr coordinate to H
                   CALL  Cnt16s                  ; Count 16s needed to bring ref alien to target
                   LD    B,C                     ; Count to B
                   DEC   B                       ; Base 0
                   SBC   A,16                    ; The counting also adds 16 no matter what
                   LD    L,A                     ; To coordinate
                   RET                           ; Done

; H contains a Xr coordinate. Find the column number within the rack that corresponds
; to the Xr coordinate. Return the column coordinate in H and the column number in C

FindColumn         LD    A,(RefAlienXr)          ; Reference alien Yn coordinate
                   CALL  Cnt16s                  ; Count 16s to bring Y to target Y
                   SBC   A,16                    ; Subtract off extra 16
                   LD    H,A                     ; To H
                   RET                           ; Done

                   LD    A,$01                   ; Mark flying ...
                   LD    (SaucerHit),A           ; ... saucer has been hit
                   JP    $1545                   ; Remove player shot

; B is row number. C is column number (starts at 1)
; Return pointer to alien-status flag for current player

GetAlienStatPtr    LD    A,B                     ; Hold original
                   RLCA                          ; *2
                   RLCA                          ; *4
                   RLCA                          ; *8
                   ADD   A,B                     ; *9
                   ADD   A,B                     ; *10
                   ADD   A,B                     ; *11
                   ADD   A,C                     ; Add row offset to column offset
                   DEC   A                       ; -1
                   LD    L,A                     ; Set LSB of HL
                   LD    A,(PlayerDataMSB)       ; Set ...
                   LD    H,A                     ; ... MSB of HL with active player indicator
                   RET                           ; Done

; This is called if the reference point is greater than the target point. I believe the goal is to
; wrap the reference back around until it is lower than the target point. But the algorithm simply adds
; until the sign bit of the the reference is 0. If the target is 2 and the reference is 238 then this
; algorithm moves the reference 238+16=244 then 244+16=4. Then the algorithm stops. But the reference is
; STILL greater than the target

; Also imagine that the target is 20 and the reference is 40. The algorithm adds 40+16=56, which is not
; negative, so it stops there

; I think the intended code is 'JP NC' instead of 'JP M', but even that doesn't make sense

WrapRef            INC   C                       ; Increase 16s count
                   ADD   A,16                    ; Add 16 to ref
                   JP    M,WrapRef               ; Keep going till result is positive
                   RET                           ; Done

; When rack bumps the edge of the screen then the direction flips and the rack
; drops 8 pixels. The deltaX and deltaY values are changed here. Interestingly
; if there is only one alien left then the right value is 3 instead of the
; usual 2. The left direction is always -2

RackBump           LD    A,(RackDirection)       ; Get rack direction
                   AND   A                       ; Moving right?
                   JP    NZ,$15B7                ; No ... handle moving left

                   LD    HL,$3EA4                ; Line down the right edge of play field
                   CALL  $15C5                   ; Check line down the edge
                   RET   NC                      ; Nothing is there ... return
                   LD    B,$FE                   ; Delta X of -2
                   LD    A,$01                   ; Rack now moving right

                   LD    (RackDirection),A       ; Set new rack direction
                   LD    A,B                     ; B has delta X
                   LD    (RefAlienDXr),A         ; Set new delta X
                   LD    A,(RackDownDelta)       ; Set delta Y ...
                   LD    (RefAlienDYr),A         ; ... to drop rack by 8
                   RET                           ; Done

                   LD    HL,$2524                ; Line down the left edge of play field
                   CALL  $15C5                   ; Check line down the edge
                   RET   NC                      ; Nothing is there ... return
                   CALL  $18F1                   ; Get moving-right delta X value of 2 (3 if just one alien left)
                   XOR   A                       ; Rack now moving left
                   JP    $15A9                   ; Set rack direction

                   LD    B,$17                   ; Checking 23 bytes in a line up the screen from near the bottom
                   LD    A,(HL)                  ; Get screen memory
                   AND   A                       ; Is screen memory empty?
                   JP    NZ,$166B                ; No ... set carry flag and out
                   INC   HL                      ; Next byte on screen
                   DEC   B                       ; All column done?
                   JP    NZ,$15C7                ; No ... keep looking
                   RET                           ; Return with carry flag clear

                   NOP

; Draw sprite at [DE] to screen at pixel position in HL
; The hardware shift register is used in converting pixel positions
; to screen coordinates

DrawSprite         CALL  CnvtPixNumber           ; Convert pixel number to screen/shift
                   PUSH  HL                      ; Preserve screen coordinate
                   PUSH  BC                      ; Hold for a second
                   PUSH  HL                      ; Hold for a second
                   LD    A,(DE)                  ; From sprite data
                   OUT   (SHFT_DATA),A           ; Write data to shift register
                   IN    A,(SHFT_IN)             ; Read back shifted amount
                   LD    (HL),A                  ; Shifted sprite to screen
                   INC   HL                      ; Adjacent cell
                   INC   DE                      ; Next in sprite data
                   XOR   A                       ; 0
                   OUT   (SHFT_DATA),A           ; Write 0 to shift register
                   IN    A,(SHFT_IN)             ; Read back remainder of previous
                   LD    (HL),A                  ; Write remainder to adjacent
                   POP   HL                      ; Old screen coordinate
                   LD    BC,32                   ; Offset screen by 32 ...
                   ADD   HL,BC                   ; ... to next row
                   POP   BC                      ; Restore count
                   DEC   B                       ; All done?
                   JP    NZ,$15D7                ; No ... do all
                   POP   HL                      ; Restore HL
                   RET                           ; Done

; Count number of aliens remaining in active game and return count 2082 holds the current count.
; If only 1, 206B gets a flag of 1 ** but ever nobody checks this

CountAliens        CALL  GetPlayerDataPtr        ; Get active player descriptor
                   LD    BC,$3700                ; B=55 aliens to check?
                   LD    A,(HL)                  ; Get byte
                   AND   A                       ; Is it a zero?
                   JP    Z,$15FF                 ; Yes ... don't count it
                   INC   C                       ; Count the live aliens
                   INC   HL                      ; Next alien
                   DEC   B                       ; Count ...
                   JP    NZ,$15F9                ; ... all alien indicators
                   LD    A,C                     ; Get the count
                   LD    (NumAliens),A           ; Hold it
                   CP    $01                     ; Just one?
                   RET   NZ                      ; No keep going
                   LD    HL,OneAlien             ; Set flag if ...
                   LD    (HL),$01                ; ... only one alien left
                   RET                           ; Done

; Set HL with 2100 if player 1 is active or 2200 if player 2 is active

GetPlayerDataPtr   LD    L,$00                   ; Byte boundary
                   LD    A,(PlayerDataMSB)       ; Active player number
                   LD    H,A                     ; Set HL to data
                   RET                           ; Done

; Initiate player fire if button is pressed
; Demo commands are parsed here if in demo mode

PlrFireOrDemo      LD    A,(PlayerAlive)         ; Is there an active player?
                   CP    $FF                     ; FF = alive
                   RET   NZ                      ; Player has been shot - no firing
                   LD    HL,Obj0Timer            ; Get player ...
                   LD    A,(HL)                  ; ... task ...
                   INC   HL                      ; ... timer ...
                   LD    B,(HL)                  ; ... value
                   OR    B                       ; Is the timer 0 (object active)?
                   RET   NZ                      ; No ... no firing till player object starts
                   LD    A,(PlyrShotStatus)      ; Does the player have ...
                   AND   A                       ; ... a shot on the screen?
                   RET   NZ                      ; Yes ... ignore
                   LD    A,(GameMode)            ; Are we in ...
                   AND   A                       ; ... game mode?
                   JP    Z,$1652                 ; No ... in demo mode ... constant firing in demo
                   LD    A,(FireBounce)          ; Is fire button ...
                   AND   A                       ; ... being held down?
                   JP    NZ,$1648                ; Yes ... wait for bounce
                   CALL  ReadInputs              ; Read active player controls
                   AND   $10                     ; Fire-button pressed?
                   RET   Z                       ; No ... out
                   LD    A,$01                   ; Flag
                   LD    (PlyrShotStatus),A      ; Flag shot active
                   LD    (FireBounce),A          ; Flag that fire button is down
                   RET                           ; Done

                   CALL  ReadInputs              ; Read active player controls
                   AND   $10                     ; Fire-button pressed?
                   RET   NZ                      ; Yes ... ignore
                   LD    (FireBounce),A          ; Else ... clear flag
                   RET                           ; Done

; Handle demo (constant fire, parse demo commands)

                   LD    HL,PlyrShotStatus       ; Demo fires ...
                   LD    (HL),$01                ; ... constantly
                   LD    HL,(DemoCmdPtr)         ; Demo command buffer
                   INC   HL                      ; Next position
                   LD    A,L                     ; Command buffer ...
                   CP    $7E                     ; ... wraps around
                   JP    C,$1663                 ; ... Buffer from 1F74 to 1F7E
                   LD    L,$74                   ; ... overflow
                   LD    (DemoCmdPtr),HL         ; Next demo command
                   LD    A,(HL)                  ; Get next command
                   LD    (NextDemoCmd),A         ; Set command for movement
                   RET                           ; Done

                   SCF                           ; Set carry flag
                   RET                           ; Done

                   XOR   A                       ; 0
                   CALL  $1A8B                   ; Print ZERO ships remain
                   CALL  CurPlyAlive             ; Get active-flag ptr for current player
                   LD    (HL),$00                ; Flag player is dead
                   CALL  $09CA                   ; Get score descriptor for current player
                   INC   HL                      ; Point to high two digits
                   LD    DE,HiScore + 1          ; Current high score upper two digits
                   LD    A,(DE)                  ; Is player score greater ...
                   CP    (HL)                    ; ... than high score?
                   DEC   DE                      ; Point to LSB
                   DEC   HL                      ; Point to LSB
                   LD    A,(DE)                  ; Go ahead and fetch high score lower two digits
                   JP    Z,$168B                 ; Upper two are the same ... have to check lower two
                   JP    NC,$1698                ; Player score is lower than high ... nothing to do
                   JP    $168F                   ; Player score is higher ... go copy the new high score

                   CP    (HL)                    ; Is lower digit higher? (upper was the same)
                   JP    NC,$1698                ; No ... high score is still greater than player's score
                   LD    A,(HL)                  ; Copy the new ...
                   LD    (DE),A                  ; ... high score lower two digits
                   INC   DE                      ; Point to MSB
                   INC   HL                      ; Point to MSB
                   LD    A,(HL)                  ; Copy the new ...
                   LD    (DE),A                  ; ... high score upper two digits
                   CALL  PrintHiScore            ; Draw the new high score
                   LD    A,(TwoPlayers)          ; Number of players
                   AND   A                       ; Is this a single player game?
                   JP    Z,$16C9                 ; Yes ... short message
                   LD    HL,$2803                ; Screen coordinates
                   LD    DE,Msg_GameOver         ; Point to string 'GAME OVER PLAYER< >'
                   LD    C,20                    ; 20 characters
                   CALL  PrintMessageDel         ; Print message
                   DEC   H                       ; Back up ...
                   DEC   H                       ; ... to player indicator
                   LD    B,$1B                   ; '1'
                   LD    A,(PlayerDataMSB)       ; Player number
                   RRCA                          ; Is this player 1?
                   JP    C,$16B7                 ; Yes ... keep the digit
                   LD    B,$001C                 ; Else ... set digit 2
                   LD    A,B                     ; To A
                   CALL  DrawChar                ; Print player number
                   CALL  OneSecDelay             ; Short delay
                   CALL  $18E7                   ; Get current player 'alive' flag
                   LD    A,(HL)                  ; Is player ...
                   AND   A                       ; ... alive?
                   JP    Z,$16C9                 ; No ... skip to 'GAME OVER' sequence
                   JP    $02ED                   ; Switch players and game loop

                   LD    HL,$2D18                ; Screen coordinates
                   LD    DE,Msg_GameOver         ; Point to string 'GAME OVER PLAYER< >'
                   LD    C,10                    ; Just the 'GAME OVER' part
                   CALL  PrintMessageDel         ; Print message
                   CALL  TwoSecDelay             ; Long delay
                   CALL  ClearPlayField          ; Clear centre window
                   XOR   A                       ; Now in ...
                   LD    (GameMode),A            ; ... demo mode
                   OUT   (SOUND2),A              ; All sound off
                   CALL  EnableGameTasks         ; Enable ISR game tasks
                   JP    $0B89                   ; Print credit information and do splash

                   LD    SP,STACK_TOP            ; Reset stack
                   EI                            ; Enable interrupts
                   XOR   A                       ; Flag ...
                   LD    (PlayerAlive),A         ; ... player is shot
                   CALL  PlayerShotHit           ; Player's shot collision detection
                   LD    B,$04                   ; Player has been hit ...
                   CALL  SoundBits3On            ; ... sound
                   CALL  $0A59                   ; Has flag been set?
                   JP    NZ,$16EE                ; No ... wait for the flag
                   CALL  DisableGameTasks        ; Disable ISR game tasks
                   LD    HL,$2701                ; Player's stash of ships
                   CALL  $19FA                   ; Erase the stash of shps
                   XOR   A                       ; Print ...
                   CALL  $1A8B                   ; ... a zero (number of ships)
                   LD    B,$FB                   ; Turn off ...
                   JP    $196B                   ; ... player shot sound

; Use the player's MSB to determine how fast the aliens reload their
; shots for another fire

UpdateReloadRate   CALL  $09CA                   ; Get score descriptor for active player
                   INC   HL                      ; MSB value
                   LD    A,(HL)                  ; Get the MSB value
                   LD    DE,$1CB8                ; Score MSB table
                   LD    HL,ShotReloadRates      ; Corresponding fire reload rate table
                   LD    C,4                     ; Only 4 entries (a 5th value of 7 is used after that)
                   LD    B,A                     ; Hold the score value
                   LD    A,(DE)                  ; Get lookup from table
                   CP    B                       ; Compare them
                   JP    NC,$1727                ; Equal or below ... use this table entry
                   INC   HL                      ; Next ...
                   INC   DE                      ; ... entry in table
                   DEC   C                       ; Do all ...
                   JP    NZ,$171C                ; ... 4 entries in the tables
                   LD    A,(HL)                  ; Load the shot reload value
                   LD    (AlienShotRelRate),A    ; Save the value for use in shot routine
                   RET                           ; Done

; Shot sound on or off depending on 2025

ShotSound          LD    A,(PlyrShotStatus)      ; Player shot flag
                   CP    $00                     ; Active shot?
                   JP    NZ,$1739                ; Yes ... go
                   LD    B,$FD                   ; Sound mask
                   JP    SoundBits3Off           ; Mask off sound

                   LD    B,$02                   ; Sound bit
                   JP    SoundBits3On            ; OR on sound

                   NOP
                   NOP

; This called from the ISR times down the fleet and sets the flag at 2095 if
; the fleet needs a change in sound handling (new delay, new sound)

TimeFleetSound     LD    HL,FleetSndHold         ; Pointer to hold time for fleet
                   DEC   (HL)                    ; Decrement hold time
                   CALL  Z,$176D                 ; If 0 turn fleet movement sound off
                   LD    A,(PlayerOK)            ; Is player OK?
                   AND   A                       ; 1  means OK
                   JP    Z,$176D                 ; Player not OK ... fleet movement sound off and out
                   LD    HL,FleetSndCnt          ; Current time on fleet sound
                   DEC   (HL)                    ; Count down
                   RET   NZ                      ; Not time to change sound ... out
                   LD    HL,SoundPort5           ; Current sound port 3 value (CHECK)
                   LD    A,(HL)                  ; Get value
                   OUT   (SOUND2),A              ; Set sounds
                   LD    A,(NumAliens)           ; Number of aliens on active screen
                   AND   A                       ; Is it zero?
                   JP    Z,$176D                 ; Yes ... turn off fleet movement sound and out
                   DEC   HL                      ; (2097) Point to fleet timer reload
                   LD    A,(HL)                  ; Get fleet delay value
                   DEC   HL                      ; (2096) Point to fleet timer
                   LD    (HL),A                  ; Reload the timer
                   DEC   HL                      ; Point to change-sound
                   LD    (HL),$01                ; (2095) time to change sound
                   LD    A,$04                   ; Set hold ...
                   LD    (FleetSndHold),A        ; ... time for fleet sound
                   RET                           ; Done

                   LD    A,(SoundPort5)          ; Current sound port 3 value
                   AND   $30                     ; Mask off fleet movement sounds
                   OUT   (SOUND2),A              ; Set sounds
                   RET                           ; Done

; This game-loop routine handles two sound functions. The routine does:
; 1) Time out the extra-ship awarded sound and turn it off when done
; 2) Load the fleet sound delay based on number of remaining aliens
; 3) Make the changing fleet sound

; The 2095 flag is set by the ISR and cleared here. The ISR does the timing and sets 2095 when it
; is time to make a new fleet sound

FleetDelayExShip   LD    A,(ChangeFleetSnd)      ; Time for new ...
                   AND   A                       ; ... fleet movement sound?
                   JP    Z,$17AA                 ; No ... skip to extra-man timing
                   LD    HL,$1A11                ; Number of aliens list coupled ...
                   LD    DE,$1A21                ; ... with delay list
                   LD    A,(NumAliens)           ; Get the number of aliens on the screen
                   CP    (HL)                    ; Compare it to the first list value
                   JP    NC,$178E                ; Number of live aliens is higher than value ... use the delay
                   INC   HL                      ; Move to ...
                   INC   DE                      ; ... next list value
                   JP    $1785                   ; Find the right delay
                   LD    A,(DE)                  ; Get the delay from the second list
                   LD    (FleetSndReload),A      ; Store the new alien sound delay
                   LD    HL,SoundPort5           ; Get current state ...
                   LD    A,(HL)                  ; ... of sound port
                   AND   $30                     ; Mask off all fleet movement sounds
                   LD    B,A                     ; Hold the value
                   LD    A,(HL)                  ; Get current state
                   AND   $0F                     ; This time ONLY the fleet movement sounds
                   RLCA                          ; Shift next to next sound
                   CP    $10                     ; Overflow?
                   JP    NZ,$17A4                ; No ... keep it
                   LD    A,$01                   ; Reset back to first sound
                   OR    B                       ; Add fleet sounds to current sound value
                   LD    (HL),A                  ; Store new sound value
                   XOR   A                       ; Restart ...
                   LD    (ChangeFleetSnd),A      ; ... waiting on fleet time

                   LD    HL,ExtraHold            ; Sound timer for award extra ship
                   DEC   (HL)                    ; Time expired?
                   RET   NZ                      ; No ... leave sound playing
                   LD    B,$EF                   ; Turn off bit set with #$10 (award extra ship)
                   JP    SoundBits3Off           ; Stop sound and out

SndOffExtPly       LD    B,$EF                   ; Mask off sound bit 4 (Extended play)
                   LD    HL,SoundPort5           ; Current sound content
                   LD    A,(HL)                  ; Get current sound bits
                   AND   B                       ; Turn off extended play
                   LD    (HL),A                  ; Remember settings
                   OUT   (SOUND2),A              ; Turn off extended play
                   RET                           ; Done

                   NOP

; Read control inputs for active player

ReadInputs         LD    A,(PlayerDataMSB)       ; Get active player
                   RRCA                          ; Test player
                   JP    NC,$17CA                ; Player 2 ... read port 2
                   IN    A,(INP1)                ; Player 1 ... read port 1
                   RET                           ; Done

                   IN    A,(INP2)                ; Get controls for player 2
                   RET                           ; Done

; Check and handle TILT

CheckHandleTilt    IN    A,(INP2)                ; Read input port
                   AND   $04                     ; Tilt?
                   RET   Z                       ; No tilt ... return
                   LD    A,(Tilt)                ; Already in TILT handle?
                   AND   A                       ; 1 = yes
                   RET   NZ                      ; Yes ... ignore it now
                   LD    SP,STACK_TOP            ; Reset stack
                   LD    B,$04                   ; Do this 4 times
                   CALL  ClearPlayField          ; Clear centre window
                   DEC   B                       ; All done?
                   JP    NZ,$17DC                ; No ... do again
                   LD    A,$01                   ; Flag ...
                   LD    (Tilt),A                ; ... handling TILT
                   CALL  DisableGameTasks        ; Disable game tasks
                   EI                            ; Re-enable interrupts
                   LD    DE,Msg_Tilt             ; Pointer to string 'TILT'
                   LD    HL,$3016                ; Centre of screen
                   LD    C,4                     ; Length of message
                   CALL  PrintMessageDel         ; Print 'TILT'
                   CALL  OneSecDelay             ; Short delay
                   XOR   A                       ; Zero
                   LD    (Tilt),A                ; TILT handle over
                   LD    (WaitStartLoop),A       ; Back into splash screens
                   JP    $16C9                   ; Handle game over for player

CtrlSaucerSound    LD    HL,SaucerActive         ; Saucer on screen flag
                   LD    A,(HL)                  ; Is the saucer ...
                   AND   A                       ; ... on the screen?
                   JP    Z,$0707                 ; No ... UFO sound off
                   INC   HL                      ; Saucer hit flag
                   LD    A,(HL)                  ; (2085) Get saucer hit flag
                   AND   A                       ; Is saucer in 'hit' sequence?
                   RET   NZ                      ; Yes ... out
                   LD    B,$01                   ; Retrigger saucer ...
                   JP    SoundBits3On            ; ... sound (retrigger makes it warble?)

; Draw 'SCORE ADVANCE TABLE'

DrawAdvTable       LD    HL,$2810                ; 0x410 is 1040 rotCol=32, rotRow=16
                   LD    DE,Msg_ScoreAdvance     ; Pointer to string '*SCORE ADVANCE TABLE*'
                   LD    C,21                    ; Length of message
                   CALL  PrintMessage            ; Print message
                   LD    A,$0A                   ; 10 bytes in every '=xx POINTS' string
                   LD    (Temp206C),A            ; Hold the count
                   LD    BC,ScoreAdvanceTable    ; Coordinate/sprite for drawing table
                   CALL  ReadPriStruct           ; Get HL=coordinate, DE=image
                   JP    C,$1837                 ; Move on if done
                   CALL  $1844                   ; Draw 16-byte sprite
                   JP    $1828                   ; Do all in table

                   CALL  OneSecDelay             ; One second delay
                   LD    BC,$1DCF                ; Coordinate/message for drawing table
                   CALL  ReadPriStruct           ; Get HL=coordinate, DE=message
                   RET   C                       ; Out if done
                   CALL  $184C                   ; Print message
                   JP    $183A                   ; Do all in table

                   PUSH  BC                      ; Hold BC
                   LD    B,16                    ; 16 bytes
                   CALL  DrawSimpleSprite        ; Draw simple
                   POP   BC                      ; Restore BC
                   RET                           ; Done

                   PUSH  BC                      ; Hold BC
                   LD    A,(Temp206C)            ; Count of 10 ...
                   LD    C,A                     ; ... to C
                   CALL  PrintMessageDel         ; Print the message with delay between letters
                   POP   BC                      ; Restore BC
                   RET                           ; Done

; Read a 4-byte print-structure pointed to by BC
; HL=Screen coordiante, DE=pointer to message
; If the first byte is FF then return with C=1

ReadPriStruct      LD    A,(BC)                  ; Get the screen LSB
                   CP    $FF                     ; Valid?
                   SCF                           ; If not C will be 1
                   RET   Z                       ; Return if 255
                   LD    L,A                     ; Screen LSB to L
                   INC   BC                      ; Next
                   LD    A,(BC)                  ; Read screen MSB
                   LD    H,A                     ; Screen MSB to H
                   INC   BC                      ; Next
                   LD    A,(BC)                  ; Read message LSB
                   LD    E,A                     ; Message LSB to E
                   INC   BC                      ; Next
                   LD    A,(BC)                  ; Read message MSB
                   LD    D,A                     ; Message MSB to D
                   INC   BC                      ; Next (for next print)
                   AND   A                       ; Clear C
                   RET                           ; Done

; Moves a sprite up or down in splash mode. Interrupt moves the sprite. When it reaches
; Y value in 20CA the flag at 20CB is raised. The image flips between two pictures every
; 4 movements

SplashSprite       LD    HL,SplashAnForm         ; Descriptor
                   INC   (HL)                    ; Change image
                   INC   HL                      ; Point to delta-x
                   LD    C,(HL)                  ; Get delta-x
                   CALL  AddDelta                ; Add delta-X and delta-Y to X and Y
                   LD    B,A                     ; Current y coordinate
                   LD    A,(SplashTargetY)       ; Has sprite reached ...
                   CP    B                       ; ... target coordinate?
                   JP    Z,$1898                 ; Yes ... flag and out
                   LD    A,(SplashAnForm)        ; Image number
                   AND   $04                     ; Watching bit 3 for flip delay
                   LD    HL,(SplashImageRest)    ; Image
                   JP    NZ,$1888                ; Did bit 3 go to 0? No ... keep current image
                   LD    DE,$0030                ; 16*3 ...
                   ADD   HL,DE                   ; ...  use other image form
                   LD    (SplashImage),HL        ; Image to descriptor structure
                   LD    HL,SplashYr             ; X,Y,Image descriptor
                   CALL  ReadDesc                ; Read sprite descriptor
                   EX    DE,HL                   ; Image to DE, position to HL
                   JP    DrawSprite              ; Draw the sprite

                   NOP
                   NOP
                   NOP

                   LD    A,$01                   ; Flag that sprite ...
                   LD    (SplashReached),A       ; ... reached location
                   RET                           ; Done

; Animate alien shot to extra 'C' in splash

                   LD    HL,Obj4Timer            ; Task descriptor for game object 4 (squiggly shot)
                   LD    DE,$1BC0                ; Task info for animate-shot-to-extra-C
                   LD    B,16                    ; Block copy ...
                   CALL  BlockCopy               ; ... 16 bytes
                   LD    A,$02                   ; Set shot sync ...
                   LD    (ShotSync),A            ; ... to run the squiggly shot
                   LD    A,$FF                   ; Shot direction (-1)
                   LD    (AlienShotDelta),A      ; Alien shot delta
                   LD    A,$04                   ; Animate ...
                   LD    (IsrSplashTask),A       ; ... shot
                   LD    A,(SquShotStatus)       ; Has shot ...
                   AND   $01                     ; ... collided?
                   JP    Z,$18B8                 ; No ... keep waiting
                   LD    A,(SquShotStatus)       ; Wait ...
                   AND   $01                     ; ... for explosion ...
                   JP    NZ,$18C0                ; ... to finish
                   LD    HL,$3311                ; Here is where the extra C is
                   LD    A,$26                   ; Space character

                   NOP

                   CALL  DrawChar                ; Draw character
                   JP    TwoSecDelay             ; Two second delay and out

;===============================================================================
; Initialisation comes here
;===============================================================================

Initialise         LD    SP,STACK_TOP            ; Set stack pointer just below screen
                   LD    B,$00                   ; Count 256 bytes
                   CALL  $01E6                   ; Copy ROM to RAM
                   CALL  DrawStatus              ; Print scores and credits

                   LD    A,$08                   ; Set alien ...
                   LD    (AlienShotRelRate),A    ; ... shot reload rate
                   JP    $0AEA                   ; Top of splash screen loop

; Get player-alive flag for OTHER player

                   LD    A,(PlayerDataMSB)       ; Player data MSB
                   LD    HL,Player1Alive         ; Alive flags (player 1 and 2)
                   RRCA                          ; Bit 1=1 for player 1
                   RET   NC                      ; Player 2 ... we have it ... out
                   INC   HL                      ; Player 1's flag
                   RET                           ; Done

; If there is one alien left then the right motion is 3 instead of 2. That's
; why the timing is hard to hit after the change

                   LD    B,$02                   ; Rack moving right delta X
                   LD    A,(NumAliens)           ; Number of aliens on screen
                   DEC   A                       ; Just one left?
                   RET   NZ                      ; No ... use right delta X of 2
                   INC   B                       ; Just one alien ... move right at 3 instead of 2
                   RET                           ; Done

; Add in bit for sound

SoundBits3On       LD    A,(SoundPort3)          ; Current value of sound port
                   OR    B                       ; Add in new sounds
                   LD    (SoundPort3),A          ; New value of sound port
                   OUT   (SOUND1),A              ; Write new value to sound hardware
                   RET                           ; Done

InitAliensP2       LD    HL,$2200                ; Player 2 data area
                   JP    $01C3                   ; Initialise player 2 aliens

PlyrShotAndBump    CALL  PlayerShotHit           ; Player's shot collision detection
                   JP    RackBump                ; Change alien deltaX and deltaY when rack bumps edges

; Get the current player's alive status

CurPlyAlive        LD    HL,Player1Alive         ; Alive flags
                   LD    A,(PlayerDataMSB)       ; Player 1 or 2
                   RRCA                          ; Will be 1 if player 1
                   RET   C                       ; Return if player 1
                   INC   HL                      ; Bump to player 2
                   RET                           ; Done

; Print score header ' SCORE<1> HI-SCORE SCORE<2> '

DrawScoreHead      LD    C,28                    ; Length of message
                   LD    HL,$241E                ; Screen coordinates
                   LD    DE,Msg_ScoreHeading     ; Score header message
                   JP    PrintMessage            ; Print score header

                   LD    HL,P1Score              ; Player 1 score descriptor
                   JP    DrawScore               ; Print score

                   LD    HL,P2Score              ; Player 2 score descriptor
                   JP    DrawScore               ; Print score

; Print score
; HL = descriptor

DrawScore          LD    E,(HL)                  ; Get score LSB
                   INC   HL                      ; Next
                   LD    D,(HL)                  ; Get score MSB
                   INC   HL                      ; Next
                   LD    A,(HL)                  ; Get coordinate LSB
                   INC   HL                      ; Next
                   LD    H,(HL)                  ; Get coordinate MSB
                   LD    L,A                     ; Set LSB
                   JP    Print4Digits            ; Print 4 digits in DE

; Print message 'CREDIT '

PrintCredit        LD    C,7                     ; Length of message
                   LD    HL,$3501                ; Screen coordinates
                   LD    DE,Msg_Credit           ; Pointer to string 'CREDIT '
                   JP    PrintMessage            ; Print message

; Display number of credits on screen

DrawNumCredits     LD    A,(NumCoins)            ; Number of credits
                   LD    HL,$3C01                ; Screen coordinates
                   JP    DrawHexByte             ; Character to screen

PrintHiScore       LD    HL,HiScore              ; Hi Score descriptor
                   JP    DrawScore               ; Print Hi-Score

; Print scores (with header) and credits (with label)

DrawStatus         CALL  ClearScreen             ; Clear the screen
                   CALL  DrawScoreHead           ; Print score header
                   CALL  $1925                   ; Print player 1 score
                   CALL  $192B                   ; Print player 2 score
                   CALL  PrintHiScore            ; Print hi score
                   CALL  PrintCredit             ; Print message 'CREDIT'
                   JP    DrawNumCredits          ; Number of credits

                   CALL  SoundBits3Off           ; From 170B with B=FB. Turn off player shot sound
                   JP    $1671                   ; Update high-score if player's score is greater

                   LD    A,$01                   ; Set flag that ...
                   LD    (Invaded),A             ; ... aliens reached bottom of screen
                   JP    $16E6                   ; End of round

                   CALL  DisableGameTasks        ; Disable ISR game tasks
                   CALL  DrawNumCredits          ; Display number of credits on screen
                   JP    PrintCredit             ; Print message 'CREDIT'

                   LD    (IsrSplashTask),A       ; Set ISR splash task
                   RET                           ; Done

; The original code (from TAITO) printed '*TAITO CORPORATION*' on the screen.
; When Midway branched the code they changed the code above (overwrote with RET)
; so it isn't printed

                   DW    $198B                   ; Remainder of 'JP $198B' after JP byte overwritten by RET above
                   JP    ClearPlayField          ; Clear play field and out
                   LD    HL,$2803                ; Screen coordinates
                   LD    DE,Msg_TaitoCorp        ; Pointer to string '*TAITO CORPORATION*'
                   LD    C,19                    ; Message length
                   JP    PrintMessage            ; Print message

; The original TAITO code ($1985 - $1995):
;                   JP    $198B
;                   CALL  ClearPlayField
;                   LD    HL,$2803
;                   LD    DE,Msg_TaitoCorp
;                   LD    C,19
;                   JP    PrintMessage

                    NOP
                    NOP
                    NOP
                    NOP

; There is a hidden message 'TAITO COP' (with no 'R') in the game. It can only be
; displayed in the demonstration game during the splash screens. You must enter
; two seqences of buttons. Timing is not critical. As long as you eventually get
; all the buttons up/down in the correct pattern then the game will register the
; sequence

; 1st: 2start(down) 1start(up)   1fire(down) 1left(down) 1right(down)
; 2nd: 2start(up)   1start(down) 1fire(down) 1left(down) 1right(up)

; Unfortunately MAME does not deliver the simultaneous button presses correctly.
; You can see the message in MAME by changing 19A6 to 02 and 19B1 to 02.
; Then the 2start(down) is the only sequence

CheckHiddenMes     LD    A,(HidMessSeq)          ; Has the 1st 'hidden-message' sequence ...
                   AND   A                       ; ... been registered?
                   JP    NZ,$19AC                ; Yes ... go look for the 2nd sequence
                   IN    A,(INP1)                ; Get player inputs
                   AND   $76                     ; 0111_0110 Keep 2Pstart, 1Pstart, 1Pshot, 1Pleft, 1Pright
                   SUB   $72                     ; 0111_0010 1st sequence: 2Pstart, 1Pshot, 1Pleft, 1Pright
                   RET   NZ                      ; Not first sequence ... out
                   INC   A                       ; Flag that 1st sequence ...
                   LD    (HidMessSeq),A          ; ... has been entered
                   IN    A,(INP1)                ; Check inputs for 2nd sequence
                   AND   $76                     ; 0111_0110 Keep 2Pstart, 1Pstart, 1Pshot, 1Pleft, 1Pright
                   CP    $34                     ; 0011_0100 2nd sequence: 1Pstart, 1Pshot, 1Pleft
                   RET   NZ                      ; If not second sequence ignore
                   LD    HL,$2E1B                ; Screen coordinates
                   LD    DE,Msg_TaitoCop         ; Pointer to string 'TAITO COP' (no R)
                   LD    C,9                     ; Message length
                   JP    PrintMessage            ; Print message and out

Msg_TaitoCorp      DB    $28, $13, $00, $08      ; '*TAITO CORPORATION*'
                   DB    $13, $0E, $26, $02
                   DB    $0E, $11, $0F, $0E
                   DB    $11, $00, $13, $08
                   DB    $0E, $0D, $28

; Enable ISR game tasks

EnableGameTasks    LD    A,$01                   ; Set ISR ...
                   LD    (SuspendPlay),A         ; ... game tasks enabled
                   RET                           ; Done

; Disable ISR game tasks
; Clear 20E9 flag

DisableGameTasks   XOR   A                       ; Clear ISR game tasks flag
                   JP    $19D3                   ; Save a byte (the RET)
                   DB    00                      ; ** Here is the byte saved. I wonder if this was an optimiser pass

; Turn off bit in sound port

SoundBits3Off      LD    A,(SoundPort3)          ; Current sound effects value
                   AND   B                       ; Mask bits off
                   LD    (SoundPort3),A          ; Store new hold value
                   OUT   (SOUND1),A              ; Change sounds
                   RET                           ; Done

; Show ships remaining in hold for the player

DrawNumShips       LD    HL,$2701                ; Screen coordinates
                   JP    Z,ClearRemainderLine    ; None in reserve ... skip display

; Draw line of ships

DrawLineOfShips    LD    DE,PlayerSprite         ; Player sprite
                   LD    B,16                    ; 16 rows
                   LD    C,A                     ; Hold count
                   CALL  DrawSimpleSprite        ; Display 1-byte sprite to screen
                   LD    A,C                     ; Restore remaining
                   DEC   A                       ; All done?
                   JP    NZ,DrawLineOfShips      ; No ... keep going

; Clear remainder of line

ClearRemainderLine LD    B,16                    ; 16 rows
                   CALL  ClearSmallSprite        ; Clear 1byte sprite at HL
                   LD    A,H                     ; Get Y coordinate
                   CP    $35                     ; At edge?
                   JP    NZ,ClearRemainderLine   ; No ... do all
                   RET                           ; Done

; The ISRs set the upper bit of 2072 based on where the beam is. This is compared
; to the upper bit of an object's Y coordinate to decide which ISR should handle
; it. When the beam passes the halfway point (or near it ... at scanline 96),
; the upper bit is cleared. When the beam reaches the end of the screen the upper
; bit is set

; The task then runs in the ISR if the Y coordinate bit matches the 2072 flag.
; Objects that are at the top of the screen (upper bit of Y clear) run in the
; mid-screen ISR when the beam has moved to the bottom of the screen. Objects
; that are at the bottom of the screen (upper bit of Y set) run in the end-screen
; ISR when the beam is moving back to the top

; The pointer to the object's Y coordinate is passed in DE. CF is set if the
; upper bits are the same (the calling ISR should execute the task)

CompYToBeam        LD    HL,VBlankStatus         ; Get the ...
                   LD    B,(HL)                  ; ... beam position status
                   LD    A,(DE)                  ; Get the task structure flag
                   AND   $80                     ; Only upper bits count
                   XOR   B                       ; XOR them together
                   RET   NZ                      ; Not the same (CF cleared)
                   SCF                           ; Set the CF if the same
                   RET                           ; Done

; Alien delay lists. First list is the number of aliens. The second list is the
; corresponding delay. This delay is only for the rate of change in the fleet's
; sound. The check takes the first num-aliens-value that is lower or the same as
; the actual num-aliens on screen

; The game starts with 55 aliens. The aliens are move/drawn one per interrupt
; which means it takes 55 interrupts. The first delay value is 52 ... which is
; almost in sync with the number of aliens. It is a tad faster and you can
; observe the sound and steps getting out of sync

                   DB    $32, $2B, $24, $1C, $16, $11, $0D, $0A
                   DB    $08, $07, $06, $05, $04, $03, $02, $01
                   DB    $34, $2E, $27, $22, $1C, $18, $15, $13
                   DB    $10, $0E, $0D, $0C, $0B, $09, $07, $05
                   DB    $FF                     ; ** Needless terminator. The list value '1' catches everything

; Copy from [DE] to [HL], B is number of bytes

BlockCopy          LD    A,(DE)                  ; Copy from [DE] to ...
                   LD    (HL),A                  ; ... [HL]
                   INC   HL                      ; Next destination
                   INC   DE                      ; Next source
                   DEC   B                       ; Count in B
                   JP    NZ,BlockCopy            ; Do all
                   RET                           ; Done

; Load 5 bytes sprite descriptor from [HL]

ReadDesc           LD    E,(HL)                  ; Descriptor ...
                   INC   HL                      ; ... sprite ...
                   LD    D,(HL)                  ; ...
                   INC   HL                      ; ... picture
                   LD    A,(HL)                  ; Descriptor ...
                   INC   HL                      ; ... screen ...
                   LD    C,(HL)                  ; ...
                   INC   HL                      ; ... location
                   LD    B,(HL)                  ; Number of bytes in sprite
                   LD    H,C                     ; From A,C to ...
                   LD    L,A                     ; ... H,L
                   RET                           ; Done

; The screen is organised as one-bit-per-pixel
; In: HL contains pixel number (bbbbbbbbbbbbbppp)
; Convert from pixel number to screen coordinates (without shift)
; Shift HL right 3 bits (clearing the top 2 bits) and set the third bit from the left

ConvToScr          PUSH  BC                      ; Hold B (will mangle)
                   LD    B,3                     ; 3 shifts (divide by 8)
ConvToScr1         LD    A,H                     ; H to A
                   RRA                           ; Shift right (into carry, from doesn't matter)
                   LD    H,A                     ; Back to H
                   LD    A,L                     ; L to A
                   RRA                           ; Shift right (from/to carry)
                   LD    L,A                     ; Back to L
                   DEC   B                       ; Do all ...
                   JP    NZ,ConvToScr1           ; ... 3 shifts
                   LD    A,H                     ; H to A
                   AND   $3F                     ; Mask off all but screen (less than or equal 3F)
                   OR    $20                     ; Offset into RAM
                   LD    H,A                     ; Back to H
                   POP   BC                      ; Restore B
                   RET                           ; Done

; Clear the screen
; Thanks to Mark Tankard for pointing out what this really does

ClearScreen        LD    HL,$2400                ; Screen coordinate
ClearScreen1       LD    (HL),$00                ; Clear it
                   INC   HL                      ; Next byte
                   LD    A,H                     ; Have we done ...
                   CP    $40                     ; ... all the screen?
                   JP    NZ,ClearScreen1         ; No ... keep going
                   RET                           ; Done

; Logically OR the player's shields back onto the play field
; DE = sprite, HL = screen, C = bytes per row, B = number of rows

RestoreShields     PUSH  BC                      ; Preserve BC
                   PUSH  HL                      ; Hold for a bit
RestoreShields1    LD    A,(DE)                  ; From sprite
                   OR    (HL)                    ; OR with screen
                   LD    (HL),A                  ; Back to screen
                   INC   DE                      ; Next sprite
                   INC   HL                      ; Next on screen
                   DEC   C                       ; Row done?
                   JP    NZ,RestoreShields1      ; No ... do entire row
                   POP   HL                      ; Original start
                   LD    BC,32                   ; Bump HL by 32 ...
                   ADD   HL,BC                   ; ... one screen row
                   POP   BC                      ; Restore
                   DEC   B                       ; Row counter
                   JP    NZ,RestoreShields       ; Do all rows
                   RET                           ; Done

; Remove a ship from the players stash and update the hold indicators on the screen

RemoveShip         CALL  $092E                   ; Get last byte from player data
                   AND   A                       ; Is it 0?
                   RET   Z                       ; Skip
                   PUSH  AF                      ; Preserve number remaining
                   DEC   A                       ; Remove a ship from the stash
                   LD    (HL),A                  ; New number of ships
                   CALL  DrawNumShips            ; Draw the line of ships
                   POP   AF                      ; Restore number
                   LD    HL,$2501                ; Screen coordinates
                   AND   $0F                     ; Make sure it is a digit
                   JP    $09C5                   ; Print number remaining


;===============================================================================
; DATA FROM HERE DOWN
;===============================================================================

                   DB    0, 0

; Splash screen animation structure 1

SplashScrAnimStr1  DB    $00                     ; Image form (increments each draw)
                   DB    $00                     ; Delta X
                   DB    $FF                     ; Delta Y is -1
                   DB    $B8, $FE                ; X, Y starting coordinates
                   DW    AlienSprCPos0           ; Base image (small alien)
                   DB    16                      ; Size of image (16 bytes)
                   DB    $9E                     ; Target Y coordinate
                   DB    $00                     ; Reached Y flag
                   DW    AlienSprCPos0           ; Base image (small alien)

; The tables at 1CB8 and 1AA1 control how fast shots are created. The speed is based
; on the upper byte of the player's score as table $1CB8: 02 10 20 30

ShotReloadRates    DB    $30                     ; <= $0200
                   DB    $10                     ; <= $1000
                   DB    $0B                     ; <= $2000
                   DB    $08                     ; <= $3000
                   DB    $07                     ; Otherwise fastest shot firing speed

Msg_GameOver       DB    $06, $00, $0C, $04      ; 'GAME OVER  PLAYER< >'
                   DB    $26, $0E, $15, $04
                   DB    $11, $26, $26, $0F
                   DB    $0B, $00 ,$18, $04
                   DB    $11, $24, $26, $25

Msg_1or2Players    DB    $1B, $26, $0E, $11      ; '1 OR 2PLAYERS BUTTON '
                   DB    $26, $1C, $0F, $0B
                   DB    $00, $18, $04, $11
                   DB    $12, $26, $01, $14
                   DB    $13, $13, $0E, $0D
                   DB    $26

Msg_Only1Player    DB    $0E, $0D, $0B, $18      ; 'ONLY 1PLAYER BUTTON '
                   DB    $26, $1B, $0F, $0B
                   DB    $00, $18, $04, $11
                   DB    $26, $26, $01, $14
                   DB    $13, $13, $0E, $0D
                   DB    $26

Msg_ScoreHeading   DB    $26, $12, $02, $0E      ; ' SCORE<1> HI-SCORE SCORE<2> '
                   DB    $11, $04, $24, $1B
                   DB    $25, $26, $07, $08
                   DB    $3F, $12, $02, $0E
                   DB    $11, $04, $26, $12
                   DB    $02, $0E, $11, $04
                   DB    $24, $1C, $25, $26

;--------------------------- RAM initialisation --------------------------------

; Copied to RAM ($2000) $C0 bytes as initialisation. See the description of RAM
; variables at the top of this file for the details on this data

Data_CopyToRam     DB    $01, $00, $00, $10, $00, $00, $00, $00
                   DB    $02, $78, $38, $78, $38, $00, $F8, $00
                   DB    $00, $80, $00, $8E, $02, $FF, $05, $0C
                   DB    $60, $1C, $20, $30, $10, $01, $00, $00
                   DB    $00, $00, $00, $BB, $03, $00, $10, $90
                   DB    $1C, $28, $30, $01, $04, $00, $FF, $FF
                   DB    $00, $00, $02, $76, $04, $00, $00, $00
                   DB    $00, $00, $04, $EE, $1C, $00, $00, $03
                   DB    $00, $00, $00, $B6, $04, $00, $00, $01
                   DB    $00, $1D, $04, $E2, $1C, $00, $00, $03
                   DB    $00, $00, $00, $82, $06, $00, $00, $01
                   DB    $06, $1D, $04, $D0, $1C, $00, $00, $03
                   DB    $FF, $00, $C0, $1C, $00, $00, $10, $21
                   DB    $01, $00, $30, $00, $12, $00, $00, $00

; These don't need to be copied over to RAM (see *** below)

Msg_Player1        DB    $0F, $0B, $00, $18      ; 'PLAY PLAYER<1>'
                   DB    $26, $0F, $0B, $00
                   DB    $18, $04, $11, $24
                   DB    $1B, $25

Temp_1B7E          DB    $FC, $00

Temp_1B80          DB    $01, $FF, $FF, $00, $00, $00, $20, $64
                   DB    $1D, $D0, $29, $18, $02, $54, $1D, $00
                   DB    $08, $00, $06, $00, $00, $01, $40, $00
                   DB    $01, $00, $00, $10, $9E, $00, $20, $1C

; ***
; These don't need to be copied over to RAM I believe this to be a mistake. The constant
; at 01E4 is C0, which is the size of this mirror with the added sprite. It should be A0.
; I believe there was a macro to size this area and later the splash screens where put in.
; Some of the data spilled over into this and the macro automatically included it. No harm

; Alien sprite type C pulling upside down Y

AlienSprCYA        DB    $00                     ;  [        ]
                   DB    $03                     ;  [**      ]
                   DB    $04                     ;  [  *     ]
                   DB    $78                     ;  [   **** ]
                   DB    $14                     ;  [  * *   ]
                   DB    $13                     ;  [**  *   ]
                   DB    $08                     ;  [   *    ]
                   DB    $1A                     ;  [ * **   ]
                   DB    $3D                     ;  [* ****  ]
                   DB    $68                     ;  [   * ** ]
                   DB    $FC                     ;  [  ******]
                   DB    $FC                     ;  [  ******]
                   DB    $68                     ;  [   * ** ]
                   DB    $3D                     ;  [* ****  ]
                   DB    $1A                     ;  [ * **   ]
                   DB    $00                     ;  [        ]

Temp_1BB0          DB    $00, $00, $01, $B8, $98, $A0, $1B, $10
                   DB    $FF, $00, $A0, $1B, $00, $00, $00, $00

;--------------------------- End of initialisation copy ------------------------

; Shot descriptor for splash shooting the extra 'C'

Temp_1BC0          DB    $00, $10, $00, $0E, $05, $00, $00, $00
                   DB    $00, $00, $07, $D0, $1C, $C8, $9B, $03

; Alien sprite C pulling upside down Y. The Y is closer to the ship. This gives
; the effect of the Y kind of 'sticking' in the animation

AlienSprCYB        DB    $00                     ;  [        ]
                   DB    $00                     ;  [        ]
                   DB    $03                     ;  [**      ]
                   DB    $04                     ;  [  *     ]
                   DB    $78                     ;  [   **** ]
                   DB    $14                     ;  [  * *   ]
                   DB    $0B                     ;  [** *    ]
                   DB    $19                     ;  [*  **   ]
                   DB    $3A                     ;  [ * ***  ]
                   DB    $6D                     ;  [* ** ** ]
                   DB    $FA                     ;  [ * *****]
                   DB    $FA                     ;  [ * *****]
                   DB    $6D                     ;  [* ** ** ]
                   DB    $3A                     ;  [ * ***  ]
                   DB    $19                     ;  [*  **   ]
                   DB    $00                     ;  [        ]

; More RAM initialisation copied by $18D9

Temp_1BE0          DB    $00, $00, $00, $00, $00, $00, $00, $00
                   DB    $00, $01, $00, $00, $01, $74, $1F, $00
                   DB    $80, $00, $00, $00, $00, $00, $1C, $2F
                   DB    $00, $00, $1C, $27, $00, $00, $1C, $39

; ALIEN IMAGES

; Alien sprite type A,B, and C at positions 0

AlienSprAPos0      DB    $00, $00, $39, $79, $7A, $6E, $EC, $FA
                   DB    $FA, $EC, $6E, $7A, $79, $39, $00, $00

AlienSprBPos0      DB    $00, $00, $00, $78, $1D, $BE, $6C, $3C
                   DB    $3C, $3C, $6C, $BE, $1D, $78, $00, $00

AlienSprCPos0      DB    $00, $00, $00, $00, $19, $3A, $6D, $FA
                   DB    $FA, $6D, $3A, $19, $00, $00, $00, $00

; Alien sprite type A,B, and C at positions 1

AlienSprAPos1      DB    $00, $00, $38, $7A, $7F, $6D, $EC, $FA
                   DB    $FA, $EC, $6D, $7F, $7A, $38, $00, $00

AlienSprBPos1      DB    $00, $00, $00, $0E, $18, $BE, $6D, $3D
                   DB    $3C, $3D, $6D, $BE, $18, $0E, $00, $00

AlienSprCPos1      DB    $00, $00, $00, $00, $1A, $3D, $68, $FC
                   DB    $FC, $68, $3D, $1A, $00, $00, $00, $00

; WRITE CODE TO TAKE 16 BYTES AND GENERATE DB STATEMENTS AND ASTERIX MATRIX

; Player sprite, and player exploding sprites

PlayerSprite       DB    $00                     ;  [        ]
                   DB    $00                     ;  [        ]
                   DB    $0F                     ;  [****    ]
                   DB    $1F                     ;  [*****   ]
                   DB    $1F                     ;  [*****   ]
                   DB    $1F                     ;  [*****   ]
                   DB    $1F                     ;  [*****   ]
                   DB    $7F                     ;  [******* ]
                   DB    $FF                     ;  [********]
                   DB    $7F                     ;  [******* ]
                   DB    $1F                     ;  [*****   ]
                   DB    $1F                     ;  [*****   ]
                   DB    $1F                     ;  [*****   ]
                   DB    $1F                     ;  [*****   ]
                   DB    $0F                     ;  [****    ]
                   DB    $00                     ;  [        ]

PlayerBlowupSpr    DB    $00, $04, $01, $13, $03, $07, $B3, $0F
                   DB    $2F, $03, $2F, $49, $04, $03, $00, $01

                   DB    $40, $08, $05, $A3, $0A, $03, $5B, $0F
                   DB    $27, $27, $0B, $4B, $40, $84, $11, $48

; Player shot (bullet) sprite

PlayerShotSpr      DB    $0F                     ;  [****    ]

; Shot exploding sprite

ShotExploding      DB    $99, $3C, $7E, $3D, $BC, $3E, $7C, $99

Msg_10Points       DB    $27, $1B, $1A, $26      ; '=10 POINTS' (ran out of space at 1DFE)
                   DB    $0F, $0E, $08, $0D
                   DB    $13, $12

Msg_ScoreAdvance   DB    $28, $12, $02, $0E      ; '*SCORE ADVANCE TABLE*'
                   DB    $11, $04, $26, $00
                   DB    $03, $15, $00, $0D
                   DB    $02, $04, $26, $13
                   DB    $00, $01, $0B, $04
                   DB    $28

; The tables at 1CB8 and 1AA1 control how fast shots are created. The speed is based
; on the upper byte of the player's score. For a score of less than or equal 0200 then
; the fire speed is 30. For a score less than or equal 1000 the shot speed is 10. Less
; than or equal 2000 the speed is 0B. Less than or equal 3000 is 08. And anything
; above 3000 is 07
;
; 1AA1: 30 10 0B 08 07

AReloadScoreTab    DB    $02, $10, $20, $30

Msg_Tilt           DB    $13, $08, $0B, $13      ; 'TILT'

; Alien exploding sprite

AlienExplode       DB    $00, $08, $49, $22, $14, $81, $42, $00
                   DB    $42, $81, $14, $22, $49, $08, $00, $00

; Squigly shot sprite in 4 animation frames

SquiglyShot        DB    $44                     ;  [  *   * ]
                   DB    $AA                     ;  [ * * * *]
                   DB    $10                     ;  [    *   ]

                   DB    $88                     ;  [   *   *]
                   DB    $54                     ;  [  * * * ]
                   DB    $22                     ;  [ *   *  ]

                   DB    $10                     ;  [    *   ]
                   DB    $AA                     ;  [ * * * *]
                   DB    $44                     ;  [  *   * ]

                   DB    $22                     ;  [ *   *  ]
                   DB    $54                     ;  [  * * * ]
                   DB    $88                     ;  [   *   *]

; Alien shot exploding

AShotExplo         DB    $4A, $15, $BE, $3F, $5E, $25

; Alien shot ... the plunger looking one, in four frames

PlungerShot        DB    $04                     ;  [  *     ]
                   DB    $FC                     ;  [  ******]
                   DB    $04                     ;  [  *     ]

                   DB    $10                     ;  [    *   ]
                   DB    $FC                     ;  [  ******]
                   DB    $10                     ;  [    *   ]

                   DB    $20                     ;  [     *  ]
                   DB    $FC                     ;  [  ******]
                   DB    $20                     ;  [     *  ]

                   DB    $80                     ;  [       *]
                   DB    $FC                     ;  [  ******]
                   DB    $80                     ;  [       *]

; Alien shot ... the rolling one, in four frames

RollShot           DB    $00                     ;  [        ]
                   DB    $FE                     ;  [ *******]
                   DB    $00                     ;  [        ]

                   DB    $24                     ;  [  *  *  ]
                   DB    $FE                     ;  [ *******]
                   DB    $12                     ;  [ *  *   ]

                   DB    $00                     ;  [        ]
                   DB    $FE                     ;  [ *******]
                   DB    $00                     ;  [        ]

                   DB    $48                     ;  [   *  * ]
                   DB    $FE                     ;  [ *******]
                   DB    $90                     ;  [    *  *]

; Message 'PLAy' with an upside down 'Y' for splash screen

Msg_PlayUY         DB    $0F, $0B, $00, $29      ; 'PLAy'

Temp_1CFE          DB    $00, $00

; This table decides which column a shot will fall from. The column number is read from
; the table (1-11) and the pointer increases for the shot type. For instance, the
; 'squiggly' shot will fall from columns in this order: 0B, 01, 06, 03. If you play the
; game you'll see that order

; The 'plunger' shot uses index 00-0F (inclusive)
; The 'squiggly' shot uses index 06-14 (inclusive)
; The 'rolling' shot targets the player

ColFireTable       DB    $01, $07, $01, $01, $01, $04, $0B, $01
                   DB    $06, $03, $01, $01, $0B, $09, $02, $08
                   DB    $02, $0B, $04, $07, $0A

; This appears to be part of the column-firing table, but it is never used.
; Perhaps this was originally intended for the 'rolling' shot but then the
; 'rolling' was change to target the player specifically

Temp_1D15          DB    $05, $02, $05, $04, $06, $07, $08, $0A
                   DB    $06, $0A, $03

; Shield image pattern. 2 bytes per column x 22 = 44 bytes

ShieldImage        DB    $FF, $0F, $FF, $1F, $FF, $3F, $FF, $7F
                   DB    $FF, $FF, $FC, $FF, $F8, $FF, $F0, $FF
                   DB    $F0, $FF, $F0, $FF, $F0, $FF, $F0, $FF
                   DB    $F0, $FF, $F0, $FF, $F8, $FF, $FC, $FF
                   DB    $FF, $FF, $FF, $FF, $FF, $7F, $FF, $3F
                   DB    $FF, $1F, $FF, $0F

Temp_1D4C          DB    $05, $10, $15, $30      ; Table of possible saucer scores
Temp_1D50          DB    $94, $97, $9A, $9D      ; Table of corresponding string prints
                                                 ; for each score. Prefix address $1D

; 208D points here to the score given when the saucer is shot. It advances every time the
; player-shot is removed. The code wraps after 15, but there are 16 values in this table.
; This is a bug in the code at 044E (thanks to Colin Dooley for finding this)
; Thus the one and only 300 comes up every 15 shots (after an initial 8)

SaucerScrTab       DB    $10, $05, $05, $10, $15, $10, $10, $05
                   DB    $30, $10, $10, $10, $05, $15, $10, $05

; Alien Saucer Sprite, and exploding version

SpriteSaucer       DB    $00                     ;  [        ]
                   DB    $00                     ;  [        ]
                   DB    $00                     ;  [        ]
                   DB    $00                     ;  [        ]
                   DB    $04                     ;  [  *     ]
                   DB    $0C                     ;  [  **    ]
                   DB    $1E                     ;  [ ****   ]
                   DB    $37                     ;  [*** **  ]
                   DB    $3E                     ;  [ *****  ]
                   DB    $7C                     ;  [  ***** ]
                   DB    $74                     ;  [  * *** ]
                   DB    $7E                     ;  [ ****** ]
                   DB    $7E                     ;  [ ****** ]
                   DB    $74                     ;  [  * *** ]
                   DB    $7C                     ;  [  ***** ]
                   DB    $3E                     ;  [ *****  ]
                   DB    $37                     ;  [*** **  ]
                   DB    $1E                     ;  [ ****   ]
                   DB    $0C                     ;  [  **    ]
                   DB    $04                     ;  [  *     ]
                   DB    $00                     ;  [        ]
                   DB    $00                     ;  [        ]
                   DB    $00                     ;  [        ]
                   DB    $00                     ;  [        ]

SpriteSaucerExp    DB    $00, $22, $00, $A5, $40, $08, $98, $3D
                   DB    $B6, $3C, $36, $1D, $10, $48, $62, $B6
                   DB    $1D, $98, $08, $42, $90, $08, $00, $00

SaucerScoreStr     DB    $26, $1F, $1A           ; _50
                   DB    $1B, $1A, $1A           ; 100
                   DB    $1B, $1F, $1A           ; 150
                   DB    $1D, $1A, $1A           ; 300

; Score table for hitting alien type

AlienScores        DB    $10                     ; Bottom 2 rows
                   DB    $20                     ; Middle row
                   DB    $30                     ; Highest row

; Starting Y coordinates for aliens at beginning of rounds. The first round is initialised
; to $78 at 07EA. After that this table is used for 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, and
; 9th. The 10th starts over at 1DA3 (60).

AlienStartTable    DB    $60, $50, $48, $48, $48, $40, $40, $40

Msg_PlayY          DB    $0F, $0B, $00, $18      ; 'PLAY' with normal Y

Msg_Invaders       DB    $12, $0F, $00, $02      ; 'SPACE  INVADERS'
                   DB    $04, $26, $26, $08
                   DB    $0D, $15, $00, $03
                   DB    $04, $11, $12

; Tables used to draw 'SCORE ADVANCE TABLE' and Alien Scores information

ScoreAdvanceTable  DW    $2C0E, SpriteSaucer + 4 ; Flying Saucer (**why +4 ?)
                   DW    $2C0C, AlienSprCPos0    ; Alien C, sprite 0
                   DW    $2C0A, AlienSprBPos1    ; Alien B, sprite 1
                   DW    $2C08, AlienSprAPos0    ; Alien A, sprite 0
                   DB    $FF                     ; End of list

AlienScoreTable    DW    $2E0E, Msg_Mystery      ; '=? MYSTERY'
                   DW    $2E0C, Msg_30Points     ; '=30 POINTS'
                   DW    $2E0A, Msg_20Points     ; '=20 POINTS'
                   DW    $2E08, Msg_10Points     ; '=10 POINTS'
                   DB    $FF                     ; End of list

Msg_Mystery        DB    $27, $38, $26, $0C      ; '=? MYSTERY'
                   DB    $18, $12, $13, $04
                   DB    $11, $18

Msg_30Points       DB    $27, $1D, $1A, $26      ; '=30 POINTS'
                   DB    $0F, $0E, $08, $0D
                   DB    $13, $12

Msg_20Points       DB    $27, $1C, $1A, $26      ; '=20 POINTS'
                   DB    $0F, $0E, $08, $0D
                   DB    $13, $12

; The '=10 POINTS' message is at 1C99, apparently to keep the font table at 1E00

                   DB    $00, $00                ; Padding to put font table at 1E00


;=========================================================================================
; Font sprites, 8 byte each
;=========================================================================================
; Some of the font characters at the end were never needed. The ROM overwrites these
; characters with data near; for instance, 1F90 would be a character but has the
; 'INSERT COIN' message. The '?' character is at 1FC0 and is used in messages as is
; 1FF8 '-'

FontSet            DB    $00, $1F, $24, $44, $24, $1F, $00, $00  ; 00 = A
                   DB    $00, $7F, $49, $49, $49, $36, $00, $00  ; 01 = B
                   DB    $00, $3E, $41, $41, $41, $22, $00, $00  ; 02 = C
                   DB    $00, $7F, $41, $41, $41, $3E, $00, $00  ; 03 = D
                   DB    $00, $7F, $49, $49, $49, $41, $00, $00  ; 04 = E
                   DB    $00, $7F, $48, $48, $48, $40, $00, $00  ; 05 = F
                   DB    $00, $3E, $41, $41, $45, $47, $00, $00  ; 06 = G
                   DB    $00, $7F, $08, $08, $08, $7F, $00, $00  ; 07 = H

                   DB    $00, $00, $41, $7F, $41, $00, $00, $00  ; 08 = I
                   DB    $00, $02, $01, $01, $01, $7E, $00, $00  ; 09 = J
                   DB    $00, $7F, $08, $14, $22, $41, $00, $00  ; 0A = K
                   DB    $00, $7F, $01, $01, $01, $01, $00, $00  ; 0B = L
                   DB    $00, $7F, $20, $18, $20, $7F, $00, $00  ; 0C = M
                   DB    $00, $7F, $10, $08, $04, $7F, $00, $00  ; 0D = N
                   DB    $00, $3E, $41, $41, $41, $3E, $00, $00  ; 0E = O
                   DB    $00, $7F, $48, $48, $48, $30, $00, $00  ; 0F = P

                   DB    $00, $3E, $41, $45, $42, $3D, $00, $00  ; 10 = Q
                   DB    $00, $7F, $48, $4C, $4A, $31, $00, $00  ; 11 = R
                   DB    $00, $32, $49, $49, $49, $26, $00, $00  ; 12 = S
                   DB    $00, $40, $40, $7F, $40, $40, $00, $00  ; 13 = T
                   DB    $00, $7E, $01, $01, $01, $7E, $00, $00  ; 14 = U
                   DB    $00, $7C, $02, $01, $02, $7C, $00, $00  ; 15 = V
                   DB    $00, $7F, $02, $0C, $02, $7F, $00, $00  ; 16 = W
                   DB    $00, $63, $14, $08, $14, $63, $00, $00  ; 17 = X

                   DB    $00, $60, $10, $0F, $10, $60, $00, $00  ; 18 = Y
                   DB    $00, $43, $45, $49, $51, $61, $00, $00  ; 19 = Z
                   DB    $00, $3E, $45, $49, $51, $3E, $00, $00  ; 1A = 0
                   DB    $00, $00, $21, $7F, $01, $00, $00, $00  ; 1B = 1
                   DB    $00, $23, $45, $49, $49, $31, $00, $00  ; 1C = 2
                   DB    $00, $42, $41, $49, $59, $66, $00, $00  ; 1D = 3
                   DB    $00, $0C, $14, $24, $7F, $04, $00, $00  ; 1E = 4
                   DB    $00, $72, $51, $51, $51, $4E, $00, $00  ; 1F = 5

                   DB    $00, $1E, $29, $49, $49, $46, $00, $00  ; 20 = 6
                   DB    $00, $40, $47, $48, $50, $60, $00, $00  ; 21 = 7
                   DB    $00, $36, $49, $49, $49, $36, $00, $00  ; 22 = 8
                   DB    $00, $31, $49, $49, $4A, $3C, $00, $00  ; 23 = 9
                   DB    $00, $08, $14, $22, $41, $00, $00, $00  ; 24 = <
                   DB    $00, $00, $41, $22, $14, $08, $00, $00  ; 25 = >
                   DB    $00, $00, $00, $00, $00, $00, $00, $00  ; 26 = SPACE
                   DB    $00, $14, $14, $14, $14, $14, $00, $00  ; 27 = '='

                   DB    $00, $22, $14, $7F, $14, $22, $00, $00  ; 28 = *
                   DB    $00, $03, $04, $78, $04, $03, $00, $00  ; 29 = Inverted Y

; Font characters $2A to $37 re-purposed as message strings and sprites...

Msg_1or2Plyrs      DB    $24, $1B, $26, $0E      ; '<1 OR 2 PLAYERS>  '
                   DB    $11, $26, $1C, $26
                   DB    $0F, $0B, $00, $18
                   DB    $04, $11, $12, $25
                   DB    $26, $26

Msg_1Coin          DB    $28, $1B, $26, $0F      ; '*1 PLAYER  1 COIN '
                   DB    $0B, $00, $18, $04
                   DB    $11, $26, $26, $1B
                   DB    $26, $02, $0E, $08
                   DB    $0D, $26

DemoCommands       DB    $01, $01, $00, $00      ; (1=Right, 2=Left)
                   DB    $01, $00, $02, $01
                   DB    $00, $02, $01, $00

; Small alien pushing Y back onto screen, version A

AlienSprCA         DB    $60, $10, $0F, $10, $60, $30, $18, $1A
                   DB    $3D, $68, $FC, $FC, $68, $3D, $1A, $00

Msg_InsertCoin     DB    $08, $0D, $12, $04      ; 'INSERT  COIN'
                   DB    $11, $13, $26, $26
                   DB    $02, $0E, $08, $0D

CreditTable        DW    $2A0D, Msg_1or2Plyrs    ; '<1 OR 2 PLAYERS>  ' to screen at 2A0D
                   DW    $2A0A, Msg_1Coin        ; '*1 PLAYER  1 COIN ' to screen at 2A0A
                   DW    $2A07, Msg_2Coins       ; '*2 PLAYERS 2 COINS' to screen at 2A07
                   DB    $FF                     ; Terminates 'table print'

Msg_Credit         DB    $02, $11, $04, $03      ; 'CREDIT ' (with space on the end)
                   DB    $08, $13, $26

; Small alien pushing Y back onto screen, version B

AlienSprCB         DB    $00, $60, $10, $0F, $10, $60, $38, $19
                   DB    $3A, $6D, $FA, $FA, $6D, $3A, $19, $00

; Font character '?'

Char38_Query       DB    $00, $20, $40, $4D, $50, $20, $00, $00  ; 38 = '?'

; Font characters $39 to $3E re-purposed also...

                   DB    $00

; Splash screen animation structure 3

SplashScrAnimStr3  DB    $00                     ; Image form (increments each draw)
                   DB    $00                     ; Delta X
                   DB    $FF                     ; Delta Y is -1
                   DB    $B8                     ; X coordinate
                   DB    $FF                     ; Y starting coordinate
                   DW    AlienSprCA              ; Base image (small alien with Y)
                   DB    $10                     ; Size of image (16 bytes)
                   DB    $97                     ; Target Y coordinate
                   DB    $00                     ; Reached Y flag
                   DW    AlienSprCA              ; Base image (small alien with Y)

; Splash screen animation structure 4

SplashScrAnimStr4  DB    $00                     ;Image form (increments each draw)
                   DB    $00                     ; Delta X
                   DB    $01                     ; Delta Y is -1
                   DB    $D0                     ; X coordinate
                   DB    $22                     ; Y starting coordinate
                   DW    AlienSprCPos0           ; Base image (small alien)
                   DB    $10                     ; Size of image (16 bytes)
                   DB    $94                     ; Target Y coordinate
                   DB    $00                     ; Reached Y flag
                   DW    AlienSprCPos0           ; Base image (small alien)

Msg_2Coins         DB    $28, $1C, $26, $0F      ; '*2 PLAYERS 2 COINS'
                   DB    $0B, $00, $18, $04
                   DB    $11, $12, $26, $1C
                   DB    $26, $02, $0E, $08
                   DB    $0D, $12

Msg_Push           DB    $0F, $14, $12, $07      ; 'PUSH ' (with space on the end)
                   DB    $26

; Font character $3F '-'

Char3F_Dash        DB    $00, $08, $08, $08, $08, $08, $00, $00  ; 3F = '-'
