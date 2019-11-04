// ============================================================================
//
// Standard Definitions for Space Invaders Arcade Game
//
// Thanks to http://computerarcheology.com/Arcade/SpaceInvaders/
//
// ============================================================================

			ORG	$2000	// General variables

WaitOnDraw		RMB	1	// Cleared by alien-draw and set by next-alien
					// This ensures no alien gets missed while drawing
unk1			RMB	1
AlienIsExploding	RMB	1	// Not-0 if an alien is exploding, 0 if not exploding
ExpAlienTimer		RMB	1	// Time (ISR ticks) left in alien-explosion
AlienRow		RMB	1	// Row number of current alien (cursor)
AlienFrame		RMB	1	// Animation frame number (0 or 1) for current alien (cursor)
AlienCurIndex		RMB	1	// Alien cursor index (from 0 to 54)
RefAlienDYr		RMB	1	// Reference alien delta Yr
RefAlienDXr		RMB	1	// Reference alien delta Xr
RefAlienYr		RMB	1	// Reference alien Yr coordinate
RefAlienXr		RMB	1	// Reference alien Xr coordinate
AlienPosLSB		RMB	1	// Alien cursor bit pos (LSB)
AlienPosMSB		RMB	1	// Alien cursor bit pos (MSB)
RackDirection		RMB	1	// Value 0 if rack is moving right or 1 if rack is moving left
RackDownDelta		RMB	1	// Constant value of alien rack dropping after bumping screen edge
unk2			RMB	1

// GameObject0 (Move/draw the player)

Obj0TimerMSB		RMB	1
Obj0TimerLSB		RMB	1	// Wait 128 interrupts (about 2 secs) before player task starts
Obj0TimerExtra		RMB	1
Obj0HandlerLSB		RMB	1
Obj0HandlerMSB		RMB	1	// Player handler code at 028E
PlayerAlive		RMB	1	// Player is alive (FF=alive). Toggles between 0 and 1 for blow-up images
ExpAnimateTimer		RMB	1	// Time till next blow-up sprite change (reloaded to 5)
ExpAnimateCnt		RMB	1	// Number of changes left in blow-up sequence
PlyrSprPicL		RMB	1	// Player sprite descriptor ... picture LSB
PlyrSprPicM		RMB	1	// Player sprite descriptor ... picture MSB
PlayerYr		RMB	1	// Player sprite descriptor ... location LSB
PlayerXr		RMB	1	// Player sprite descriptor ... location MSB
PlayerSprSiz		RMB	1	// Player sprite descriptor ... size of sprite
NextDemoCmd		RMB	1	// Next movement command for demo
HidMessSeq		RMB	1	// Set to 1 after first of 2 sequences are entered for hidden-message display
unk3			RMB	1	// Appears to be unused

Obj1TimerMSB		RMB	1

// etc
	
			ORG	$20E5

Player1Ex		RMB	1	// Extra ship has been awarded = 0
Player2Ex		RMB	1	// Extra ship has been awarded = 0
Player1Alive		RMB	1	// 1 if player is alive, 0 if dead (after last man)
Player2Alive		RMB	1	// 1 if player is alive, 0 if dead (after last man)
SuspendPlay		RMB	1	// 1=game things are moving, 0=game things are suspended
CoinSwitch		RMB	1	// 1=switch down, 0=switch up (used to debounce coin switch)
NumCoins		RMB	1	// Number of coin credits in BCD format (99 max)
SplashAnimate		RMB	1	// 0 for animation during splash and 1 for not. This alternates after every cycle
DemoCmdPtrLSB		RMB	1	// Pointer to demo commands
DemoCmdPtrMSB		RMB	1	
GameMode		RMB	1	// 1=game running, 0=demo or splash screens
unk20f0			RMB	1
AdjustScore		RMB	1	// Set to 1 if score needs adjusting

//etc



// Stack base

StackTop		EQU	$2400

// Some addresses

Reset			EQU	$0000	// RST 0
ScanLine96		EQU	$0008	// RST 1
ScanLine224		EQU	$0010	// RST 2

DrawAlien		EQU	$0100
CursorNextAlien		EQU	$0141
GetAlienCoords		EQU	$017A
InitAliens		EQU	$01C0
DrawBottomLine		EQU	$01CF
CopyShields		EQU	$021E
RunGameObjs		EQU	$0248
GameObj0		EQU	$028E
GameObj1		EQU	$03BB
GameObj2		EQU	$0476
GameObj3		EQU	$04B6
HandleAlienShot		EQU	$0563
GameObj4		EQU	$0682
WaitForStart		EQU	$0765
NewGame			EQU	$0798
GetAlienRefPtr		EQU	$0886
PrintMessage		EQU	$08F3
TimeToSaucer		EQU	$0913
DrawHexByte		EQU	$09B2
ClearPlayField		EQU	$09D6
PrintMessageDel		EQU	$0A93
OneSecDelay		EQU	$0AB1
DrawSimpleSprite	EQU	$1439
TimeFleetSound		EQU	$1740
CheckHandleTilt		EQU	$17CD
Initialisation		EQU	$18D4
DrawScore		EQU	$1931
DrawNumCredits		EQU	$1947
PrintHiScore		EQU	$1950
DrawStatus		EQU	$1956
DisableGameTasks	EQU	$19D7



