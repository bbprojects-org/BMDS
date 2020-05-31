  // 8080 in the Z80 STYLE ///

  // Move, load, store
  ( M: 'LD';   A: 'B,B';    S: 02{mReg}; O: $40; N: 1; C:  5; R: rNIL; T: %10 ), // MOV B,reg
  ( M: 'LD';   A: 'B,C';    S: 02{mReg}; O: $41; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'B,D';    S: 02{mReg}; O: $42; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'B,E';    S: 02{mReg}; O: $43; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'B,H';    S: 02{mReg}; O: $44; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'B,L';    S: 02{mReg}; O: $45; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'B,(HL)'; S: 02{mReg}; O: $46; N: 1; C:  7; R: rNIL; T: %10 ), // MOV B,M
  ( M: 'LD';   A: 'B,A';    S: 02{mReg}; O: $47; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'B,*';    S: 03{mIMM}; O: $06; N: 2; C:  7; R: rNIL; T: %10 ), // MVI B

  ( M: 'LD';   A: 'C,B';    S: 02{mReg}; O: $48; N: 1; C:  5; R: rNIL; T: %10 ), // MOV C,reg
  ( M: 'LD';   A: 'C,C';    S: 02{mReg}; O: $49; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'C,D';    S: 02{mReg}; O: $4A; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'C,E';    S: 02{mReg}; O: $4B; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'C,H';    S: 02{mReg}; O: $4C; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'C,L';    S: 02{mReg}; O: $4D; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'C,(HL)'; S: 02{mReg}; O: $4E; N: 1; C:  7; R: rNIL; T: %10 ), // MOV C,M
  ( M: 'LD';   A: 'C,A';    S: 02{mReg}; O: $4F; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'C,*';    S: 03{mIMM}; O: $0E; N: 2; C:  7; R: rNIL; T: %10 ), // MVI C

  ( M: 'LD';   A: 'D,B';    S: 02{mReg}; O: $50; N: 1; C:  5; R: rNIL; T: %10 ), // MOV D,reg
  ( M: 'LD';   A: 'D,C';    S: 02{mReg}; O: $51; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'D,D';    S: 02{mReg}; O: $52; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'D,E';    S: 02{mReg}; O: $53; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'D,H';    S: 02{mReg}; O: $54; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'D,L';    S: 02{mReg}; O: $55; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'D,(HL)'; S: 02{mReg}; O: $56; N: 1; C:  7; R: rNIL; T: %10 ), // MOV D,M
  ( M: 'LD';   A: 'D,A';    S: 02{mReg}; O: $57; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'D,*';    S: 03{mIMM}; O: $16; N: 2; C:  7; R: rNIL; T: %10 ), // MVI D

  ( M: 'LD';   A: 'E,B';    S: 02{mReg}; O: $58; N: 1; C:  5; R: rNIL; T: %10 ), // MOV E,reg
  ( M: 'LD';   A: 'E,C';    S: 02{mReg}; O: $59; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'E,D';    S: 02{mReg}; O: $5A; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'E,E';    S: 02{mReg}; O: $5B; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'E,H';    S: 02{mReg}; O: $5C; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'E,L';    S: 02{mReg}; O: $5D; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'E,(HL)'; S: 02{mReg}; O: $5E; N: 1; C:  7; R: rNIL; T: %10 ), // MOV E,M
  ( M: 'LD';   A: 'E,A';    S: 02{mReg}; O: $5F; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'E,*';    S: 03{mIMM}; O: $1E; N: 2; C:  7; R: rNIL; T: %10 ), // MVI E

  ( M: 'LD';   A: 'H,B';    S: 02{mReg}; O: $60; N: 1; C:  5; R: rNIL; T: %10 ), // MOV H,reg
  ( M: 'LD';   A: 'H,C';    S: 02{mReg}; O: $61; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'H,D';    S: 02{mReg}; O: $62; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'H,E';    S: 02{mReg}; O: $63; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'H,H';    S: 02{mReg}; O: $64; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'H,L';    S: 02{mReg}; O: $65; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'H,(HL)'; S: 02{mReg}; O: $66; N: 1; C:  7; R: rNIL; T: %10 ), // MOV H,M
  ( M: 'LD';   A: 'H,A';    S: 02{mReg}; O: $67; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'H,*';    S: 03{mIMM}; O: $26; N: 2; C:  7; R: rNIL; T: %10 ), // MVI H

  ( M: 'LD';   A: 'L,B';    S: 02{mReg}; O: $68; N: 1; C:  5; R: rNIL; T: %10 ), // MOV L,reg
  ( M: 'LD';   A: 'L,C';    S: 02{mReg}; O: $69; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'L,D';    S: 02{mReg}; O: $6A; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'L,E';    S: 02{mReg}; O: $6B; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'L,H';    S: 02{mReg}; O: $6C; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'L,L';    S: 02{mReg}; O: $6D; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'L,(HL)'; S: 02{mReg}; O: $6E; N: 1; C:  7; R: rNIL; T: %10 ), // MOV L,M
  ( M: 'LD';   A: 'L,A';    S: 02{mReg}; O: $6F; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'L,*';    S: 03{mIMM}; O: $2E; N: 2; C:  7; R: rNIL; T: %10 ), // MVI L

  ( M: 'LD';   A: '(HL),B'; S: 02{mReg}; O: $70; N: 1; C:  7; R: rNIL; T: %10 ), // MOV M,reg
  ( M: 'LD';   A: '(HL),C'; S: 02{mReg}; O: $71; N: 1; C:  7; R: rNIL; T: %10 ),
  ( M: 'LD';   A: '(HL),D'; S: 02{mReg}; O: $72; N: 1; C:  7; R: rNIL; T: %10 ),
  ( M: 'LD';   A: '(HL),E'; S: 02{mReg}; O: $73; N: 1; C:  7; R: rNIL; T: %10 ),
  ( M: 'LD';   A: '(HL),H'; S: 02{mReg}; O: $74; N: 1; C:  7; R: rNIL; T: %10 ),
  ( M: 'LD';   A: '(HL),L'; S: 02{mReg}; O: $75; N: 1; C:  7; R: rNIL; T: %10 ),
  ( M: 'LD';   A: '(HL),A'; S: 02{mReg}; O: $77; N: 1; C:  7; R: rNIL; T: %10 ),
  ( M: 'LD';   A: '(HL),*'; S: 03{mIMM}; O: $36; N: 2; C:  7; R: rNIL; T: %10 ), // MVI r

  ( M: 'LD';   A: 'A,B';    S: 02{mReg}; O: $78; N: 1; C:  5; R: rNIL; T: %10 ), // MOV A,reg
  ( M: 'LD';   A: 'A,C';    S: 02{mReg}; O: $79; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'A,D';    S: 02{mReg}; O: $7A; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'A,E';    S: 02{mReg}; O: $7B; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'A,H';    S: 02{mReg}; O: $7C; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'A,L';    S: 02{mReg}; O: $7D; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'A,(HL)'; S: 02{mReg}; O: $7E; N: 1; C:  7; R: rNIL; T: %10 ), // MOV A,M
  ( M: 'LD';   A: 'A,A';    S: 02{mReg}; O: $7F; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'LD';   A: 'A,(*)';  S: 04{mDir}; O: $3A; N: 3; C: 13; R: rNIL; T: %10 ), // LDA A
  ( M: 'LD';   A: 'A,*';    S: 03{mIMM}; O: $3E; N: 2; C:  7; R: rNIL; T: %10 ), // MVI A

  ( M: 'LD';   A: 'A,(BC)'; S: 02{mReg}; O: $0A; N: 1; C:  7; R: rNIL; T: %10 ), // LDAX B
  ( M: 'LD';   A: 'A,(DE)'; S: 02{mReg}; O: $1A; N: 1; C:  7; R: rNIL; T: %10 ), // LDAX D

  ( M: 'LD';   A: '(BC),A'; S: 02{mReg}; O: $02; N: 1; C:  7; R: rNIL; T: %10 ), // STAX B
  ( M: 'LD';   A: '(DE),A'; S: 02{mReg}; O: $12; N: 1; C:  7; R: rNIL; T: %10 ), // STAX D

  ( M: 'LD';   A: 'BC,*';   S: 04{mIMM}; O: $01; N: 3; C: 10; R: rNIL; T: %10 ), // LXI B
  ( M: 'LD';   A: 'DE,*';   S: 04{mIMM}; O: $11; N: 3; C: 10; R: rNIL; T: %10 ), // LXI D
  ( M: 'LD';   A: 'HL,*';   S: 04{mIMM}; O: $21; N: 3; C: 10; R: rNIL; T: %10 ), // LXI H

  ( M: 'LD';   A: '(*),HL'; S: 04{mDir}; O: $22; N: 3; C: 16; R: rNIL; T: %10 ), // SHLD
  ( M: 'LD';   A: 'HL,(*)'; S: 04{mDir}; O: $2A; N: 3; C: 16; R: rNIL; T: %10 ), // LHLD
  ( M: 'LD';   A: '(*),A';  S: 04{mDir}; O: $32; N: 3; C: 13; R: rNIL; T: %10 ), // STA

  ( M: 'LD';   A: 'SP,HL';  S: 02{mReg}; O: $F9; N: 1; C:  5; R: rNIL; T: %10 ), // SPHL
  ( M: 'LD';   A: 'SP,*';   S: 04{mIMM}; O: $31; N: 3; C: 10; R: rNIL; T: %10 ), // LXI SP

  ( M: 'EX';   A: 'DE,HL';   S: 02{mReg}; O: $EB; N: 1; C:  4; R: rNIL; T: %10 ), // XCHG
  ( M: 'EX';   A: '(SP),HL'; S: 02{mReg}; O: $E3; N: 1; C: 18; R: rNIL; T: %10 ), // XTHL

  // Stack Ops
  ( M: 'PUSH'; A: 'BC';     S: 02{mReg}; O: $C5; N: 1; C: 11; R: rNIL; T: %10 ), // PUSH B
  ( M: 'PUSH'; A: 'DE';     S: 02{mReg}; O: $D5; N: 1; C: 11; R: rNIL; T: %10 ), // PUSH D
  ( M: 'PUSH'; A: 'HL';     S: 02{mReg}; O: $E5; N: 1; C: 11; R: rNIL; T: %10 ), // PUSH H
  ( M: 'PUSH'; A: 'AF';     S: 02{mReg}; O: $F5; N: 1; C: 11; R: rNIL; T: %10 ), // PUSH PSW

  ( M: 'POP';  A: 'BC';     S: 02{mReg}; O: $C1; N: 1; C: 10; R: rNIL; T: %10 ), // POP B
  ( M: 'POP';  A: 'DE';     S: 02{mReg}; O: $D1; N: 1; C: 10; R: rNIL; T: %10 ), // POP D
  ( M: 'POP';  A: 'HL';     S: 02{mReg}; O: $E1; N: 1; C: 10; R: rNIL; T: %10 ), // POP H
  ( M: 'POP';  A: 'AF';     S: 02{mReg}; O: $F1; N: 1; C: 10; R: rNIL; T: %10 ), // POP PSW

  // Jump
  ( M: 'JP';   A: 'NZ,*';   S: 04{mDir}; O: $C2; N: 3; C: 10; R: rNIL; T: %10 ), // JNZ
  ( M: 'JP';   A: 'Z,*';    S: 04{mDir}; O: $CA; N: 3; C: 10; R: rNIL; T: %10 ), // JZ
  ( M: 'JP';   A: 'NC,*';   S: 04{mDir}; O: $D2; N: 3; C: 10; R: rNIL; T: %10 ), // JNC
  ( M: 'JP';   A: 'C,*';    S: 04{mDir}; O: $DA; N: 3; C: 10; R: rNIL; T: %10 ), // JC
  ( M: 'JP';   A: 'PO,*';   S: 04{mDir}; O: $E2; N: 3; C: 10; R: rNIL; T: %10 ), // JPO
  ( M: 'JP';   A: 'PE,*';   S: 04{mDir}; O: $EA; N: 3; C: 10; R: rNIL; T: %10 ), // JPE
  ( M: 'JP';   A: 'P,*';    S: 04{mDir}; O: $F2; N: 3; C: 10; R: rNIL; T: %10 ), // JP
  ( M: 'JP';   A: 'M,*';    S: 04{mDir}; O: $FA; N: 3; C: 10; R: rNIL; T: %10 ), // JM
  ( M: 'JP';   A: '(HL)';   S: 02{mReg}; O: $E9; N: 1; C:  5; R: rNIL; T: %10 ), // PCHL
  ( M: 'JP';   A: '*';      S: 04{mDir}; O: $C3; N: 3; C: 10; R: rNIL; T: %10 ), // JMP

  // Call
  ( M: 'CALL'; A: 'NZ,*';   S: 04{mDir}; O: $C4; N: 3; C: 11; R: rNIL; T: %10 ), // CNZ
  ( M: 'CALL'; A: 'Z,*';    S: 04{mDir}; O: $CC; N: 3; C: 11; R: rNIL; T: %10 ), // CZ
  ( M: 'CALL'; A: 'NC,*';   S: 04{mDir}; O: $D4; N: 3; C: 11; R: rNIL; T: %10 ), // CNC
  ( M: 'CALL'; A: 'C,*';    S: 04{mDir}; O: $DC; N: 3; C: 11; R: rNIL; T: %10 ), // CC
  ( M: 'CALL'; A: 'PO,*';   S: 04{mDir}; O: $E4; N: 3; C: 11; R: rNIL; T: %10 ), // CPO
  ( M: 'CALL'; A: 'PE,*';   S: 04{mDir}; O: $EC; N: 3; C: 11; R: rNIL; T: %10 ), // CPE
  ( M: 'CALL'; A: 'P,*';    S: 04{mDir}; O: $F4; N: 3; C: 11; R: rNIL; T: %10 ), // CP
  ( M: 'CALL'; A: 'M,*';    S: 04{mDir}; O: $FC; N: 3; C: 11; R: rNIL; T: %10 ), // CM
  ( M: 'CALL'; A: '*';      S: 04{mDir}; O: $CD; N: 3; C: 17; R: rNIL; T: %10 ), // CALL

  // Return
  ( M: 'RET';  A: 'NZ';     S: 01{mINH}; O: $C0; N: 1; C:  5; R: rNIL; T: %10 ), // RNZ
  ( M: 'RET';  A: 'Z';      S: 01{mINH}; O: $C8; N: 1; C:  5; R: rNIL; T: %10 ), // RZ
  ( M: 'RET';  A: 'NC';     S: 01{mINH}; O: $D0; N: 1; C:  5; R: rNIL; T: %10 ), // RNC
  ( M: 'RET';  A: 'C';      S: 01{mINH}; O: $D8; N: 1; C:  5; R: rNIL; T: %10 ), // RC
  ( M: 'RET';  A: 'PO';     S: 01{mINH}; O: $E0; N: 1; C:  5; R: rNIL; T: %10 ), // RPO
  ( M: 'RET';  A: 'PE';     S: 01{mINH}; O: $E8; N: 1; C:  5; R: rNIL; T: %10 ), // RPE
  ( M: 'RET';  A: 'P';      S: 01{mINH}; O: $F0; N: 1; C:  5; R: rNIL; T: %10 ), // RP
  ( M: 'RET';  A: 'M';      S: 01{mINH}; O: $F8; N: 1; C:  5; R: rNIL; T: %10 ), // RM
  ( M: 'RET';  A: '';       S: 01{mINH}; O: $C9; N: 1; C: 10; R: rNIL; T: %10 ), // RET

  // Restart
  ( M: 'RST';  A: '0';      S: 01{mINH}; O: $C7; N: 1; C: 11; R: rNIL; T: %10 ), // RST n
  ( M: 'RST';  A: '1';      S: 01{mINH}; O: $CF; N: 1; C: 11; R: rNIL; T: %10 ),
  ( M: 'RST';  A: '2';      S: 01{mINH}; O: $D7; N: 1; C: 11; R: rNIL; T: %10 ),
  ( M: 'RST';  A: '3';      S: 01{mINH}; O: $DF; N: 1; C: 11; R: rNIL; T: %10 ),
  ( M: 'RST';  A: '4';      S: 01{mINH}; O: $E7; N: 1; C: 11; R: rNIL; T: %10 ),
  ( M: 'RST';  A: '5';      S: 01{mINH}; O: $EF; N: 1; C: 11; R: rNIL; T: %10 ),
  ( M: 'RST';  A: '6';      S: 01{mINH}; O: $F7; N: 1; C: 11; R: rNIL; T: %10 ),
  ( M: 'RST';  A: '7';      S: 01{mINH}; O: $FF; N: 1; C: 11; R: rNIL; T: %10 ),

  // Increment and Decrement
  ( M: 'INC';  A: 'B';      S: 02{mReg}; O: $04; N: 1; C:  5; R: rNIL; T: %10 ), // INR reg
  ( M: 'INC';  A: 'C';      S: 02{mReg}; O: $0C; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'INC';  A: 'D';      S: 02{mReg}; O: $14; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'INC';  A: 'E';      S: 02{mReg}; O: $1C; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'INC';  A: 'H';      S: 02{mReg}; O: $24; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'INC';  A: 'L';      S: 02{mReg}; O: $2C; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'INC';  A: 'A';      S: 02{mReg}; O: $3C; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'INC';  A: 'BC';     S: 02{mReg}; O: $03; N: 1; C:  5; R: rNIL; T: %10 ), // INX B
  ( M: 'INC';  A: 'DE';     S: 02{mReg}; O: $13; N: 1; C:  5; R: rNIL; T: %10 ), // INX D
  ( M: 'INC';  A: 'HL';     S: 02{mReg}; O: $23; N: 1; C:  5; R: rNIL; T: %10 ), // INX H
  ( M: 'INC';  A: '(HL)';   S: 02{mReg}; O: $34; N: 1; C: 10; R: rNIL; T: %10 ), // INR M
  ( M: 'INC';  A: 'SP';     S: 02{mReg}; O: $33; N: 1; C:  5; R: rNIL; T: %10 ), // INX SP

  ( M: 'DEC';  A: 'B';      S: 02{mReg}; O: $05; N: 1; C:  5; R: rNIL; T: %10 ), // DCR reg
  ( M: 'DEC';  A: 'C';      S: 02{mReg}; O: $0D; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'DEC';  A: 'D';      S: 02{mReg}; O: $15; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'DEC';  A: 'E';      S: 02{mReg}; O: $1D; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'DEC';  A: 'H';      S: 02{mReg}; O: $25; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'DEC';  A: 'L';      S: 02{mReg}; O: $2D; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'DEC';  A: 'A';      S: 02{mReg}; O: $3D; N: 1; C:  5; R: rNIL; T: %10 ),
  ( M: 'DEC';  A: 'BC';     S: 02{mReg}; O: $0B; N: 1; C:  5; R: rNIL; T: %10 ), // DCX B
  ( M: 'DEC';  A: 'DE';     S: 02{mReg}; O: $1B; N: 1; C:  5; R: rNIL; T: %10 ), // DCX D
  ( M: 'DEC';  A: 'HL';     S: 02{mReg}; O: $2B; N: 1; C:  5; R: rNIL; T: %10 ), // DCX H
  ( M: 'DEC';  A: '(HL)';   S: 02{mReg}; O: $35; N: 1; C: 10; R: rNIL; T: %10 ), // DCR M
  ( M: 'DEC';  A: 'SP';     S: 02{mReg}; O: $3B; N: 1; C:  5; R: rNIL; T: %10 ), // DCX SP

  // Add
  ( M: 'ADD';  A: 'A,B';    S: 02{mReg}; O: $80; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADD';  A: 'A,C';    S: 02{mReg}; O: $81; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADD';  A: 'A,D';    S: 02{mReg}; O: $82; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADD';  A: 'A,E';    S: 02{mReg}; O: $83; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADD';  A: 'A,H';    S: 02{mReg}; O: $84; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADD';  A: 'A,L';    S: 02{mReg}; O: $85; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADD';  A: 'A,(HL)'; S: 02{mReg}; O: $86; N: 1; C: 7;  R: rNIL; T: %10 ), // ADD M
  ( M: 'ADD';  A: 'A,A';    S: 02{mReg}; O: $87; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADD';  A: 'A,*';    S: 03{mIMM}; O: $C6; N: 2; C: 7;  R: rNIL; T: %10 ), // ADI

  ( M: 'ADD';  A: 'HL,BC';  S: 02{mReg}; O: $09; N: 1; C: 10; R: rNIL; T: %10 ), // DAD B
  ( M: 'ADD';  A: 'HL,DE';  S: 02{mReg}; O: $19; N: 1; C: 10; R: rNIL; T: %10 ), // DAD D
  ( M: 'ADD';  A: 'HL,HL';  S: 02{mReg}; O: $29; N: 1; C: 10; R: rNIL; T: %10 ), // DAD H
  ( M: 'ADD';  A: 'HL,SP';  S: 02{mReg}; O: $39; N: 1; C: 10; R: rNIL; T: %10 ), // DAD SP

  ( M: 'ADC';  A: 'A,B';    S: 02{mReg}; O: $88; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADC';  A: 'A,C';    S: 02{mReg}; O: $89; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADC';  A: 'A,D';    S: 02{mReg}; O: $8A; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADC';  A: 'A,E';    S: 02{mReg}; O: $8B; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADC';  A: 'A,H';    S: 02{mReg}; O: $8C; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADC';  A: 'A,L';    S: 02{mReg}; O: $8D; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADC';  A: 'A,(HL)'; S: 02{mReg}; O: $8E; N: 1; C: 7;  R: rNIL; T: %10 ), // ADC M
  ( M: 'ADC';  A: 'A,A';    S: 02{mReg}; O: $8F; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'ADC';  A: 'A,*';    S: 03{mIMM}; O: $CE; N: 2; C: 7;  R: rNIL; T: %10 ), // ACI

  // Sub
  ( M: 'SUB';  A: 'B';      S: 02{mReg}; O: $90; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SUB';  A: 'C';      S: 02{mReg}; O: $91; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SUB';  A: 'D';      S: 02{mReg}; O: $92; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SUB';  A: 'E';      S: 02{mReg}; O: $93; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SUB';  A: 'H';      S: 02{mReg}; O: $94; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SUB';  A: 'L';      S: 02{mReg}; O: $95; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SUB';  A: '(HL)';   S: 02{mReg}; O: $96; N: 1; C: 7;  R: rNIL; T: %10 ), // SUB M
  ( M: 'SUB';  A: 'A';      S: 02{mReg}; O: $97; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SUB';  A: '*';      S: 03{mIMM}; O: $D6; N: 2; C: 7;  R: rNIL; T: %10 ), // SUI

  ( M: 'SBC';  A: 'A,B';    S: 02{mReg}; O: $98; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SBC';  A: 'A,C';    S: 02{mReg}; O: $99; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SBC';  A: 'A,D';    S: 02{mReg}; O: $9A; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SBC';  A: 'A,E';    S: 02{mReg}; O: $9B; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SBC';  A: 'A,H';    S: 02{mReg}; O: $9C; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SBC';  A: 'A,L';    S: 02{mReg}; O: $9D; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SBC';  A: 'A,(HL)'; S: 02{mReg}; O: $9E; N: 1; C: 7;  R: rNIL; T: %10 ), // SBB M
  ( M: 'SBC';  A: 'A,A';    S: 02{mReg}; O: $9F; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'SBC';  A: 'A,*';    S: 03{mIMM}; O: $DE; N: 2; C: 7;  R: rNIL; T: %10 ), // SBI

  // Logical
  ( M: 'AND';  A: 'B';      S: 02{mReg}; O: $A0; N: 1; C: 4;  R: rNIL; T: %10 ), // ANA reg
  ( M: 'AND';  A: 'C';      S: 02{mReg}; O: $A1; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'AND';  A: 'D';      S: 02{mReg}; O: $A2; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'AND';  A: 'E';      S: 02{mReg}; O: $A3; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'AND';  A: 'H';      S: 02{mReg}; O: $A4; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'AND';  A: 'L';      S: 02{mReg}; O: $A5; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'AND';  A: '(HL)';   S: 02{mReg}; O: $A6; N: 1; C: 7;  R: rNIL; T: %10 ), // ANA M
  ( M: 'AND';  A: 'A';      S: 02{mReg}; O: $A7; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'AND';  A: '*';      S: 03{mIMM}; O: $E6; N: 2; C: 7;  R: rNIL; T: %10 ), // ANI

  ( M: 'XOR';  A: 'B';      S: 02{mReg}; O: $A8; N: 1; C: 4;  R: rNIL; T: %10 ), // XRA reg
  ( M: 'XOR';  A: 'C';      S: 02{mReg}; O: $A9; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'XOR';  A: 'D';      S: 02{mReg}; O: $AA; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'XOR';  A: 'E';      S: 02{mReg}; O: $AB; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'XOR';  A: 'H';      S: 02{mReg}; O: $AC; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'XOR';  A: 'L';      S: 02{mReg}; O: $AD; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'XOR';  A: '(HL)';   S: 02{mReg}; O: $AE; N: 1; C: 7;  R: rNIL; T: %10 ), // XRA M
  ( M: 'XOR';  A: 'A';      S: 02{mReg}; O: $AF; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'XOR';  A: '*';      S: 03{mIMM}; O: $EE; N: 2; C: 7;  R: rNIL; T: %10 ), // XRI

  ( M: 'OR';   A: 'B';      S: 02{mReg}; O: $B0; N: 1; C: 4;  R: rNIL; T: %10 ), // ORA reg
  ( M: 'OR';   A: 'C';      S: 02{mReg}; O: $B1; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'OR';   A: 'D';      S: 02{mReg}; O: $B2; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'OR';   A: 'E';      S: 02{mReg}; O: $B3; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'OR';   A: 'H';      S: 02{mReg}; O: $B4; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'OR';   A: 'L';      S: 02{mReg}; O: $B5; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'OR';   A: '(HL)';   S: 02{mReg}; O: $B6; N: 1; C: 7;  R: rNIL; T: %10 ), // ORA M
  ( M: 'OR';   A: 'A';      S: 02{mReg}; O: $B7; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'OR';   A: '*';      S: 03{mIMM}; O: $F6; N: 2; C: 7;  R: rNIL; T: %10 ), // ORI

  ( M: 'CP';   A: 'B';      S: 02{mReg}; O: $B8; N: 1; C: 4;  R: rNIL; T: %10 ), // CMP reg
  ( M: 'CP';   A: 'C';      S: 02{mReg}; O: $B9; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'CP';   A: 'D';      S: 02{mReg}; O: $BA; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'CP';   A: 'E';      S: 02{mReg}; O: $BB; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'CP';   A: 'H';      S: 02{mReg}; O: $BC; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'CP';   A: 'L';      S: 02{mReg}; O: $BD; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'CP';   A: '(HL)';   S: 02{mReg}; O: $BE; N: 1; C: 7;  R: rNIL; T: %10 ), // CMP M
  ( M: 'CP';   A: 'A';      S: 02{mReg}; O: $BF; N: 1; C: 4;  R: rNIL; T: %10 ),
  ( M: 'CP';   A: '*';      S: 03{mIMM}; O: $FE; N: 2; C: 7;  R: rNIL; T: %10 ), // CPI

  // Rotate
  ( M: 'RLCA'; A: '';       S: 01{mINH}; O: $07; N: 1; C: 4;  R: rNIL; T: %10 ), // RLC
  ( M: 'RRCA'; A: '';       S: 01{mINH}; O: $0F; N: 1; C: 4;  R: rNIL; T: %10 ), // RRC
  ( M: 'RLA';  A: '';       S: 01{mINH}; O: $17; N: 1; C: 4;  R: rNIL; T: %10 ), // RAL
  ( M: 'RRA';  A: '';       S: 01{mINH}; O: $1F; N: 1; C: 4;  R: rNIL; T: %10 ), // RAR

  // Specials
  ( M: 'CPL';  A: '';       S: 01{mINH}; O: $2F; N: 1; C: 4;  R: rNIL; T: %10 ), // CMA
  ( M: 'SCF';  A: '';       S: 01{mINH}; O: $37; N: 1; C: 4;  R: rNIL; T: %10 ), // STC
  ( M: 'CCF';  A: '';       S: 01{mINH}; O: $3F; N: 1; C: 4;  R: rNIL; T: %10 ), // CMC
  ( M: 'DAA';  A: '';       S: 01{mINH}; O: $27; N: 1; C: 4;  R: rNIL; T: %10 ),

  // Input / Output
  ( M: 'OUT';  A: '(*),A';  S: 03{mIMM}; O: $D3; N: 2; C: 10; R: rNIL; T: %10 ),
  ( M: 'IN';   A: 'A,(*)';  S: 03{mIMM}; O: $DB; N: 2; C: 10; R: rNIL; T: %10 ),

  // Control
  ( M: 'EI';   A: '';       S: 01{mINH}; O: $FB; N: 1; C:  4; R: rNIL; T: %10 ),
  ( M: 'DI';   A: '';       S: 01{mINH}; O: $F3; N: 1; C:  4; R: rNIL; T: %10 ),
  ( M: 'NOP';  A: '';       S: 01{mINH}; O: $00; N: 1; C:  4; R: rNIL; T: %10 ),
  ( M: 'HALT'; A: '';       S: 01{mINH}; O: $76; N: 1; C: 7;  R: rNIL; T: %10 )  // HLT

