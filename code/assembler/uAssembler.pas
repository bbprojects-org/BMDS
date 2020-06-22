{ ==============================================================================

  ASSEMBLER

    This is the main assembler which processes each token, taking the
    appropriate action, including:
    - managing # directives
    - processing macros
    - scan lines checking labels opcodes and operands, generating code
    - producing listing


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

  { TODO : uAssembler -> rework BuildIdentifierList to use new TDictionary routines }
  { TODO : uAssembler -> for DoWord, follow ENDIAN flag to define order of bytes for
                       each specific CPU }
  { TODO : uAssembler -> stop spare line after macro definition, same line # }
  { TODO : uAssembler -> add DEFINE to create pseudonym for given text,
                         e.g. CHIP8 register. Replaced by parser? }

//
// Have spcific CPU functionality in CPU units, supporting following Public:
//   INIT      - setup opcode tables (if not already done), bigendian, etc
//   MNEMONIC  - process mnemonic passed
//   DIRECTIVE - process unknown directive passed
//   FINISHED  - do any cleanup required
//

unit uAssembler;

{$mode objfpc}{$H+}
{.$define assembler_debug}

interface

uses Forms, Classes, SysUtils, FileUtil, SynEdit, SynPluginSyncroEdit, Controls,
     Graphics, Dialogs, Menus, ExtCtrls, ActnList, LCLType, Math,
     //
    {$ifdef assembler_debug}
    uCommon,
    {$endif}
     uParser, uSymbols, uAsmListing, uAsmFiles, uAsmPrefsFrame,
     uPreferencesForm, uCpuTypes, uDefs6502, uDefs8080, uDefsChip8, uMachineBase,
     uAsmErrors;

type
  TMemSect = (msRAM, msCode);

  TOnLogEvent = procedure(Msg: string; DestMain: boolean) of object;

  TBytes256 = array [1..256] of Byte;

  TLineInfo = record
    SourceIndex: integer;               // Index to source file filename (=0 if mainfile)
    LineNumber: integer;                // Current line being processed
    MachineAddr: LongWord;              // PC address for this line of code
    FileName: string;                   // Current file being parsed
  end;
  TArrayLineInfo = array of TLineInfo;

  TItemData = record                    // Used by Identifers, Errors, Warnings
    SourceIndex: integer;               // Index to source file filename (=0 if mainfile)
    LineNumber: integer;                // Current line being processed
    StringData: string;                 // Value of string
  end;
  TArrayIdentfiers = array of TItemData;

  { TAssembler }

  TAssembler = class(TObject)
  private
    fPassNumber: integer;               // Current pass number; 1 or 2
    fFiles: TFiles;                     // Source files manager
    fListing: TListing;                 // Manages assembler listing
    fErrors: TErrors;                   // Manages assembler errors
    fSymbolTable: TSymbols;             // Manages symbols
    fIdentifiers: TArrayIdentfiers;     // Used to return identifiers list
    fLinesInfo: TArrayLineInfo;         // Array used to hold all lines data for debug
    fParser: TParser;                   // Token parser (line-by-line)
    fCpuName: string;
    fOnLog: TOnLogEvent;                // Callback with status/log info
    //
    PC: integer;                        // Program / location counter
    MemorySection: TMemSect;            // Memory type assembling to
    BytesArray: TBytes256;              // Byte buffer for program code (max 256 bytes)
    NumBytes: integer;                  // Number of bytes being generated (DB/DW)
    LastGlobalLabel: string;            // Last global label defined, supports local labels
    IfStackArray: array of boolean;     // Used to keep state of IF/ELSE/ENDIF
    fIsAssembling: boolean;             // Used by IF/ENDIF routines to determine state
    LineHasLabel: boolean;              // Does current line have a label?
    DefiningMacro: boolean;             // Is a macro currently being defined?
    MacroLevel: integer;                // Need to set 0 during initialisation
    WriteToMemoryError: boolean;
    procedure AddDebugLineInfo;
    procedure AddDirectives;
    procedure AddOpcodes;
    procedure AddToMacro;
    procedure DefineMacro;
    procedure DoEqu;
    procedure DoMacro;
    procedure DoPass(PassNo: Integer; FileName: string);
    procedure DefineLabel;
    procedure ListSymbolTable;
    procedure ProcessInstruction;
    procedure DoOutputs(StartAddr: integer);
    procedure DoMnemonic(Instruction: TInstruction);
    procedure DoOrg;
    procedure DoReserve;
    procedure DoByte;
    procedure DoWord;
    procedure DoText;
    function  AddrOnly(Value: integer): string;
    function  AddrPlus(Bytes: integer; Offset: integer = 0): string;
    function  Expecting(ExpectedTok: TTokenTypes; ExpectedStr: string): boolean;
    procedure HashDirective;
    function  ParseExpr: integer;
    function  ParseTerm: integer;
    function  ParseOperand: integer;
    procedure BuildIdentifiersList;    
    procedure SetAssemblingFlag(state: boolean);
    procedure DoError(Sender: TObject; ErrMsg: string);
    procedure DoLog(msg: string; DestMain: boolean = False);
  public
    constructor Create;
    destructor  Destroy; override;
    //
    procedure Execute(SourceFileName: string);
    //
    property PassNumber: integer read fPassNumber;
    property Files: TFiles read fFiles;
    property Listing: TListing read fListing write fListing;
    property SymbolTable: TSymbols read fSymbolTable;
    property Identifiers: TArrayIdentfiers read fIdentifiers;
    property LinesInfo: TArrayLineInfo read fLinesInfo;   
    property IsAssembling: boolean read fIsAssembling;
    property CpuName: string read fCpuName;
    property OnLog: TOnLogEvent read fOnLog write fOnLog;
  end;


implementation

const
  ASSEMBLY_SUMMARY         = 'Assembly generated %d error%s and %d warning%s';
  SECOND_PASS_ABORTED      = 'Second pass aborted due to %d %s in first pass';
  ASSEMBLING_FOR_PROCESSOR = 'Assembling ''%s'' for processor %s';


{ CREATE }

constructor TAssembler.Create;
begin
  AsmPrefs := PreferencesForm.Frames[ciAsm] as TAsmPrefsFrame;
end;


{ DESTROY }

destructor TAssembler.Destroy;
begin
  SetLength(IfStackArray, 0);
  IfStackArray := nil;
  SetLength(fLinesInfo, 0);
  fLinesInfo := nil;
  SetLength(fIdentifiers, 0);
  fIdentifiers := nil;
  inherited;
end;


{ EXECUTE }

{ Main assembler processor; runs first pass and, if no errors, then runs the
  second pass }

procedure TAssembler.Execute(SourceFileName: string);
var
  errorStr: string;
begin
  fListing := TListing.Create;
  fSymbolTable := TSymbols.Create;
  AddDirectives;
  AddOpcodes;
  fErrors := TErrors.Create;
  fErrors.OnError := @DoError;
  fFiles := TFiles.Create(self, SourceFileName);
  fParser := TParser.Create(@fFiles.GetSourceLine);

  SetLength(fLinesInfo, 0);             // Initialise array of lines data
  SetLength(IfStackArray, 0);           // Initialise IF stack
  DefiningMacro := False;
  MacroLevel := 0;
  WriteToMemoryError := False;

  PC := 0;
  SetAssemblingFlag(True);              // Conditional assembly flag
  try
    fListing.Start(SourceFileName, Machine.CPU.Info.Name);
    DoLog(Format(ASSEMBLING_FOR_PROCESSOR, [ExtractFileName(SourceFileName), Machine.CPU.Info.Name]));
    DoPass(1, SourceFileName);
    if (fErrors.ErrorCount = 0) then    // If no errors after Pass 1
      begin
        DoPass(2, SourceFileName);      // ... then do Pass 2
        fListing.Summary := fErrors.ErrorList + Format(ASSEMBLY_SUMMARY,
                     [fErrors.ErrorCount,   BoolToStr(fErrors.ErrorCount = 1, '', 's'),
                      fErrors.WarningCount, BoolToStr(fErrors.WarningCount = 1, '', 's')]);
        if (AsmPrefs.WriteToMemory and (not WriteToMemoryError)) then
          DoLog(Format('Machine code for "%s" written to %s memory', [ExtractFileName(SourceFileName), Machine.Name]), True); // 'True' = msg for main form
      end
    else
      begin
        errorStr := 'error' + BoolToStr(fErrors.ErrorCount = 1, '', 's');
        fListing.Summary := fErrors.ErrorList + Format(SECOND_PASS_ABORTED, [fErrors.ErrorCount, errorStr]);
      end;
    DoLog(fListing.Summary);
    ListSymbolTable;
    fListing.Finish;                    // List summary and output

    BuildIdentifiersList;
  finally
    fParser.Free;
    fFiles.Free;
    fErrors.Free;
    fSymbolTable.Free;
    fListing.Free;
  end;
end;


{ DO AN ASSEMBLER PASS ON THE SOURCE FILE }

procedure TAssembler.DoPass(PassNo: Integer; FileName: string);
var
  StartPC: integer;
  ExpectingInstruction: boolean;
begin
  {$ifdef assembler_debug}
  AppLog.Debug('');
  AppLog.Debug('TAssembler.DoPass, PASS=%d', [PassNo]);
  {$endif}
  fPassNumber := PassNo;
  fListing.PassNumber := PassNo;
  fFiles.Init;                          // Initialise file stack, etc
  fFiles.OpenFile(FileName);            // Get source text
  PC := 0;                              // Initialise program counter
  fParser.Initialise;
  while (fParser.Token.Typ <> tkEOF) do // Loop until End Of File
    try
      // Start of line, expecting to see one of: label / #directive /
      //   comment / whitespace & instruction / EOL (blank line)
      NumBytes := 0;
      StartPC := PC;
      LineHasLabel := False;
      ExpectingInstruction := True;
      fParser.GetToken;                 // Get first token in line
      fListing.SetLine(fFiles.CurrentFileName, fFiles.CurrentLineNo, fParser.SourceLine);
      fErrors.SetLine(fFiles.CurrentFileName, fFiles.CurrentLineNo);

      // or if in a macro definition, need to add line to that until "endm"
      if (DefiningMacro) then
        begin
          AddToMacro;                   // Processes up to an including EOL
          fListing.ListLine;
          Continue;
        end;

      case fParser.Token.Typ of
        tkLabel, tkLocal:               // Label?
          begin
            LineHasLabel := True;
            fListing.LabelStr := fParser.Token.StringVal;
            DefineLabel;
            if (fParser.PeekNextToken.Typ in [tkComment, tkEOL]) then
              begin
                ExpectingInstruction := False; // Just a label, no instruction
                fListing.HexData := AddrOnly(PC); // show label address
              end
            else
              fParser.GetToken;         // Expecting instruction next
          end;

        tkHashDirective:                // #directive?
          begin
            fListing.LabelStr := '#' + fParser.Token.StringVal;
            HashDirective;
            ExpectingInstruction := False;
          end;

        tkComment:                      // Comment?
          ExpectingInstruction := False; // No action, just list comments

        tkEOL:                          // EOL = blank line?
          begin
            fListing.SourceLine := '';
            fListing.ListLine;          // Just list the blank line
            Continue;                   // ... and loop to next
          end;

        tkEOF: Continue;                // EOF = no more lines to process

        // None of the above, must be instruction, so drop through
      end;

      if (ExpectingInstruction) then
        begin                
          fListing.OpcodeStr := fParser.Token.StringVal;
          ProcessInstruction;
        end;

      // Skip comment on line, list line, output data
      if (fParser.PeekNextToken.Typ = tkComment) then
        begin
          fParser.GetToken;             // Skip comment      
          fListing.CommentStr := fParser.Token.StringVal;
        end;

      // Should be no more on line, so check if it is actually EOL
      if Expecting([tkEOL], 'EOL') then
        fParser.GetToken;               // Get EOL
      fListing.ListLine;                // List assembled line
      DoOutputs(StartPC);               // Output data to memory / file

    except
      on E: Exception do fErrors.AddError(E.Message);
    end;
end;


{ PROCESS AN INSTRUCTION }

{ Either a mnemonic, assembler directive, or a macro call }

procedure TAssembler.ProcessInstruction;
var
  StrVal: string;
  OperandStart: integer;
  Instruction: TInstruction;
begin
  if (not IsAssembling) then
    begin
      fParser.SkipRestOfLine;
      Exit;
    end;

  OperandStart := fParser.PeekNextToken.StartPos;
  case (fParser.Token.Typ) of
    tkId, tkDotId:
      begin
        if (fParser.Token.Typ = tkDotId) then
          // If .Directive then strip leading '.'
          StrVal := RightStr(fParser.Token.StringVal, Length(fParser.Token.StringVal)-1)
        else
          StrVal := fParser.Token.StringVal;
        StrVal := UpperCase(StrVal);
        Instruction := fSymbolTable.FindInstruction(StrVal);
        if (Instruction = nil) then
          begin
            fErrors.AddErrorFmt(emInstrNotRecognised, [fParser.Token.StringVal]);
            Exit;
          end;
      end;

    tkEqual: fSymbolTable.FindInstruction('EQU'); // Translate '=' -> 'equ'

  else
    fErrors.AddErrorFmt(emInstrExpected, [fParser.Token.StringVal]);
    Exit;
  end;

  case fSymbolTable.Instruction.InstructionType of
    itMnem:    DoMnemonic(Instruction); // Process mnemonic
    itOrg:     DoOrg;                   // Set location counter
    itReserve: DoReserve;               // Reserve space for variables, etc
    itByte:    DoByte;                  // Define byte values
    itWord:    DoWord;                  // Define word values
    itText:    DoText;                  // Define text string
    itMacro:   DoMacro;                 // Expand a macro
    itEqu:     DoEqu;                   // Assign value to a label
    itEnd:     begin
                 // If END encountered, flag it here. At next GetSourceLine
                 // this flag will cause the current file to close, ignoring
                 // any other lines after the END statement in the current file
                 fFiles.FlagEnd := True;
               end;
  end;
  if (fIsAssembling) then
    Inc(PC, NumBytes);

  // At this point the operand has been examined as appropriate, so copy
  // it to the OperandStr
  fListing.OperandStr := fParser.GetLineText(OperandStart, fParser.Token.EndPos);
end;


{ DO OUTPUTS }

procedure TAssembler.DoOutputs(StartAddr: integer);
var
  i, nOffset, nNumOf3, nRem, WriteAddr: integer;
begin
  // If pass 2 and option selected, then write code to memory / file
  if ((fPassNumber = 2) and (fIsAssembling)) then
    for i := 1 to NumBytes do
      begin
        if (AsmPrefs.WriteToMemory and (not WriteToMemoryError)) then
          begin
            WriteAddr := StartAddr + i - 1;
            if (Machine.IsRAM(WriteAddr)) then
              Machine.Memory[WriteAddr] := BytesArray[i]
            else
              begin
                WriteToMemoryError := True;
                fErrors.AddWarningFmt(emNotRam, [WriteAddr], True);
              end;
          end;
        if (AsmPrefs.WriteToFile) then
          fFiles.WriteDataByte(BytesArray[i]);
      end;

  if (fIsAssembling and (NumBytes > 3)) then // Multibytes?
    begin
      fListing.IsMultiBytes := True;
      nNumOf3 := NumBytes div 3;
      nRem := NumBytes mod 3;
      nOffset := 3;
      while (nOffset < (nNumOf3 * 3)) do
        begin
          PC := StartAddr + nOffset;    // Need to adjust PC
          fListing.HexData := AddrPlus(3, nOffset);
          fListing.SourceLine := '+';   // Mark multibyte line
          fListing.ListLine;
          Inc(nOffset,3);
        end;
      if (nRem > 0) then
        begin
          PC := StartAddr + nOffset;
          fListing.HexData := AddrPlus(nRem, nOffset);
          fListing.SourceLine := '+';   // Mark multibyte line
          fListing.ListLine;
        end;
      PC := StartAddr + NumBytes;       // Reset program counter
      fListing.IsMultiBytes := False;
    end;
end;


{ PROCESS A MNEMONIC }

{ TODO : uAssembler -> have DoMenomic call CPU specific code }

procedure TAssembler.DoMnemonic(Instruction: TInstruction);
var
  nDataIdx, nOffset: integer;
  Value: word;
  sAddrMode: string;
  bDoneExpr: boolean;
  ThisData: TOpcodeRawData;
begin
  AddDebugLineInfo;

  // Given the instruction, parse the operand text to build an address mode for
  // checking against CPU opcode data array. Source text checked for CPU
  // register addresses, operand value (replaced by *) or any other non-space
  // text just added to the address mode mask
  nDataIdx := Instruction.Value;
  sAddrMode := '';
  bDoneExpr := False;
  Value := 0;

  // Build address mode for checking
  while not (fParser.PeekNextToken.Typ in [tkComment, tkEOL]) do
    begin
      case fParser.PeekNextToken.Typ of // Look for any token that could be
       tkId,                            // part of an expression including
       tkDotId,                         // unary operators
       tkNumber,
       tkString,                        // Single character only
       tkPlus,
       tkMinus,
       tkGreater,
       tkLower:   if ((fParser.PeekNextToken.Typ = tkId) and (Pos(Uppercase(fParser.PeekNextToken.StringVal), Machine.CPU.Info.Registers) > 0)) then
                    begin
                      fParser.GetToken;
                      sAddrMode := sAddrMode + UpperCase(fParser.Token.StringVal);
                    end
                  else
                    begin
                      if (bDoneExpr = True) then
                        begin
                          fErrors.AddError(emOperandNotFound);
                          Exit;
                        end
                      else
                        begin
                          Value := ParseExpr;
                          sAddrMode := sAddrMode + '*';
                        end;
                      bDoneExpr := True;
                    end;

       tkComment: begin
                    fParser.GetToken;
                    break;
                  end;
      else
        begin
          fParser.GetToken;
          sAddrMode := sAddrMode + fParser.Token.StringVal;
        end;
      end;
    end;

  {$ifdef assembler_debug}
  AppLog.Debug('TAssembler.DoMnemonic, %.4d, %s, AM=[%s]', [fFiles.CurrentLineNo + 1, Instruction.Name, sAddrMode]);
  {$endif}


  // Now check address mode against those in OpcodesList. Note that the
  // opcode data has all the address modes for a specific mnemonic grouped
  // together hence when name changes all address modes for that mnemonic
  // have been checked

  ThisData := Machine.CPU.DataByIndex[nDataIdx];
  while (ThisData.A <> sAddrMode) and (ThisData.M = Instruction.Name) do
    begin
      Inc(nDataIdx);
      ThisData := Machine.CPU.DataByIndex[nDataIdx];
    end;

  // If still pointing at the current instruction mnemonic, can then get
  // opcode value and assign the operand bytes too. Although assign three
  // bytes, not all will apply depending on particular opcode
  if (ThisData.M = Instruction.Name) then
    begin
      NumBytes := ThisData.N;
      { TODO : uAssembler -> need to cater for opcodes > 1 byte }
      BytesArray[1] := ThisData.O;
      BytesArray[2] := Lo(Value);
      BytesArray[3] := Hi(Value);
    end
  else
    fErrors.AddErrorFmt(emAddrModeNotRecognised, [sAddrMode]);

  // Check for any actions modifying operand
  case ThisData.R of
    rNIL: ;                           // Do nothing
    rZP:  if (BytesArray[3] = 0) then // If zero page (hi byte = 0)
            begin
              Dec(NumBytes);          // ... reduce to two bytes
              // Get opcode from next entry which is ZP version
              BytesArray[1] := Machine.CPU.DataByIndex[nDataIdx+1].O and $FF;
            end;
    rCR: ;
    rREL: begin
            nOffset := Value - (PC + 2);
            if (nOffset >= -$7f) and (nOffset <= $80) then
              BytesArray[2] := nOffset and $FF
            else
              fErrors.AddError(emBranchTooFar);
          end;
    { TODO : uAssembler -> process registers }
  end;
  fListing.HexData := AddrPlus(NumBytes);
end;


procedure TAssembler.AddDebugLineInfo;
var
  nLen: integer;
begin
  // Debug support; builds array with an element for each executable line
  // with program counter address, source line number, and source filename
  // Used by debug to show relevant sourceline when it stops at an address
  if (fIsAssembling) then
    begin
      nLen := Length(fLinesInfo);       // Only add lines data if executable (i.e. mnemonic)
      SetLength(fLinesInfo, nLen + 1);
      fLinesInfo[nLen].SourceIndex := fFiles.CurrentFileIndex;
      fLinesInfo[nLen].LineNumber := fFiles.CurrentLineNo + 1; // Zero based
      fLinesInfo[nLen].MachineAddr := PC;
    end;
end;

{ DO ORG INSTRUCTION }

{ Set program/location counter to expression operand. Can also have optional
  RAM/CODE to identify type of memory to which follow-on code applies }

procedure TAssembler.DoOrg;
var
  sText: string;
begin
  PC := ParseExpr;                      // Set PC this value
  fListing.HexData := AddrOnly(PC);
  MemorySection := msCode;              // Default mode is CODE
  // Check if memory type is specified
  if (fParser.PeekNextToken.Typ = tkComma) then
    begin
      fParser.GetToken;                 // Skip comma
      if Expecting([tkId], 'identifier') then
        begin
          fParser.GetToken;             // Get memory type
          sText := UpperCase(fParser.Token.StringVal);
          if (sText = 'RAM') then
            MemorySection := msRAM
          else if (sText <> 'CODE') then
            begin
              fErrors.AddErrorFmt(emMemModeNotRecognised, [fParser.Token.StringVal]);
              Exit;                     // Exit on mode not recognised error
            end;
        end
      else
        Exit;                           // Exit on missing identifier error
    end;
  if (MemorySection = msCode) then
    fFiles.SetDataStart(PC);            // Set object code addr
end;


{ DO RESERVE INSTRUCTION }

{ Reserve space for data/variables. If memory mode = RAM then it just increments
  the PC, but if memory mode = CODE then bytes are written to the output data.
  These bytes default to zero, but can be set as an optional value following the
  space declaration }

procedure TAssembler.DoReserve;
var
  idx, Operand, Count: integer;
  FillByte: byte;
begin
  fListing.HexData := AddrOnly(PC);
  NumBytes := 0;
  Operand := ParseExpr;
  FillByte := 0;                        // Default fill value = 0
  if (fParser.PeekNextToken.Typ = tkComma) then
    begin
      fParser.GetToken;                 // Skip comma
      FillByte := ParseExpr;            // Get user fill value
    end;
  if ((MemorySection = msCode) and fIsAssembling) then // If CODE section then
    begin
      for idx := 1 to Operand do
        fFiles.WriteDataByte(FillByte); // ... write to output file
      // Warn just in case user actually intended this for RAM rather than CODE
      fErrors.AddWarning(emReserveInCode, True);
      Count := Min(Operand, 3);         // Limit for listing
      for idx := 1 to Count do          // Save bytes for listing
        BytesArray[idx] := FillByte;
      fListing.HexData := AddrPlus(Count);
      // Warn user if reserving more than 3 bytes space, as only showing 3
      if (Operand > 3) then
        fErrors.AddWarning(emLimitedBytesList, True);
    end;

  Inc(PC, Operand);                     // Set PC for space reserved
end;


{ DO BYTE INSTRUCTION }

{ Define bytes to be written. Values are 8-bit expressions separated by commas }

{ TODO : uAssembler -> if string too long, issue warning and truncate }

procedure TAssembler.DoByte;
var
  i: integer;
  Value: word;
begin
  while (True) do
    begin
      if (fParser.PeekNextToken.Typ = tkString) then
        begin
          fParser.GetToken;
          for i := 1 to Length(fParser.Token.StringVal) do
            begin
              Inc(NumBytes);
              BytesArray[NumBytes] := Ord(fParser.Token.StringVal[i]);
            end;
        end
      else
        begin
          Value := ParseExpr;           // Get byte value in operand field
          Inc(NumBytes);
          BytesArray[NumBytes] := Lo(Value);
        end;
      if (fParser.PeekNextToken.Typ = tkComma) then
        fParser.GetToken                // Skip comma
      else
        break;
    end;
  fListing.HexData := AddrPlus(NumBytes);
end;


{ DO WORD INSTRUCTION }

{ Define words to be written. Values are 16-bit expressions separated by commas
  and the byte order follows the CPU's endian flag }

procedure TAssembler.DoWord;
var
  Value: word;
begin
  while (True) do
    begin
      Value := ParseExpr;               // Get word value in operand field
      Inc(NumBytes, 2);
      BytesArray[NumBytes-1] := Lo(Value);
      BytesArray[NumBytes] := Hi(Value);
      if (fParser.PeekNextToken.Typ = tkComma) then
        fParser.GetToken                // Skip comma
      else
        Break;
    end;
  fListing.HexData := AddrPlus(NumBytes);
end;


{ DO TEXT INSTRUCTION }

{  Define a text string }

procedure TAssembler.DoText;
var
  idx: integer;
  sText: string;
begin
  if Expecting([tkString], 'string') then
    begin
      fParser.GetToken;
      sText := fParser.Token.StringVal;
      if (Length(sText) > 0) then
        begin
          for idx := 1 to Length(sText) do
            BytesArray[idx] := ord(sText[idx]);
          NumBytes := Length(sText);
          fListing.HexData := AddrPlus(NumBytes);
        end;
    end;
end;


{ DO EQUATE INSTRUCTION }

procedure TAssembler.DoEqu;
var
  ThisSymbol: TSymbol;
begin
  if (not fIsAssembling) then
    begin
      ParseExpr;                        // Just get expression, and leave
      Exit;
    end;

  // Define a constant value. A label is assigned the value of
  // the expression that follows it
  if (LineHasLabel) then
    begin
      ThisSymbol := fSymbolTable.Symbol;
      ThisSymbol.Value := ParseExpr;
      ThisSymbol.Use := [symDefine, symSet];
      fListing.HexData := AddrOnly(ThisSymbol.Value);
      {$ifdef assembler_debug}
      AppLog.Debug('TAssembler.DoEqu [%s = %.4x]', [ThisSymbol.Name, ThisSymbol.Value]);
      {$endif}
    end
  else
    fErrors.AddError(emLabelMissing);
end;


{ GENERATE FORMATTED CODE LISTING - either address only, or address plus data }

function TAssembler.AddrOnly(Value: integer): string;
begin
  if (fPassNumber = 2) then             // Only output text on second pass
    Result := Format('%.4x', [Value])
  else
    Result := '';
end;


function TAssembler.AddrPlus(Bytes: integer; Offset: integer): string;
var
  i: integer;
begin
  if (fPassNumber = 2) then
    begin
      Result := Format('%.4x ', [PC]);
      Bytes := Min(Bytes, 3);           // Max 3 bytes output here
      for i := 1 to Bytes do
        // Offset defaults to 0 unless set otherwise (i.e. multibytes)
        Result := Result + Format(' %.2x', [BytesArray[Offset + i]]);
    end
  else
    Result := '';
end;


{ EXPECTING }

{ Check next token is of the expected type; if not, then flag error }

function TAssembler.Expecting(ExpectedTok: TTokenTypes; ExpectedStr: string): boolean;
begin
  Result := (fParser.PeekNextToken.Typ in ExpectedTok);
  if (not Result) then
    fErrors.AddErrorFmt(emExpectedNotFound, [ExpectedStr, fParser.PeekNextToken.StringVal]);
end;


{ DEFINE LABEL }

{ Add currently parsed symbol to the symbol table. On first pass it adds the
  string and program counter. On the second pass it checks that the PC has the
  same value, else raises a phasing error message
  If it is a local label, the LastGlobalLabel name is added as a prefix to
  give a composite (and unique) label name }

procedure TAssembler.DefineLabel;
var
  SymbolStr: string;
  ThisSymbol: TSymbol;
begin
  if (not fIsAssembling) then Exit;

  if (fParser.Token.Typ = tkLocal) then
    // Local label, so add last global label as prefix
    SymbolStr := LastGlobalLabel + fParser.Token.StringVal
  else
    // Global label, save as prefix for next local label
    begin
      SymbolStr := fParser.Token.StringVal;
      LastGlobalLabel := SymbolStr;
    end;

  if (fPassNumber = 1) then
    fSymbolTable.AddSymbol(SymbolStr, PC, [symLabel, symSet], 0)  // 0 = dummy line number
  else
    begin
      ThisSymbol := fSymbolTable.FindSymbol(SymbolStr);
      ThisSymbol.Line := fFiles.CurrentLineNo + 1;       // Zero based
      ThisSymbol.SourceIndex := fFiles.CurrentFileIndex; // and file index
      if (symLabel in ThisSymbol.Use) and (ThisSymbol.Value <> PC) then
        // Only report this error once, hence the 'True' parameter
        fErrors.AddErrorFmt(emPhasingError, [ThisSymbol.Value, PC], True);
    end;
end;


{ HASH DIRECTIVE }

{ Process the current # directive; INCLUDE/IF/ELSE/ENDIF/etc }

procedure TAssembler.HashDirective;
var
  DirectiveName, FileName: string;
  Len: integer;
  OperandStart: integer;
begin
  DirectiveName := UpperCase(fParser.Token.StringVal);  
  OperandStart := fParser.PeekNextToken.StartPos;

  if (DirectiveName = 'INCLUDE') then
    begin

      if Expecting([tkString], 'filename string') then
        begin
          fParser.GetToken;                            
          fListing.OperandStr := '"' + fParser.Token.StringVal + '"';
          // Assumes INCLUDE file is in same folder as main source file
          FileName := ExtractFilePath(fFiles.SourceFiles[0].FileName) + fParser.Token.StringVal;
          (*
          // If next token = comment, get it here
          if (fParser.PeekNextToken.Typ = tkComment) then
            begin
              fParser.GetToken;  
              fListing.CommentStr := fParser.Token.StringVal;;
            end;
          fParser.PeekNextToken;        // Should be EOL
          *)
          if (FileExists(FileName)) then
            fFiles.OpenFile(FileName)
          else if (fPassNumber = 1) then
            fErrors.AddErrorFmt(emIncludeNotFound, [FileName]);
        end;
    end

  else if (DirectiveName = 'IF') then
    begin
      Len := Length(IfStackArray);
      SetLength(IfStackArray, Len + 1);  // Make room for new item on stack
      IfStackArray[Len] := fIsAssembling; // ... and 'push' it on
      // If expression FALSE, stop assembling until ELSE/ENDIF
      SetAssemblingFlag(fIsAssembling and (ParseExpr <> 0));
      fListing.OperandStr := fParser.GetLineText(OperandStart, fParser.Token.EndPos);
    end

  else if (DirectiveName = 'ELSE') then
    begin
      if (Length(IfStackArray) = 0) then
        fErrors.AddError(emElseWithoutIf)
      else
        SetAssemblingFlag(not fIsAssembling);
    end

  else if (DirectiveName = 'ENDIF') then
    begin
      if (Length(IfStackArray) = 0) then
        fErrors.AddError(emEndifWithoutIf)
      else
        begin
          Len := Length(IfStackArray);
          SetAssemblingFlag(IfStackArray[Len - 1]); // 'Pop' entry off stack
          SetLength(IfStackArray, Len - 1); // ... and clear item off stack
        end;
    end

  else if (DirectiveName = 'MACRO') then
    begin
      DefineMacro;                      // Define a macro
    end

  else if (DirectiveName = 'EXITM') then
    begin
      // How to do this?
    end

  else if (DirectiveName = 'NOWARN') then
    begin
      // Mark next line to ignore. Add 2 as line numbers are zero based here
      fErrors.NoWarnLine := fFiles.CurrentLineNo + 2;
    end;
end;


procedure TAssembler.SetAssemblingFlag(state: boolean);
begin
  fIsAssembling := state;
  fLIsting.IsAssembling := state;
end;


{ MACRO RELATED }

{ TODO : uAssembler -> not listing macro definition into LST file }

procedure TAssembler.DefineMacro;
var
  MacroNumber: integer;
begin
  fParser.GetToken;                     // Expecting name
  if (fParser.Token.Typ = tkId) then
    begin
      DefiningMacro := True;
      if (fPassNumber = 1) then         // Only add macro to list on first pass
        begin
          MacroNumber := fFiles.AddMacro(fParser.Token.StringVal);
          fSymbolTable.AddInstruction(fParser.Token.StringVal, itMacro, MacroNumber);
        end;

      while (fParser.PeekNextToken.Typ <> tkEOL) do
        fParser.GetToken;               // Skip rest of line, can be anything
    end
  else
    fErrors.AddError(emMacroNameMissing);
end;


procedure TAssembler.AddToMacro;
var
  Mnem: string;
begin
  if ((fParser.Token.Typ = tkHashDirective) and (UpperCase(fParser.Token.StringVal) = 'ENDM')) then
    begin
      DefiningMacro := False;           // Switch off macro definition
      Exit;                             // Do not save this line
    end;

  if (fParser.Token.Typ = tkComment) then
    begin
      fParser.GetToken;                 // Skip comment and EOL
      Exit;
    end;

  if (fParser.Token.Typ in [tkLabel, tkLocal]) then
    fParser.GetToken;                   // Skip over label

  // Must be a mnemonic, check if allowed inside a Macro
  Mnem := UpperCase(fParser.Token.StringVal);
  if (Pos(Mnem, 'END INCLUDE') > 0) then
    begin
      fErrors.AddError(emMacroBadInstr);
      Exit;
    end;

  if (fPassNumber = 1) then             // Only add lines to macro on first pass
    fFiles.AddMacroLine(fParser.SourceLine);
  fParser.SkipRestOfLine;               // Skip anything else
  fParser.GetToken;                     // ... and skip EOL
end;


{ DO MACRO - start expanding a macro }

procedure TAssembler.DoMacro;
var
  Params: string;
begin
  // TODO: uAssembler -> need to get macro parameters from line
  fFiles.ExpandMacro(fSymbolTable.Instruction.Value, Params);
  fListing.IsMacroExp := True;
end;


{ EXPRESSION HANDLER; based on The Delphi Magazine Issue 84, August 2002 }


{ PARSE EXPRESSION }

{ Checks for unary operators + or - or > or < and then parses for a term.
  On return the character pointer is at the first non-expression token value }

function TAssembler.ParseExpr: integer;
var
  tkUnaryOp: TTokenType;
begin
  if fParser.PeekNextToken.Typ in [tkPlus, tkMinus, tkGreater, tkLower] then
    begin
      fParser.GetToken;
      tkUnaryOp := fParser.Token.Typ;
      Result := ParseTerm;		// Get an operand to act on
      case tkUnaryOp of
        tkMinus:   Result := -(Result);
        tkGreater: Result := ((Result shr 8) and $FF); // High byte of operand
        tkLower:   Result := (Result and $FF);         // Low byte of operand
      end;
    end
  else
    Result := ParseTerm;
end;


{ PARSE TERM }

{ Gets first operand value. If there is an operator following, then gets next
  operand and calculates the value, repeating until no more operators found }

function TAssembler.ParseTerm: integer;
var
  tkOperator: TTokenType;
  Op2: integer;
begin
  Result := ParseOperand;		// Get first operand
  while fParser.PeekNextToken.Typ in [tkPlus, tkMinus, tkStar, tkSlash,
                                      tkLogicalAnd, tkLogicalOr] do
    begin
      fParser.GetToken;
      tkOperator := fParser.Token.Typ;
      Op2 := ParseOperand;		// then second operand
      case tkOperator of
        tkPlus:  Result := Result + Op2; // ... and calculate
        tkMinus: Result := Result - Op2;
        tkStar:  Result := Result * Op2;
        tkSlash: Result := Trunc(Result / Op2);
        tkLogicalAnd: Result := Result and Op2;
        tkLogicalOr:  Result := Result or Op2;
      end;
    end;
end;


{ PARSE OPERAND }

{ Handles parentheses and iterates expression parser as required, gets number
  values, and if an identifier tries to get its value reporting an error on
  the second pass if there is no value to return (identifier undefined).
  If the identifier has a preceding '.' (reference to local label) it is
  expanded by adding the LastGlobalLabel value before searching the symbol table }

function TAssembler.ParseOperand: integer;
var
  ThisSymbol: TSymbol;
  sSymbol: string;
begin
  Result := 0;
  fParser.GetToken;
  case fParser.Token.Typ of
    tkLeftParen: begin                  // Check for subexpression
                   Result := ParseExpr; // ... and recurse if so
                   // Make sure closing parenthesis ok
                   if Expecting([tkRightParen], ')') then
                     fParser.GetToken;  // Skip over right parenthesis
                 end;

    tkNumber: Result := fParser.Token.NumberVal;

    tkString: begin
                if Length(fParser.Token.StringVal) > 1 then
                  fErrors.AddErrorFmt(emStringTooLong, [fParser.Token.StringVal])
                else
                  Result := Ord(fParser.Token.StringVal[1]);
              end;

    tkId,
    tkDotId:  begin
                sSymbol := fParser.Token.StringVal;
                if (fParser.Token.Typ = tkDotId) then // Expand local label name
                  sSymbol := LastGlobalLabel + sSymbol;
                ThisSymbol := fSymbolTable.FindSymbol(sSymbol);
                if (ThisSymbol <> nil) then
                  Result := ThisSymbol.Value
                else
                  if ((fPassNumber = 1) or (not fIsAssembling)) then
                    Result := PC
                  else
                    fErrors.AddErrorFmt(emSymbolNotDefined, [sSymbol]);
              end;

    tkDollar,
    tkStar:   Result := PC;             // $ or * can represent the location counter

  else
    fErrors.AddError(emOperandNotFound);
  end;
end;


{ ADD ERROR }

procedure TAssembler.DoError(Sender: TObject; ErrMsg: string);
begin
  if (ErrMsg <> '') then                // If repeat error not allowed, will be empty
    fListing.ListError(ErrMsg);         // Add to listing
  fParser.SkipRestOfLine;               // One error per line, skip rest
end;


{ BUILD ORDERED IDENTIFIERS LIST FOR DEBUGGER }

procedure TAssembler.BuildIdentifiersList;
var
  sSymbol: string;
  Counter: integer;
begin
  if (fSymbolTable.SymbolCount = 0) then // Anything to list?
    Exit;

  fSymbolTable.FirstSymbol;
  Counter := 0;
  while (not fSymbolTable.DoneSymbols) do
    begin
      sSymbol := fSymbolTable.Symbol.Name;
      // If symbol too long, truncate and add '..' to indicate truncation
      if (Length(sSymbol) > 18) then
        sSymbol := LeftStr(sSymbol, 16) + '..';
      SetLength(fIdentifiers, Counter + 1);
      fIdentifiers[Counter].StringData := Format('%.4x  %s', [fSymbolTable.Symbol.Value, sSymbol]);
      fIdentifiers[Counter].LineNumber := fSymbolTable.Symbol.Line;
      fIdentifiers[Counter].SourceIndex := fSymbolTable.Symbol.SourceIndex;
      fSymbolTable.NextSymbol;
      Inc(Counter);
    end;
end;


{ ADD DIRECTIVES }

{ Add assembler directives to the instructions hash table. Several directives
  relate to the same functionality to support different manufacturer's standard
  assembly terminology; e.g. byte, fcb, db, defb all define a byte value }

{ TODO : uAssembler -> add facility for CPU to add  special directives, eg set DP in 6809 }

procedure TAssembler.AddDirectives;
begin
  // Ensure any changes made here are reflected in TAssemblerForm 'DIRECTIVES'
  fSymbolTable.AddInstruction('END',      itEnd,      0); // Source control

  fSymbolTable.AddInstruction('ORG',      itOrg,      0); // Location control

  fSymbolTable.AddInstruction('RMB',      itReserve,  0);
  fSymbolTable.AddInstruction('DS',       itReserve,  0);
  fSymbolTable.AddInstruction('DEFS',     itReserve,  0);

  fSymbolTable.AddInstruction('BYTE',     itByte,     0); // Data declarations
  fSymbolTable.AddInstruction('FCB',      itByte,     0);
  fSymbolTable.AddInstruction('DB',       itByte,     0);
  fSymbolTable.AddInstruction('DEFB',     itByte,     0);

  fSymbolTable.AddInstruction('WORD',     itWord,     0);
  fSymbolTable.AddInstruction('FDB',      itWord,     0);
  fSymbolTable.AddInstruction('DW',       itWord,     0);
  fSymbolTable.AddInstruction('DEFW',     itWord,     0);

  fSymbolTable.AddInstruction('TEXT',     itText,     0);
  fSymbolTable.AddInstruction('FCC',      itText,     0);

  fSymbolTable.AddInstruction('EQU',      itEqu,      0); // Symbol declarations
end;


{ Add mnemonic / opcode instructions to the instructions hash table }

procedure TAssembler.AddOpcodes;
var
  MnemStr: string;
  Idx: integer;
  RawOpcode: TOpcodeRawData;
begin
  for Idx := 0 to (Machine.CPU.DataCount - 1) do
    begin
      RawOpcode := Machine.CPU.DataByIndex[Idx];
      MnemStr := UpperCase(RawOpcode.M); // Work in uppercase in instruction table
      // Add mnemonic to hashtable once, skip over repeats
      if (fSymbolTable.FindInstruction(MnemStr) = nil) then
        fSymbolTable.AddInstruction(MnemStr, itMnem, Idx);
    end;
end;


{ SEND STATUS MESSAGE }

procedure TAssembler.DoLog(msg: string; DestMain: boolean);
begin
  if Assigned(fOnLog) then
    fOnLog(msg, DestMain);
end;


{ LIST SYMBOL TABLE }

{ Generates a formatted symbol table in columns of three. Any symbol over 18
  characters is truncated to maintain formatting }

procedure TAssembler.ListSymbolTable;
var
  cSymType: char;
  Counter: integer;
  sLine, sSymbol: string;
begin
  if ( (not AsmPrefs.ListSymbols) or (fSymbolTable.SymbolCount = 0) ) then
    Exit;                               // If not in options, or no symbols to list, then exit

  fListing.List('');
  fListing.List(Format('Symbols table (%d):', [fSymbolTable.SymbolCount]));
  Counter := 1;
  sLine := '';                          // Start new symbol table line

  fSymbolTable.FirstSymbol;
  while (not fSymbolTable.DoneSymbols) do
    begin

      cSymType := #32;
      if (symDefine in fSymbolTable.Symbol.Use) then
        cSymType := char('D');
      if (symLabel in fSymbolTable.Symbol.Use) then
        cSymType := char('L');

      sSymbol := fSymbolTable.Symbol.Name;
      if (Length(sSymbol) > 18) then
        sSymbol := LeftStr(sSymbol, 16) + '..';
      sLine := sLine + Format('[%.4d]%s %.4x %-20s',
                              [fSymbolTable.Symbol.Line,
                               cSymType,
                               fSymbolTable.Symbol.Value,
                               sSymbol]);

      if ((Counter mod 3) = 0) then     // Every three columns, restart for next line
        begin
          fListing.List(sLine);
          sLine := '';
        end;
      fSymbolTable.NextSymbol;
      Inc(Counter);
    end;
  fListing.List(sLine);
end;


end.
