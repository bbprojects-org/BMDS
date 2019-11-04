{ ==============================================================================

  ASSEMBLER

    This is the main assembler which processes each token, taking the
    appropriate action, including:
    - managing # directives
    - processing macros
    - scan lines checking labels opcodes and operands, generating code
    - producing listing

    Needs to be provided with a reference to the current CPU for building the
    symbol table opcodes list, etc


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

{ TODO : uAssembler -> add Macro routines }

{ TODO : uAssembler -> Add CPU_TYPE selector to create function }

{ TODO : uAssembler -> in DoMnemonic, rules for modifying operand should
                       probably be in specific CPU code section? Could have
                       procedure call to specific routine in lieu of Rules? }

{ TODO : uAssembler -> for DoWord, add ENDIAN flag to define order of bytes for
                       each specific CPU }

{ TODO : uAssembler -> for #INCLUDE need to ensure not in a macro definition }

{ TODO : uAssembler -> do we need #DEFINE, since have EQU }

unit uAssembler;

{$mode objfpc}{$H+}

interface

uses Forms, Classes, SysUtils, FileUtil, SynEdit, SynPluginSyncroEdit, Controls,
     Graphics, Dialogs, Menus, ExtCtrls, ActnList, LCLType, Math,
     //
     uParser, uSymbols, uMachineBase, uCpuBase, uAsmErrors, uAsmListing,
     uAsmFiles, uAsmConfigFrame;

type
  TBytes256 = array [0..255] of Byte;

  TLineInfo = record
    SourceIndex: integer;               // Index to source file filename (=0 if mainfile)
    LineNumber: integer;                // Current line being processed
    MachineAddr: LongWord;              // PC address for this line of code
    Filename: string;                   // Current file being parsed
  end;
  TArrayLineInfo = array of TLineInfo;

  TItemData = record                    // Used by Identifers, Errors, Warnings
    SourceIndex: integer;               // Index to source file filename (=0 if mainfile)
    LineNumber: integer;                // Current line being processed
    StringData: string;                 // Value of string
  end;
  TArrayIdentfiers = array of TItemData;

  TOpcodeData = record
    OpNumPtr: Word;
    AddrModeStr: string;
    Opcode: Longword;                   // Up to four bytes
    NumBytes: byte;
    Rule: TRule;
  end;
  TOpcodeList = array of TOpcodeData;

  { TAssembler }

  TAssembler = class(TObject)
  private
    fMachine: TMachineBase;             // Reference to parent machine object
    fPassNumber: integer;               // Current pass number; 1 or 2
    fFiles: TFiles;                     // Source files manager
    fListing: TListing;                 // Manages assembler listing
    fErrors: TErrors;                   // Manages errors & warnings
    fSymbolTable: TSymbols;             // Manages symbols
    fIdentifiers: TArrayIdentfiers;     // Used to return identifiers list
    fLinesInfo: TArrayLineInfo;         // Array used to hold all lines data for debug
    fParser: TParser;                   // Token parser (line-by-line)
    OpcodeList: TOpcodeList;            // List of opcodes assembly data
    //
    PC: integer;                        // Program / location counter
    MemorySection: byte;                // Memory type assembling to: 0=RAM, 1=CODE
    BytesArray: TBytes256;              // Array of bytes for program code (max 256 bytes)
    NumBytes: integer;                  // Number of bytes being generated (DB/DW)
    LastGlobalLabel: string;            // Last global label defined, supports local labels
    IfStackArray: array of boolean;     // Used to keep state of IF/ELSE/ENDIF
    fIsAssembling: boolean;             // Used by IF/ENDIF routines to determine state
    LineHasLabel: boolean;              // Does current line have a label?

    procedure AddDirectives;
    procedure AddOpcodes(OpcodeData: TOpcodeArray; CpuType: byte);
    procedure DefineLabel;
    procedure DoPass(PassNo: Integer; Filename: string);
    procedure ProcessInstruction;
    procedure DoOutputs(StartAddr: integer);
    procedure DoMnemonic;
    procedure DoOrg;
    procedure DoReserve;
    procedure DoByte;
    procedure DoWord;
    procedure DoText;
    function  AddrOnly(Value: integer): string;
    function  AddrPlus(Bytes: integer; Offset: integer = 0): string;
    function  Expecting(ExpectedTok: TTokenTypes; ExpectedStr: string): boolean;
    //procedure FindEnd;
    //procedure FindElse;
    procedure HashDirective;
    function  ParseExpr: integer;
    function  ParseTerm: integer;
    function  ParseOperand: integer;
    procedure BuildIdentifiersList;    
    procedure ClearListingData;
  public
    constructor Create(Machine: TMachineBase);
    destructor  Destroy; override;
    //
    procedure Execute(SourceFilename: string);
    //
    property Machine: TMachineBase read fMachine write fMachine;
    property Parser: TParser read fParser;
    property PassNumber: integer read fPassNumber;
    property Files: TFiles read fFiles;
    property Listing: TListing read fListing write fListing;
    property Errors: TErrors read fErrors;
    property SymbolTable: TSymbols read fSymbolTable;
    property Identifiers: TArrayIdentfiers read fIdentifiers;
    property LinesInfo: TArrayLineInfo read fLinesInfo;   
    property IsAssembling: boolean read fIsAssembling;
  end;


implementation


{ CREATE }

{ Passes parent Machine to permit access to other machine properties }

constructor TAssembler.Create(Machine: TMachineBase);
begin
  fMachine := Machine;
  fSymbolTable := TSymbols.Create;
  AddDirectives;                        // Add directives to Symbol table
  AddOpcodes(Machine.CPU.OpcodeDataArray, 1);
end;


{ DESTROY }

destructor TAssembler.Destroy;
begin
  fSymbolTable.Free;
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

procedure TAssembler.Execute(SourceFilename: string);
begin
  fFiles := TFiles.Create(self, SourceFilename);     // Filename used for data file
  fListing := TListing.Create(self, SourceFilename); // Filename used for list file
  fErrors := TErrors.Create(self);
  fParser := TParser.Create(@fFiles.GetSourceLine);
  SetLength(fLinesInfo, 0);             // Initialise array of lines data
  SetLength(IfStackArray, 0);           // Initialise IF stack
  PC := 0;
  fIsAssembling := True;                // Conditional assembly flag
  try
    fListing.ListHeader;                // List CPU, filename, date/time
    DoPass(1, SourceFilename);
    if (Errors.ErrorCount = 0) then     // If no errors after Pass 1
      DoPass(2, SourceFilename)         // ... then do Pass 2
    else
      fListing.ListError(PASS_2_ABORTED);
    fListing.ListFooter;                // Listing summary
    fListing.ListSymbolTable;           // List symbol table
    BuildIdentifiersList;
    fListing.Write;
  finally
    fParser.Free;
    fErrors.Free;
    fListing.Free;
    fFiles.Free;
  end;
end;


{ DO AN ASSEMBLER PASS ON THE SOURCE FILE }

procedure TAssembler.DoPass(PassNo: Integer; Filename: string);
var
  StartPC: integer;
  ExpectingInstruction: boolean;
begin
  fPassNumber := PassNo;
  fFiles.Init;                          // Initialise file stack, etc
  fFiles.OpenFile(Filename);            // Get source text
  PC := 0;                              // Initialise program counter
  fParser.Initialise;
  while (fParser.Token.Typ <> tkEOF) do // Loop until End Of File
    try
      // Entry here should always be with Token.Typ = tkEOL (or tkEOF)
      NumBytes := 0;
      ClearListingData;
      StartPC := PC;
      LineHasLabel := False;
      ExpectingInstruction := True;

      // Start of line, expecting to see one of: label / #directive /
      //   comment / whitespace & instruction / EOL (blank line)
      fParser.GetToken;
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

      fListing.SourceLine := fParser.SourceLine;
      fListing.ListLine;                // List assembled line

      DoOutputs(StartPC);           // Output data to memory / file

    except
      on E: Exception do Errors.AddError(E.Message);
    end;
end;


{ CLEAR LINE - RESET LINE VARIABLES TO EMPTY }

procedure TAssembler.ClearListingData;
begin
  fListing.HexData := '';
  fListing.LabelStr := '';
  fListing.OpcodeStr := '';
  fListing.OperandStr := '';
  fListing.CommentStr := '';
end;


{ PROCESS AN INSTRUCTION }

{ Either a mnemonic, assembler directive, or a macro call }

procedure TAssembler.ProcessInstruction;
var
  StrVal: string;
  OperandStart: integer;
begin           
  OperandStart := fParser.PeekNextToken.StartPos;
  case (fParser.Token.Typ) of
    tkId, tkDotId:
      begin
        if (fParser.Token.Typ = tkDotId) then
          // If .Directive then strip leading '.'
          StrVal := RightStr(fParser.Token.StringVal, Length(fParser.Token.StringVal) - 1)
        else
          StrVal := fParser.Token.StringVal;
        if (fSymbolTable.FindInstruction(LowerCase(StrVal)) = nil) then
          begin
            Errors.AddError(Format(INSTR_NOT_RECOGNISED, [fParser.Token.StringVal]));
            Exit;
          end;
      end;

    tkEqual: fSymbolTable.FindInstruction('equ'); // Translate '=' -> 'equ'

  else
    Errors.AddError(Format('Instruction expected, got [%s]', [fParser.Token.StringVal]));
    Exit;
  end;

  case fSymbolTable.Instruction.InstructionType of
    itMnem:    DoMnemonic;              // Process CPU mnemonic
    itOrg:     DoOrg;                   // Set location counter
    itReserve: DoReserve;               // Reserve space for variables, etc
    itByte:    DoByte;                  // Define byte values
    itWord:    DoWord;                  // Define word values
    itText:    DoText;                  // Define text string
    itEqu:     begin
                 // Define a constant value. A label is assigned the value of
                 // the expression that follows it
                 if (LineHasLabel) then
                   begin
                     fSymbolTable.Symbol.Value := ParseExpr;
                     fSymbolTable.Symbol.Use := [symDefine, symSet];
                     fListing.HexData := AddrOnly(fSymbolTable.Symbol.Value);
                   end
                 else
                   Errors.AddError(LABEL_MISSING);
               end;
    itEnd:     begin
                 // If END encountered, flag it here. At next GetSourceLine
                 // this flag will cause the current file to close, ignoring
                 // any other lines after the END statement in the current file
                 fFiles.FlagEnd := True;
               end;
  end;
  if (fIsAssembling) then
    Inc(PC, NumBytes);

  fListing.OperandStr := fParser.GetLineText(OperandStart, fParser.Token.EndPos);
end;


{ DO OUTPUTS }

procedure TAssembler.DoOutputs(StartAddr: integer);
var
  {i,} nOffset, nNumOf3, nRem: integer;
begin
  // If pass 2 and option selected, then write code to memory
  { DEBUG }
  //if (fPassNumber = 2) and (AsmOptions.WriteToMemory) then
  //  for i := 1 to NumBytes do
  //    Machine.Memory[StartPC + i - 1] := BytesArray[i];
  { Adding offset of $8000 to Program Counter }
  //for i := 1 to NumBytes do
  //  Machine.Memory[$8000 + StartAddr + i - 1] := BytesArray[i];
  { END_DEBUG }

  if (fIsAssembling and (NumBytes > 3)) then // Multibytes?
    begin
      fListing.Types := fListing.Types + [ltMultiBytes];
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
      fListing.Types := fListing.Types - [ltMultiBytes];
    end;
end;


{ PROCESS A MNEMONIC }

procedure TAssembler.DoMnemonic;
var
  thisOpcode: TInstruction;
  nLen, nDataIdx, nMnemNum, nOffset: integer;
  Value: word;
  sAddrMode: string;
  bDoneExpr: boolean;
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

  // Lookup instruction and parse the operand text to build an address mode for
  // checking against CPU opcode data array. Source text checked for CPU
  // register addresses, operand value (replaced by *) or any other non-space
  // text just added to the address mode mask
  thisOpcode := fSymbolTable.FindInstruction(LowerCase(fParser.Token.StringVal));
  sAddrMode := '';
  nDataIdx := thisOpcode.Value;
  bDoneExpr := False;
  Value := 0;

  // Build address mode for checking
  while not (fParser.PeekNextToken.Typ in [tkComment, tkEOL]) do
    begin
      case fParser.PeekNextToken.Typ of
       tkId,
       tkDotId,
       tkNumber:  if (Pos(Uppercase(fParser.PeekNextToken.StringVal), fMachine.CPU.AssemblerRegisters) > 0) then
                    begin
                      fParser.GetToken;
                      sAddrMode := sAddrMode + UpperCase(fParser.Token.StringVal);
                    end
                  else
                    begin
                      if (bDoneExpr = True) then
                        raise EAnalyseError.Create(OPERAND_NOT_FOUND)
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

  // Now check address mode against those in OpcodesList. Note that the
  // opcode data has all the address modes for a specific mnemonic grouped
  // together hence when mnemonic number changes, all address modes for that
  // mnemonic have been checked
  nMnemNum := OpcodeList[nDataIdx].OpNumPtr;
  while (OpcodeList[nDataIdx].AddrModeStr <> sAddrMode) and
        (OpcodeList[nDataIdx].OpNumPtr = nMnemNum) do
    Inc(nDataIdx);

  // If still pointing at the current instruction mnemonic, can then get
  // opcode value and assign the operand bytes too. Although assign three
  // bytes, not all will apply depending on particular opcode
  if (OpcodeList[nDataIdx].OpNumPtr = nMnemNum) then
    begin
      NumBytes := OpcodeList[nDataIdx].NumBytes;
      BytesArray[1] := OpcodeList[nDataIdx].Opcode;
      BytesArray[2] := Lo(Value);
      BytesArray[3] := Hi(Value);
    end
  else
    Errors.AddError(Format(ADDR_MODE_NOT_RECOGNISED, [sAddrMode]));

  // Check for any actions modifying operand
  case OpcodeList[nDataIdx].Rule of
    rNIL: ;                           // Do nothing
    rZP:  if (BytesArray[3] = 0) then // If zero page (hi byte = 0)
            begin
              Dec(NumBytes);          // ... reduce to two bytes
              // Get opcode from next entry which is ZP version
              BytesArray[1] := OpcodeList[nDataIdx+1].Opcode; // BytesArray[1] and $F7;
            end;
    rCR: ;
    rREL: begin
            nOffset := Value - (PC + 2);
            if (nOffset >= -$7f) and (nOffset <= $80) then
              BytesArray[2] := nOffset
            else
              Errors.AddError(BRANCH_TOO_FAR);
          end;
  end;
  fListing.HexData := AddrPlus(NumBytes);
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
  MemorySection := 1;                   // Default mode is CODE
  // Check if memory type is specified
  if (fParser.PeekNextToken.Typ = tkComma) then
    begin
      fParser.GetToken;                 // Skip comma
      if Expecting([tkId], 'identifier') then
        begin
          fParser.GetToken;             // Get memory type
          sText := UpperCase(fParser.Token.StringVal);
          if (sText = 'RAM') then
            MemorySection := 0
          else if (sText <> 'CODE') then
            begin
              Errors.AddError(Format(MEM_MODE_NOT_RECOGNISED, [fParser.Token.StringVal]));
              Exit;                     // Exit on mode not recognised error
            end;
        end
      else
        Exit;                           // Exit on missing identifier error
    end;
    if (MemorySection = 1) then
      fFiles.SetDataStart(PC); // Set object code addr
end;


{ DO RESERVE INSTRUCTION }

{ Reserve space for data/variables. If memory mode = RAM then it just increments
  the PC, but if memory mode = CODE then bytes are written to the output data.
  These bytes default to zero, but can be set as an optional value following the
  space declaration }

procedure TAssembler.DoReserve;
var
  idx, Operand: integer;
  FillByte: byte;
begin
  fListing.HexData := AddrOnly(PC);
  NumBytes := 0;
  Operand := ParseExpr;
  Inc(PC, Operand);                     // Set space as defined by operand
  FillByte := 0;                        // Default fill value = 0
  if (fParser.PeekNextToken.Typ = tkComma) then
    begin
      fParser.GetToken;                 // Skip comma
      FillByte := ParseExpr;            // Get user fill value
    end;
  if (MemorySection = 1) then           // If CODE section then
    for idx := 1 to Operand do
      fFiles.WriteDataByte(FillByte);   // ... write to output file
end;


{ DO BYTE INSTRUCTION }

{ Define bytes to be written. Values are 8-bit expressions separated by commas }

procedure TAssembler.DoByte;
var
  Value: word;
begin
  while (True) do
    begin
      Value := ParseExpr;               // Get byte value in operand field
      Inc(NumBytes);
      BytesArray[NumBytes] := Lo(Value);
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


{ ADDRESS ONLY }

function TAssembler.AddrOnly(Value: integer): string;
begin
  Result := '';
  if (fPassNumber = 2) then             // Only output text on second pass
    Result := Format('%.4x', [Value]);
end;


function TAssembler.AddrPlus(Bytes: integer; Offset: integer): string;
var
  i: integer;
begin
  Result := '';
  if (fPassNumber = 2) then
    begin
      Result := Format('%.4x ', [PC]);
      Bytes := Min(Bytes, 3);           // Max 3 bytes output here
      for i := 1 to Bytes do
        begin
          // Offset defaults to 0 unless set otherwise (i.e. multibytes)
          Result := Result + Format(' %.2x', [BytesArray[Offset + i]]);
          if (AsmConfigFrame.WriteToFile and (fIsAssembling)) then
            fFiles.WriteDataByte(BytesArray[Offset + i]);
        end;
    end;
end;


{ EXPECTING }

{ Check next token is of the expected type; if not, then flag error }

function TAssembler.Expecting(ExpectedTok: TTokenTypes; ExpectedStr: string): boolean;
begin
  Result := (fParser.PeekNextToken.Typ in ExpectedTok);
  if (not Result) then
    Errors.AddError(Format(EXPECTED_NOT_FOUND, [ExpectedStr, fParser.PeekNextToken.StringVal]));
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
        Errors.AddError(PHASING_ERROR);
    end;
end;


{ HASH DIRECTIVE }

{ Process the current # directive; INCLUDE file, conditional IF/ELSE/ENDIF or
  DEFINE }

procedure TAssembler.HashDirective;
var
  DirectiveName, Filename, sText: string;
  Value, Len: integer;            
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
          Filename := ExtractFilePath(fFiles.SourceFiles[0].Filename) + fParser.Token.StringVal;
          (*
          // If next token = comment, get it here
          if (fParser.PeekNextToken.Typ = tkComment) then
            begin
              fParser.GetToken;  
              fListing.CommentStr := fParser.Token.StringVal;;
            end;
          fParser.PeekNextToken;        // Should be EOL
          *)
          if (FileExists(Filename)) then
            fFiles.OpenFile(Filename)
          else if (fPassNumber = 1) then
            Errors.AddError(Format(CANNOT_FIND_INCLUDE_FILE, [Filename]));
        end;
    end

  else if (DirectiveName = 'IF') then
    begin
      Len := Length(IfStackArray);
      SetLength(IfStackArray, Len + 1);  // Make room for new item on stack
      IfStackArray[Len] := IsAssembling; // ... and 'push' it on
      // If expression FALSE, stop assembling until ELSE/ENDIF
      fIsAssembling := fIsAssembling and (ParseExpr <> 0);         
      fListing.OperandStr := fParser.GetLineText(OperandStart, fParser.Token.EndPos);
    end

  else if (DirectiveName = 'ELSE') then
    begin
      if (Length(IfStackArray) = 0) then
        Errors.AddError(ELSE_WITHOUT_IF)
      else
        fIsAssembling := not fIsAssembling;
    end

  else if (DirectiveName = 'ENDIF') then
    begin
      if (Length(IfStackArray) = 0) then
        Errors.AddError(ENDIF_WITHOUT_IF)
      else
        begin
          Len := Length(IfStackArray);
          fIsAssembling := IfStackArray[Len - 1]; // 'Pop' entry off stack
          SetLength(IfStackArray, Len - 1); // ... and clear item off stack
        end;
    end

  else if (DirectiveName = 'DEFINE') then
    begin
      if Expecting([tkId], 'identifier') then
        begin
          fParser.GetToken;             // Get identifier
          sText := fParser.Token.StringVal;
          // Looking for optional expression next
          Value := 0;
          if (fParser.PeekNextToken.Typ in [tkId, tkNumber, tkLeftParen, tkPlus, tkMinus]) then
            Value := ParseExpr;
          if (fPassNumber = 1) then
            fSymbolTable.AddSymbol(sText, Value, [symDefine], 1 {Token.Line});   
          fListing.OperandStr := fParser.GetLineText(OperandStart, fParser.Token.EndPos);
        end;
    end;
end;


(*
// Skip over lines until a line with ENDIF or END found
procedure TAssembler.FindEnd;
var
  nIfNestLevel: integer;
begin
  nIfNestLevel := 0;
  repeat
  until (nIfNestLevel < 0);
(*
     nest := 0;
     repeat { READ AND CHECK FOLLOWING LINES }
		  if listing then listline { FIRST LIST PREVIOUS LINE };
		  getop;
		  if optype = opif
		    then nest := nest + 1
		    else if optype = opendif
			   then nest := nest - 1;
	     until (optype = opend) or (nest < 0);
	     if optype = opend then begin
		  error(miseif,0,0);
		  popget;
	     end;
	end { FINDEND };
*)
end;


// Skip over lines until one with an ELSE, ELSEIF <True>, or END found
procedure TAssembler.FindElse;
begin
end;
*)


{ EXPRESSION HANDLER; based on The Delphi Magazine Issue 84, August 2002 }


{ PARSE EXPRESSION }

{ Checks for unary operators + or - and then parses for a term
  On return the character pointer is at the first non-expression token value }

function TAssembler.ParseExpr: integer;
var
  tkUnaryOp: TTokenType;
begin
  if fParser.PeekNextToken.Typ in [tkPlus, tkMinus] then
    begin
      fParser.GetToken;
      tkUnaryOp := fParser.Token.Typ;
      Result := ParseTerm;		// Get an operand to act on
      if (tkUnaryOp = tkMinus) then
        Result := -(Result);
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
  while fParser.PeekNextToken.Typ in [tkPlus,tkMinus,tkStar,tkSlash] do
    begin
      fParser.GetToken;
      tkOperator := fParser.Token.Typ;
      Op2 := ParseOperand;		// then second operand
      case tkOperator of
        tkPlus:  Result := Result + Op2; // ... and calculate
        tkMinus: Result := Result - Op2;
        tkStar:  Result := Result * Op2;
        tkSlash: Result := Trunc(Result / Op2);
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
                    Errors.AddError(Format(SYMBOL_NOT_DEFINED, [sSymbol]))
              end;
  else
    Errors.AddError(OPERAND_NOT_FOUND);
  end;
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

procedure TAssembler.AddDirectives;
begin
  fSymbolTable.AddInstruction('end',      itEnd,      0, 0); // Source control

  fSymbolTable.AddInstruction('org',      itOrg,      0, 0); // Location control
  fSymbolTable.AddInstruction('rmb',      itReserve,  0, 0);
  fSymbolTable.AddInstruction('ds',       itReserve,  0, 0);
  fSymbolTable.AddInstruction('defs',     itReserve,  0, 0);

  fSymbolTable.AddInstruction('byte',     itByte,     0, 0); // Data declarations
  fSymbolTable.AddInstruction('fcb',      itByte,     0, 0);
  fSymbolTable.AddInstruction('db',       itByte,     0, 0);
  fSymbolTable.AddInstruction('defb',     itByte,     0, 0);
  fSymbolTable.AddInstruction('word',     itWord,     0, 0);
  fSymbolTable.AddInstruction('fdb',      itWord,     0, 0);
  fSymbolTable.AddInstruction('dw',       itWord,     0, 0);
  fSymbolTable.AddInstruction('defw',     itWord,     0, 0);
  fSymbolTable.AddInstruction('text',     itText,     0, 0);
  fSymbolTable.AddInstruction('fcc',      itText,     0, 0);

  fSymbolTable.AddInstruction('equ',      itEqu,      0, 0); // Symbol declarations

  fSymbolTable.AddInstruction('macro',    itMacro,    0, 0); // Macro assembly
  fSymbolTable.AddInstruction('endm',     itEndmacro, 0, 0);
end;


{ ADD OPCODES }

{ Add mnemonic / opcode instructions to the instructions hash table }

procedure TAssembler.AddOpcodes(OpcodeData: TOpcodeArray; CpuType: byte);
var
  MnemStr: string;
  Idx, MnemNumber, OpcodeListIndex: integer;
  Count: integer;
  ThisOpcode: TOpcodeData;
  RawOpcode: TOpcodeRawData;
begin
  CpuType := CpuType or 1;              // Ensure standard CPU instr included
  Count := Length(OpcodeData);
  SetLength(OpcodeList, Count + 1);     // Reserve appropriate amount of space, 1 based
  MnemNumber := 0;                      // Mnemonic number, inc when string changes
  OpcodeListIndex := 1;                 // Pointer into separate opcode data list
  for Idx := 0 to (Count - 1) do
    begin
      RawOpcode := OpcodeData[Idx];
      MnemStr := LowerCase(RawOpcode.M); // Work in lowercase in instr table
      ThisOpcode.Opcode := RawOpcode.O;
      ThisOpcode.AddrModeStr := UpperCase(RawOpcode.A);
      ThisOpcode.NumBytes := RawOpcode.N;
      ThisOpcode.Rule := RawOpcode.R;
      // If this Opcode is relevant to this CPU type, add to hashtable
      if ((RawOpcode.T and CpuType) > 0) then
        begin
          if (fSymbolTable.FindInstruction(MnemStr) = nil) then
            begin
              Inc(MnemNumber);          // Next mnemonic, inc number
              fSymbolTable.AddInstruction(MnemStr, itMnem, OpcodeListIndex, MnemNumber);
            end;
          ThisOpcode.OpNumPtr := MnemNumber;
          OpcodeList[OpcodeListIndex] := ThisOpcode;
          Inc(OpcodeListIndex);
        end;
    end;
end;


(*

// Build HTML file in same format as Rockwell 6502 datasheet to check all
// opcode parameters correct

procedure TAssembler.BuildOpcodesFile;
var
  sExePath, sBox: string;
  I, J, nOpcode: integer;
  ListingLines: TListing;
  eOpcode: TOpcodes;
begin
  sExePath := ExtractFilePath(Application.ExeName);
  ListingLines := TListing.Create('6502', 'opcodes.htm', sExePath + 'style.css');
  with (ListingLines) do
  try
    ListStart;
    List('<table border=1 cellpadding=5>');
    for J := 0 to 15 do
      begin
        List('<tr>');
        for I := 0 to 15 do
          begin
            nOpcode := (J * 16) + I;
            eOpcode := Mnemonics256[nOpcode];
            if (eOpcode = _000) then
              sBox := '&nbsp&nbsp&nbsp&nbsp&nbsp'
            else
              begin
                sBox := Mnemonics[eOpcode] + '<br>';
                case AddressMode256[nOpcode] of
                  mINH:  sBox := sBox + 'Implied';
                  mIMM:  sBox := sBox + 'IMM';
                  mACC:  sBox := sBox + 'Accum';
                  mZP:   sBox := sBox + 'ZP';
                  mZPX:  sBox := sBox + 'ZP,X';
                  mZPY:  sBox := sBox + 'ZP,Y';
                  mABS:  sBox := sBox + 'ABS';
                  mABSX: sBox := sBox + 'ABS,X';
                  mABSY: sBox := sBox + 'ABS,Y';
                  mIND:  sBox := sBox + 'Indirect';
                  mINDX: sBox := sBox + '(IND,X)';
                  mINDY: sBox := sBox + '(IND),Y';
                  mREL:  sBox := sBox + 'Relative';
                end;
                sBox := sBox + '<br>' + IntToStr(NumBytes256[nOpcode]) + '&nbsp&nbsp'
                             + IntToStr(Cycles256[nOpcode]);
              end;
            List('<td align=center>' + sBox + '</td>');
          end;
        List('</tr>');
      end;
    List('</table>');
    ListEnd;
  finally
    ListingLines.Free;
  end;
end;


function TAssembler.ListToken: string;
var
  sTEMP: string;
begin

  case fParser.Token.Typ of
    tkNil     : sTEMP := 'tkNIL';
    tkEOL     : sTEMP := 'tkEOL';
    tkLabel   : sTEMP := 'tkLabel';
    tkId      : sTEMP := 'tkId';
    tkNumber  : sTEMP := 'tkNumber';
    tkString  : sTEMP := 'tkString';
    tkComment : sTEMP := 'tkComment';
    tkHashDirective : sTEMP := 'tkHashDirective';
    tkDirective : sTEMP := 'tkDirective';
    tkEOF    : sTEMP := 'tkEOF';
  else
    sTEMP := '';
  end;
  Result := Format('[%-40s], [%s], [%d], [%s]',
            [fParser.SourceLine, sTEMP, fParser.Token.NumberVal, fParser.Token.StringVal]);
end;

*)


end.
