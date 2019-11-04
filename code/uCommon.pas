{ ==============================================================================

  COMMON TYPES, HELPER FUNCTIONS, ETC

    Provides any type definitions, etc, pertinent to several units


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

{ TODO : uCommon -> remove all GlobalVars; set as properties somewhere }

unit uCommon;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef UNIX}
    {$ifdef UseCThreads}
      cthreads,
    {$endif}
  {$endif}
  Forms, Classes, SysUtils, ExtCtrls, VersionResource, Dialogs, Controls,
  StdCtrls, EventLog,
  // Units required for GetAppInfo
  fileinfo,
  winpeimagereader, {for reading exe info, Windows}
  elfreader,        {for reading ELF executables, Linux}
  machoreader       {for reading MACH-O executables, MacOS};

type
  // === GLOBAL VARIABLES ======================================================
  TGlobalVars = record
    MachineDataFolder: string;          // Path to AppResources/machinename/ folder
    ScreenSize: TPoint;                 // Screen X,Y dimensions in pixels
    ScreenPosition: TPoint;
    ScaleModifier: integer;             // Used for small screen sizes (eg CHIP-8)
  end;

  // === DISASSEMBLER ==========================================================
  TAddrLabel = record
    Address: word;                      // Address of location counter
    LabelStr: string;                   // Associated machine string label
    RW: char;                           // Read/Write flag, '' if either
    Used: boolean;                      // Has this label been used in disassembly?
  end;
  TAddrDefsArray = array of TAddrLabel;

  // Data to support disassembly; machine specific module returns generic
  // data in this record to enable complex disassembly, or just a simple text

  TDisassembledData = record
    Addr: word;                         // Instruction address
    MnemStr: string;                    // Mnemonic value
    Opcode: word;                       // Opcode value, caters for word size opcodes
    NumBytes: integer;                  // Number of bytes for this opcode
    BytesStr: string;                   // Representation of opcode/operand data
    HasOperand: boolean;                // Operand can be replaced by label
    Operand: word;                      // Operand value
    OperandStr: string;                 // Hex string value (2 or 4 char)
    AddrModeStr: string;                // Address mode string
    AddBlankLine: boolean;              // Add blank line in disassembly (after JMP/RTS/etc)
    RegStr: array of string;            // Array of register representations
    Text: string;                       // 'Simple' disassembled string
  end;

  // === TRACE =================================================================

  // Definitions array for columns in trace display; defined for each CPU

  TTraceColumns = record
    Title: string;                      // Column title
    Width: integer;                     // Column width
    Align: TAlignment;                  // taCenter, taLeftJustify, taRightJustify
  end;
  TTraceColArray = array of TTraceColumns;

  TReadIniProcedure = procedure of object;

  function  GetAppResourcesDirectory: string;
  function  GetAppDataDirectory: string;
  function  MessageQuery(aText: string): boolean;
  procedure MessageWarning(aText: string);
  function  ConfirmResetDefault: boolean;
  function  GetHex(sText: string): integer;
  procedure SwapButtons({%H-}aButt1, {%H-}aButt2: TButton);
  function  GetAppInfo: string;

var
  GlobalVars: TGlobalVars;
  //AppLog: TEventLog;

const
  {$ifdef darwin}
    DIRECTORY_SEPARATOR = '/';
    CRLF = #10;
  {$else}       
    DIRECTORY_SEPARATOR = '\';
    CRLF = #13#10;
  {$endif}


implementation

{ GET FOLDER / DIRECTORY PATHS }

{ Apple advise should not *write* to Application bundle as might not have
  permission to do so. Can *read* Application resources from the bundle though }

function GetAppResourcesDirectory: string;
begin
  {$ifdef darwin}
    Result := ExtractFileDir(Application.ExeName) + '/Resources/';
  {$endif}
  {$ifdef windows}
    Result := ExtractFilePath(Application.ExeName)
  {$endif}
end;


{ I wanted to put dynamic app data in "/Library/Application Support" as advised
  by some Apple documentation, but found no write privileges in High Sierra
  so used user's Application Support folder }

function GetAppDataDirectory: string;
var
  AppName: string;
begin
  AppName := ExtractFilename(Application.ExeName);
  Result := '/usr/share/' + AppName + '/';
  {$ifdef darwin}    
    { TODO : Should be ExpandFileNameUTF8 ? }
    Result := ExpandFileName('~/Library/Application Support/') + AppName + '/';
  {$endif}
  {$ifdef windows}
    Result := ExtractFilePath(Application.ExeName);
    // Or perhaps...
    // Result := GetEnvironmentVariableUTF8('appdata')+ '\' + AppName;
  {$endif}
end;


{ QUERY MESSAGE }

function MessageQuery(aText: string): boolean;
begin
  Result := MessageDlg(aText, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;


{ WARNING MESSAGE }

procedure MessageWarning(aText: string);
begin
  MessageDlg(aText, mtWarning, [mbOK], 0);;
end;


{ CONFIRM RESET DEFAULT }

function ConfirmResetDefault: boolean;
begin
  Result := (MessageDlg('Confirm', 'Confirm reset to defaults, this cannot be undone', mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;


{ GET HEX }

{ Convert string to hex number }

function GetHex(sText: string): integer;
var
  idx, nDigit: integer;
  ch: char;
begin
  sText := UpperCase(sText);
  Result := 0;
  nDigit := 0;                          // To satisfy compiler warning
  for idx := 1 to length(sText) do
  begin
    ch := sText[idx];
    if (ch in ['0'..'9']) then
      nDigit := Ord(ch) - Ord('0')
    else if (ch in ['A'..'F']) then
      nDigit := Ord(ch) - Ord('A') + 10
    else if ((ch ='$') and (idx = 1)) then
      Continue
    else
      Break;                            // Invalid character, force exit
    Result := (Result * 16) + nDigit;
  end;
end;


{ SWAP BUTTONS - assumes buttons set for MacOS, this will reverse them for the
                 common look on Windows }

procedure SwapButtons(aButt1, aButt2: TButton);
{$ifdef windows}
var
  tmpLeft: integer;
{$endif}
begin
  {$ifdef windows}
    tmpLeft := aButt1.Left;
    aButt2.Left := aButt1.Left;
    aButt1.Left := tmpLeft;
  {$endif}
end;



{ GET APP VERSION }

{ Based on code from:
  https://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company

  Displays file version info for
  - Windows PE executables
  - Linux ELF executables (compiled by Lazarus)
  - OSX MACH-O executables (compiled by Lazarus)
  Runs on Windows, Linux, OSX...
}

function GetAppInfo: string;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    Result := FileVerInfo.VersionStrings.Values['FileVersion'];
    // FileVerInfo.VersionStrings.Values['CompanyName'];
    // FileVerInfo.VersionStrings.Values['FileDescription'];
    // FileVerInfo.VersionStrings.Values['InternalName'];
    // FileVerInfo.VersionStrings.Values['LegalCopyright'];
    // FileVerInfo.VersionStrings.Values['OriginalFilename'];
    // FileVerInfo.VersionStrings.Values['ProductName'];
    // FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    FileVerInfo.Free;
  end;
end;


end.

