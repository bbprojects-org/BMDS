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
  TReadIniProcedure = procedure of object;

  function  GetAppResourcesDirectory: string;
  function  GetAppDataDirectory: string;
  procedure Split(Input: string; const Delimiter: char; const Strings: TStrings);
  function  GetRegisterIndex(aRegToFind: string; aRegisters: string): integer;
  function  MessageQuery(aText: string): boolean;
  procedure MessageWarning(aText: string);
  function  ConfirmResetDefault: boolean;
  function  GetHex(sText: string): integer;
  procedure SwapButtons({%H-}aButt1, {%H-}aButt2: TButton);
  function  GetAppInfo: string;

var
  AppLog: TEventLog;

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
var
  ResFolder: string;
begin
  ResFolder := DIRECTORY_SEPARATOR + 'Resources' + DIRECTORY_SEPARATOR;
  {$ifdef darwin}
    // Uses ../BMDS.app/Contents/Resources/ folder
    Result := ExtractFileDir(ExtractFileDir(Application.ExeName)) + ResFolder;
  {$endif}
  {$ifdef windows}
    // Uses ../BMDS.exe/Resources/ folder
    Result := ExtractFilePath(Application.ExeName) + ResFolder;
  {$endif}
end;


{ I wanted to put dynamic app data in "/Library/Application Support" as advised
  by some Apple documentation, but found no write privileges in High Sierra
  so used user's Application Support folder }

function GetAppDataDirectory: string;
var
  AppFolder: string;
begin
  AppFolder := ExtractFileName(Application.ExeName);
  Result := '/usr/share/' + AppFolder + '/';
  {$ifdef darwin}    
    { TODO : uCommon -> should be ExpandFileNameUTF8 ? }
    Result := ExpandFileName('~/Library/Application Support/') + AppFolder + '/';
  {$endif}
  {$ifdef windows}
    Result := ExtractFilePath(Application.ExeName);
    // Or perhaps...
    // Result := GetEnvironmentVariableUTF8('appdata')+ '\' + AppFolder;
  {$endif}
end;


procedure Split(Input: string; const Delimiter: char; const Strings: TStrings);
begin
   Assert(Assigned(Strings));
   Strings.Clear;
   Strings.StrictDelimiter := true;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;


function GetRegisterIndex(aRegToFind: string; aRegisters: string): integer;
var
  n, i: integer;
begin
  Result := -1;
  n := Pos(aRegToFind, aRegisters);
  if (n > 0) then
    begin
      Inc(Result);
      for i := 1 to n do
        if (aRegisters[i] = ' ') then
          Inc(Result);
    end;
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
    // FileVerInfo.VersionStrings.Values['OriginalFileName'];
    // FileVerInfo.VersionStrings.Values['ProductName'];
    // FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    FileVerInfo.Free;
  end;
end;


end.

