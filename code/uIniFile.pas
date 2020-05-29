{ ==============================================================================

  INI FILE

    This unit allows simple access to a common INI file for all other units
    to utilise as required
    - call TAppIni.Create; sets global variable for AppIni
    - call relevant read write routines for Integer/Bool/String
    - call Free; closes IniFile


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

unit uIniFile;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, SysUtils, IniFiles;

type
  TOnCustomSectionEvent = procedure(var Value: string) of object;

  TAppIni = class(TObject)
  private
    IniFile: TMemIniFile;
    fOnCustomSection: TOnCustomSectionEvent;
    procedure CheckIfCustomSection(var aSect: string);
  public
    constructor Create(aFileName: string; CustomSectHandler: TOnCustomSectionEvent = nil);
    destructor  Destroy; override;

    function  ValueExists(aSection, aKey: string): boolean;
    procedure DeleteKey(aSection, aKey: string);
    procedure DeleteSection(aSection: string);
    function  ReadInteger(aSection, aKey: string; aDefault: integer): integer;
    procedure WriteInteger(aSection, aKey: string; aValue: integer);
    function  ReadString(aSection, aKey, aDefault: string): string;
    procedure WriteString(aSection, aKey, aValue: string);
    function  ReadBool(aSection, aKey: string; aDefault: boolean): boolean;
    procedure WriteBool(aSection, aKey: string; aValue: boolean);

    property OnCustomSection: TOnCustomSectionEvent read fOnCustomSection write fOnCustomSection;
  end;

var
  AppIni: TAppIni;

const
  INI_WDW_LEFT   = 'Left';                // Common INI file items
  INI_WDW_TOP    = 'Top';
  INI_WDW_WIDTH  = 'Width';
  INI_WDW_HEIGHT = 'Height';
  INI_WDW_VIS    = 'Vis';
  SECT_CUSTOM    = '*custom*';


implementation

{ CREATE }

constructor TAppIni.Create(aFileName: string; CustomSectHandler: TOnCustomSectionEvent);
begin
  try
    IniFile := TMemIniFile.Create(aFileName);
  except
    on E: Exception do
      MessageDlg('Error', Format('Error with INI file "%s"; %s', [aFileName, E.Message]), mtError, [mbOK], 0);
  end;                       
  IniFile.CacheUpdates := True;         // Keep updates in memory, write once
  fOnCustomSection := CustomSectHandler;
end;


destructor TAppIni.Destroy;
begin
  IniFile.UpdateFile;                   // Save any cached changes to disk
  IniFile.Free;
end;


{ Check if key value exists in INI file section }

function TAppIni.ValueExists(aSection, aKey: string): boolean;
begin
  CheckIfCustomSection(aSection);
  Result := IniFile.ValueExists(aSection, aKey);
end;


{ INI DELETE KEY }

procedure TAppIni.DeleteKey(aSection, aKey: string);
begin
  CheckIfCustomSection(aSection);
  IniFile.DeleteKey(aSection, aKey);
end;


{ INI DELETE SECTION }

procedure TAppIni.DeleteSection(aSection: string);
begin
  CheckIfCustomSection(aSection);
  IniFile.EraseSection(aSection);
end;


{ INI READ/WRITE INTEGER }

function TAppIni.ReadInteger(aSection, aKey: string; aDefault: integer): integer;
begin
  CheckIfCustomSection(aSection);
  Result := IniFile.ReadInteger(aSection, aKey, aDefault);
end;


procedure TAppIni.WriteInteger(aSection, aKey: string; aValue: integer);
begin
  CheckIfCustomSection(aSection);
  IniFile.WriteInteger(aSection, aKey, aValue);
end;


{ INI READ/WRITE BOOLEAN }

function TAppIni.ReadBool(aSection, aKey: string; aDefault: boolean): boolean;
begin
  CheckIfCustomSection(aSection);
  Result := IniFile.ReadBool(aSection, aKey, aDefault);
end;


procedure TAppIni.WriteBool(aSection, aKey: string; aValue: boolean);
begin
  CheckIfCustomSection(aSection);
  IniFile.WriteBool(aSection, aKey, aValue);
end;


{ INI READ/WRITE STRING }

function TAppIni.ReadString(aSection, aKey, aDefault: string): string;
begin
  CheckIfCustomSection(aSection);
  Result := IniFile.ReadString(aSection, aKey, aDefault);
end;


procedure TAppIni.WriteString(aSection, aKey, aValue: string);
begin
  CheckIfCustomSection(aSection);
  IniFile.WriteString(aSection, aKey, aValue);
end;


{ CHECK IF A CUSTOM SECTION }

procedure TAppIni.CheckIfCustomSection(var aSect: string);
begin
  if (LeftStr(aSect, 8) = SECT_CUSTOM) then
    begin
      aSect := Copy(aSect, 9, 999);
      if Assigned(fOnCustomSection) then
        fOnCustomSection(aSect);
    end;
end;


end.
