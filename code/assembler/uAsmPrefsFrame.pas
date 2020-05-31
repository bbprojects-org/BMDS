{ ==============================================================================

  ASSEMBLER PREFERENCES FRAME

    Provides facility to amend assembler options


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

unit uAsmPrefsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  //
  uPrefsFrameBase, uIniFile, uCommon;

type

  { TAsmPrefsFrame }

  TAsmPrefsFrame = class(TPrefsFrame)
    cbGenerateListing: TCheckBox;
    cbListIncludes: TCheckBox;
    cbListMacros: TCheckBox;
    cbListMultiBytes: TCheckBox;
    cbListSymbols: TCheckBox;
    cbShowTime: TCheckBox;
    cbWriteCodeToFile: TCheckBox;
    cbWriteCodeToMemory: TCheckBox;
    comboOutputFormat: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lblFormat: TLabel;
    rg8080Asm: TRadioGroup;
    procedure btnResetDefaultsClick(Sender: TObject);
    procedure cbChange(Sender: TObject);
    procedure cbGenerateListingChange(Sender: TObject);
    procedure cbWriteCodeToFileChange(Sender: TObject);
    procedure comboOutputFormatChange(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    procedure ReadIniItems;
    procedure WriteIniItems;
    function GetGenerateListing: boolean;
    function GetListIncludes: boolean;
    function GetListMacros: boolean;
    function GetListMultiBytes: boolean;
    function GetListSymbols: boolean;
    function GetShowTime: boolean;
    function GetWriteToFile: boolean;
    function GetWriteToMemory: boolean;
  public
    procedure Init; override;
    destructor Destroy; override;
    procedure SaveChanges; override;
    procedure CancelChanges; override;
    //
    property GenerateListing: boolean read GetGenerateListing;
    property ListIncludes: boolean read GetListIncludes;
    property ListMacros: boolean read GetListMacros;
    property ListMultiBytes: boolean read GetListMultiBytes;
    property ListSymbols: boolean read GetListSymbols;
    property ShowTime: boolean read GetShowTime;
    property WriteToFile: boolean read GetWriteToFile;
    property WriteToMemory: boolean read GetWriteToMemory;
  end;

var
  AsmPrefs: TAsmPrefsFrame;


implementation

{$R *.lfm}

const
  SECT_ASMPREFS = 'AsmPrefs';
  INI_GENLIST   = 'GenerateListing';
  INI_LSTINC    = 'ListIncludes';
  INI_LSTMULT   = 'ListMulti';
  INI_LSTMAC    = 'ListMacros';
  INI_LSTSYM    = 'ListSymbols';
  INI_SHOWTIME  = 'ShowTime';
  INI_WRMEM     = 'WriteMemory';
  INI_WRFILE    = 'WriteFile';
  INI_FILEIDX   = 'WriteFileIndex';


{ INIT }

procedure TAsmPrefsFrame.Init;
begin
  inherited Init;
  ReadIniItems;
end;


{ DESTROY }

destructor TAsmPrefsFrame.Destroy;
begin
  inherited Destroy;
end;


{ SAVE CHANGES / CANCEL CHANGES / RESET DEFAULTS }

procedure TAsmPrefsFrame.SaveChanges;
begin
  if (fChanged) then
    WriteIniItems;
end;


procedure TAsmPrefsFrame.CancelChanges;
begin
  if (fChanged) then
    ReadIniItems;
end;


procedure TAsmPrefsFrame.btnResetDefaultsClick(Sender: TObject);
begin
  if (ConfirmResetDefault) then
    begin
      AppIni.DeleteSection(SECT_ASMPREFS);
      ReadIniItems;
    end;
end;


{ GET OPTION STATES }

function TAsmPrefsFrame.GetGenerateListing: boolean;
begin
  Result := cbGenerateListing.Checked;
end;


function TAsmPrefsFrame.GetListIncludes: boolean;
begin
  Result := cbGenerateListing.Checked and cbListIncludes.Checked;
end;


function TAsmPrefsFrame.GetListMacros: boolean;
begin
  Result := cbGenerateListing.Checked and cbListMacros.Checked;
end;


function TAsmPrefsFrame.GetListMultiBytes: boolean;
begin
  Result := cbGenerateListing.Checked and cbListMultiBytes.Checked;
end;


function TAsmPrefsFrame.GetListSymbols: boolean;
begin
  Result := cbGenerateListing.Checked and cbListSymbols.Checked;
end;


function TAsmPrefsFrame.GetShowTime: boolean;
begin
  Result := cbGenerateListing.Checked and cbShowTime.Checked;
end;


function TAsmPrefsFrame.GetWriteToFile: boolean;
begin
  Result := cbWriteCodeToFile.Checked;
end;


function TAsmPrefsFrame.GetWriteToMemory: boolean;
begin
  Result := cbWriteCodeToMemory.Checked;
end;


{ CHECKBOX CHANGES }

procedure TAsmPrefsFrame.cbGenerateListingChange(Sender: TObject);
begin
  cbListIncludes.Enabled := cbGenerateListing.Checked;
  cbListSymbols.Enabled := cbGenerateListing.Checked;
  cbListMacros.Enabled := cbGenerateListing.Checked;
  cbListMultiBytes.Enabled := cbGenerateListing.Checked;
  cbShowTime.Enabled := cbGenerateListing.Checked;
  fChanged := True;
end;


procedure TAsmPrefsFrame.cbWriteCodeToFileChange(Sender: TObject);
begin
  lblFormat.Enabled := cbWriteCodeToFile.Checked;
  comboOutputFormat.Enabled := cbWriteCodeToFile.Checked;
  fChanged := True;
end;


procedure TAsmPrefsFrame.comboOutputFormatChange(Sender: TObject);
begin
  // Do any changes needed
  fChanged := True;
end;

procedure TAsmPrefsFrame.Label1Click(Sender: TObject);
begin

end;


procedure TAsmPrefsFrame.cbChange(Sender: TObject);
begin
  fChanged := True;
end;


{ READ / WRITE TO INI FILE }

procedure TAsmPrefsFrame.ReadIniItems;
begin
  cbGenerateListing.Checked := AppIni.ReadBool(SECT_ASMPREFS, INI_GENLIST, True);
  cbListIncludes.Checked := AppIni.ReadBool(SECT_ASMPREFS, INI_LSTINC, False);
  cbListMultiBytes.Checked := AppIni.ReadBool(SECT_ASMPREFS, INI_LSTMULT, False);
  cbListMacros.Checked := AppIni.ReadBool(SECT_ASMPREFS, INI_LSTMAC, False);
  cbListSymbols.Checked := AppIni.ReadBool(SECT_ASMPREFS, INI_LSTSYM, False);
  cbShowTime.Checked := AppIni.ReadBool(SECT_ASMPREFS, INI_SHOWTIME, False);
  cbWriteCodeToMemory.Checked := AppIni.ReadBool(SECT_ASMPREFS, INI_WRMEM, False);
  cbWriteCodeToFile.Checked := AppIni.ReadBool(SECT_ASMPREFS, INI_WRFILE, False);
  comboOutputFormat.ItemIndex := AppIni.ReadInteger(SECT_ASMPREFS, INI_FILEIDX, 0);
  cbGenerateListingChange(nil);
  cbWriteCodeToFileChange(nil);
end;


procedure TAsmPrefsFrame.WriteIniItems;
begin
  AppIni.WriteBool(SECT_ASMPREFS, INI_GENLIST, cbGenerateListing.Checked);
  AppIni.WriteBool(SECT_ASMPREFS, INI_LSTINC, cbListIncludes.Checked);
  AppIni.WriteBool(SECT_ASMPREFS, INI_LSTMULT, cbListMultiBytes.Checked);
  AppIni.WriteBool(SECT_ASMPREFS, INI_LSTMAC, cbListMacros.Checked);
  AppIni.WriteBool(SECT_ASMPREFS, INI_LSTSYM, cbListSymbols.Checked);
  AppIni.WriteBool(SECT_ASMPREFS, INI_SHOWTIME, cbShowTime.Checked);
  AppIni.WriteBool(SECT_ASMPREFS, INI_WRMEM, cbWriteCodeToMemory.Checked);
  AppIni.WriteBool(SECT_ASMPREFS, INI_WRMEM, cbWriteCodeToMemory.Checked);
  AppIni.WriteBool(SECT_ASMPREFS, INI_WRFILE, cbWriteCodeToFile.Checked);
  AppIni.WriteInteger(SECT_ASMPREFS, INI_FILEIDX, comboOutputFormat.ItemIndex);
end;


end.

