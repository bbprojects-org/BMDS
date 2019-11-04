{ ==============================================================================

  ASSEMBLER OPTIONS FRAME

    This frame is displayed on a tab on the main Preferences form if the
    currently selected CPU type supports the assembler mode, otherwise the
    tab, and this frame, is hidden

    The frame saves option settings in the application's INI file


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

unit uAsmConfigFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  //
  uCommon, uIniFile;

type

  { TAsmConfigFrame }

  TAsmConfigFrame = class(TFrame)
    btnResetDefaults: TButton;
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
    Label1: TLabel;
    lblFormat: TLabel;
    procedure btnResetDefaultsClick(Sender: TObject);
    procedure cbGenerateListingChange(Sender: TObject);
    procedure cbWriteCodeToFileChange(Sender: TObject);
  private
    function GetGenerateListing: boolean;
    function GetListIncludes: boolean;
    function GetListMacros: boolean;
    function GetListMultiBytes: boolean;
    function GetListSymbols: boolean;
    function GetShowTime: boolean;
    function GetWriteToFile: boolean;
    function GetWriteToMemory: boolean;
    procedure ReadIniItems;
  public                 
    procedure Init;
    destructor Destroy; override;
    //
    property GenerateListing: boolean read GetGenerateListing;
    property ListIncludes: boolean read GetListIncludes;
    property ListMultiBytes: boolean read GetListMultiBytes;
    property ListMacros: boolean read GetListMacros;
    property ListSymbols: boolean read GetListSymbols;
    property ShowTime: boolean read GetShowTime;
    property WriteToMemory: boolean read GetWriteToMemory;
    property WriteToFile: boolean read GetWriteToFile;
  end;

var
  AsmConfigFrame: TAsmConfigFrame;


implementation

{$R *.lfm}

const
  INI_PREFIX   = 'AsmCfg';
  INI_GENLIST  = 'GenerateListing';
  INI_LSTINC   = 'ListIncludes';
  INI_LSTMULT  = 'ListMulti';
  INI_LSTMAC   = 'ListMacros';
  INI_LSTSYM   = 'ListSymbols';
  INI_SHOWTIME = 'ShowTime';
  INI_WRMEM    = 'WriteMemory';
  INI_WRFILE   = 'WriteFile';
  INI_FILEIDX  = 'WriteFileIndex';


{ INITIALISE CONFIG FRAME... read settings }

procedure TAsmConfigFrame.Init;
begin
  ReadIniItems;
end;


procedure TAsmConfigFrame.ReadIniItems;
begin
  cbGenerateListing.Checked := AppIni.ReadBool(INI_PREFIX, INI_GENLIST, True);
  cbListIncludes.Checked := AppIni.ReadBool(INI_PREFIX, INI_LSTINC, False);
  cbListMultiBytes.Checked := AppIni.ReadBool(INI_PREFIX, INI_LSTMULT, False);
  cbListMacros.Checked := AppIni.ReadBool(INI_PREFIX, INI_LSTMAC, False);
  cbListSymbols.Checked := AppIni.ReadBool(INI_PREFIX, INI_LSTSYM, False);
  cbShowTime.Checked := AppIni.ReadBool(INI_PREFIX, INI_SHOWTIME, False);
  cbWriteCodeToMemory.Checked := AppIni.ReadBool(INI_PREFIX, INI_WRMEM, False);
  cbWriteCodeToFile.Checked := AppIni.ReadBool(INI_PREFIX, INI_WRFILE, False);
  comboOutputFormat.ItemIndex := AppIni.ReadInteger(INI_PREFIX, INI_FILEIDX, 0);
end;


{ DESTROY }

destructor TAsmConfigFrame.Destroy;
begin
  AppIni.WriteBool(INI_PREFIX, INI_GENLIST, cbGenerateListing.Checked);
  AppIni.WriteBool(INI_PREFIX, INI_LSTINC, cbListIncludes.Checked);
  AppIni.WriteBool(INI_PREFIX, INI_LSTMULT, cbListMultiBytes.Checked);
  AppIni.WriteBool(INI_PREFIX, INI_LSTMAC, cbListMacros.Checked);
  AppIni.WriteBool(INI_PREFIX, INI_LSTSYM, cbListSymbols.Checked);
  AppIni.WriteBool(INI_PREFIX, INI_SHOWTIME, cbShowTime.Checked);
  AppIni.WriteBool(INI_PREFIX, INI_WRMEM, cbWriteCodeToMemory.Checked);
  AppIni.WriteBool(INI_PREFIX, INI_WRFILE, cbWriteCodeToFile.Checked);
  AppIni.WriteInteger(INI_PREFIX, INI_FILEIDX, comboOutputFormat.ItemIndex);
  inherited Destroy;
end;


{ GET OPTION STATES }

function TAsmConfigFrame.GetGenerateListing: boolean;
begin
  Result := cbGenerateListing.Checked;
end;


function TAsmConfigFrame.GetListIncludes: boolean;
begin                                                   
  Result := cbGenerateListing.Checked and cbListIncludes.Checked;
end;


function TAsmConfigFrame.GetListMacros: boolean;
begin                            
  Result := cbGenerateListing.Checked and cbListMacros.Checked;
end;


function TAsmConfigFrame.GetListMultiBytes: boolean;
begin    
  Result := cbGenerateListing.Checked and cbListMultiBytes.Checked;
end;


function TAsmConfigFrame.GetListSymbols: boolean;
begin                         
  Result := cbGenerateListing.Checked and cbListSymbols.Checked;
end;


function TAsmConfigFrame.GetShowTime: boolean;
begin  
  Result := cbGenerateListing.Checked and cbShowTime.Checked;
end;


function TAsmConfigFrame.GetWriteToFile: boolean;
begin 
  Result := cbWriteCodeToFile.Checked;
end;


function TAsmConfigFrame.GetWriteToMemory: boolean;
begin 
  Result := cbWriteCodeToMemory.Checked;
end;


{ ENABLE / DISABLE OTHER ITEMS AS REQUIRED }

procedure TAsmConfigFrame.cbGenerateListingChange(Sender: TObject);
begin
  cbListIncludes.Enabled := cbGenerateListing.Checked;
  cbListSymbols.Enabled := cbGenerateListing.Checked;
  cbListMacros.Enabled := cbGenerateListing.Checked;
  cbListMultiBytes.Enabled := cbGenerateListing.Checked;
  cbShowTime.Enabled := cbGenerateListing.Checked;
end;


procedure TAsmConfigFrame.cbWriteCodeToFileChange(Sender: TObject);
begin
  lblFormat.Enabled := cbWriteCodeToFile.Checked;
  comboOutputFormat.Enabled := cbWriteCodeToFile.Checked;
end;


{ RESET DEFAULTS }

procedure TAsmConfigFrame.btnResetDefaultsClick(Sender: TObject);
begin
  if (ConfirmResetDefault) then
    begin
      AppIni.DeleteSection(INI_PREFIX);
      ReadIniItems;
    end;
end;


end.

