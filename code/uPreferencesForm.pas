{ ==============================================================================

  PREFERENCES FORM

    Provides user with facility to amend:
      - path to application data (where ROMS etc are stored) - NOT USED AT PRESENT
      - assembler options; frame provided by assembler
      - other options; as required by other units, providing their own frames

    Saves form position to application's INI file


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

{ TODO : uPreferencesForm -> add button to browse to folder }

unit uPreferencesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls,
  //
  uMachineConfigBase, uMachineBase, uIniFile, uAsmConfigFrame, uCommon;

const
  MAIN_TAB        = 0;                  // Define tabs for external reference
  ASM_TAB         = 1;
  MACH_CONFIG_TAB = 2;

type
  TPreferencesForm = class(TForm)
    edDataPath: TLabeledEdit;
    GroupBox1: TGroupBox;
    PageControl1: TPageControl;        
    TabSheetMain: TTabSheet;
    TabSheetMachineConfig: TTabSheet;
    TabSheetAssembler: TTabSheet;  
    btnOK: TButton;
    btnCancel: TButton;
    Button1: TButton;                       
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);    
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    fMachineConfigFrame: TMachineConfigFrame;
    fShowAsmConfig: boolean;
    procedure SetMachineConfigFrame(aConfigFrame: TMachineConfigFrame);
    procedure SetConfigTabCaption(const aText: string);
    procedure SetTab(const aTab: integer);
    procedure SetShowAsmConfig(const aValue: boolean);
  public
    property MachineConfigFrame: TMachineConfigFrame write SetMachineConfigFrame;  
    property ConfigTabCaption: string write SetConfigTabCaption;
    property Tab: integer write SetTab;
    property ShowAsmConfig: boolean read fShowAsmConfig write SetShowAsmConfig;
  end;


implementation

{$R *.lfm}

const
  INI_SECT = 'PrefsForm';

{ CREATE }

procedure TPreferencesForm.FormCreate(Sender: TObject);
begin
  {$IFDEF darwin}
    Caption := 'Preferences';
  {$ELSE}
    Caption := 'Options';
  {$ENDIF}
  Top := AppIni.ReadInteger(INI_SECT, INI_WDW_TOP, 60);
  Left := AppIni.ReadInteger(INI_SECT, INI_WDW_LEFT, 60);

  //edDataPath.Text := GlobalVars.MachineDataPath;

  AsmConfigFrame := TAsmConfigFrame.Create(nil);
  AsmConfigFrame.Parent := TabSheetAssembler;
  AsmConfigFrame.Init;
end;


{ DESTROY }

procedure TPreferencesForm.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(INI_SECT, INI_WDW_TOP, Top);
  AppIni.WriteInteger(INI_SECT, INI_WDW_LEFT, Left);

  if (fMachineConfigFrame <> nil) then
    fMachineConfigFrame.Parent := nil;  // Disconnect from Frames (else pointer errors)
  AsmConfigFrame.Parent := nil;
  AsmConfigFrame.Free;
end;


{ SHOW / HIDE ASM CONFIG FRAME }

procedure TPreferencesForm.SetShowAsmConfig(const aValue: boolean);
begin
  fShowAsmConfig := aValue;
  TabSheetAssembler.Visible := aValue;
end;


{ SET MACHINE CONFIG FRAME }

procedure TPreferencesForm.SetMachineConfigFrame(aConfigFrame: TMachineConfigFrame);
begin
  if (aConfigFrame <> nil) and (fMachineConfigFrame <> aConfigFrame) then // Do assignment once only
    begin
      fMachineConfigFrame := aConfigFrame;
      fMachineConfigFrame.Parent := TabSheetMachineConfig;
      PageControl1.TabIndex := MAIN_TAB;
    end;
  TabSheetMachineConfig.Visible := (aConfigFrame <> nil);
end;


{ SET CONFIG TAB CAPTION }

procedure TPreferencesForm.SetConfigTabCaption(const aText: string);
begin
  TabSheetMachineConfig.Caption := aText;
end;


procedure TPreferencesForm.SetTab(const aTab: integer);
begin
  PageControl1.TabIndex := aTab;
end;


{ BUTTON OK }

procedure TPreferencesForm.btnOKClick(Sender: TObject);
begin
  // Save updated config data to Globals
  //GlobalVars.MachineDataPath := edDataPath.Text;
  Close;
end;


{ BUTTON CANCEL }

procedure TPreferencesForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;


end.

