{ ==============================================================================

  ABOUT FORM

    Standard 'about the application' form, showing versions, etc. This form
    also shows where the application's dynamic data is stored

    If there is an 'about.jpg' image in the current machine's root folder
    then this will be loaded and shown in this About form. Image size on this
    form is 320W x 170H, but file image will be stretched to fit


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

unit uAboutForm;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Resource,
  //
  uMachineBase, uIniFile, uCommon;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    lblAppDataFolder: TLabel;
    lblAppTitle1: TLabel;
    lblFPCVer: TLabel;
    imgAbout: TImage;
    lblLazarusVer: TLabel;
    Panel2: TPanel;
    lblAppTitle2: TLabel;
    lblAppVersion: TLabel;
    lblCopyright: TLabel;
    Panel1: TPanel;
    Image1: TImage;
    btnOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure SetMachineDataFolder(const Value: string);
  public
    property MachineDataFolder: string write SetMachineDataFolder;
  end;

var
  AboutForm: TAboutForm;

const
  APP_NAME    = 'BMDS';
  APP_COPY    = 'Copyright RC Beveridge, 2002-2019';


implementation

{$R *.lfm}

const
  SECT_INI = 'AboutForm';
  MONTHS: array[1..12] of string
    = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

{ CREATE }

procedure TAboutForm.FormCreate(Sender: TObject);
var
  A: TStringArray;
  thisDate: string;
begin
  Left := AppIni.ReadInteger(SECT_INI, INI_WDW_LEFT, 50);
  Top := AppIni.ReadInteger(SECT_INI, INI_WDW_TOP, 50);

  thisDate := {$I %DATE%};
  A := thisDate.Split('/');
  thisDate := Format('%d %s %s', [StrToInt(A[2]), MONTHS[StrToInt(A[1])], A[0]]);
  A := GetAppInfo.Split('.');
  lblAppVersion.Caption := Format('Version %s.%s (build %s, %s)', [A[0], A[1], A[3], thisDate]);
  lblCopyright.Caption := APP_COPY;
  lblLazarusVer.Caption := 'Lazarus: ' + LCLVersion;
  lblFPCVer.Caption := 'FPC: ' + {$I %FPCVERSION%};
  {$ifdef darwin}
    btnOk.Visible := False;             // On MacOS generally no exit button
  {$endif}
  lblAppDataFolder.Caption := 'App data folder: ' + GetAppDataDirectory;
end;


{ DESTROY }

procedure TAboutForm.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_INI, INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_INI, INI_WDW_TOP, Top);
end;


{ SET MACHINE DATA FOLDER; show About image }

procedure TAboutForm.SetMachineDataFolder(const Value: string);
var
  thisFilename: string;
begin
  // If about.jpg for this machine does not exist, then caption shows through
  // the empty About image. If it does exist the file is loaded and hides
  // the message underneath
  thisFilename := Value + 'about.jpg';
  if FileExists(thisFilename) then
    imgAbout.Picture.LoadFromFile(thisFilename);
end;


end.
