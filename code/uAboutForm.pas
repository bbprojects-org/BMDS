{ ==============================================================================

  ABOUT FORM

    Standard 'about the application' form, showing versions, etc. This form
    also shows where the application's dynamic data is stored

    If present, loads 'about' images from resources folder. Image size on this
    form is 320W x 170H, but image will be stretched to fit


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
    lblImgDesc: TLabel;
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
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    ImageIndex: integer;
  public
  end;

var
  AboutForm: TAboutForm;

const
  APP_NAME    = 'BMDS';
  APP_COPY    = 'Copyright RC Beveridge, 2002-';


implementation

{$R *.lfm}

const
  SECT_ABOUT = 'AboutForm';
  MONTHS: array[1..12] of string
    = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  ABOUT_ITEMS: array[0..3] of string = (
    'Assembler', 'CHIP-8', 'Microtan 65', 'Space Invaders' );


{ CREATE }

procedure TAboutForm.FormCreate(Sender: TObject);
var
  A: TStringArray;
  thisDate, thisYear: string;
begin
  Left := AppIni.ReadInteger(SECT_ABOUT, INI_WDW_LEFT, 50);
  Top := AppIni.ReadInteger(SECT_ABOUT, INI_WDW_TOP, 50);

  thisDate := {$I %DATE%};
  A := thisDate.Split('/');
  thisDate := Format('%d %s %s', [StrToInt(A[2]), MONTHS[StrToInt(A[1])], A[0]]);
  thisYear := A[0];
  A := GetAppInfo.Split('.');
  lblAppVersion.Caption := Format('Version %s.%s (build %s, %s)', [A[0], A[1], A[3], thisDate]);
  lblCopyright.Caption := APP_COPY + thisYear;
  lblLazarusVer.Caption := 'Lazarus: ' + LCLVersion;
  lblFPCVer.Caption := 'FPC: ' + {$I %FPCVERSION%};
  {$ifdef darwin}
    btnOk.Visible := False;             // On MacOS generally no exit button
  {$endif}
  lblAppDataFolder.Caption := 'App data folder: ' + GetAppDataDirectory;
  Randomize;
  ImageIndex := Random(Length(ABOUT_ITEMS));
  Timer1Timer(self);
end;


{ DESTROY }

procedure TAboutForm.FormDestroy(Sender: TObject);
begin
  AppIni.WriteInteger(SECT_ABOUT, INI_WDW_LEFT, Left);
  AppIni.WriteInteger(SECT_ABOUT, INI_WDW_TOP, Top);
  inherited;
end;


{ TIMER1 EVENT - show random about image }

procedure TAboutForm.Timer1Timer(Sender: TObject);
var
  thisFileName: string;
begin
  ImageIndex := (ImageIndex + 1 ) mod Length(ABOUT_ITEMS);
  thisFileName := GetAppResourcesDirectory + '/about/' + ABOUT_ITEMS[ImageIndex] + '.jpg';
  // If image file found it is loaded and hides caption behind image box
  if FileExists(thisFileName) then
    begin
      imgAbout.Picture.LoadFromFile(thisFileName);
      lblImgDesc.Caption := ABOUT_ITEMS[ImageIndex];
    end
  else
    lblImgDesc.Caption := '';
end;


end.
