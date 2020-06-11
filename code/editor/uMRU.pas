{ ==============================================================================

  MOST RECENTLY USED FILE LIST

    Provides an MRU list for a reference menu item passed after creation

    Any files added to the MRU are saved in the application INI file and
    restored each time on run

    Clicking on an MRU item initiates a callback with the filename selected


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

unit uMRU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Dialogs,
  //
  uIniFile;

type
  TOnMenuClickEvent = procedure(FileName: string) of object;

  { TMRU }

  TMRU = class(TObject)
    private
      fRecent: TStringList;
      fRecentMenuRef: TMenuItem;
      fOnMenuClick: TOnMenuClickEvent;
      procedure BuildReopenList;
      procedure FileReopen(Sender: TObject);
      procedure LoadRecent;
      procedure SaveRecent;
      function GetCount: integer;
      procedure SetRecentMenuRef(aItem: TMenuItem);
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddToRecent(FileName: string);
      //
      property RecentMenuItem: TMenuItem read fRecentMenuRef write SetRecentMenuRef;
      property Count: integer read GetCount;
      property OnMenuClick: TOnMenuClickEvent read fOnMenuClick write fOnMenuClick;
  end;


implementation

const
  MAX_MRU    = 10;
  INI_PREFIX = 'MRU';


{ CREATE }

constructor TMRU.Create;
begin
  fRecent := TStringList.Create;
end;


{ DESTROY }

destructor TMRU.Destroy;
begin
  SaveRecent;
  FreeAndNil(fRecent);
end;


{ SET RECENT MENU REFERENCE - this needs to be assigned before the MRU works }

procedure TMRU.SetRecentMenuRef(aItem: TMenuItem);
begin
  if (fRecentMenuRef = aItem) then
    Exit;
  fRecentMenuRef := aItem;
  LoadRecent;
end;


{ FILE REOPEN - open selected MRU item in editor }

procedure TMRU.FileReopen(Sender: TObject);
begin
  if Assigned(fOnMenuClick) then
    fOnMenuClick((Sender as TMenuItem).Caption);
end;


{ ADD TO RECENT - add filename provided as top item in MRU list }

procedure TMRU.AddToRecent(FileName: string);
var
  Index: integer;
begin
  Index := fRecent.IndexOf(FileName);
  if (Index <> -1) then                 // Move recent to top of list
    fRecent.Delete(Index);
  fRecent.Insert(0, FileName);
  while (fRecent.Count > MAX_MRU) do    // and delete any beyond max
    fRecent.Delete(fRecent.Count-1);
  BuildReopenList;
end;


{ BUILD REOPEN LIST - writes items to menu item as required }

procedure TMRU.BuildReopenList;

  function NewRecentMenuItem(aFileName: string): TMenuItem;
  begin
    Result := TMenuItem.Create(nil);
    Result.Caption := aFileName;
    Result.OnClick := @FileReopen;
  end;

var
  i: integer;
  mi: TMenuItem;
begin
  if Assigned(fRecentMenuRef) then
    begin
      fRecentMenuRef.Clear;
      for i := 0 to fRecent.Count-1 do
        begin
          mi := NewRecentMenuItem(fRecent[i]);
          fRecentMenuRef.Add(mi);
        end;
    end;
  fRecentMenuRef.Enabled := (fRecent.Count > 0);
end;


{ GET COUNT - number of items in MRU list }

function TMRU.GetCount: integer;
begin
  Result := fRecent.Count;
end;


{ LOAD RECENT - get MRU items from application INI file }

procedure TMRU.LoadRecent;
var
  i, ThisCount: integer;
  filename: string;
begin
  fRecent.Clear;
  ThisCount := AppIni.ReadInteger(SECT_CUSTOM, INI_PREFIX + 'Count', 0);
  for i := 1 to ThisCount do
    begin
      filename := AppIni.ReadString(SECT_CUSTOM, INI_PREFIX + 'File' + IntToStr(i), '');
      if (filename <> '') then
        fRecent.Add(filename);
    end;
  BuildReopenList;
end;


{ SAVE RECENT - save MRU items to application INI file }

procedure TMRU.SaveRecent;
var
  i: integer;
begin
  AppIni.WriteInteger(SECT_CUSTOM, INI_PREFIX + 'Count', fRecent.Count);
  for i := 1 to fRecent.Count do
    AppIni.WriteString(SECT_CUSTOM, INI_PREFIX + 'File' + IntToStr(i), fRecent[i-1]);
end;


end.

