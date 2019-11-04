{ ==============================================================================

  This code from the Delphi Persistent Container Library by Tommi Johtela
  (www.cs.uti.fi)

  22 Jun 03 - Minor correction to THashTable.Current
  24 Apr 14 - Converted to Lazarus / FPC

  =============================================================================}

unit uHashTables;

{$mode delphi}{$H+}

interface

uses SysUtils, Classes;

const
  EMPTY   = Pointer(-1);
  DELETED = Pointer(-2);

type
  THashTable = class(TObject)
  private
    Alpha: Double;
    fTable: PPointerList;
    fCount: Integer;
    fCapacity: Integer;
    fMaximumFillRatio: Double;
    fPosition: Integer;
    fCollisions: Integer;
    fInsertions: Integer;
    function GetAverageCollision: Real;
    procedure SetMaximumFillRatio(Value: Double);
  protected
    procedure Error(const msg: string);
    function Get(Key: Integer): Pointer; virtual;
    function GetIndex(Key: Integer): Integer;
    procedure Grow; virtual;
    function Hash(Key: Integer): Integer; virtual;
    procedure Put(Key: Integer; Item: Pointer); virtual;
    procedure Rehash(OldTable: PPointerList; OldCount: Integer);
    procedure SetCapacity(NewCapacity: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Current: Pointer; virtual;
    function DeleteCurrent: Pointer;
    function First: Pointer; virtual;
    function Insert(Key: Integer; Item: Pointer): Pointer; virtual;
    function Next: Pointer; virtual;
    function NextSame(Key: Integer): Pointer;
    function Remove(Key: Integer): Pointer; virtual;
    procedure Pack;
    property Capacity: Integer read fCapacity write SetCapacity;
    property Count: Integer read fCount;
    property MaximumFillRatio: Double read fMaximumFillRatio write SetMaximumFillRatio;
    property Items[Key: Integer]: Pointer read Get write Put; default;
    property AverageCollisions: Real read GetAverageCollision;
  end;

  TStringTableNode = record
    FKey: string;
    FObject: TObject;
  end;
  PStringTableNode = ^TStringTableNode;

  TStringTable = class(THashTable)
  private
    function ConvertKey(const Key: string): Integer;
    function FindKey(const Key: string; var Node: PStringTableNode): Boolean;
  protected
    function Get(const Key: string): TObject; reintroduce;
    procedure Put(const Key: string; Item: TObject); reintroduce;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function Current: TObject; reintroduce;
    function CurrentKey: string; reintroduce;
    function First: TObject; reintroduce;
    function Insert(const Key: string; Item: TObject): Pointer; reintroduce;
    function Next: TObject; reintroduce;
    function Remove(const Key: string): TObject; reintroduce;
    property Items[const Key: string]: TObject read Get write Put; default;
  end;

  EHashTableError = class(Exception);


implementation

constructor THashTable.Create;
begin
  Alpha := (Sqrt(5.0) - 1) / 2.0;
  fMaximumFillRatio := 0.8;
  SetCapacity(256);
end;


destructor THashTable.Destroy;
begin
  FreeMem(fTable, fCapacity * (SizeOf(Pointer) * 2));
end;


procedure THashTable.Clear;
begin
  fCount := 0;
  fPosition := -2;
  FillChar(fTable^, fCapacity * (SizeOf(Pointer) * 2), Char(EMPTY));
end;


function THashTable.Current: Pointer;
begin
  {
  Following line corrected, 22 Jun 03
  if (fPosition >= 0) and (fPosition < fCapacity) and
  }
  if (fPosition >= 0) and (fPosition < (fCapacity * 2)) and
    (fTable[fPosition] <> EMPTY) and (fTable[fPosition] <> DELETED) then
    Result := fTable[fPosition + 1]
  else
    Result := nil;
end;


function THashTable.DeleteCurrent: Pointer;
begin
  fTable[fPosition] := DELETED;
  Result := fTable[fPosition + 1];
  Dec(fCount);
end;


procedure THashTable.Error(const msg: string);
begin
  raise EHashTableError.Create(msg);
end;


function THashTable.First: Pointer;
begin
  fPosition := -2;
  Result := Next;
end;


function THashTable.Get(Key: Integer): Pointer;
begin
  fPosition := GetIndex(Key);
  if Integer(fTable[fPosition]) = Key then
    Result := fTable[fPosition + 1]
  else
    Result := nil;
end;


function THashTable.GetAverageCollision: Real;
begin
  if fInsertions = 0 then
    Result := 0.0
  else
    Result := fCollisions / fInsertions;
end;


function THashTable.GetIndex(Key: Integer): Integer;
var I: Integer;
begin
  Result := Hash(Key) * 2;
  I := 0;
  while (I < fCapacity) and (fTable[Result] <> Pointer(Key)) and
    (fTable[Result] <> EMPTY) do
    begin
      Inc(Result, 2);
      Inc(I);
      Result := Result mod (fCapacity * 2);
    end;
end;


procedure THashTable.Grow;
begin
  SetCapacity(fCapacity * 2);
end;


function THashTable.Hash(Key: Integer): Integer;
begin
  if (Key < 0) then
    Error('Keys Must Be Positive');
  Result := Trunc(fCapacity * Frac(Alpha * Key));
end;


function THashTable.Insert(Key: Integer; Item: Pointer): Pointer;
begin
  if (fCount + 1) >= Round(fCapacity * fMaximumFillRatio) then
    Grow;
  Inc(fCount);
  fPosition := Hash(Key) * 2;
  while (fTable[fPosition] <> EMPTY) and (fTable[fPosition] <> DELETED) do
    begin
      Inc(fCollisions);
      Inc(fPosition, 2);
      fPosition := fPosition mod (fCapacity * 2);
    end;
  fTable[fPosition] := Pointer(Key);
  fTable[fPosition + 1] := Item;
  Result := @fTable[fPosition + 1];
  Inc(fInsertions);
end;


function THashTable.Next: Pointer;
begin
  Inc(fPosition, 2);
  while (fPosition < (fCapacity * 2)) and ((fTable[fPosition] = EMPTY) or
    (fTable[fPosition] = DELETED)) do
    Inc(fPosition, 2);
  if fPosition < (fCapacity * 2) then
    Result := fTable[fPosition + 1]
  else
    Result := nil;
end;


function THashTable.NextSame(Key: Integer): Pointer;
var oldpos: Integer;
begin
  oldpos := fPosition;
  Inc(fPosition, 2);
  while (fPosition <> oldpos) and (fTable[fPosition] <> EMPTY) and
    (fTable[fPosition] <> Pointer(Key)) do
    begin
      Inc(fPosition, 2);
      fPosition := fPosition mod (fCapacity * 2);
    end;
  if (fTable[fPosition] = Pointer(Key)) then
    Result := fTable[fPosition + 1]
  else
    Result := nil;
end;


procedure THashTable.Pack;
begin
  SetCapacity(Round(fCount * (1 / fMaximumFillRatio)) + 2);
end;


procedure THashTable.Put(Key: Integer; Item: Pointer);
begin
  fPosition := GetIndex(Key);
  if (Integer(fTable[fPosition]) = Key) then
    fTable[fPosition + 1] := Item
  else
    Insert(Key, Item);
end;


function THashTable.Remove(Key: Integer): Pointer;
begin
  fPosition := GetIndex(Key);
  if (Integer(fTable[fPosition]) = Key) then
    begin
      fTable[fPosition] := DELETED;
      Result := fTable[fPosition + 1];
      Dec(fCount);
    end
  else
    Result := nil;
end;


procedure THashTable.Rehash(OldTable: PPointerList; OldCount: Integer);
var
  i: Integer;
begin
  i := 0;
  while fCount < OldCount do
    begin
      while (OldTable[i] = EMPTY) or (OldTable[i] = DELETED) do
        Inc(i, 2);
      Insert(Integer(OldTable[i]), OldTable[i + 1]);
      Inc(i, 2);
    end;
end;


procedure THashTable.SetCapacity(NewCapacity: Integer);
var
  OldTable: Pointer;
  OldCapacity, OldCount: Integer;
begin
  if (fCount >= Round(NewCapacity * fMaximumFillRatio)) or
    (NewCapacity > (MaxListSize div 2)) then
    Error('Invalid Capacity');
  if (NewCapacity <> fCapacity) then
    begin
      OldTable := fTable;
      fTable := AllocMem(NewCapacity * (SizeOf(Pointer) * 2));
      OldCapacity := fCapacity;
      fCapacity := NewCapacity;
      OldCount := fCount;
      fPosition := -1;
      Clear;
      ReHash(OldTable, OldCount);
      FreeMem(OldTable, OldCapacity * (SizeOf(Pointer) * 2));
    end;
end;


procedure THashTable.SetMaximumFillRatio(Value: Double);
begin
  if (Value < 0.5) or (Value > 1.0) then
    Error('Max Fill Ratio');
  fMaximumFillRatio := Value;
  if (fCount > Round(fCapacity * fMaximumFillRatio)) then
    Grow;
end;


{ TStringTable }

procedure TStringTable.Clear;
var pt: PStringTableNode;
begin
  pt := PStringTableNode(inherited First);
  while pt <> nil do
    begin
      Dispose(pt);
      pt := inherited Next;
    end;
  inherited Clear;
end;


function TStringTable.ConvertKey(const Key: string): Integer;
var i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Key) do
    Result := (131 * Result) + Ord(Key[i]);
  Result := Abs(Result);
end;


function TStringTable.Current: TObject;
var pt: PStringTableNode;
begin
  pt := inherited Current;
  if (pt <> nil) then
    Result := pt^.FObject
  else
    Result := nil;
end;


function TStringTable.CurrentKey: string;
var pt: PStringTableNode;
begin
  pt := inherited Current;
  if (pt <> nil) then
    Result := pt^.FKey
  else
    Result := '';
end;


destructor TStringTable.Destroy;
begin
  Clear;
  inherited Destroy;
end;


function TStringTable.FindKey(const Key: string; var Node: PStringTableNode): Boolean;
var k: Integer;
begin
  k := ConvertKey(Key);
  Node := inherited Get(k);
  while (Node <> nil) and (Node^.FKey <> Key) do
    NextSame(k);
  Result := (Node <> nil);
end;


function TStringTable.First: TObject;
var pt: PStringTableNode;
begin
  pt := inherited First;
  if (pt <> nil) then
    Result := pt^.FObject
  else
    Result := nil;
end;


function TStringTable.Get(const Key: string): TObject;
var pt: PStringTableNode;
begin
  if FindKey(Key, pt) then
    Result := pt^.FObject
  else
    Result := nil;
end;


function TStringTable.Insert(const Key: string; Item: TObject): Pointer;
var pt: PStringTableNode;
begin
  New(pt);
  pt^.FKey := Key;
  pt^.FObject := Item;
  inherited Insert(ConvertKey(Key), pt);
  Result := @(pt^.FObject);
end;


function TStringTable.Next: TObject;
var pt: PStringTableNode;
begin
  pt := inherited Next;
  if (pt <> nil) then
    Result := pt^.FObject
  else
    Result := nil;
end;


procedure TStringTable.Put(const Key: string; Item: TObject);
var pt: PStringTableNode;
begin
  if FindKey(Key, pt) then
    pt^.FObject := Item
  else
    Insert(Key, Item);
end;


function TStringTable.Remove(const Key: string): TObject;
var pt: PStringTableNode;
begin
  if FindKey(Key, pt) then
    begin
      DeleteCurrent;
      Result := pt^.FObject;
      Dispose(pt);
    end
  else
    Result := nil;
end;


end.
