{ ==============================================================================

  READ/WRITE BINARY IN TEXT FORMAT (Intel Hex / Motorola S9)

  When reading a file, the ReadHex routine will set property StartAddress
  to the first record's address, set BytesRead to be the number of bytes read,
  and the data is in the property BytesArray.
  Any errors will result in exceptions

  When writing a file...


  Intel hex format:

  :aabbbbccdddd...ddee

  aa    = record data length (the number of dd bytes)
  bbbb  = address for this record
  cc    = record type
        00 = data (data is in the dd bytes)
        01 = end of file (bbbb is transfer address)
        02 = extended segment address record
             dddd (two bytes) represents the segment address
        03 = Start segment address record
             dddd (two bytes) represents the segment of the transfer address
        04 = extended linear address record
             dddd (two bytes) represents the high address word
        05 = Start linear address record
             dddd (two bytes) represents the high word of the transfer address
  dd... = data bytes as required
  ee    = checksum byte: add all bytes aa through dd (2's complement negate)


  Motorola S-record format:

  Sabbcc...ccdd...ddee

  a     = record type
          0 starting record (optional)
          1 data record with 16-bit address
          2 data record with 24-bit address
          3 data record with 32-bit address
          4 symbol record (LSI extension) - S4<length><address><name>,<checksum>
          5 number of data records in preceeding block
          6 unused
          7 ending record for S3 records
          8 ending record for S2 records
          9 ending record for S1 records
  bb    = record data length (from bb through ee)
  cc... = address for this record, 2 bytes for S1/S9, 3 bytes for S2/S8, 4 bytes for S3/S7
  dd... = data bytes if record type needs it
  ee    = checksum byte: add all bytes bb through dd (1's complement)

  =============================================================================}

unit uReadWriteHex;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Dialogs;

const
  MAX_BYTES_PER_LINE     = 16;
  ERR_MISSING_COLON      = 'Missing colon at start of line';
  ERR_TYPE_NOT_SUPPORTED = 'Record type %d not supported';
  ERR_TYPE_NOT_VALID     = 'Record type %d not valid';
  ERR_INVALID_HEX        = '[%s] is not a valid hex number';

type
  TRecordType = (rtAuto, rtIntelHex, rtMotorolaS19);

  TBytesArray = array of byte;

  { TReadHex }

  TReadHex = class
  private
    fBytesArray: TBytesArray;
    fTotalBytes: integer;
    fStartAddress: word;
    AddressHasBeenSet: boolean;
    fErrorMessage: string;
    function ConvertLineIntelHex(s: string): boolean;
    function HexToInt(s: string): integer;
    procedure AddError(const Msg: string);
  public
    constructor Create(FileName: string; RecType: TRecordType = rtAuto);
    destructor Destroy; override;
    property BytesArray: TBytesArray read fBytesArray;
    property BytesRead: integer read fTotalBytes;
    property StartAddress: word read fStartAddress;
    property ErrorMessage: string read fErrorMessage;
  end;


  { TWriteHex }

  TWriteHex = class
  private
    BytesArray: array[1..MAX_BYTES_PER_LINE] of byte;
    ByteCount: integer;
    fAddress: word;
    HexFile: TextFile;
    fRecType: TRecordType;
    fErrorMessage: string;
    procedure FlushBytes;
    procedure WriteLastRecord;
    procedure AddError(const Msg: string);
  public
    constructor Create(FileName: string; RecType: TRecordType = rtIntelHex);
    destructor Destroy; override;
    procedure SetStart(aAddress: integer);
    procedure WriteByte(aByte: byte);
    procedure WriteBytes(aBytesArray: TBytesArray; Pos1, Pos2: integer);
    property ErrorMessage: string read fErrorMessage;
  end;


implementation

{ ReadHex }

constructor TReadHex.Create(FileName: string; RecType: TRecordType);
var
  HexFileLines: TStringList;
  i: integer;
  LineWasData: boolean;
  FirstChar: char;
begin
  fErrorMessage := '';
  fTotalBytes := 0;
  fStartAddress := 0;
  AddressHasBeenSet := False;
  HexFileLines := TStringList.Create;
  try
    HexFileLines.LoadFromFile(FileName);
    FirstChar := HexFileLines[0][1];
    if (RecType = rtAuto) then
      case FirstChar of
        ':': RecType := rtIntelHex;
      else
        RecType := rtMotorolaS19;
      end;

    for i := 0 to (HexFileLines.Count - 1) do
      begin
        case RecType of
          rtIntelHex: LineWasData := ConvertLineIntelHex(HexFileLines[i]);
          rtMotorolaS19: LineWasData := ConvertLineIntelHex(HexFileLines[i]);
        end;
        if (not LineWasData) then
          break;
      end;
  finally
    HexFileLines.Free;
  end;
  if (fErrorMessage <> '') then
    fErrorMessage := 'ReadHex Error:' + LineEnding + fErrorMessage;
end;


destructor TReadHex.Destroy;
begin
  //
  inherited;
end;


function TReadHex.ConvertLineIntelHex(s: string): boolean;
var
  BytesCount, RecordType, ThisByte: byte;
  i, CheckSum, FileCheckSum: integer;
  LineAddress: word;
begin
  Result := False;
  if (s[1] <> ':') then
    AddError(ERR_MISSING_COLON);
  BytesCount := HexToInt(Copy(s, 2, 2));
  LineAddress := HexToInt(Copy(s, 4, 4));
  RecordType := HexToInt(Copy(s, 8, 2));
  FileCheckSum := HexToInt(Copy(s, 10 + 2*BytesCount, 2));

  // Read the data bytes
  SetLength(fBytesArray, Length(fBytesArray) + BytesCount);
  CheckSum := BytesCount + Hi(LineAddress) + Lo(LineAddress) + RecordType;
  case (RecordType) of
    0: begin // Data record
         if (BytesCount > 0) then
           begin
             if (not AddressHasBeenSet) then
               begin
                 fStartAddress := LineAddress; // Available for use by caller
                 AddressHasBeenSet := True;
               end;
             for i := 0 to (BytesCount - 1) do
               begin
                 ThisByte := HexToInt(Copy(s, 10 + 2*i, 2));
                 fBytesArray[fTotalBytes] := ThisByte;
                 inc(fTotalBytes);
                 CheckSum := CheckSum + ThisByte;
               end;

             // Test the checksum against the record's value
             CheckSum := ((CheckSum xor $FF) + 1) and $FF;
             Result := (Checksum = FileCheckSum);
           end;
       end;
    1: begin // End of File Record
         // Do nothing, just flag done as Result flag already FALSE
       end;
    2..5: begin
            AddError(Format(ERR_TYPE_NOT_SUPPORTED, [RecordType]));
          end;
    else
      AddError(Format(ERR_TYPE_NOT_VALID, [RecordType]));
  end;
end;


{ HexToInt }

function TReadHex.HexToInt(s: string): integer;
var
  i: integer;
  Value: integer;
begin
  s := UpperCase(Trim(s));
  Result := 0;
  for i:=1 to length(s) do begin
    case s[i] of
      '0'..'9': Value := ord(s[i]) - ord('0');
      'A'..'F': Value := ord(s[i]) - ord('A')  + 10;
      else
        AddError(Format(ERR_INVALID_HEX, [s]));
    end;
    Result := (Result shl 4) + Value;
  end;
end;


procedure TReadHex.AddError(const Msg: string);
begin
  if (fErrorMessage <> '') then
    fErrorMessage := fErrorMessage + LineEnding;
  fErrorMessage := fErrorMessage + Msg;
end;



{ TWriteHex }

constructor TWriteHex.Create(FileName: string; RecType: TRecordType);
begin
  AssignFile(HexFile, ChangeFileExt(FileName, '.hex'));
  Rewrite(HexFile);
  fRecType := RecType;
  fAddress := 0;
  ByteCount := 0;
end;


destructor TWriteHex.Destroy;
begin
  WriteLastRecord;
  CloseFile(HexFile);
  inherited;
end;


procedure TWriteHex.SetStart(aAddress: integer);
begin
  FlushBytes;
  fAddress := aAddress;
end;


procedure TWriteHex.WriteBytes(aBytesArray: TBytesArray; Pos1, Pos2: integer);
var
  i: integer;
begin
  for i := Pos1 to Pos2 do
    WriteByte(aBytesArray[i]);
end;


procedure TWriteHex.WriteByte(aByte: byte);
begin
  if (ByteCount = MAX_BYTES_PER_LINE) then
    FlushBytes;
  Inc(ByteCount);
  BytesArray[ByteCount] := aByte;
end;


procedure TWriteHex.FlushBytes;
var
  Checksum, i: integer;
begin
  if (ByteCount > 0) then
    case (fRecType) of

      // Intel HEX Record
      rtIntelHex: begin
                    CheckSum := ByteCount + Hi(fAddress) + Lo(fAddress) {+ RecordType=0};
                    Write(HexFile, Format(':%.2x%.4x00', [ByteCount, fAddress]));
                    for i := 1 to ByteCount do
                      begin
                        Write(HexFile, Format('%.2x', [BytesArray[i]]));
                        CheckSum := Checksum + BytesArray[i];
                      end;
                    CheckSum := ((CheckSum xor $FF) + 1) and $FF;
                    WriteLn(HexFile, Format('%.2x', [CheckSum]));
                    fAddress := (fAddress + ByteCount) and $FFFF;
                  end;

      // Motorola S-Record
      rtMotorolaS19: begin
                       CheckSum := (ByteCount + 3) + Hi(fAddress) + Lo(fAddress);
                       Write(HexFile, Format('S1%.2x%.4x', [ByteCount + 3, fAddress]));
                       for i := 1 to ByteCount do
                         begin
                           Write(HexFile, Format('%.2x', [BytesArray[i]]));
                           CheckSum := Checksum + BytesArray[i];
                         end;
                       WriteLn(HexFile, Format('%.2x', [(Checksum xor $FFFFFFFF) and $FF]));
                       fAddress := (fAddress + ByteCount) and $FFFF;
                     end;

    end;
  ByteCount := 0;
end;


procedure TWriteHex.WriteLastRecord;
begin
  if (ByteCount > 0) then
    FlushBytes;
    case fRecType of
      rtIntelHex:  WriteLn(HexFile, ':00000001FF');
      rtMotorolaS19: WriteLn(HexFile, 'S9030000FC');
    end;
end;


procedure TWriteHex.AddError(const Msg: string);
begin
  if (fErrorMessage <> '') then
    fErrorMessage := fErrorMessage + LineEnding;
  fErrorMessage := fErrorMessage + Msg;
end;


end.
