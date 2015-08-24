unit DragDropTimeOfDay;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
  DragDrop,
  DragDropFormats,
  DropTarget,
  DropSource,
  Classes,
  Graphics,
  ActiveX;

type
  // TTimeOfDay is the structure which is transferred from the drop source to
  // the drop target.
  TTimeOfDay = record
    hours,
    minutes,
    seconds,
    milliseconds: word;
    color: TColor;
  end;

  // TTimeOfDayClipboardFormat defines our custom clipboard format.
  TTimeOfDayClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FTOD: TTimeOfDay;
    FGotData: boolean;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;
    procedure SetTOD(const Value: TTimeOfDay);
  public
    function GetClipboardFormat: TClipFormat; override;
    procedure Clear; override;
    function HasData: boolean; override;
    property TOD: TTimeOfDay read FTOD write SetTOD;
  end;

  // TTimeOfDayDataFormat defines our custom data format.
  // In this case the data format is identical to the clipboard format, but we
  // need a data format class anyway.
  TTimeOfDayDataFormat = class(TCustomDataFormat)
  private
    FTOD: TTimeOfDay;
    FGotData: boolean;
  protected
    class procedure RegisterCompatibleFormats; override;
    procedure SetTOD(const Value: TTimeOfDay);
  public
    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property TOD: TTimeOfDay read FTOD write SetTOD;
  end;

  (*
  ** For simplicity I have not used the following source and target components
  ** in this demo, but instead use and extend the standard TDropTextSource and
  ** TDropTextTarget components.
  *)
  TDropTimeOfDayTarget = class(TCustomDropMultiTarget)
  private
    FTimeOfDayDataFormat: TTimeOfDayDataFormat;
  protected
    function GetTOD: TTimeOfDay;
    function GetColor: TColor;
    function GetTime: TDateTime;
  public
    constructor Create(AOwner: TComponent); override;
    property TOD: TTimeOfDay read GetTOD;
    property Time: TDateTime read GetTime;
    property Color: TColor read GetColor;
  end;

  TDropTimeOfDaySource = class(TCustomDropMultiSource)
  private
    FTimeOfDayDataFormat: TTimeOfDayDataFormat;
  protected
    function GetTOD: TTimeOfDay;
    procedure SetTOD(const Value: TTimeOfDay);
    function GetColor: TColor;
    function GetTime: TDateTime;
    procedure SetColor(const Value: TColor);
    procedure SetTime(const Value: TDateTime);
  public
    constructor Create(aOwner: TComponent); override;
  published
    property TOD: TTimeOfDay read GetTOD write SetTOD;
    property Time: TDateTime read GetTime write SetTime;
    property Color: TColor read GetColor write SetColor;
  end;

const
  // Name of our custom clipboard format.
  sTimeOfDayName = 'TimeOfDay';

implementation

uses
  Windows,
  SysUtils;

{ TTimeOfDayClipboardFormat }

procedure TTimeOfDayClipboardFormat.Clear;
begin
  FillChar(FTOD, SizeOf(FTOD), 0);
  FGotData := False;
end;

var
  CF_TOD: TClipFormat = 0;

function TTimeOfDayClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_TOD = 0) then
    CF_TOD := RegisterClipboardFormat(sTimeOfDayName);
  Result := CF_TOD;
end;

function TTimeOfDayClipboardFormat.GetSize: integer;
begin
  Result := SizeOf(TTimeOfDay);
end;

function TTimeOfDayClipboardFormat.HasData: boolean;
begin
  Result := FGotData;
end;

function TTimeOfDayClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  // Copy data from buffer into our structure.
  Move(Value^, FTOD, Size);

  FGotData := True;
  Result := True;
end;

procedure TTimeOfDayClipboardFormat.SetTOD(const Value: TTimeOfDay);
begin
  FTOD := Value;
  FGotData := True;
end;

function TTimeOfDayClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  Result := (Size = SizeOf(TTimeOfDay));
  if (Result) then
    // Copy data from our structure into buffer.
    Move(FTOD, Value^, Size);
end;

{ TTimeOfDayDataFormat }

function TTimeOfDayDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  if (Source is TTimeOfDayClipboardFormat) then
    FTOD := TTimeOfDayClipboardFormat(Source).TOD
  else
    Result := inherited Assign(Source);

  FGotData := Result;
end;

function TTimeOfDayDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  Result := True;

  if (Dest is TTimeOfDayClipboardFormat) then
    TTimeOfDayClipboardFormat(Dest).TOD := FTOD
  else
    Result := inherited AssignTo(Dest);
end;

procedure TTimeOfDayDataFormat.Clear;
begin
  Changing;
  FillChar(FTOD, SizeOf(FTOD), 0);
  FGotData := False;
end;

function TTimeOfDayDataFormat.HasData: boolean;
begin
  Result := FGotData;
end;

function TTimeOfDayDataFormat.NeedsData: boolean;
begin
  Result := not FGotData;
end;

class procedure TTimeOfDayDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataConversion(TTimeOfDayClipboardFormat);
end;

procedure TTimeOfDayDataFormat.SetTOD(const Value: TTimeOfDay);
begin
  Changing;
  FTOD := Value;
  FGotData := True;
end;

{ TDropTimeOfDayTarget }

constructor TDropTimeOfDayTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeOfDayDataFormat := TTimeOfDayDataFormat.Create(Self);
end;

function TDropTimeOfDayTarget.GetColor: TColor;
begin
  Result := FTimeOfDayDataFormat.TOD.color;
end;

function TDropTimeOfDayTarget.GetTime: TDateTime;
begin
  with FTimeOfDayDataFormat.TOD do
    Result := EncodeTime(hours, minutes, seconds, milliseconds);
end;

function TDropTimeOfDayTarget.GetTOD: TTimeOfDay;
begin
  Result := FTimeOfDayDataFormat.TOD;
end;

{ TDropTimeOfDaySource }

constructor TDropTimeOfDaySource.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeOfDayDataFormat := TTimeOfDayDataFormat.Create(Self);
end;

function TDropTimeOfDaySource.GetColor: TColor;
begin
  Result := FTimeOfDayDataFormat.TOD.color;
end;

function TDropTimeOfDaySource.GetTime: TDateTime;
begin
  with FTimeOfDayDataFormat.TOD do
    Result := EncodeTime(hours, minutes, seconds, milliseconds);
end;

function TDropTimeOfDaySource.GetTOD: TTimeOfDay;
begin
  Result := FTimeOfDayDataFormat.TOD;
end;

procedure TDropTimeOfDaySource.SetColor(const Value: TColor);
var
  TOD: TTimeOfDay;
begin
  TOD := FTimeOfDayDataFormat.TOD;
  TOD.color := Value;
  FTimeOfDayDataFormat.TOD := TOD;
end;

procedure TDropTimeOfDaySource.SetTime(const Value: TDateTime);
var
  TOD: TTimeOfDay;
begin
  TOD.color := FTimeOfDayDataFormat.TOD.color;
  DecodeTime(Value, TOD.hours, TOD.minutes, TOD.seconds, TOD.milliseconds);
  FTimeOfDayDataFormat.TOD := TOD;
end;

procedure TDropTimeOfDaySource.SetTOD(const Value: TTimeOfDay);
begin
  FTimeOfDayDataFormat.TOD := Value;
end;

initialization
  // Data format registration
  TTimeOfDayDataFormat.RegisterDataFormat;
  // Clipboard format registration
  TTimeOfDayClipboardFormat.RegisterFormat;

finalization
end.
