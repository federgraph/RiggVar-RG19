unit Saling3Eck;

interface

uses
  System.Math,
  RggTypes,
  Vcalc116,
  Rggunit4;

type
  TSalingDreieck = class
  private
    FSalingHMin, FSalingH, FSalingHMax: real;
    FSalingAMin, FSalingA, FSalingAMax: real;

    procedure SetSalingH(Value: real);
    procedure SetSalingA(Value: real);
    procedure SetSalingL(Value: real);
    procedure SetSalingW(Value: real);

    function GetSalingL: real;
    function GetSalingW: real;

    function GetSalingLMin: real;
    function GetSalingLMax: real;
    function GetSalingWMin: real;
    function GetSalingWMax: real;
  public
    constructor Create;
    procedure CopyFromRigg(Rigg: TRigg);
    procedure GetLW(H, A: real; var L, W: real);

    property Saling_H: real read FSalingH write SetSalingH;
    property Saling_A: real read FSalingH write SetSalingA;
    property Saling_L: real read GetSalingL write SetSalingL;
    property Saling_W: real read GetSalingW write SetSalingW;

    property Saling_AMin: real read FSalingAMin write FSalingAMin;
    property Saling_AMax: real read FSalingAMax write FSalingAMax;
    property Saling_HMin: real read FSalingHMin write FSalingHMin;
    property Saling_HMax: real read FSalingHMax write FSalingHMax;

    property Saling_LMin: real read GetSalingLMin;
    property Saling_LMax: real read GetSalingLMax;
    property Saling_WMin: real read GetSalingWMin;
    property Saling_WMax: real read GetSalingWMax;
  end;

implementation

constructor TSalingDreieck.Create;
begin
  FSalingHMin := 150;
  FSalingH := 200;
  FSalingHMax := 300;
  FSalingAMin := 700;
  FSalingA := 850;
  FSalingAMax := 1100;
end;

procedure TSalingDreieck.GetLW(H, A: real; var L, W: real);
begin
  L := Hypot(H, A / 2);
  W := arctan2(H, A * 2);
end;

function TSalingDreieck.GetSalingL: real;
begin
  result := Hypot(FSalingH, FSalingA / 2);
end;

function TSalingDreieck.GetSalingW: real;
begin
  result := arctan2(FSalingH, FSalingA * 2);
end;

procedure TSalingDreieck.SetSalingH(Value: real);
begin
  if Value < FSalingHMin then
    FSalingH := FSalingHMin
  else if Value > FSalingHMax then
    FSalingH := FSalingHMax
  else
    FSalingH := Value;
end;

procedure TSalingDreieck.SetSalingA(Value: real);
begin
  if Value < FSalingAMin then
    FSalingA := FSalingAMin
  else if Value > FSalingHMax then
    FSalingA := FSalingAMax
  else
    FSalingA := Value;
end;

procedure TSalingDreieck.SetSalingL(Value: real);
var
  temp: real;
begin
  temp := Value / Saling_L;
  FSalingH := temp * FSalingH;
  FSalingA := temp * FSalingA;
end;

procedure TSalingDreieck.SetSalingW(Value: real);
var
  tempW, tempL: real;
begin
  tempW := Value * pi / 180;
  tempL := Saling_L;
  FSalingH := tempL * sin(tempW);
  FSalingA := 2 * tempL * cos(tempW);
end;

function TSalingDreieck.GetSalingLMin: real;
var
  tempL, tempW: real;
begin
  GetLW(FSalingHMin, FSalingAMin, tempL, tempW);
  result := tempL;
end;

function TSalingDreieck.GetSalingLMax: real;
var
  tempL, tempW: real;
begin
  GetLW(FSalingHMax, FSalingAMax, tempL, tempW);
  result := tempL;
end;

function TSalingDreieck.GetSalingWMin: real;
var
  tempL, tempW: real;
begin
  GetLW(FSalingHMin, FSalingAMax, tempL, tempW);
  result := tempW;
end;

function TSalingDreieck.GetSalingWMax: real;
var
  tempL, tempW: real;
begin
  GetLW(FSalingHMax, FSalingAMin, tempL, tempW);
  result := tempW;
end;

procedure TSalingDreieck.CopyFromRigg(Rigg: TRigg);
begin
  FSalingHMin := Rigg.GSB.SalingH.Min;
  FSalingHMax := Rigg.GSB.SalingH.Max;
  FSalingH := Rigg.GSB.SalingH.Ist;
  FSalingAMin := Rigg.GSB.SalingA.Min;
  FSalingAMax := Rigg.GSB.SalingA.Max;
  FSalingA := Rigg.GSB.SalingA.Ist;
end;

end.
