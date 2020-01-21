﻿unit RggGraph;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  RggTypes,
  RggDisplay,
  RggPolarKar;

type
  TRggGraph = class
  private
    FFixPunkt: TRealPoint;
    FOffset: TPoint;
    FZoom: double;
    FColored: Boolean;
  protected
    GrafikOK: Boolean; // loaded
    Updated: Boolean; // transformed
    procedure SetFixPunkt(Value: TRealPoint);
    procedure SetOffset(Value: TPoint); virtual;
    procedure SetZoom(Value: double); virtual;
  public
    Rotator: TPolarKar;
    DL: TRggDisplayList;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Update; virtual;
    procedure Draw(Canvas: TCanvas); virtual;
    procedure GetPlotList(List: TStringList); virtual;

    property FixPunkt: TRealPoint read FFixPunkt write SetFixPunkt;
    property Offset: TPoint read FOffset write SetOffset;
    property Zoom: double read FZoom write SetZoom;
    property Coloriert: Boolean read FColored write FColored;
  end;

implementation

constructor TRggGraph.Create;
begin
  DL := TRggDisplayList.Create;
  FColored := True;
  FOffset := Point(1000, 1000);
  FZoom := 0.05;
end;

destructor TRggGraph.Destroy;
begin
  DL.Free;
  inherited;
end;

procedure TRggGraph.SetFixPunkt(Value: TRealPoint);
begin
  FFixPunkt := Value;
  Updated := False;
end;

procedure TRggGraph.SetOffset(Value: TPoint);
begin
  FOffset := Value;
  Updated := False;
end;

procedure TRggGraph.SetZoom(Value: double);
begin
  FZoom := Value;
  Updated := False;
end;

procedure TRggGraph.Update;
begin
  //if GrafikOK then ...
  //virtual
end;

procedure TRggGraph.Draw(Canvas: TCanvas);
begin
  //if GrafikOK then ...
  //virtual
end;

procedure TRggGraph.GetPlotList(List: TStringList);
begin
  //if GrafikOK then ...
  //virtual
end;

end.
