unit RggGraph;

interface

uses
  System.Classes,
  System.Types,
  Vcl.Graphics,
  RggTypes,
  RggZug,
  RggPolarKar;

type
  TRggGraph = class
  protected
    FFixPunkt: TRealPoint;
    FZoom: single;
    KoppelKurveNeedFill: Boolean;
    FColor: TColor;
    FColored: Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetColored(const Value: Boolean);
    procedure SetFixPunkt(Value: TRealPoint);
    procedure SetZoom(Value: single); virtual;
  protected
    GrafikOK: Boolean; // loaded
    Updated: Boolean; // transformed
  public
    RaumGraphData: TRaumGraphData;
    RaumGraphProps: TRaumGraphProps;
    Rotator: TPolarKar; // injected, not owned

    constructor Create;
    destructor Destroy; override;

    procedure Update; virtual;
    procedure DrawToCanvas(Canvas: TCanvas); virtual;
    procedure GetPlotList(ML: TStrings); virtual;

    property FixPunkt: TRealPoint read FFixPunkt write SetFixPunkt;
    property Zoom: single read FZoom write SetZoom;
    property Coloriert: Boolean read FColored write SetColored;
    property Color: TColor read FColor write SetColor;
  end;

implementation

constructor TRggGraph.Create;
begin
  RaumGraphData := TRaumGraphData.Create;
  RaumGraphProps := TRaumGraphProps.Create;
  FColor := clGray;
  FColored := True;
  FZoom := 0.05;
end;

destructor TRggGraph.Destroy;
begin
  RaumGraphData.Free;
  RaumGraphProps.Free;
  inherited;
end;

procedure TRggGraph.SetColor(const Value: TColor);
begin
  FColor := Value;
  RaumGraphProps.Color := Value;
end;

procedure TRggGraph.SetColored(const Value: Boolean);
begin
  FColored := Value;
  RaumGraphProps.Coloriert := FColored;
end;

procedure TRggGraph.SetFixPunkt(Value: TRealPoint);
begin
  FFixPunkt := Value;
  Updated := False;
end;

procedure TRggGraph.SetZoom(Value: single);
begin
  FZoom := Value;
  Updated := False;
  KoppelKurveNeedFill := True;
end;

procedure TRggGraph.Update;
begin
  //if GrafikOK then ...
  //virtual
end;

procedure TRggGraph.DrawToCanvas(Canvas: TCanvas);
begin
  //if GrafikOK then ...
  //virtual
end;

procedure TRggGraph.GetPlotList(ML: TStrings);
begin
  //if GrafikOK then ...
  //virtual
end;

end.
