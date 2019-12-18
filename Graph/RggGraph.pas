unit RggGraph;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  RggTypes,
  Polarkar;

type
  TRggGraph = class
  private
    FFixPunkt: TRealPoint;
    FOffset: TPoint;
    FZoom: real;
    FColored: Boolean;
  protected
    GrafikOK: Boolean; // loaded
    Updated: Boolean; // transformed
    procedure SetFixPunkt(Value: TRealPoint);
    procedure SetOffset(Value: TPoint); virtual;
    procedure SetZoom(Value: real); virtual;
  public
    Rotator: TPolarKar2;
    constructor Create; virtual;
    procedure Update; virtual;
    procedure Draw(Canvas: TCanvas); virtual;
    procedure GetPlotList(List: TStringList); virtual;

    property FixPunkt: TRealPoint read FFixPunkt write SetFixPunkt;
    property Offset: TPoint read FOffset write SetOffset;
    property Zoom: real read FZoom write SetZoom;
    property Coloriert: Boolean read FColored write FColored;
  end;

implementation

constructor TRggGraph.Create;
begin
  FColored := True;
  FOffset := Point(1000, 1000);
  FZoom := 0.05;
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

procedure TRggGraph.SetZoom(Value: real);
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
