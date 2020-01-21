unit RggServer;

interface

uses
  System.SysUtils,
  System.Classes,
  ScktComp,
  RggTypes,
  RggDoc,
  RggModul;

type
  THandleMsgEvent = procedure(Sender: TObject; s: string) of object;

  TRggServer = class
  private
    FOnHandleMsg: THandleMsgEvent;
    FOnTrace: THandleMsgEvent;
    procedure ServerSocketClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure HandleMsg(s: string);
    procedure Trace(s: string);
    procedure ServerSocketError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  public
    ServerSocket: TServerSocket;
    constructor Create(APort: Integer);
    destructor Destroy; override;
    procedure SendMsg(s: string);
    property OnHandleMsg: THandleMsgEvent read FOnHandleMsg write FOnHandleMsg;
    property OnTrace: THandleMsgEvent read FOnTrace write FOnTrace;
  end;

  TRggInputServer = class
  private
    //function GetMsg: string;
    procedure SetMsg(Sender: TObject; s: string);
    procedure Trace(Sender: TObject; s: string);
  public
    RggServer: TRggServer;
    constructor Create;
    destructor Destroy; override;
  end;

  TRggOutputServer = class
  private
    function GetMsg: string;
    procedure SetMsg(Sender: TObject; s: string);
    procedure Trace(Sender: TObject; s: string);
  public
    RggServer: TRggServer;
    constructor Create;
    destructor Destroy; override;
    procedure SendMsg;
  end;

var
  RggInputServer: TRggInputServer;
  RggOutputServer: TRggOutputServer;

const
  RggInputServerPort = 3027; //1024;
  RggOutputServerPort = 3028; //3010;

implementation

{ TRggServer }

constructor TRggServer.Create(APort: Integer);
begin
  inherited Create;
  ServerSocket := TServerSocket.Create(nil);
  ServerSocket.Port := APort;
  ServerSocket.OnClientRead := ServerSocketClientRead;
  ServerSocket.Socket.OnClientError := ServerSocketError;
  ServerSocket.Active := True;
end;

destructor TRggServer.Destroy;
begin
  ServerSocket.Active := False;
  ServerSocket.Close;
  ServerSocket.Free;
  inherited;
end;

procedure TRggServer.ServerSocketClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  s: string;
begin
  s := Socket.ReceiveText;
  HandleMsg(s);
end;

procedure TRggServer.HandleMsg(s: string);
begin
  if Assigned(OnHandleMsg) then
    OnHandleMsg(Self, s);
end;

procedure TRggServer.SendMsg(s: string);
var
  i: Integer;
begin
  for i := 0 to ServerSocket.Socket.ActiveConnections-1 do
    ServerSocket.Socket.Connections[i].SendText(s)
end;

procedure TRggServer.ServerSocketError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  Trace('-- ServerSocketError');
  ErrorCode := 0; //###
end;

procedure TRggServer.Trace(s: string);
begin
  if Assigned(OnTrace) then OnTrace(Self, s);
end;

{ TRggInputServer }

constructor TRggInputServer.Create;
begin
  inherited;
  RggServer := TRggServer.Create(RggInputServerPort);
  RggServer.OnTrace := Trace;
  RggServer.OnHandleMsg := SetMsg;
end;

destructor TRggInputServer.Destroy;
begin
  RggServer.Free;
  inherited;
end;

{
function TRggInputServer.GetMsg: string;
var
  T: TTrimmControlls;
  sep: string;
begin
  sep := ',';
  T := RiggModul.Rigg.Glieder;
  result :=
    IntToStr(T.Controller) + sep +
    IntToStr(T.Winkel) + sep +
    IntToStr(T.Vorstag) + sep +
    IntToStr(T.Wanten) + sep +
    IntToStr(T.Woben) + sep +
    IntToStr(T.Wunten) + sep +
    IntToStr(T.SalingH) + sep +
    IntToStr(T.SalingA) + sep +
    IntToStr(T.SalingL) + sep +
    IntToStr(T.WPowerOS);
end;
}

procedure TRggInputServer.SetMsg(Sender: TObject; s: string);
var
  T: TTrimmControlls;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.CommaText := s;
    T := RiggModul.Rigg.Glieder;
    if SL.Count = 10 then
    begin
      T.Controller := StrToIntDef(SL[0], T.Controller);
      T.Winkel := StrToIntDef(SL[1], T.Winkel);
      T.Vorstag := StrToIntDef(SL[2], T.Vorstag);
      T.Wanten := StrToIntDef(SL[3], T.Wanten);
      T.Woben := StrToIntDef(SL[4], T.Woben);
      T.Wunten := StrToIntDef(SL[5], T.Wunten);
      T.SalingH := StrToIntDef(SL[6], T.SalingH);
      T.SalingA := StrToIntDef(SL[7], T.SalingA);
      T.SalingL := StrToIntDef(SL[8], T.SalingL);
      T.WPowerOS := StrToIntDef(SL[9], T.WPowerOS);
    end;
    RiggModul.Rigg.Glieder := T;
    RiggModul.UpdateGetriebe;
    RiggModul.UpdateGCtrls(T);
    RggOutputServer.SendMsg;
  finally
    SL.Free;
  end;
end;

procedure TRggInputServer.Trace(Sender: TObject; s: string);
begin
  //
end;

{ TRggOutputServer }

constructor TRggOutputServer.Create;
begin
  RggServer := TRggServer.Create(RggOutputServerPort);
  RggServer.OnTrace := Trace;
  RggServer.OnHandleMsg := SetMsg;
end;

destructor TRggOutputServer.Destroy;
begin
  RggServer.Free;
  inherited;
end;

function TRggOutputServer.GetMsg: string;
var
  RggDocument: TRggDocument;
  sep: string;
  sSignature: string;
  sCount: string;
  sData: string;
begin
  sep := ',';
  RggDocument := TRggDocument.Create;
  try
    RiggModul.Rigg.UpdateGSB;
    RiggModul.Rigg.GetDocument(RggDocument);
    sSignature := RggDocSignature;
    sData := RggDocument.SaveToXML;
    //sData := RggDocument.SaveToXMLBase64;
    sCount := IntToStr(Length(sData));
    result :=
      //sSignature + sep +
      //sCount + sep +
      sData;
  finally
    RggDocument.Free;
  end;
end;

procedure TRggOutputServer.SendMsg;
begin
  RggServer.SendMsg(#2 + GetMsg + #3);
end;

procedure TRggOutputServer.SetMsg(Sender: TObject; s: string);
begin
end;

procedure TRggOutputServer.Trace(Sender: TObject; s: string);
begin
  //
end;

end.
