unit fServer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  ClockSampleUnit,
  Olf.Net.Socket.Messaging, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    FClientCount: integer;
    procedure DoReceiveACKDateTimeMessage(Const ASender
      : TOlfSMSrvConnectedClient; Const AMessage: TACKDateTimeMessage);
    procedure DoClientConnected(Const AConnectedClient
      : TOlfSMSrvConnectedClient);
  public
    { Déclarations publiques }
    MyClockServer: TClockSampleServer;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.DoClientConnected(const AConnectedClient
  : TOlfSMSrvConnectedClient);
begin
  tthread.Synchronize(nil,
    procedure
    begin
      inc(FClientCount);
      AConnectedClient.TagString := 'Client' + FClientCount.ToString;
      Memo1.Lines.Add('Hello ' + AConnectedClient.TagString);
    end);
end;

procedure TForm1.DoReceiveACKDateTimeMessage(const ASender
  : TOlfSMSrvConnectedClient; const AMessage: TACKDateTimeMessage);
begin
  tthread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(ASender.TagString + ' - ' + AMessage.Counter.ToString);
    end);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FClientCount := 0;
  MyClockServer := TClockSampleServer.Create;
  MyClockServer.onReceiveACKDateTimeMessage := DoReceiveACKDateTimeMessage;
  MyClockServer.onClientConnected := DoClientConnected;
  MyClockServer.Listen('0.0.0.0', 8080);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MyClockServer);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  msg: TCurrentDateAndTimeMessage;
  dt: TDateTime;
begin
  if assigned(MyClockServer) and MyClockServer.isListening then
  begin
    msg := TCurrentDateAndTimeMessage.Create;
    try
      dt := now;
      msg.Date := datetostr(dt);
      msg.Time := timetostr(dt);
      MyClockServer.SendMessageToAll(msg);
    finally
      msg.Free;
    end;
  end;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
