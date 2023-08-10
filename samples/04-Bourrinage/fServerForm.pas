unit fServerForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  UBourrinageMessages,
  Olf.Net.Socket.Messaging, FMX.StdCtrls, FMX.Layouts;

type
  TfrmServer = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    GridPanelLayout1: TGridPanelLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSentMessages: integer;
    FCounter: integer;
    FReceivedMessages: integer;
    procedure SetCounter(const Value: integer);
    procedure SetSentMessages(const Value: integer);
    procedure SetReceivedMessages(const Value: integer);
  protected
    Server: TBourrinageServer;
    property Counter: integer read FCounter write SetCounter;
    property SentMessages: integer read FSentMessages write SetSentMessages;
    property ReceivedMessages: integer read FReceivedMessages
      write SetReceivedMessages;
    procedure DoReceiveBourrinClientToServerMessage(Const ASender
      : TOlfSMSrvConnectedClient;
      Const AMessage: TBourrinClientToServerMessage);
    procedure DoClientConnected(Const AConnectedClient
      : TOlfSMSrvConnectedClient);
  public
  end;

var
  frmServer: TfrmServer;

implementation

{$R *.fmx}

uses
  fClientForm;

procedure TfrmServer.DoClientConnected(const AConnectedClient
  : TOlfSMSrvConnectedClient);
begin
  Counter := Counter + 1;
  AConnectedClient.tag := Counter;
end;

procedure TfrmServer.DoReceiveBourrinClientToServerMessage
  (const ASender: TOlfSMSrvConnectedClient;
  const AMessage: TBourrinClientToServerMessage);
var
  msg: TBourrinServerToClientMessage;
begin
  tthread.Synchronize(nil,
    procedure
    begin
      ReceivedMessages := ReceivedMessages + 1;

      if Memo1.Lines.Count > 512 then
        Memo1.Lines.Clear;
      Memo1.Lines.Add('From ' + ASender.tag.ToString + ' : ' +
        AMessage.RandomNumber.ToString);
      Memo1.GoToTextEnd;
    end);

  msg := TBourrinServerToClientMessage.Create;
  try
    msg.RandomNumber := random(high(integer));
    ASender.SendMessage(msg);
  finally
    msg.Free;
  end;
  SentMessages := SentMessages + 1;
end;

procedure TfrmServer.FormCreate(Sender: TObject);
begin
  Timer1.Interval := random(100) + 50;
  Timer1.Enabled := true;

  Counter := 0;
  SentMessages := 0;
  ReceivedMessages := 0;

  Server := TBourrinageServer.Create;
  Server.onReceiveBourrinClientToServerMessage :=
    DoReceiveBourrinClientToServerMessage;
  Server.onClientConnected := DoClientConnected;
  Server.listen('0.0.0.0', 8080);

  tthread.ForceQueue(nil,
    procedure
    begin
      TfrmClients.Create(Self).Show;
    end);
end;

procedure TfrmServer.SetCounter(const Value: integer);
begin
  FCounter := Value;
  tthread.Synchronize(nil,
    procedure
    begin
      Label1.Text := FCounter.ToString;
    end);
end;

procedure TfrmServer.SetReceivedMessages(const Value: integer);
begin
  FReceivedMessages := Value;
  tthread.Synchronize(nil,
    procedure
    begin
      Label3.Text := FReceivedMessages.ToString;
    end);
end;

procedure TfrmServer.SetSentMessages(const Value: integer);
begin
  FSentMessages := Value;
  tthread.Synchronize(nil,
    procedure
    begin
      Label2.Text := FSentMessages.ToString;
    end);
end;

procedure TfrmServer.Timer1Timer(Sender: TObject);
var
  msg: TBourrinServerToClientMessage;
begin
  if not Timer1.Enabled then
    exit;
  if not assigned(Server) then
    exit;
  if not Server.isListening then
    exit;

  msg := TBourrinServerToClientMessage.Create;
  try
    msg.RandomNumber := -random(high(integer));
    Server.SendMessageToAll(msg);
  finally
    msg.Free;
  end;
  SentMessages := SentMessages + 1;
end;

initialization

randomize;

end.
