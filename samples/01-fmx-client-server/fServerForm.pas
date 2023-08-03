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
  Olf.Net.Socket.Messaging,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TfrmServer = class(TForm)
    pnlStart: TPanel;
    edtIP: TEdit;
    edtPort: TEdit;
    btnStartServer: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    SendTextToAllClients: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartServerClick(Sender: TObject);
    procedure SendTextToAllClientsClick(Sender: TObject);
  private
    { Déclarations privées }
    ServeurThread: TOlfSocketMessagingServer;
  protected
    procedure onReceivedMessage1(Const ASender
      : TOlfSocketMessagingServerConnectedClient;
      Const AMessage: TOlfSocketMessage);
    procedure onReceivedMessage2(Const ASender
      : TOlfSocketMessagingServerConnectedClient;
      Const AMessage: TOlfSocketMessage);
  public
    { Déclarations publiques }
  end;

var
  frmServer: TfrmServer;

implementation

{$R *.fmx}

uses
  uTestMessages;

procedure TfrmServer.btnStartServerClick(Sender: TObject);
begin
  ServeurThread.Listen(edtIP.Text, edtPort.Text.ToInteger);
  pnlStart.Enabled := false;
end;

procedure TfrmServer.FormCreate(Sender: TObject);
begin
  ServeurThread := TOlfSocketMessagingServer.Create;
  RegisterMessages(ServeurThread);
  ServeurThread.SubscribeToMessage(1, onReceivedMessage1);
  ServeurThread.SubscribeToMessage(2, onReceivedMessage2);
end;

procedure TfrmServer.FormDestroy(Sender: TObject);
begin
  ServeurThread.Free;
end;

procedure TfrmServer.onReceivedMessage1(Const ASender
  : TOlfSocketMessagingServerConnectedClient;
  const AMessage: TOlfSocketMessage);
var
  msg: tmessage2;
begin
  if not(AMessage is TMessage1) then
    raise exception.Create('Wrong received message !');

  Memo1.Lines.Add((AMessage as TMessage1).Texte);

  msg := tmessage2.Create;
  try
    msg.x := random(width);
    msg.y := random(height);
    msg.z := (AMessage as TMessage1).Texte.Length;
    ASender.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TfrmServer.onReceivedMessage2(Const ASender
  : TOlfSocketMessagingServerConnectedClient;
  const AMessage: TOlfSocketMessage);
var
  msg: tmessage2;
begin
  if not(AMessage is tmessage2) then
    raise exception.Create('Wrong received message !');

  msg := AMessage as tmessage2;
  Memo1.Lines.Add(msg.x.tostring + ' / ' + msg.y.tostring + ' / ' +
    msg.z.tostring);
end;

procedure TfrmServer.SendTextToAllClientsClick(Sender: TObject);
var
  msg: TMessage1;
begin
  msg := TMessage1.Create;
  try
    msg.Texte := Edit1.Text;
    ServeurThread.SendMessageToAll(msg);
  finally
    msg.Free;
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
randomize;

finalization

end.
