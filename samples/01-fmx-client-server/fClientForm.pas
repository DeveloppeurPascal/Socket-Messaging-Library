unit fClientForm;

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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo;

type
  TfrmClient = class(TForm)
    pnlConnect: TPanel;
    edtIP: TEdit;
    edtPort: TEdit;
    btnConnectToTheServer: TButton;
    Edit1: TEdit;
    SendTextToServer: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectToTheServerClick(Sender: TObject);
    procedure SendTextToServerClick(Sender: TObject);
  private
    { Déclarations privées }
    ClientThread: TOlfSocketMessagingClient;
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
  frmClient: TfrmClient;

implementation

{$R *.fmx}

uses
  uTestMessages;

procedure TfrmClient.btnConnectToTheServerClick(Sender: TObject);
begin
  ClientThread.Connect(edtIP.Text, edtPort.Text.ToInteger);
  pnlConnect.Enabled := false;
end;

procedure TfrmClient.FormCreate(Sender: TObject);
begin
  ClientThread := TOlfSocketMessagingClient.Create;
  RegisterMessages(ClientThread);
  ClientThread.SubscribeToMessage(1, onReceivedMessage1);
  ClientThread.SubscribeToMessage(2, onReceivedMessage2);
end;

procedure TfrmClient.FormDestroy(Sender: TObject);
begin
  ClientThread.Free;
end;

procedure TfrmClient.onReceivedMessage1(const ASender
  : TOlfSocketMessagingServerConnectedClient;
  const AMessage: TOlfSocketMessage);
begin
  if not(AMessage is TMessage1) then
    raise exception.Create('Wrong received message !');

  Memo1.Lines.Add((AMessage as TMessage1).Texte);
end;

procedure TfrmClient.onReceivedMessage2(const ASender
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

  ClientThread.SendMessage(msg);
end;

procedure TfrmClient.SendTextToServerClick(Sender: TObject);
var
  msg: TMessage1;
begin
  msg := TMessage1.Create;
  try
    msg.Texte := Edit1.Text;
    ClientThread.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

finalization

end.
