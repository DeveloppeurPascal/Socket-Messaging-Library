unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Layouts, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.Edit, Olf.Net.Socket.Messaging, UDialMessages;

type
  TForm1 = class(TForm)
    edtPseudo: TEdit;
    edtTexte: TEdit;
    Memo1: TMemo;
    btnSend: TButton;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    { Déclarations privées }
  protected
    Client: TDialClient;
    procedure DoReceiveBroadcastMessage(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TBroadcastMessage);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{ TForm1 }

procedure TForm1.btnSendClick(Sender: TObject);
var
  msg: TSendMessage;
begin
  if not assigned(Client) then
    exit;

  if not Client.isConnected then
    exit;

  msg := TSendMessage.Create;
  try
    msg.Pseudo := edtPseudo.Text;
    msg.Texte := edtTexte.Text;
    Client.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TForm1.DoReceiveBroadcastMessage(const ASender
  : TOlfSMSrvConnectedClient; const AMessage: TBroadcastMessage);
begin
  tthread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(datetimetostr(AMessage.DateTime) + slinebreak +
        AMessage.Pseudo + ' : ' + AMessage.Texte);
    end);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Client := TDialClient.Create('127.0.0.1', 8080);
  Client.onReceiveBroadcastMessage := DoReceiveBroadcastMessage;
  Client.Connect('127.0.0.1', 8080);
end;

end.
