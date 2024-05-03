program ChatSampleServer;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  UDialMessages in '..\UDialMessages.pas',
  Olf.Net.Socket.Messaging in '..\..\..\src\Olf.Net.Socket.Messaging.pas';

type
  TMyServer = class(tdialserver)
  private
  protected
    procedure DoReceiveSendMessage(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TSendMessage);
  public
  end;

  { TMyServer }

procedure TMyServer.DoReceiveSendMessage(const ASender
  : TOlfSMSrvConnectedClient; const AMessage: TSendMessage);
var
  msg: TBroadcastMessage;
begin
  msg := TBroadcastMessage.Create;
  try
    writeln(msg.texte);
    msg.Pseudo := AMessage.Pseudo;
    msg.texte := AMessage.texte;
    msg.DateTime := now;
    SendMessageToAll(msg);
  finally
    msg.Free;
  end;
end;

var
  Server: TMyServer;

begin
  try
    Server := TMyServer.Create;
    Server.onReceiveSendMessage := Server.DoReceiveSendMessage;
    Server.Listen('0.0.0.0', 8080);
    readln;
  except
    on E: Exception do
      writeln(E.ClassName, ': ', E.Message);
  end;

end.
