/// <summary>
/// ***************************************************************************
///
/// Socket Messaging Library
///
/// Copyright 2023-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Socket Messaging is a Delphi Library for managing a messaging system over
/// TCP/IP sockets between clients and servers or peer to peer programs.
///
/// The library can be used in any Delphi project on recent versions of the
/// environment. It uses the TSocket class from System.Net.Socket, threads and
/// generic collections.
///
/// To use this library more simply, and obtain the source code you need to
/// use a client and server in your project, with your own interface, without
/// coding, use this code generator : https://smcodegenerator.olfsoftware.fr/
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://socketmessaging.developpeur-pascal.fr/
///
/// Project site :
///      https://github.com/DeveloppeurPascal/Socket-Messaging-Library
///
/// ***************************************************************************
/// File last update : 28/05/2024 12:19:19
/// Signature : f180520b39ae9dd1661b4e6ddde14047c4a9da7f
/// ***************************************************************************
/// </summary>

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
