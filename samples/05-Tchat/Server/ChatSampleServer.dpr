(* C2PP
  ***************************************************************************

  Socket Messaging Library
  Copyright (c) 2023-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  Socket Messaging is a Delphi Library for managing a messaging system over
  TCP/IP sockets between clients and servers or peer to peer programs.

  The library can be used in any Delphi project on recent versions of the
  environment. It uses the TSocket class from System.Net.Socket, threads and
  generic collections.

  To use this library more simply, and obtain the source code you need to
  use a client and server in your project, with your own interface, without
  coding, use this code generator : https://smcodegenerator.olfsoftware.fr/

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://socketmessaging.developpeur-pascal.fr/

  Project site :
  https://github.com/DeveloppeurPascal/Socket-Messaging-Library

  ***************************************************************************
  File last update : 2025-05-26T15:45:36.000+02:00
  Signature : f98b812a29fa829ee4d40920682fc7221ddd638d
  ***************************************************************************
*)

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
