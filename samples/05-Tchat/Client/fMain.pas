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
  File last update : 2025-05-26T15:45:34.843+02:00
  Signature : d5f9449d360429b0019b19801c314ecb6ebdb3b9
  ***************************************************************************
*)

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
