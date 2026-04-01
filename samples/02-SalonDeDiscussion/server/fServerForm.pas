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
  File last update : 2026-04-01T20:10:24.000+02:00
  Signature : d602953e5859b116f384092ac1b74d8525d8d33b
  ***************************************************************************
*)

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
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo, SalonDeDiscussion;

type
  TServer = class(tolfsmserver)
  private
  protected
    procedure onClientIdentifie(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
    procedure onPublicMessage(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
    procedure onPrivateMessage(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);

    procedure ClientConnected(Const AClient: TOlfSMSrvConnectedClient);
    procedure ClientLostConnexion(Const AClient: TOlfSMSrvConnectedClient);
    procedure ClientDisconnected(Const AClient: TOlfSMSrvConnectedClient);

    procedure AddLog(Const Txt: string);
  public
    onMessage1
      : TOlfSMReceivedMessageEvent<TIdentificationDUnUtilisateurMessage>;
    constructor Create(AIP: string; APort: Word); override;
    destructor Destroy; override;
  end;

  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
begin
  TServer.Create('0.0.0.0', 8080).Listen;
end;

{ TServer }

procedure TServer.AddLog(const Txt: string);
begin
  tthread.Synchronize(nil,
    procedure
    begin
      Form2.Memo1.Lines.Insert(0, Txt)
    end);
end;

procedure TServer.ClientConnected(const AClient: TOlfSMSrvConnectedClient);
begin
  AddLog('New user');
end;

procedure TServer.ClientDisconnected(const AClient: TOlfSMSrvConnectedClient);
begin
  AddLog('Bye ' + AClient.TagString);
end;

procedure TServer.ClientLostConnexion(const AClient: TOlfSMSrvConnectedClient);
begin
  AddLog('Lost ' + AClient.TagString);
end;

constructor TServer.Create(AIP: string; APort: Word);
begin
  inherited;
  RegisterMessagesReceivedByTheServer(self);

  SubscribeToMessage(1, onClientIdentifie);
  SubscribeToMessage(2, onPublicMessage);
  SubscribeToMessage(3, onPrivateMessage);

  onClientConnected := ClientConnected;
  onClientLostConnection := ClientLostConnexion;
  onClientDisconnected := ClientDisconnected;
end;

destructor TServer.Destroy;
begin

  inherited;
end;

procedure TServer.onClientIdentifie(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TIdentificationDUnUtilisateurMessage;
begin
  if not(AMessage is TIdentificationDUnUtilisateurMessage) then
    exit;
  msg := AMessage as TIdentificationDUnUtilisateurMessage;

  AddLog(msg.Pseudo + ' connected');
  ASender.TagString := msg.Pseudo;
  if not assigned(onMessage1) then
    exit;
  onMessage1(ASender, AMessage as TIdentificationDUnUtilisateurMessage);
end;

procedure TServer.onPrivateMessage(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TEnvoiDUnMessageAQuelquUnMessage;
  msg2: TTransmissionDUnMessageRecuMessage;
begin
  if not(AMessage is TEnvoiDUnMessageAQuelquUnMessage) then
    exit;
  msg := AMessage as TEnvoiDUnMessageAQuelquUnMessage;

  AddLog('From : ' + ASender.TagString + slinebreak + 'To: ' + msg.Destinataire
    + slinebreak + msg.Texte);

  msg2 := TTransmissionDUnMessageRecuMessage.Create;
  try
    msg2.Emetteur := ASender.TagString;
    msg2.Texte := msg.Texte;
    ForEachConnectedClient(
      procedure(Const Client: TOlfSMSrvConnectedClient)
      begin
        if Client.TagString = msg.Destinataire then
          Client.SendMessage(msg2);
      end);
  finally
    msg2.Free;
  end;
end;

procedure TServer.onPublicMessage(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TEnvoiDUnMessageATousMessage;
  msg2: TTransmissionDUnMessageRecuMessage;
begin
  if not(AMessage is TEnvoiDUnMessageATousMessage) then
    exit;
  msg := AMessage as TEnvoiDUnMessageATousMessage;

  AddLog('From : ' + ASender.TagString + slinebreak + msg.Texte);

  msg2 := TTransmissionDUnMessageRecuMessage.Create;
  try
    msg2.Emetteur := msg.Emetteur;
    msg2.Texte := msg.Texte;
    // SendMessageToAll(msg2, ASender);
    SendMessageToAll(msg2);
  finally
    msg2.Free;
  end;
end;

end.
