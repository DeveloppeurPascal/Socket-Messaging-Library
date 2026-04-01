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
  File last update : 2026-04-01T19:57:38.000+02:00
  Signature : fb2766d20616e989bc63ec226790fde95b5c44eb
  ***************************************************************************
*)

unit UBourrinageMessages;

// ****************************************
// * Bourrinage
// ****************************************
// 
// Messages
// d'un projet de test de charge pour voir si l'unité
// générée et l'unité de la librairie Socket Messaging sont assez solides en conditions extrêmes.
// 
// ****************************************
// File generator : Socket Messaging Code Generator (v1.0)
// Website : https://socketmessaging.developpeur-pascal.fr/ 
// Generation date : 10/08/2023 07:54:39
// 
// Don't do any change on this file. They will be erased by next generation !
// ****************************************

// To compile this unit you need Olf.Net.Socket.Messaging.pas from
// https://github.com/DeveloppeurPascal/Socket-Messaging-Library
//
// Direct link to the file :
// https://raw.githubusercontent.com/DeveloppeurPascal/Socket-Messaging-Library/main/src-library/Olf.Net.Socket.Messaging.pas

interface

uses
  System.Classes,
  Olf.Net.Socket.Messaging;

type
  /// <summary>
  /// Message ID 1: ClientToServer
  /// </summary>
  TBourrinClientToServerMessage = class(TOlfSMMessage)
  private
    FRandomNumber: integer;
    procedure SetRandomNumber(const Value: integer);
  public
    /// <summary>
    /// RandomNumber
    /// </summary>
    property RandomNumber: integer read FRandomNumber write SetRandomNumber;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  /// <summary>
  /// Message ID 2: ServerToClient
  /// </summary>
  TBourrinServerToClientMessage = class(TOlfSMMessage)
  private
    FRandomNumber: integer;
    procedure SetRandomNumber(const Value: integer);
  public
    /// <summary>
    /// RandomNumber
    /// </summary>
    property RandomNumber: integer read FRandomNumber write SetRandomNumber;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  TBourrinageServer = class(TOlfSMServer)
  private
  protected
    procedure onReceiveMessage1(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
  public
    onReceiveBourrinClientToServerMessage
      : TOlfSMReceivedMessageEvent<TBourrinClientToServerMessage>;
    constructor Create; override;
  end;

  TBourrinageClient = class(TOlfSMClient)
  private
  protected
    procedure onReceiveMessage2(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
  public
    onReceiveBourrinServerToClientMessage
      : TOlfSMReceivedMessageEvent<TBourrinServerToClientMessage>;
    constructor Create; override;
  end;

procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);
procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);

implementation

uses
  System.SysUtils;

procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);
begin
  Server.RegisterMessageToReceive(TBourrinClientToServerMessage.Create);
end;

procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);
begin
  Client.RegisterMessageToReceive(TBourrinServerToClientMessage.Create);
end;

{$REGION 'TBourrinageServer'}

constructor TBourrinageServer.Create;
begin
  inherited;
  RegisterMessagesReceivedByTheServer(self);
  SubscribeToMessage(1, onReceiveMessage1);
end;

procedure TBourrinageServer.onReceiveMessage1(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
begin
  if not(AMessage is TBourrinClientToServerMessage) then
    exit;
  if not assigned(onReceiveBourrinClientToServerMessage) then
    exit;
  onReceiveBourrinClientToServerMessage(ASender, AMessage as TBourrinClientToServerMessage);
end;

{$ENDREGION}

{$REGION 'TBourrinageClient'}

constructor TBourrinageClient.Create;
begin
  inherited;
  RegisterMessagesReceivedByTheClient(self);
  SubscribeToMessage(2, onReceiveMessage2);
end;

procedure TBourrinageClient.onReceiveMessage2(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
begin
  if not(AMessage is TBourrinServerToClientMessage) then
    exit;
  if not assigned(onReceiveBourrinServerToClientMessage) then
    exit;
  onReceiveBourrinServerToClientMessage(ASender, AMessage as TBourrinServerToClientMessage);
end;

{$ENDREGION}

{$REGION 'TBourrinClientToServerMessage' }

constructor TBourrinClientToServerMessage.Create;
begin
  inherited;
  MessageID := 1;
  FRandomNumber := 0;
end;

function TBourrinClientToServerMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TBourrinClientToServerMessage.Create;
end;

procedure TBourrinClientToServerMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  if (Stream.read(FRandomNumber, sizeof(FRandomNumber)) <> sizeof(FRandomNumber)) then
    raise exception.Create('Can''t load "RandomNumber" value.');
end;

procedure TBourrinClientToServerMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FRandomNumber, sizeof(FRandomNumber));
end;

procedure TBourrinClientToServerMessage.SetRandomNumber(const Value: integer);
begin
  FRandomNumber := Value;
end;

{$ENDREGION}

{$REGION 'TBourrinServerToClientMessage' }

constructor TBourrinServerToClientMessage.Create;
begin
  inherited;
  MessageID := 2;
  FRandomNumber := 0;
end;

function TBourrinServerToClientMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TBourrinServerToClientMessage.Create;
end;

procedure TBourrinServerToClientMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  if (Stream.read(FRandomNumber, sizeof(FRandomNumber)) <> sizeof(FRandomNumber)) then
    raise exception.Create('Can''t load "RandomNumber" value.');
end;

procedure TBourrinServerToClientMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FRandomNumber, sizeof(FRandomNumber));
end;

procedure TBourrinServerToClientMessage.SetRandomNumber(const Value: integer);
begin
  FRandomNumber := Value;
end;

{$ENDREGION}

end.
