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
  File last update : 2026-04-01T19:58:30.000+02:00
  Signature : cfe984fdb39dac72547c1c420c8337958f11482d
  ***************************************************************************
*)

unit UMultiUserDrawMessages;

// ****************************************
// * Multi User Draw
// ****************************************
// 
// ****************************************
// File generator : Socket Messaging Code Generator (v1.0)
// Website : https://socketmessaging.developpeur-pascal.fr/ 
// Generation date : 10/08/2023 19:14:12
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
  System.UITypes,
  System.Classes,
  Olf.Net.Socket.Messaging;

type
  /// <summary>
  /// Message ID 1: Add a pixel
  /// </summary>
  /// <remarks>
  /// A user add a pixel on its screen.
  /// </remarks>
  TMUDAddAPixelMessage = class(TOlfSMMessage)
  private
    FColor: TAlphaColor;
    FX: integer;
    FY: integer;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
  public
    /// <summary>
    /// Color
    /// </summary>
    property Color: TAlphaColor read FColor write SetColor;
    /// <summary>
    /// X
    /// </summary>
    property X: integer read FX write SetX;
    /// <summary>
    /// Y
    /// </summary>
    property Y: integer read FY write SetY;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  /// <summary>
  /// Message ID 2: Change pixel color from an other user
  /// </summary>
  TMUDChangePixelColorFromAnOtherUserMessage = class(TOlfSMMessage)
  private
    FColor: TAlphaColor;
    FX: integer;
    FY: integer;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
  public
    /// <summary>
    /// Color
    /// </summary>
    property Color: TAlphaColor read FColor write SetColor;
    /// <summary>
    /// X
    /// </summary>
    property X: integer read FX write SetX;
    /// <summary>
    /// Y
    /// </summary>
    property Y: integer read FY write SetY;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  TMultiUserDrawServer = class(TOlfSMServer)
  private
  protected
    procedure onReceiveMessage1(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
  public
    onReceiveMUDAddAPixelMessage
      : TOlfSMReceivedMessageEvent<TMUDAddAPixelMessage>;
    constructor Create; override;
  end;

  TMultiUserDrawClient = class(TOlfSMClient)
  private
  protected
    procedure onReceiveMessage2(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
  public
    onReceiveMUDChangePixelColorFromAnOtherUserMessage
      : TOlfSMReceivedMessageEvent<TMUDChangePixelColorFromAnOtherUserMessage>;
    constructor Create; override;
  end;

procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);
procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);

implementation

uses
  System.SysUtils;

procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);
begin
  Server.RegisterMessageToReceive(TMUDAddAPixelMessage.Create);
end;

procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);
begin
  Client.RegisterMessageToReceive(TMUDChangePixelColorFromAnOtherUserMessage.Create);
end;

{$REGION 'TMultiUserDrawServer'}

constructor TMultiUserDrawServer.Create;
begin
  inherited;
  RegisterMessagesReceivedByTheServer(self);
  SubscribeToMessage(1, onReceiveMessage1);
end;

procedure TMultiUserDrawServer.onReceiveMessage1(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
begin
  if not(AMessage is TMUDAddAPixelMessage) then
    exit;
  if not assigned(onReceiveMUDAddAPixelMessage) then
    exit;
  onReceiveMUDAddAPixelMessage(ASender, AMessage as TMUDAddAPixelMessage);
end;

{$ENDREGION}

{$REGION 'TMultiUserDrawClient'}

constructor TMultiUserDrawClient.Create;
begin
  inherited;
  RegisterMessagesReceivedByTheClient(self);
  SubscribeToMessage(2, onReceiveMessage2);
end;

procedure TMultiUserDrawClient.onReceiveMessage2(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
begin
  if not(AMessage is TMUDChangePixelColorFromAnOtherUserMessage) then
    exit;
  if not assigned(onReceiveMUDChangePixelColorFromAnOtherUserMessage) then
    exit;
  onReceiveMUDChangePixelColorFromAnOtherUserMessage(ASender, AMessage as TMUDChangePixelColorFromAnOtherUserMessage);
end;

{$ENDREGION}

{$REGION 'TMUDAddAPixelMessage' }

constructor TMUDAddAPixelMessage.Create;
begin
  inherited;
  MessageID := 1;
  FX := 0;
  FY := 0;
end;

function TMUDAddAPixelMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TMUDAddAPixelMessage.Create;
end;

procedure TMUDAddAPixelMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  if (Stream.read(FColor, sizeof(FColor)) <> sizeof(FColor)) then
    raise exception.Create('Can''t load "Color" value.');
  if (Stream.read(FX, sizeof(FX)) <> sizeof(FX)) then
    raise exception.Create('Can''t load "X" value.');
  if (Stream.read(FY, sizeof(FY)) <> sizeof(FY)) then
    raise exception.Create('Can''t load "Y" value.');
end;

procedure TMUDAddAPixelMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FColor, sizeof(FColor));
  Stream.Write(FX, sizeof(FX));
  Stream.Write(FY, sizeof(FY));
end;

procedure TMUDAddAPixelMessage.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
end;

procedure TMUDAddAPixelMessage.SetX(const Value: integer);
begin
  FX := Value;
end;

procedure TMUDAddAPixelMessage.SetY(const Value: integer);
begin
  FY := Value;
end;

{$ENDREGION}

{$REGION 'TMUDChangePixelColorFromAnOtherUserMessage' }

constructor TMUDChangePixelColorFromAnOtherUserMessage.Create;
begin
  inherited;
  MessageID := 2;
  FX := 0;
  FY := 0;
end;

function TMUDChangePixelColorFromAnOtherUserMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TMUDChangePixelColorFromAnOtherUserMessage.Create;
end;

procedure TMUDChangePixelColorFromAnOtherUserMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  if (Stream.read(FColor, sizeof(FColor)) <> sizeof(FColor)) then
    raise exception.Create('Can''t load "Color" value.');
  if (Stream.read(FX, sizeof(FX)) <> sizeof(FX)) then
    raise exception.Create('Can''t load "X" value.');
  if (Stream.read(FY, sizeof(FY)) <> sizeof(FY)) then
    raise exception.Create('Can''t load "Y" value.');
end;

procedure TMUDChangePixelColorFromAnOtherUserMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FColor, sizeof(FColor));
  Stream.Write(FX, sizeof(FX));
  Stream.Write(FY, sizeof(FY));
end;

procedure TMUDChangePixelColorFromAnOtherUserMessage.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
end;

procedure TMUDChangePixelColorFromAnOtherUserMessage.SetX(const Value: integer);
begin
  FX := Value;
end;

procedure TMUDChangePixelColorFromAnOtherUserMessage.SetY(const Value: integer);
begin
  FY := Value;
end;

{$ENDREGION}

end.
