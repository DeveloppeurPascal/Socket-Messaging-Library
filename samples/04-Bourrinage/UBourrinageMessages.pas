(* C2PP
  ***************************************************************************

  Socket Messaging Library

  Copyright 2023-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

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
  File last update : 2025-02-09T11:04:06.339+01:00
  Signature : 62e204780168bddedb9b9e7bf013f1dcc2393866
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
var
  msg: TBourrinClientToServerMessage;
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
var
  msg: TBourrinServerToClientMessage;
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
