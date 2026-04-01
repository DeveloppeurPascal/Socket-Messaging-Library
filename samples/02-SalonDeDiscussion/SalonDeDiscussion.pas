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
  File last update : 2026-04-01T19:57:02.000+02:00
  Signature : bc66d3b23b292fdd63877e6be585410ba5c3dfb6
  ***************************************************************************
*)

unit SalonDeDiscussion;

// ****************************************
// * Salon de discussion
// ****************************************
// 
// ****************************************
// File generator : Socket Message Generator (v1.0)
// Website : https://socketmessaging.developpeur-pascal.fr/ 
// Generation date : 04/08/2023 21:28:48
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
  /// Message ID 3: Envoi d'un message à quelqu'un
  /// </summary>
  TEnvoiDUnMessageAQuelquUnMessage = class(TOlfSMMessage)
  private
    FDestinataire: string;
    FTexte: string;
    procedure SetDestinataire(const Value: string);
    procedure SetTexte(const Value: string);
  public
    /// <summary>
    /// Destinataire
    /// </summary>
    property Destinataire: string read FDestinataire write SetDestinataire;
    /// <summary>
    /// Texte
    /// </summary>
    property Texte: string read FTexte write SetTexte;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  /// <summary>
  /// Message ID 2: Envoi d'un message à tous
  /// </summary>
  TEnvoiDUnMessageATousMessage = class(TOlfSMMessage)
  private
    FEmetteur: string;
    FTexte: string;
    procedure SetEmetteur(const Value: string);
    procedure SetTexte(const Value: string);
  public
    /// <summary>
    /// Emetteur
    /// </summary>
    property Emetteur: string read FEmetteur write SetEmetteur;
    /// <summary>
    /// Texte
    /// </summary>
    property Texte: string read FTexte write SetTexte;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  /// <summary>
  /// Message ID 1: Identification d'un utilisateur
  /// </summary>
  TIdentificationDUnUtilisateurMessage = class(TOlfSMMessage)
  private
    FPseudo: string;
    procedure SetPseudo(const Value: string);
  public
    /// <summary>
    /// Pseudo de l'utilisateur
    /// </summary>
    property Pseudo: string read FPseudo write SetPseudo;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  /// <summary>
  /// Message ID 4: Transmission d'un message reçu
  /// </summary>
  TTransmissionDUnMessageRecuMessage = class(TOlfSMMessage)
  private
    FEmetteur: string;
    FTexte: string;
    procedure SetEmetteur(const Value: string);
    procedure SetTexte(const Value: string);
  public
    /// <summary>
    /// Emetteur
    /// </summary>
    property Emetteur: string read FEmetteur write SetEmetteur;
    /// <summary>
    /// Texte
    /// </summary>
    property Texte: string read FTexte write SetTexte;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  TSalonDeDiscussionServer = class(TOlfSMServer)
  private
  protected
    procedure onReceiveMessage3(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
    procedure onReceiveMessage2(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
    procedure onReceiveMessage1(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
  public
    onReceiveEnvoiDUnMessageAQuelquUnMessage
      : TOlfSMReceivedMessageEvent<TEnvoiDUnMessageAQuelquUnMessage>;
    onReceiveEnvoiDUnMessageATousMessage
      : TOlfSMReceivedMessageEvent<TEnvoiDUnMessageATousMessage>;
    onReceiveIdentificationDUnUtilisateurMessage
      : TOlfSMReceivedMessageEvent<TIdentificationDUnUtilisateurMessage>;
    constructor Create; override;
  end;

  TSalonDeDiscussionClient = class(TOlfSMClient)
  private
  protected
    procedure onReceiveMessage4(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
  public
    onReceiveTransmissionDUnMessageRecuMessage
      : TOlfSMReceivedMessageEvent<TTransmissionDUnMessageRecuMessage>;
    constructor Create; override;
  end;

procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);
procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);

implementation

uses
  System.SysUtils;

{$REGION 'Olf.RTLVersion.Streams'}

procedure SaveStringToStream(AString: string; AStream: TStream;
  AEncoding: TEncoding); overload;
// From unit Olf.RTL.Streams.pas in repository :
// https://github.com/DeveloppeurPascal/librairies
var
  StrLen: int64; // typeof(System.Classes.TStream.size)
  StrStream: TStringStream;
begin
  StrStream := TStringStream.Create(AString, AEncoding);
  try
    StrLen := StrStream.Size;
    AStream.write(StrLen, sizeof(StrLen));
    if (StrLen > 0) then
    begin
      StrStream.Position := 0;
      AStream.CopyFrom(StrStream);
    end;
  finally
    StrStream.Free;
  end;
end;

procedure SaveStringToStream(AString: string; AStream: TStream); overload;
// From unit Olf.RTL.Streams.pas in repository :
// https://github.com/DeveloppeurPascal/librairies
begin
  SaveStringToStream(AString, AStream, TEncoding.UTF8);
end;

function LoadStringFromStream(AStream: TStream; AEncoding: TEncoding)
  : string; overload;
// From unit Olf.RTL.Streams.pas in repository :
// https://github.com/DeveloppeurPascal/librairies
var
  StrLen: int64; // typeof(System.Classes.TStream.size)
  StrStream: TStringStream;
begin
  AStream.Read(StrLen, sizeof(StrLen));
  if (StrLen > 0) then
  begin
    StrStream := TStringStream.Create('', AEncoding);
    try
      StrStream.CopyFrom(AStream, StrLen);
      result := StrStream.DataString;
    finally
      StrStream.Free;
    end;
  end
  else
    result := '';
end;

function LoadStringFromStream(AStream: TStream): string; overload;
// From unit Olf.RTL.Streams.pas in repository :
// https://github.com/DeveloppeurPascal/librairies
begin
  result := LoadStringFromStream(AStream, TEncoding.UTF8);
end;

{$ENDREGION}

procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);
begin
  Server.RegisterMessageToReceive(TEnvoiDUnMessageAQuelquUnMessage.Create);
  Server.RegisterMessageToReceive(TEnvoiDUnMessageATousMessage.Create);
  Server.RegisterMessageToReceive(TIdentificationDUnUtilisateurMessage.Create);
end;

procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);
begin
  Client.RegisterMessageToReceive(TTransmissionDUnMessageRecuMessage.Create);
end;

{$REGION TSalonDeDiscussionServer}

constructor TSalonDeDiscussionServer.Create;
begin
  inherited;
  RegisterMessagesReceivedByTheServer(self);
  SubscribeToMessage(3, onReceiveMessage3);
  SubscribeToMessage(2, onReceiveMessage2);
  SubscribeToMessage(1, onReceiveMessage1);
end;

procedure TSalonDeDiscussionServer.onReceiveMessage3(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
begin
  if not(AMessage is TEnvoiDUnMessageAQuelquUnMessage) then
    exit;
  if not assigned(onReceiveEnvoiDUnMessageAQuelquUnMessage) then
    exit;
  onReceiveEnvoiDUnMessageAQuelquUnMessage(ASender, AMessage as TEnvoiDUnMessageAQuelquUnMessage);
end;

procedure TSalonDeDiscussionServer.onReceiveMessage2(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
begin
  if not(AMessage is TEnvoiDUnMessageATousMessage) then
    exit;
  if not assigned(onReceiveEnvoiDUnMessageATousMessage) then
    exit;
  onReceiveEnvoiDUnMessageATousMessage(ASender, AMessage as TEnvoiDUnMessageATousMessage);
end;

procedure TSalonDeDiscussionServer.onReceiveMessage1(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
begin
  if not(AMessage is TIdentificationDUnUtilisateurMessage) then
    exit;
  if not assigned(onReceiveIdentificationDUnUtilisateurMessage) then
    exit;
  onReceiveIdentificationDUnUtilisateurMessage(ASender, AMessage as TIdentificationDUnUtilisateurMessage);
end;

{$ENDREGION}

{$REGION TSalonDeDiscussionClient}

constructor TSalonDeDiscussionClient.Create;
begin
  inherited;
  RegisterMessagesReceivedByTheClient(self);
  SubscribeToMessage(4, onReceiveMessage4);
end;

procedure TSalonDeDiscussionClient.onReceiveMessage4(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
begin
  if not(AMessage is TTransmissionDUnMessageRecuMessage) then
    exit;
  if not assigned(onReceiveTransmissionDUnMessageRecuMessage) then
    exit;
  onReceiveTransmissionDUnMessageRecuMessage(ASender, AMessage as TTransmissionDUnMessageRecuMessage);
end;

{$ENDREGION}

{$REGION TEnvoiDUnMessageAQuelquUnMessage }

constructor TEnvoiDUnMessageAQuelquUnMessage.Create;
begin
  inherited;
  MessageID := 3;
end;

function TEnvoiDUnMessageAQuelquUnMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TEnvoiDUnMessageAQuelquUnMessage.Create;
end;

procedure TEnvoiDUnMessageAQuelquUnMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FDestinataire := LoadStringFromStream(Stream);
  FTexte := LoadStringFromStream(Stream);
end;

procedure TEnvoiDUnMessageAQuelquUnMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FDestinataire, Stream);
  SaveStringToStream(FTexte, Stream);
end;

procedure TEnvoiDUnMessageAQuelquUnMessage.SetDestinataire(const Value: string);
begin
  FDestinataire := Value;
end;

procedure TEnvoiDUnMessageAQuelquUnMessage.SetTexte(const Value: string);
begin
  FTexte := Value;
end;

{$ENDREGION}

{$REGION TEnvoiDUnMessageATousMessage }

constructor TEnvoiDUnMessageATousMessage.Create;
begin
  inherited;
  MessageID := 2;
end;

function TEnvoiDUnMessageATousMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TEnvoiDUnMessageATousMessage.Create;
end;

procedure TEnvoiDUnMessageATousMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FEmetteur := LoadStringFromStream(Stream);
  FTexte := LoadStringFromStream(Stream);
end;

procedure TEnvoiDUnMessageATousMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FEmetteur, Stream);
  SaveStringToStream(FTexte, Stream);
end;

procedure TEnvoiDUnMessageATousMessage.SetEmetteur(const Value: string);
begin
  FEmetteur := Value;
end;

procedure TEnvoiDUnMessageATousMessage.SetTexte(const Value: string);
begin
  FTexte := Value;
end;

{$ENDREGION}

{$REGION TIdentificationDUnUtilisateurMessage }

constructor TIdentificationDUnUtilisateurMessage.Create;
begin
  inherited;
  MessageID := 1;
end;

function TIdentificationDUnUtilisateurMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TIdentificationDUnUtilisateurMessage.Create;
end;

procedure TIdentificationDUnUtilisateurMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FPseudo := LoadStringFromStream(Stream);
end;

procedure TIdentificationDUnUtilisateurMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FPseudo, Stream);
end;

procedure TIdentificationDUnUtilisateurMessage.SetPseudo(const Value: string);
begin
  FPseudo := Value;
end;

{$ENDREGION}

{$REGION TTransmissionDUnMessageRecuMessage }

constructor TTransmissionDUnMessageRecuMessage.Create;
begin
  inherited;
  MessageID := 4;
end;

function TTransmissionDUnMessageRecuMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TTransmissionDUnMessageRecuMessage.Create;
end;

procedure TTransmissionDUnMessageRecuMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FEmetteur := LoadStringFromStream(Stream);
  FTexte := LoadStringFromStream(Stream);
end;

procedure TTransmissionDUnMessageRecuMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FEmetteur, Stream);
  SaveStringToStream(FTexte, Stream);
end;

procedure TTransmissionDUnMessageRecuMessage.SetEmetteur(const Value: string);
begin
  FEmetteur := Value;
end;

procedure TTransmissionDUnMessageRecuMessage.SetTexte(const Value: string);
begin
  FTexte := Value;
end;

{$ENDREGION}

end.
