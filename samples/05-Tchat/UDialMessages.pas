unit UDialMessages;

// ****************************************
// * Dial
// ****************************************
// 
// ****************************************
// File generator : Socket Messaging Code Generator (v1.0)
// Website : https://socketmessaging.developpeur-pascal.fr/ 
// Generation date : 10/08/2023 15:52:10
// 
// Don't do any change on this file. They will be erased by next generation !
// ****************************************

// To compile this unit you need Olf.Net.Socket.Messaging.pas from
// https://github.com/DeveloppeurPascal/Socket-Messaging-Library
//
// Direct link to the file :
// https://raw.githubusercontent.com/DeveloppeurPascal/Socket-Messaging-Library/main/src/Olf.Net.Socket.Messaging.pas

interface

uses
  System.Classes,
  Olf.Net.Socket.Messaging;

type
  /// <summary>
  /// Message ID 2: BroadcastMessage
  /// </summary>
  TBroadcastMessage = class(TOlfSMMessage)
  private
    FPseudo: string;
    FTexte: string;
    FDateTime: TDateTime;
    procedure SetPseudo(const Value: string);
    procedure SetTexte(const Value: string);
    procedure SetDateTime(const Value: TDateTime);
  public
    /// <summary>
    /// Pseudo
    /// </summary>
    property Pseudo: string read FPseudo write SetPseudo;
    /// <summary>
    /// Texte
    /// </summary>
    property Texte: string read FTexte write SetTexte;
    /// <summary>
    /// DateTime
    /// </summary>
    property DateTime: TDateTime read FDateTime write SetDateTime;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  /// <summary>
  /// Message ID 1: SendMessage
  /// </summary>
  TSendMessage = class(TOlfSMMessage)
  private
    FPseudo: string;
    FTexte: string;
    procedure SetPseudo(const Value: string);
    procedure SetTexte(const Value: string);
  public
    /// <summary>
    /// Pseudo
    /// </summary>
    property Pseudo: string read FPseudo write SetPseudo;
    /// <summary>
    /// Texte
    /// </summary>
    property Texte: string read FTexte write SetTexte;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  TDialServer = class(TOlfSMServer)
  private
  protected
    procedure onReceiveMessage1(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
  public
    onReceiveSendMessage
      : TOlfSMReceivedMessageEvent<TSendMessage>;
    constructor Create; override;
  end;

  TDialClient = class(TOlfSMClient)
  private
  protected
    procedure onReceiveMessage2(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
  public
    onReceiveBroadcastMessage
      : TOlfSMReceivedMessageEvent<TBroadcastMessage>;
    constructor Create; override;
  end;

procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);
procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);

implementation

uses
  System.SysUtils;

{$REGION 'code from Olf.RTLVersion.Streams'}

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
  Server.RegisterMessageToReceive(TSendMessage.Create);
end;

procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);
begin
  Client.RegisterMessageToReceive(TBroadcastMessage.Create);
end;

{$REGION 'TDialServer'}

constructor TDialServer.Create;
begin
  inherited;
  RegisterMessagesReceivedByTheServer(self);
  SubscribeToMessage(1, onReceiveMessage1);
end;

procedure TDialServer.onReceiveMessage1(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TSendMessage;
begin
  if not(AMessage is TSendMessage) then
    exit;
  if not assigned(onReceiveSendMessage) then
    exit;
  onReceiveSendMessage(ASender, AMessage as TSendMessage);
end;

{$ENDREGION}

{$REGION 'TDialClient'}

constructor TDialClient.Create;
begin
  inherited;
  RegisterMessagesReceivedByTheClient(self);
  SubscribeToMessage(2, onReceiveMessage2);
end;

procedure TDialClient.onReceiveMessage2(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TBroadcastMessage;
begin
  if not(AMessage is TBroadcastMessage) then
    exit;
  if not assigned(onReceiveBroadcastMessage) then
    exit;
  onReceiveBroadcastMessage(ASender, AMessage as TBroadcastMessage);
end;

{$ENDREGION}

{$REGION 'TBroadcastMessage' }

constructor TBroadcastMessage.Create;
begin
  inherited;
  MessageID := 2;
  FDateTime := Now;
end;

function TBroadcastMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TBroadcastMessage.Create;
end;

procedure TBroadcastMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FPseudo := LoadStringFromStream(Stream);
  FTexte := LoadStringFromStream(Stream);
  if (Stream.read(FDateTime, sizeof(FDateTime)) <> sizeof(FDateTime)) then
    raise exception.Create('Can''t load "DateTime" value.');
end;

procedure TBroadcastMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FPseudo, Stream);
  SaveStringToStream(FTexte, Stream);
  Stream.Write(FDateTime, sizeof(FDateTime));
end;

procedure TBroadcastMessage.SetPseudo(const Value: string);
begin
  FPseudo := Value;
end;

procedure TBroadcastMessage.SetTexte(const Value: string);
begin
  FTexte := Value;
end;

procedure TBroadcastMessage.SetDateTime(const Value: TDateTime);
begin
  FDateTime := Value;
end;

{$ENDREGION}

{$REGION 'TSendMessage' }

constructor TSendMessage.Create;
begin
  inherited;
  MessageID := 1;
end;

function TSendMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TSendMessage.Create;
end;

procedure TSendMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FPseudo := LoadStringFromStream(Stream);
  FTexte := LoadStringFromStream(Stream);
end;

procedure TSendMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FPseudo, Stream);
  SaveStringToStream(FTexte, Stream);
end;

procedure TSendMessage.SetPseudo(const Value: string);
begin
  FPseudo := Value;
end;

procedure TSendMessage.SetTexte(const Value: string);
begin
  FTexte := Value;
end;

{$ENDREGION}

end.
