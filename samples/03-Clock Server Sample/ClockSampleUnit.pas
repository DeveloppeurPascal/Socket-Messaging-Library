﻿unit ClockSampleUnit;

// ****************************************
// * Clock sample
// ****************************************
// 
// ****************************************
// File generator : Socket Message Generator (v1.0)
// Website : https://socketmessaging.developpeur-pascal.fr/ 
// Generation date : 04/08/2023 21:36:34
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
  /// Message ID 2: ACK Date Time
  /// </summary>
  TACKDateTimeMessage = class(TOlfSMMessage)
  private
    FCounter: integer;
    procedure SetCounter(const Value: integer);
  public
    /// <summary>
    /// Counter
    /// </summary>
    property Counter: integer read FCounter write SetCounter;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  /// <summary>
  /// Message ID 1: Current date and time
  /// </summary>
  TCurrentDateAndTimeMessage = class(TOlfSMMessage)
  private
    FDate: string;
    FTime: string;
    procedure SetDate(const Value: string);
    procedure SetTime(const Value: string);
  public
    /// <summary>
    /// Date
    /// </summary>
    property Date: string read FDate write SetDate;
    /// <summary>
    /// Time
    /// </summary>
    property Time: string read FTime write SetTime;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  TClockSampleServer = class(TOlfSMServer)
  private
  protected
    procedure onReceiveMessage2(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
  public
    onReceiveACKDateTimeMessage
      : TOlfSMReceivedMessageEvent<TACKDateTimeMessage>;
    constructor Create; override;
  end;

  TClockSampleClient = class(TOlfSMClient)
  private
  protected
    procedure onReceiveMessage1(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
  public
    onReceiveCurrentDateAndTimeMessage
      : TOlfSMReceivedMessageEvent<TCurrentDateAndTimeMessage>;
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
  Server.RegisterMessageToReceive(TACKDateTimeMessage.Create);
end;

procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);
begin
  Client.RegisterMessageToReceive(TCurrentDateAndTimeMessage.Create);
end;

{$REGION 'TClockSampleServer'}

constructor TClockSampleServer.Create;
begin
  inherited;
  RegisterMessagesReceivedByTheServer(self);
  SubscribeToMessage(2, onReceiveMessage2);
end;

procedure TClockSampleServer.onReceiveMessage2(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TACKDateTimeMessage;
begin
  if not(AMessage is TACKDateTimeMessage) then
    exit;
  if not assigned(onReceiveACKDateTimeMessage) then
    exit;
  onReceiveACKDateTimeMessage(ASender, AMessage as TACKDateTimeMessage);
end;

{$ENDREGION}

{$REGION 'TClockSampleClient'}

constructor TClockSampleClient.Create;
begin
  inherited;
  RegisterMessagesReceivedByTheClient(self);
  SubscribeToMessage(1, onReceiveMessage1);
end;

procedure TClockSampleClient.onReceiveMessage1(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TCurrentDateAndTimeMessage;
begin
  if not(AMessage is TCurrentDateAndTimeMessage) then
    exit;
  if not assigned(onReceiveCurrentDateAndTimeMessage) then
    exit;
  onReceiveCurrentDateAndTimeMessage(ASender, AMessage as TCurrentDateAndTimeMessage);
end;

{$ENDREGION}

{$REGION 'TACKDateTimeMessage' }

constructor TACKDateTimeMessage.Create;
begin
  inherited;
  MessageID := 2;
  FCounter := 0;
end;

function TACKDateTimeMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TACKDateTimeMessage.Create;
end;

procedure TACKDateTimeMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  if (Stream.read(FCounter, sizeof(FCounter)) <> sizeof(FCounter)) then
    raise exception.Create('Can''t load "Counter" value.');
end;

procedure TACKDateTimeMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FCounter, sizeof(FCounter));
end;

procedure TACKDateTimeMessage.SetCounter(const Value: integer);
begin
  FCounter := Value;
end;

{$ENDREGION}

{$REGION 'TCurrentDateAndTimeMessage' }

constructor TCurrentDateAndTimeMessage.Create;
begin
  inherited;
  MessageID := 1;
end;

function TCurrentDateAndTimeMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TCurrentDateAndTimeMessage.Create;
end;

procedure TCurrentDateAndTimeMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FDate := LoadStringFromStream(Stream);
  FTime := LoadStringFromStream(Stream);
end;

procedure TCurrentDateAndTimeMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FDate, Stream);
  SaveStringToStream(FTime, Stream);
end;

procedure TCurrentDateAndTimeMessage.SetDate(const Value: string);
begin
  FDate := Value;
end;

procedure TCurrentDateAndTimeMessage.SetTime(const Value: string);
begin
  FTime := Value;
end;

{$ENDREGION}

end.
