unit Olf.Net.Socket.Messaging;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Net.Socket,
  System.Classes;

type
  TOlfSMMessageID = byte; // 256 messages (0..255)

  TOlfSMMessageSize = word; // 65535 bytes for a message (0..65535)

  TOlfSMSrvConnectedClient = class;

  TOlfSMException = class(exception)
  end;

  TOlfSMMessage = class
  private
    FMessageID: TOlfSMMessageID;

    procedure SetMessageID(const Value: TOlfSMMessageID);
  public
    property MessageID: TOlfSMMessageID read FMessageID write SetMessageID;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    function GetNewInstance: TOlfSMMessage; virtual;
    constructor Create; virtual;
  end;

  TOlfSMMessagesDict = TObjectDictionary<TOlfSMMessageID, TOlfSMMessage>;

  TOlfReceivedMessageEvent = procedure(Const ASender: TOlfSMSrvConnectedClient;
    Const AMessage: TOlfSMMessage) of object;
  TOlfMessageSubscribers = TList<TOlfReceivedMessageEvent>;
  TOlfSubscribers = TObjectDictionary<TOlfSMMessageID, TOlfMessageSubscribers>;

  IOlfSMMessagesRegister = interface
    ['{6728BA4A-44AD-415D-9436-1626920DF655}']
    procedure RegisterMessageToReceive(AMessage: TOlfSMMessage);
  end;

  TOlfSMServer = class(TInterfacedObject, IOlfSMMessagesRegister)
  private
    FThread: TThread;
    FSocket: TSocket;
    FPort: word;
    FIP: string;
    FThreadNameForDebugging: string;
    FMessagesDict: TOlfSMMessagesDict;
    // TODO : manage the messages list as an other class and use it here and in the client
    FSubscribers: TOlfSubscribers;
    // TODO : manage the subscribers list as an other class and use it here and in the client
    procedure SetIP(const Value: string);
    procedure SetPort(const Value: word);
    procedure SetSocket(const Value: TSocket);
    function GetIP: string;
    function GetPort: word;
    function GetSocket: TSocket;
    procedure SetThreadNameForDebugging(const Value: string);
    function GetThreadNameForDebugging: string;
  protected
    property Socket: TSocket read GetSocket write SetSocket;
    procedure ServerLoop; virtual;
    function LockMessagesDict: TOlfSMMessagesDict;
    procedure UnlockMessagesDict;
    function LockSubscribers: TOlfSubscribers;
    procedure UnlockSubscribers;
  public
    property IP: string read GetIP write SetIP;
    property Port: word read GetPort write SetPort;
    property ThreadNameForDebugging: string read GetThreadNameForDebugging
      write SetThreadNameForDebugging;
    constructor Create(AIP: string; APort: word); overload; virtual;
    constructor Create; overload; virtual;
    procedure Listen; overload; virtual;
    procedure Listen(AIP: string; APort: word); overload; virtual;
    function isListening: boolean;
    function isConnected: boolean;
    destructor Destroy; override;
    procedure RegisterMessageToReceive(AMessage: TOlfSMMessage);
    procedure SubscribeToMessage(AMessageID: TOlfSMMessageID;
      aReceivedMessageEvent: TOlfReceivedMessageEvent);
    procedure UnsubscribeToMessage(AMessageID: TOlfSMMessageID;
      aReceivedMessageEvent: TOlfReceivedMessageEvent);
  end;

  TOlfSMSrvConnectedClient = class(TInterfacedObject)
  private
    FThread: TThread;
    FSocket: TSocket;
    FSocketServer: TOlfSMServer;
    FThreadNameForDebugging: string;
    procedure SetSocket(const Value: TSocket);
    function GetSocket: TSocket;
    function GetThreadNameForDebugging: string;
    procedure SetThreadNameForDebugging(const Value: string);
  protected
    property Socket: TSocket read GetSocket write SetSocket;
    procedure ClientLoop; virtual;
    procedure StartClientLoop; virtual;
    function GetNewMessageInstance(AMessageID: TOlfSMMessageID)
      : TOlfSMMessage; virtual;
    procedure DispatchReceivedMessage(AMessage: TOlfSMMessage); virtual;
  public
    property ThreadNameForDebugging: string read GetThreadNameForDebugging
      write SetThreadNameForDebugging;
    constructor Create(AServer: TOlfSMServer; AClientSocket: TSocket);
      overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure SendMessage(Const AMessage: TOlfSMMessage);
    function isConnected: boolean;
  end;

  TOlfSMClient = class(TOlfSMSrvConnectedClient, IOlfSMMessagesRegister)
  private
    FServerPort: word;
    FServerIP: string;
    FMessagesDict: TOlfSMMessagesDict;
    FSubscribers: TOlfSubscribers;
    procedure SetServerIP(const Value: string);
    procedure SetServerPort(const Value: word);
    constructor Create(AServer: TOlfSMServer; AClientSocket: TSocket); override;
    function GeServerIP: string;
    function GeServerPort: word;
  protected
    function GetNewMessageInstance(AMessageID: byte): TOlfSMMessage; override;
    procedure DispatchReceivedMessage(AMessage: TOlfSMMessage); override;
    function LockMessagesDict: TOlfSMMessagesDict;
    procedure UnlockMessagesDict;
    function LockSubscribers: TOlfSubscribers;
    procedure UnlockSubscribers;
  public
    property ServerIP: string read GeServerIP write SetServerIP;
    property ServerPort: word read GeServerPort write SetServerPort;
    procedure Connect(AServerIP: string; AServerPort: word); overload; virtual;
    procedure Connect; overload; override;
    constructor Create(AServerIP: string; AServerPort: word); overload; virtual;
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure RegisterMessageToReceive(AMessage: TOlfSMMessage);
    procedure SubscribeToMessage(AMessageID: TOlfSMMessageID;
      aReceivedMessageEvent: TOlfReceivedMessageEvent);
    procedure UnsubscribeToMessage(AMessageID: TOlfSMMessageID;
      aReceivedMessageEvent: TOlfReceivedMessageEvent);
  end;

  // **************************************************
  // * For compatibility with existing code
  // * Don't use this types in a new project.
  // **************************************************

  /// <summary>
  /// DEPRECATED : use TOlfSMMessageID
  /// </summary>
  TOlfMessageID = TOlfSMMessageID;
  /// <summary>
  /// DEPRECATED : use TOlfSMMessageSize
  /// </summary>
  TOlfMessageSize = TOlfSMMessageSize;
  /// <summary>
  /// DEPRECATED : use TOlfSMSrvConnectedClient
  /// </summary>
  TOlfSocketMessagingServerConnectedClient = TOlfSMSrvConnectedClient;
  /// <summary>
  /// DEPRECATED : use TOlfSMException
  /// </summary>
  TOlfSocketMessagingException = TOlfSMException;
  /// <summary>
  /// DEPRECATED : use TOlfSMServer
  /// </summary>
  TOlfSocketMessagingServer = TOlfSMServer;
  /// <summary>
  /// DEPRECATED : use TOlfSMClient
  /// </summary>
  TOlfSocketMessagingClient = TOlfSMClient;
  /// <summary>
  /// DEPRECATED : use TOlfSMMessage
  /// </summary>
  TOlfSocketMessage = TOlfSMMessage;
  /// <summary>
  /// DEPRECATED : use TOlfSMMessagesDict
  /// </summary>
  TOlfSocketMessagesDict = TOlfSMMessagesDict;
  /// <summary>
  /// DEPRECATED : use IOlfSMMessagesRegister
  /// </summary>
  IOlfSocketMessagesRegister = IOlfSMMessagesRegister;

  // **************************************************

implementation

uses
  System.Threading;

{ TOlfSMServer }

constructor TOlfSMServer.Create;
begin
  inherited;
  FIP := '';
  FPort := 0;
  FSocket := nil;
  FThread := nil;
  FMessagesDict := TOlfSMMessagesDict.Create([doOwnsValues]);
  FSubscribers := TOlfSubscribers.Create([doOwnsValues]);
end;

constructor TOlfSMServer.Create(AIP: string; APort: word);
begin
  Create;
  IP := AIP;
  Port := APort;
end;

destructor TOlfSMServer.Destroy;
begin
  if assigned(FThread) then
    FThread.Terminate;
  // FSocket.Free; // done by the thread
  FMessagesDict.Free;
  FSubscribers.Free;
  inherited;
end;

function TOlfSMServer.GetIP: string;
begin
  tmonitor.Enter(self);
  try
    Result := FIP;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSMServer.GetPort: word;
begin
  tmonitor.Enter(self);
  try
    Result := FPort;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSMServer.GetSocket: TSocket;
begin
  tmonitor.Enter(self);
  try
    Result := FSocket;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSMServer.GetThreadNameForDebugging: string;
begin
  tmonitor.Enter(self);
  try
    if FThreadNameForDebugging.IsEmpty then
      Result := classname
    else
      Result := FThreadNameForDebugging;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSMServer.isConnected: boolean;
begin
  Result := assigned(FSocket) and (TSocketState.connected in FSocket.State);
end;

function TOlfSMServer.isListening: boolean;
begin
  Result := assigned(FSocket) and (TSocketState.Listening in FSocket.State);
end;

procedure TOlfSMServer.Listen(AIP: string; APort: word);
begin
  IP := AIP;
  Port := APort;
  Listen;
end;

function TOlfSMServer.LockMessagesDict: TOlfSMMessagesDict;
begin
  tmonitor.Enter(FMessagesDict);
  Result := FMessagesDict;
end;

function TOlfSMServer.LockSubscribers: TOlfSubscribers;
begin
  tmonitor.Enter(FSubscribers);
  Result := FSubscribers;
end;

procedure TOlfSMServer.RegisterMessageToReceive(AMessage: TOlfSMMessage);
var
  dict: TOlfSMMessagesDict;
begin
  dict := LockMessagesDict;
  try
    dict.AddOrSetValue(AMessage.MessageID, AMessage);
  finally
    UnlockMessagesDict;
  end;
end;

procedure TOlfSMServer.Listen;
begin
  if assigned(FThread) then
    FThread.Terminate;

  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      ServerLoop;
    end);

{$IFDEF DEBUG}
  FThread.NameThreadForDebugging(ThreadNameForDebugging);
{$ENDIF}
  FThread.Start;
end;

procedure TOlfSMServer.ServerLoop;
var
  NewClientSocket: TSocket;
begin
  Socket := TSocket.Create(tsockettype.tcp, tencoding.UTF8);
  try
    Socket.Listen(IP, '', Port);
    try
      if (isConnected) then
      begin
        if (isListening) then
          while not TThread.CheckTerminated do
          begin
            try
              NewClientSocket := Socket.accept(100); // wait 0.1 second max
              if assigned(NewClientSocket) then
                TOlfSMSrvConnectedClient.Create(self, NewClientSocket)
                  .StartClientLoop;
            except
              on e: exception do
                exception.RaiseOuterException
                  (TOlfSMException.Create('Server except: ' + e.Message));
            end;
          end
        else
          raise TOlfSMException.Create('Server not listening.');
      end
      else
        raise TOlfSMException.Create('Server not connected.');
    finally
      Socket.Close;
    end;
  finally
    FreeAndNil(Socket);
  end;
end;

procedure TOlfSMServer.SetIP(const Value: string);
begin
  tmonitor.Enter(self);
  try
    FIP := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSMServer.SetPort(const Value: word);
begin
  tmonitor.Enter(self);
  try
    FPort := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSMServer.SetSocket(const Value: TSocket);
begin
  tmonitor.Enter(self);
  try
    FSocket := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSMServer.SetThreadNameForDebugging(const Value: string);
begin
  tmonitor.Enter(self);
  try
    FThreadNameForDebugging := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSMServer.SubscribeToMessage(AMessageID: TOlfSMMessageID;
aReceivedMessageEvent: TOlfReceivedMessageEvent);
var
  sub: TOlfSubscribers;
  msgSub: TOlfMessageSubscribers;
  // found: boolean;
  // proc: TOlfReceivedMessageEvent;
begin
  if not assigned(aReceivedMessageEvent) then
    Exit;

  sub := LockSubscribers;
  try
    if not sub.TryGetValue(AMessageID, msgSub) then
    begin
      msgSub := TOlfMessageSubscribers.Create;
      sub.Add(AMessageID, msgSub);
      msgSub.Add(aReceivedMessageEvent);
    end
    else if (msgSub.Count < 1) then
      msgSub.Add(aReceivedMessageEvent)
    else
    begin
      // TODO : check if the subscriber is already in the list
      // found := false;
      // for proc in msgSub do
      // begin
      // found := (@(proc) = @(aReceivedMessageEvent));
      // if found then
      // break;
      // end;
      // if not found then
      msgSub.Add(aReceivedMessageEvent)
    end;
  finally
    UnlockSubscribers;
  end;
end;

procedure TOlfSMServer.UnlockMessagesDict;
begin
  tmonitor.Exit(FMessagesDict);
end;

procedure TOlfSMServer.UnlockSubscribers;
begin
  tmonitor.Exit(FSubscribers);
end;

procedure TOlfSMServer.UnsubscribeToMessage(AMessageID: TOlfSMMessageID;
aReceivedMessageEvent: TOlfReceivedMessageEvent);
begin
  // TODO : unsubscribe the listener
end;

{ TOlfSMSrvConnectedClient }

constructor TOlfSMSrvConnectedClient.Create(AServer: TOlfSMServer;
AClientSocket: TSocket);
begin
  Create;
  FSocketServer := AServer;
  Socket := AClientSocket;
end;

procedure TOlfSMSrvConnectedClient.ClientLoop;
var
  Buffer: TBytes;
  RecCount, i: integer;
  ms: TMemoryStream;
  MessageSize: TOlfSMMessageSize;
  MessageID: TOlfSMMessageID;
  ReceivedMessage: TOlfSMMessage;
begin
  MessageSize := 0;
  ms := TMemoryStream.Create;
  try
    while not TThread.CheckTerminated do
    begin
      RecCount := FSocket.Receive(Buffer);
      if (RecCount > 0) then
        for i := 0 to RecCount - 1 do
        begin
          ms.Write(Buffer[i], sizeof(Buffer[i]));
          if (MessageSize = 0) then
          begin
            // size of next message received
            if ms.Size = sizeof(MessageSize) then
            begin
              ms.Position := 0;
              ms.Read(MessageSize, sizeof(MessageSize));
              ms.Clear;
            end;
          end
          else if ms.Size = MessageSize then
          begin
            // message received
            ms.Position := 0;
            ms.Read(MessageID, sizeof(MessageID));
            ReceivedMessage := GetNewMessageInstance(MessageID);
            if assigned(ReceivedMessage) then
              try
                ms.Position := 0;
                ReceivedMessage.LoadFromStream(ms);
                DispatchReceivedMessage(ReceivedMessage);
              finally
                ReceivedMessage.Free;
              end
            else
              raise TOlfSMException.Create('No message with ID ' +
                MessageID.ToString);
            ms.Clear;
            MessageSize := 0;
          end;
        end
      else
        sleep(100);
    end;
  finally
    ms.Free;
  end;
end;

procedure TOlfSMSrvConnectedClient.Connect;
begin
  // Do nothing here
end;

constructor TOlfSMSrvConnectedClient.Create;
begin
  inherited;
  FThread := nil;
  FSocket := nil;
  FSocketServer := nil;
end;

destructor TOlfSMSrvConnectedClient.Destroy;
begin
  if assigned(FThread) then
    FThread.Terminate;
  Socket.Free;
  inherited;
end;

procedure TOlfSMSrvConnectedClient.DispatchReceivedMessage(AMessage: TOlfSMMessage);
var
  Subscribers: TOlfSubscribers;
  MessageSubscribers: TOlfMessageSubscribers;
begin
  if not assigned(FSocketServer) then
    Exit;

  Subscribers := FSocketServer.LockSubscribers;
  try
    if Subscribers.TryGetValue(AMessage.MessageID, MessageSubscribers) then
      tparallel.for(0, MessageSubscribers.Count - 1,
        procedure(Index: integer)
        begin
          MessageSubscribers[index](self, AMessage);
        end);
  finally
    FSocketServer.UnlockSubscribers;
  end;
end;

function TOlfSMSrvConnectedClient.GetNewMessageInstance
  (AMessageID: TOlfSMMessageID): TOlfSMMessage;
var
  dict: TOlfSMMessagesDict;
  msg: TOlfSMMessage;
begin
  if not assigned(FSocketServer) then
    Exit(nil);

  dict := FSocketServer.LockMessagesDict;
  try
    if dict.TryGetValue(AMessageID, msg) then
      Result := msg.GetNewInstance
    else
      Result := nil;
  finally
    FSocketServer.UnlockMessagesDict;
  end;
end;

function TOlfSMSrvConnectedClient.GetSocket: TSocket;
begin
  tmonitor.Enter(self);
  try
    Result := FSocket;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSMSrvConnectedClient.GetThreadNameForDebugging: string;
begin
  tmonitor.Enter(self);
  try
    if FThreadNameForDebugging.IsEmpty then
      Result := classname
    else
      Result := FThreadNameForDebugging;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSMSrvConnectedClient.isConnected: boolean;
begin
  Result := assigned(FSocket) and (TSocketState.connected in FSocket.State);
end;

procedure TOlfSMSrvConnectedClient.SendMessage(Const AMessage: TOlfSMMessage);
var
  ms: TMemoryStream;
  MessageSize: TOlfSMMessageSize;
  ss: TSocketStream;
begin
  if not assigned(AMessage) then
    Exit;

  if not assigned(FSocket) then
    Exit;

  if not isConnected then
    Exit;

  ms := TMemoryStream.Create;
  try
    AMessage.SaveToStream(ms);
    ss := TSocketStream.Create(FSocket, false);
    try
      if (ms.Size > high(TOlfSMMessageSize)) then
        raise exception.Create('Message too big (' + ms.Size.ToString + ').');
      MessageSize := ms.Size;
      FSocket.Send(MessageSize, sizeof(MessageSize));
      ms.Position := 0;
      ss.CopyFrom(ms);
    finally
      ss.Free;
    end;
  finally
    ms.Free;
  end;
end;

procedure TOlfSMSrvConnectedClient.SetSocket(const Value: TSocket);
begin
  tmonitor.Enter(self);
  try
    FSocket := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSMSrvConnectedClient.SetThreadNameForDebugging
  (const Value: string);
begin
  tmonitor.Enter(self);
  try
    FThreadNameForDebugging := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSMSrvConnectedClient.StartClientLoop;
begin
  if assigned(FThread) then
  begin
    FThread.Terminate;
    Connect;
  end;

  if isConnected then
  begin
    FThread := TThread.CreateAnonymousThread(
      procedure
      begin
        ClientLoop;
      end);
{$IFDEF DEBUG}
    FThread.NameThreadForDebugging(ThreadNameForDebugging);
{$ENDIF}
    FThread.Start;
  end
  else
    raise TOlfSMException.Create('Can''t connect to the server.');
end;

{ TOlfSMClient }

procedure TOlfSMClient.Connect;
begin
  if assigned(Socket) then
    Socket.Free;

  Socket := TSocket.Create(tsockettype.tcp, tencoding.UTF8);
  if assigned(Socket) then
  begin
    Socket.Connect('', ServerIP, '', ServerPort);
    StartClientLoop;
  end
  else
    raise TOlfSMException.Create('Can''t create a socket.');
end;

procedure TOlfSMClient.Connect(AServerIP: string; AServerPort: word);
begin
  ServerIP := AServerIP;
  ServerPort := AServerPort;
  Connect;
end;

constructor TOlfSMClient.Create;
begin
  inherited;
  FServerIP := '';
  FServerPort := 0;
  FMessagesDict := TOlfSMMessagesDict.Create([doOwnsValues]);
  FSubscribers := TOlfSubscribers.Create([doOwnsValues]);
end;

destructor TOlfSMClient.Destroy;
begin
  FMessagesDict.Free;
  FSubscribers.Free;
  inherited;
end;

procedure TOlfSMClient.DispatchReceivedMessage(AMessage: TOlfSMMessage);
var
  Subscribers: TOlfSubscribers;
  MessageSubscribers: TOlfMessageSubscribers;
begin
  Subscribers := LockSubscribers;
  try
    if Subscribers.TryGetValue(AMessage.MessageID, MessageSubscribers) then
      tparallel.for(0, MessageSubscribers.Count - 1,
        procedure(Index: integer)
        begin
          MessageSubscribers[index](self, AMessage);
        end);
  finally
    UnlockSubscribers;
  end;
end;

function TOlfSMClient.GeServerIP: string;
begin
  tmonitor.Enter(self);
  try
    Result := FServerIP;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSMClient.GeServerPort: word;
begin
  tmonitor.Enter(self);
  try
    Result := FServerPort;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSMClient.GetNewMessageInstance(AMessageID: byte): TOlfSMMessage;
var
  dict: TOlfSMMessagesDict;
  msg: TOlfSMMessage;
begin
  dict := LockMessagesDict;
  try
    if dict.TryGetValue(AMessageID, msg) then
      Result := msg.GetNewInstance
    else
      Result := nil;
  finally
    UnlockMessagesDict;
  end;
end;

function TOlfSMClient.LockMessagesDict: TOlfSMMessagesDict;
begin
  tmonitor.Enter(FMessagesDict);
  Result := FMessagesDict;
end;

function TOlfSMClient.LockSubscribers: TOlfSubscribers;
begin
  tmonitor.Enter(FSubscribers);
  Result := FSubscribers;
end;

procedure TOlfSMClient.RegisterMessageToReceive(AMessage: TOlfSMMessage);
var
  dict: TOlfSMMessagesDict;
begin
  dict := LockMessagesDict;
  try
    dict.AddOrSetValue(AMessage.MessageID, AMessage);
  finally
    UnlockMessagesDict;
  end;
end;

constructor TOlfSMClient.Create(AServerIP: string; AServerPort: word);
begin
  Create;
  ServerIP := AServerIP;
  ServerPort := AServerPort;
end;

constructor TOlfSMClient.Create(AServer: TOlfSMServer; AClientSocket: TSocket);
begin
  raise TOlfSMException.Create('Can''t use this constructor !');
end;

procedure TOlfSMClient.SetServerIP(const Value: string);
begin
  tmonitor.Enter(self);
  try
    FServerIP := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSMClient.SetServerPort(const Value: word);
begin
  tmonitor.Enter(self);
  try
    FServerPort := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSMClient.SubscribeToMessage(AMessageID: TOlfSMMessageID;
aReceivedMessageEvent: TOlfReceivedMessageEvent);
var
  sub: TOlfSubscribers;
  msgSub: TOlfMessageSubscribers;
  // found: boolean;
  // proc: TOlfReceivedMessageEvent;
begin
  if not assigned(aReceivedMessageEvent) then
    Exit;

  sub := LockSubscribers;
  try
    if not sub.TryGetValue(AMessageID, msgSub) then
    begin
      msgSub := TOlfMessageSubscribers.Create;
      sub.Add(AMessageID, msgSub);
      msgSub.Add(aReceivedMessageEvent);
    end
    else if (msgSub.Count < 1) then
      msgSub.Add(aReceivedMessageEvent)
    else
    begin
      // TODO : check if the subscriber is already in the list
      // found := false;
      // for proc in msgSub do
      // begin
      // found := (@(proc) = @(aReceivedMessageEvent));
      // if found then
      // break;
      // end;
      // if not found then
      msgSub.Add(aReceivedMessageEvent)
    end;
  finally
    UnlockSubscribers;
  end;
end;

procedure TOlfSMClient.UnlockMessagesDict;
begin
  tmonitor.Exit(FMessagesDict);
end;

procedure TOlfSMClient.UnlockSubscribers;
begin
  tmonitor.Exit(FSubscribers);
end;

procedure TOlfSMClient.UnsubscribeToMessage(AMessageID: TOlfSMMessageID;
aReceivedMessageEvent: TOlfReceivedMessageEvent);
begin
  // TODO : unsubscribe the listener
end;

{ TOlfSMMessage }

constructor TOlfSMMessage.Create;
begin
  FMessageID := 0;
end;

function TOlfSMMessage.GetNewInstance: TOlfSMMessage;
begin
  Result := TOlfSMMessage.Create;
end;

procedure TOlfSMMessage.LoadFromStream(Stream: TStream);
begin
  if not assigned(Stream) then
    Exit;

  if (Stream.Read(FMessageID, sizeof(FMessageID)) <> sizeof(FMessageID)) then
    raise TOlfSMException.Create('');
end;

procedure TOlfSMMessage.SaveToStream(Stream: TStream);
begin
  if not assigned(Stream) then
    Exit;

  Stream.Write(FMessageID, sizeof(FMessageID));
end;

procedure TOlfSMMessage.SetMessageID(const Value: TOlfSMMessageID);
begin
  FMessageID := Value;
end;

end.
