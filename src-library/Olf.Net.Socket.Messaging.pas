unit Olf.Net.Socket.Messaging;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Net.Socket,
  System.Classes;

type
  TOlfMessageID = byte; // 256 messages (0..255)

  TOlfMessageSize = word; // 65535 bytes for a message (0..65535)

  TOlfSocketMessagingServerConnectedClient = class;

  TOlfSocketMessagingException = class(exception)
  end;

  TOlfSocketMessage = class
  private
    FMessageID: TOlfMessageID;
    procedure SetMessageID(const Value: TOlfMessageID);
  public
    property MessageID: TOlfMessageID read FMessageID write SetMessageID;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    function GetNewInstance: TOlfSocketMessage; virtual;
    constructor Create; virtual;
  end;

  TOlfSocketMessagesDict = TObjectDictionary<TOlfMessageID, TOlfSocketMessage>;

  TOlfReceivedMessageEvent = procedure(Const ASender
    : TOlfSocketMessagingServerConnectedClient;
    Const AMessage: TOlfSocketMessage) of object;
  TOlfMessageSubscribers = TList<TOlfReceivedMessageEvent>;
  TOlfSubscribers = TObjectDictionary<TOlfMessageID, TOlfMessageSubscribers>;

  IOlfSocketMessagesRegister = interface
    ['{6728BA4A-44AD-415D-9436-1626920DF655}']
    procedure RegisterMessageToReceive(AMessage: TOlfSocketMessage);
  end;

  TOlfSocketMessagingServer = class(TInterfacedObject,
    IOlfSocketMessagesRegister)
  private
    FThread: TThread;
    FSocket: TSocket;
    FPort: word;
    FIP: string;
    FThreadNameForDebugging: string;
    FMessagesDict: TOlfSocketMessagesDict;
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
    function LockMessagesDict: TOlfSocketMessagesDict;
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
    destructor Destroy; override;
    procedure RegisterMessageToReceive(AMessage: TOlfSocketMessage);
    procedure SubscribeToMessage(AMessageID: TOlfMessageID;
      aReceivedMessageEvent: TOlfReceivedMessageEvent);
    procedure UnsubscribeToMessage(AMessageID: TOlfMessageID;
      aReceivedMessageEvent: TOlfReceivedMessageEvent);
  end;

  TOlfSocketMessagingServerConnectedClient = class(TInterfacedObject)
  private
    FThread: TThread;
    FSocket: TSocket;
    FSocketServer: TOlfSocketMessagingServer;
    FThreadNameForDebugging: string;
    procedure SetSocket(const Value: TSocket);
    function GetSocket: TSocket;
    function GetThreadNameForDebugging: string;
    procedure SetThreadNameForDebugging(const Value: string);
  protected
    property Socket: TSocket read GetSocket write SetSocket;
    procedure ClientLoop; virtual;
    procedure StartClientLoop; virtual;
    function GetNewMessageInstance(AMessageID: TOlfMessageID)
      : TOlfSocketMessage; virtual;
    procedure DispatchMessage(AMessage: TOlfSocketMessage); virtual;
  public
    property ThreadNameForDebugging: string read GetThreadNameForDebugging
      write SetThreadNameForDebugging;
    constructor Create(AServer: TOlfSocketMessagingServer;
      AClientSocket: TSocket); overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure SendMessage(Const AMessage: TOlfSocketMessage);
  end;

  TOlfSocketMessagingClient = class(TOlfSocketMessagingServerConnectedClient,
    IOlfSocketMessagesRegister)
  private
    FServerPort: word;
    FServerIP: string;
    FMessagesDict: TOlfSocketMessagesDict;
    FSubscribers: TOlfSubscribers;
    procedure SetServerIP(const Value: string);
    procedure SetServerPort(const Value: word);
    constructor Create(AServer: TOlfSocketMessagingServer;
      AClientSocket: TSocket); override;
    function GeServerIP: string;
    function GeServerPort: word;
  protected
    function GetNewMessageInstance(AMessageID: byte)
      : TOlfSocketMessage; override;
    procedure DispatchMessage(AMessage: TOlfSocketMessage); override;
    function LockMessagesDict: TOlfSocketMessagesDict;
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
    procedure RegisterMessageToReceive(AMessage: TOlfSocketMessage);
    procedure SubscribeToMessage(AMessageID: TOlfMessageID;
      aReceivedMessageEvent: TOlfReceivedMessageEvent);
    procedure UnsubscribeToMessage(AMessageID: TOlfMessageID;
      aReceivedMessageEvent: TOlfReceivedMessageEvent);
  end;

implementation

uses
  System.Threading;

{ TOlfSocketMessagingServer }

constructor TOlfSocketMessagingServer.Create;
begin
  inherited;
  FIP := '';
  FPort := 0;
  FSocket := nil;
  FThread := nil;
  FMessagesDict := TOlfSocketMessagesDict.Create([doOwnsValues]);
  FSubscribers := TOlfSubscribers.Create([doOwnsValues]);
end;

constructor TOlfSocketMessagingServer.Create(AIP: string; APort: word);
begin
  Create;
  IP := AIP;
  Port := APort;
end;

destructor TOlfSocketMessagingServer.Destroy;
begin
  if assigned(FThread) then
    FThread.Terminate;
  // FSocket.Free; // done by the thread
  FMessagesDict.Free;
  FSubscribers.Free;
  inherited;
end;

function TOlfSocketMessagingServer.GetIP: string;
begin
  tmonitor.Enter(self);
  try
    Result := FIP;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSocketMessagingServer.GetPort: word;
begin
  tmonitor.Enter(self);
  try
    Result := FPort;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSocketMessagingServer.GetSocket: TSocket;
begin
  tmonitor.Enter(self);
  try
    Result := FSocket;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSocketMessagingServer.GetThreadNameForDebugging: string;
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

procedure TOlfSocketMessagingServer.Listen(AIP: string; APort: word);
begin
  IP := AIP;
  Port := APort;
  Listen;
end;

function TOlfSocketMessagingServer.LockMessagesDict: TOlfSocketMessagesDict;
begin
  tmonitor.Enter(FMessagesDict);
  Result := FMessagesDict;
end;

function TOlfSocketMessagingServer.LockSubscribers: TOlfSubscribers;
begin
  tmonitor.Enter(FSubscribers);
  Result := FSubscribers;
end;

procedure TOlfSocketMessagingServer.RegisterMessageToReceive
  (AMessage: TOlfSocketMessage);
var
  dict: TOlfSocketMessagesDict;
begin
  dict := LockMessagesDict;
  try
    dict.AddOrSetValue(AMessage.MessageID, AMessage);
  finally
    UnlockMessagesDict;
  end;
end;

procedure TOlfSocketMessagingServer.Listen;
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

procedure TOlfSocketMessagingServer.ServerLoop;
var
  NewClientSocket: TSocket;
begin
  Socket := TSocket.Create(tsockettype.tcp, tencoding.UTF8);
  try
    Socket.Listen(IP, '', Port);
    try
      if (TSocketState.connected in Socket.State) then
      begin
        if (TSocketState.listening in Socket.State) then
          while not TThread.CheckTerminated do
          begin
            try
              NewClientSocket := Socket.accept(100); // wait 0.1 second max
              if assigned(NewClientSocket) then
                TOlfSocketMessagingServerConnectedClient.Create(self,
                  NewClientSocket).StartClientLoop;
            except
              on e: exception do
                exception.RaiseOuterException
                  (TOlfSocketMessagingException.Create('Server except: ' +
                  e.Message));
            end;
          end
        else
          raise TOlfSocketMessagingException.Create('Server not listening.');
      end
      else
        raise TOlfSocketMessagingException.Create('Server not connected.');
    finally
      Socket.Close;
    end;
  finally
    FreeAndNil(Socket);
  end;
end;

procedure TOlfSocketMessagingServer.SetIP(const Value: string);
begin
  tmonitor.Enter(self);
  try
    FIP := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSocketMessagingServer.SetPort(const Value: word);
begin
  tmonitor.Enter(self);
  try
    FPort := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSocketMessagingServer.SetSocket(const Value: TSocket);
begin
  tmonitor.Enter(self);
  try
    FSocket := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSocketMessagingServer.SetThreadNameForDebugging
  (const Value: string);
begin
  tmonitor.Enter(self);
  try
    FThreadNameForDebugging := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSocketMessagingServer.SubscribeToMessage
  (AMessageID: TOlfMessageID; aReceivedMessageEvent: TOlfReceivedMessageEvent);
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

procedure TOlfSocketMessagingServer.UnlockMessagesDict;
begin
  tmonitor.Exit(FMessagesDict);
end;

procedure TOlfSocketMessagingServer.UnlockSubscribers;
begin
  tmonitor.Exit(FSubscribers);
end;

procedure TOlfSocketMessagingServer.UnsubscribeToMessage
  (AMessageID: TOlfMessageID; aReceivedMessageEvent: TOlfReceivedMessageEvent);
begin
  // TODO : unsubscribe the listener
end;

{ TOlfSocketMessagingServerConnectedClient }

constructor TOlfSocketMessagingServerConnectedClient.Create
  (AServer: TOlfSocketMessagingServer; AClientSocket: TSocket);
begin
  Create;
  FSocketServer := AServer;
  Socket := AClientSocket;
end;

procedure TOlfSocketMessagingServerConnectedClient.ClientLoop;
var
  Buffer: TBytes;
  RecCount, i: integer;
  ms: TMemoryStream;
  MessageSize: TOlfMessageSize;
  MessageID: TOlfMessageID;
  ReceivedMessage: TOlfSocketMessage;
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
                DispatchMessage(ReceivedMessage);
              finally
                ReceivedMessage.Free;
              end
            else
              raise TOlfSocketMessagingException.Create('No message with ID ' +
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

procedure TOlfSocketMessagingServerConnectedClient.Connect;
begin
  // Do nothing here
end;

constructor TOlfSocketMessagingServerConnectedClient.Create;
begin
  inherited;
  FThread := nil;
  FSocket := nil;
  FSocketServer := nil;
end;

destructor TOlfSocketMessagingServerConnectedClient.Destroy;
begin
  if assigned(FThread) then
    FThread.Terminate;
  Socket.Free;
  inherited;
end;

procedure TOlfSocketMessagingServerConnectedClient.DispatchMessage
  (AMessage: TOlfSocketMessage);
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

function TOlfSocketMessagingServerConnectedClient.GetNewMessageInstance
  (AMessageID: TOlfMessageID): TOlfSocketMessage;
var
  dict: TOlfSocketMessagesDict;
  msg: TOlfSocketMessage;
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

function TOlfSocketMessagingServerConnectedClient.GetSocket: TSocket;
begin
  tmonitor.Enter(self);
  try
    Result := FSocket;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSocketMessagingServerConnectedClient.
  GetThreadNameForDebugging: string;
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

procedure TOlfSocketMessagingServerConnectedClient.SendMessage
  (Const AMessage: TOlfSocketMessage);
var
  ms: TMemoryStream;
  MessageSize: TOlfMessageSize;
  ss: TSocketStream;
begin
  if not assigned(AMessage) then
    Exit;

  if not assigned(FSocket) then
    Exit;

  if not(TSocketState.connected in FSocket.State) then
    Exit;

  ms := TMemoryStream.Create;
  try
    AMessage.SaveToStream(ms);
    ss := TSocketStream.Create(FSocket, false);
    try
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

procedure TOlfSocketMessagingServerConnectedClient.SetSocket
  (const Value: TSocket);
begin
  tmonitor.Enter(self);
  try
    FSocket := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSocketMessagingServerConnectedClient.SetThreadNameForDebugging
  (const Value: string);
begin
  tmonitor.Enter(self);
  try
    FThreadNameForDebugging := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSocketMessagingServerConnectedClient.StartClientLoop;
begin
  if assigned(FThread) then
  begin
    FThread.Terminate;
    Connect;
  end;

  if (TSocketState.connected in Socket.State) then
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
    raise TOlfSocketMessagingException.Create('Can''t connect to the server.');
end;

{ TOlfSocketMessagingClient }

procedure TOlfSocketMessagingClient.Connect;
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
    raise TOlfSocketMessagingException.Create('Can''t create a socket.');
end;

procedure TOlfSocketMessagingClient.Connect(AServerIP: string;
AServerPort: word);
begin
  ServerIP := AServerIP;
  ServerPort := AServerPort;
  Connect;
end;

constructor TOlfSocketMessagingClient.Create;
begin
  inherited;
  FServerIP := '';
  FServerPort := 0;
  FMessagesDict := TOlfSocketMessagesDict.Create([doOwnsValues]);
  FSubscribers := TOlfSubscribers.Create([doOwnsValues]);
end;

destructor TOlfSocketMessagingClient.Destroy;
begin
  FMessagesDict.Free;
  FSubscribers.Free;
  inherited;
end;

procedure TOlfSocketMessagingClient.DispatchMessage
  (AMessage: TOlfSocketMessage);
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

function TOlfSocketMessagingClient.GeServerIP: string;
begin
  tmonitor.Enter(self);
  try
    Result := FServerIP;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSocketMessagingClient.GeServerPort: word;
begin
  tmonitor.Enter(self);
  try
    Result := FServerPort;
  finally
    tmonitor.Exit(self);
  end;
end;

function TOlfSocketMessagingClient.GetNewMessageInstance(AMessageID: byte)
  : TOlfSocketMessage;
var
  dict: TOlfSocketMessagesDict;
  msg: TOlfSocketMessage;
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

function TOlfSocketMessagingClient.LockMessagesDict: TOlfSocketMessagesDict;
begin
  tmonitor.Enter(FMessagesDict);
  Result := FMessagesDict;
end;

function TOlfSocketMessagingClient.LockSubscribers: TOlfSubscribers;
begin
  tmonitor.Enter(FSubscribers);
  Result := FSubscribers;
end;

procedure TOlfSocketMessagingClient.RegisterMessageToReceive
  (AMessage: TOlfSocketMessage);
var
  dict: TOlfSocketMessagesDict;
begin
  dict := LockMessagesDict;
  try
    dict.AddOrSetValue(AMessage.MessageID, AMessage);
  finally
    UnlockMessagesDict;
  end;
end;

constructor TOlfSocketMessagingClient.Create(AServerIP: string;
AServerPort: word);
begin
  Create;
  ServerIP := FServerIP;
  ServerPort := FServerPort;
end;

constructor TOlfSocketMessagingClient.Create(AServer: TOlfSocketMessagingServer;
AClientSocket: TSocket);
begin
  raise TOlfSocketMessagingException.Create('Can''t use this constructor !');
end;

procedure TOlfSocketMessagingClient.SetServerIP(const Value: string);
begin
  tmonitor.Enter(self);
  try
    FServerIP := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSocketMessagingClient.SetServerPort(const Value: word);
begin
  tmonitor.Enter(self);
  try
    FServerPort := Value;
  finally
    tmonitor.Exit(self);
  end;
end;

procedure TOlfSocketMessagingClient.SubscribeToMessage
  (AMessageID: TOlfMessageID; aReceivedMessageEvent: TOlfReceivedMessageEvent);
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

procedure TOlfSocketMessagingClient.UnlockMessagesDict;
begin
  tmonitor.Exit(FMessagesDict);
end;

procedure TOlfSocketMessagingClient.UnlockSubscribers;
begin
  tmonitor.Exit(FSubscribers);
end;

procedure TOlfSocketMessagingClient.UnsubscribeToMessage
  (AMessageID: TOlfMessageID; aReceivedMessageEvent: TOlfReceivedMessageEvent);
begin
  // TODO : unsubscribe the listener
end;

{ TOlfSocketMessage }

constructor TOlfSocketMessage.Create;
begin
  FMessageID := 0;
end;

function TOlfSocketMessage.GetNewInstance: TOlfSocketMessage;
begin
  Result := TOlfSocketMessage.Create;
end;

procedure TOlfSocketMessage.LoadFromStream(Stream: TStream);
begin
  if not assigned(Stream) then
    Exit;

  if (Stream.Read(FMessageID, sizeof(FMessageID)) <> sizeof(FMessageID)) then
    raise TOlfSocketMessagingException.Create('');
end;

procedure TOlfSocketMessage.SaveToStream(Stream: TStream);
begin
  if not assigned(Stream) then
    Exit;

  Stream.Write(FMessageID, sizeof(FMessageID));
end;

procedure TOlfSocketMessage.SetMessageID(const Value: TOlfMessageID);
begin
  FMessageID := Value;
end;

end.
