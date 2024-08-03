/// <summary>
/// ***************************************************************************
///
/// Socket Messaging Library
///
/// Copyright 2023-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Socket Messaging is a Delphi Library for managing a messaging system over
/// TCP/IP sockets between clients and servers or peer to peer programs.
///
/// The library can be used in any Delphi project on recent versions of the
/// environment. It uses the TSocket class from System.Net.Socket, threads and
/// generic collections.
///
/// To use this library more simply, and obtain the source code you need to
/// use a client and server in your project, with your own interface, without
/// coding, use this code generator : https://smcodegenerator.olfsoftware.fr/
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://socketmessaging.developpeur-pascal.fr/
///
/// Project site :
///      https://github.com/DeveloppeurPascal/Socket-Messaging-Library
///
/// ***************************************************************************
/// File last update : 28/05/2024 12:19:19
/// Signature : 8bc0536e5b81b097a02934e6b9ce4a388f5c9ff9
/// ***************************************************************************
/// </summary>

unit Olf.Net.Socket.Messaging;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Net.Socket,
  System.Classes;

type
  TOlfSMMessageID = byte; // 256 messages (0..255)

  TOlfSMMessageSize = int64; // typeOf(TStream.Size)

  TOlfSMServer = class;
  TOlfSMSrvConnectedClient = class;
  TOlfSMSrvConnectedClientsList = class;
  TOlfSMClient = class;

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

  TOlfSMReceivedMessageEvent = procedure(Const ASender
    : TOlfSMSrvConnectedClient; Const AMessage: TOlfSMMessage) of object;
  TOlfSMReceivedMessageEvent<T: TOlfSMMessage> = procedure
    (Const ASender: TOlfSMSrvConnectedClient; Const AMessage: T) of object;
  TOlfMessageSubscribers = TList<TOlfSMReceivedMessageEvent>;
  TOlfSubscribers = TObjectDictionary<TOlfSMMessageID, TOlfMessageSubscribers>;

  IOlfSMMessagesRegister = interface
    ['{6728BA4A-44AD-415D-9436-1626920DF655}']
    procedure RegisterMessageToReceive(AMessage: TOlfSMMessage);
  end;

  TOlfSMServerEvent = procedure(AServer: TOlfSMServer) of object;
  TOlfSMEncodeDecodeMessageEvent = function(AFrom: TStream): TStream of object;

  TOlfSMConnectedClientProc = reference to procedure(Const AConnectedClient
    : TOlfSMSrvConnectedClient);
  TOlfSMConnectedClientEvent = procedure(Const AConnectedClient
    : TOlfSMSrvConnectedClient) of object;

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
    FonServerConnected: TOlfSMServerEvent;
    FonServerDisconnected: TOlfSMServerEvent;
    FonDecodeReceivedMessage: TOlfSMEncodeDecodeMessageEvent;
    FonEncodeMessageToSend: TOlfSMEncodeDecodeMessageEvent;
    FConnectedClients: TOlfSMSrvConnectedClientsList;
    FonClientConnected: TOlfSMConnectedClientEvent;
    FonClientDisconnected: TOlfSMConnectedClientEvent;
    FonClientLostConnection: TOlfSMConnectedClientEvent;
    procedure SetIP(const Value: string);
    procedure SetPort(const Value: word);
    procedure SetSocket(const Value: TSocket);
    function GetIP: string;
    function GetPort: word;
    function GetSocket: TSocket;
    procedure SetThreadNameForDebugging(const Value: string);
    function GetThreadNameForDebugging: string;
    procedure SetonServerConnected(const Value: TOlfSMServerEvent);
    procedure SetonServerDisconnected(const Value: TOlfSMServerEvent);
    procedure SetonDecodeReceivedMessage(const Value
      : TOlfSMEncodeDecodeMessageEvent);
    procedure SetonEncodeMessageToSend(const Value
      : TOlfSMEncodeDecodeMessageEvent);
    procedure SetonClientConnected(const Value: TOlfSMConnectedClientEvent);
    procedure SetonClientDisconnected(const Value: TOlfSMConnectedClientEvent);
    procedure SetonClientLostConnection(const Value
      : TOlfSMConnectedClientEvent);
  protected
    property Socket: TSocket read GetSocket write SetSocket;
    procedure ServerLoop; virtual;
    function LockMessagesDict: TOlfSMMessagesDict;
    procedure UnlockMessagesDict;
    function LockSubscribers: TOlfSubscribers;
    procedure UnlockSubscribers;
    procedure DoClientConnected(Const AClient: TOlfSMSrvConnectedClient);
    procedure DoClientLostConnexion(Const AClient: TOlfSMSrvConnectedClient);
    procedure DoClientDisconnected(Const AClient: TOlfSMSrvConnectedClient);
  public
    property IP: string read GetIP write SetIP;
    property Port: word read GetPort write SetPort;
    property ThreadNameForDebugging: string read GetThreadNameForDebugging
      write SetThreadNameForDebugging;
    property onServerConnected: TOlfSMServerEvent read FonServerConnected
      write SetonServerConnected;
    property onServerDisconnected: TOlfSMServerEvent read FonServerDisconnected
      write SetonServerDisconnected;
    property onEncodeMessageToSend: TOlfSMEncodeDecodeMessageEvent
      read FonEncodeMessageToSend write SetonEncodeMessageToSend;
    property onDecodeReceivedMessage: TOlfSMEncodeDecodeMessageEvent
      read FonDecodeReceivedMessage write SetonDecodeReceivedMessage;
    property onClientConnected: TOlfSMConnectedClientEvent
      read FonClientConnected write SetonClientConnected;
    property onClientLostConnection: TOlfSMConnectedClientEvent
      read FonClientLostConnection write SetonClientLostConnection;
    property onClientDisconnected: TOlfSMConnectedClientEvent
      read FonClientDisconnected write SetonClientDisconnected;
    constructor Create(AIP: string; APort: word); overload; virtual;
    constructor Create; overload; virtual;
    procedure Listen; overload; virtual;
    procedure Listen(AIP: string; APort: word); overload; virtual;
    function isListening: boolean;
    function isConnected: boolean;
    destructor Destroy; override;
    procedure RegisterMessageToReceive(AMessage: TOlfSMMessage);
    procedure SubscribeToMessage(AMessageID: TOlfSMMessageID;
      aReceivedMessageEvent: TOlfSMReceivedMessageEvent);
    procedure UnsubscribeToMessage(AMessageID: TOlfSMMessageID;
      aReceivedMessageEvent: TOlfSMReceivedMessageEvent);
    procedure SendMessageToAll(Const AMessage: TOlfSMMessage;
      Const ExceptToClient: TOlfSMSrvConnectedClient = nil);
    procedure ForEachConnectedClient(DoSomethingProc: TOlfSMConnectedClientProc;
      AllowParallelFor: boolean = true); overload;
    procedure ForEachConnectedClient(DoSomethingEvent
      : TOlfSMConnectedClientEvent; AllowParallelFor: boolean = true); overload;
  end;

  TOlfSMSrvConnectedClient = class(TInterfacedObject)
  private
    FThread: TThread;
    FSocket: TSocket;
    FSocketServer: TOlfSMServer;
    FThreadNameForDebugging: string;
    FonConnected: TOlfSMConnectedClientEvent;
    FonDisconnected: TOlfSMConnectedClientEvent;
    FonLostConnection: TOlfSMConnectedClientEvent;
    FonDecodeReceivedMessage: TOlfSMEncodeDecodeMessageEvent;
    FonEncodeMessageToSend: TOlfSMEncodeDecodeMessageEvent;
    FTagBool: boolean;
    FTagFloat: single;
    FTagObject: TObject;
    FTagString: string;
    FTag: nativeint;
    procedure SetSocket(const Value: TSocket);
    function GetSocket: TSocket;
    function GetThreadNameForDebugging: string;
    procedure SetThreadNameForDebugging(const Value: string);
    procedure SetonConnected(const Value: TOlfSMConnectedClientEvent);
    procedure SetonDisconnected(const Value: TOlfSMConnectedClientEvent);
    procedure SetonLostConnection(const Value: TOlfSMConnectedClientEvent);
    procedure SetonDecodeReceivedMessage(const Value
      : TOlfSMEncodeDecodeMessageEvent);
    procedure SetonEncodeMessageToSend(const Value
      : TOlfSMEncodeDecodeMessageEvent);
    procedure SetTag(const Value: nativeint);
    procedure SetTagBool(const Value: boolean);
    procedure SetTagFloat(const Value: single);
    procedure SetTagObject(const Value: TObject);
    procedure SetTagString(const Value: string);
  protected
    FErrorDuringSend: boolean;
    property Socket: TSocket read GetSocket write SetSocket;
    procedure ClientLoop; virtual;
    procedure StartClientLoop; virtual;
    function GetNewMessageInstance(AMessageID: TOlfSMMessageID)
      : TOlfSMMessage; virtual;
    procedure DispatchReceivedMessage(AMessage: TOlfSMMessage); virtual;
  public
    property ThreadNameForDebugging: string read GetThreadNameForDebugging
      write SetThreadNameForDebugging;
    property onConnected: TOlfSMConnectedClientEvent read FonConnected
      write SetonConnected;
    property onLostConnection: TOlfSMConnectedClientEvent read FonLostConnection
      write SetonLostConnection;
    property onDisconnected: TOlfSMConnectedClientEvent read FonDisconnected
      write SetonDisconnected;
    property onEncodeMessageToSend: TOlfSMEncodeDecodeMessageEvent
      read FonEncodeMessageToSend write SetonEncodeMessageToSend;
    property onDecodeReceivedMessage: TOlfSMEncodeDecodeMessageEvent
      read FonDecodeReceivedMessage write SetonDecodeReceivedMessage;
    property Tag: nativeint read FTag write SetTag;
    property TagBool: boolean read FTagBool write SetTagBool;
    property TagString: string read FTagString write SetTagString;
    property TagFloat: single read FTagFloat write SetTagFloat;
    property TagObject: TObject read FTagObject write SetTagObject;
    constructor Create(AServer: TOlfSMServer; AClientSocket: TSocket);
      overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure SendMessage(Const AMessage: TOlfSMMessage);
    function isConnected: boolean;
  end;

  TOlfSMSrvConnectedClientsList = class(TThreadList<TOlfSMSrvConnectedClient>)
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
      aReceivedMessageEvent: TOlfSMReceivedMessageEvent);
    procedure UnsubscribeToMessage(AMessageID: TOlfSMMessageID;
      aReceivedMessageEvent: TOlfSMReceivedMessageEvent);
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
  /// <summary>
  /// DEPRECATED : use TOlfSMReceivedMessageEvent
  /// </summary>
  TOlfReceivedMessageEvent = TOlfSMReceivedMessageEvent;

  // **************************************************

implementation

uses
  System.Threading;

procedure TerminateAThreadAndWait(var Thread: TThread);
begin
  if assigned(Thread) then
  begin
    Thread.FreeOnTerminate := false;
    Thread.Terminate;
    Thread.WaitFor;
    Thread.free;
    Thread := nil;
  end;
end;

procedure CloseFreeAndNilASocket(var Socket: TSocket);
begin
  if assigned(Socket) then
  begin
    if TSocketState.Connected in Socket.State then
      try
        if TSocketState.Listening in Socket.State then
          Socket.Close(true)
        else
          Socket.Close;
      except
        Socket.Close(true);
      end;
    Socket.free;
    Socket := nil;
  end;
end;

{ TOlfSMServer }

constructor TOlfSMServer.Create;
begin
  inherited;
  FIP := '';
  FPort := 0;
  FSocket := nil;
  FThread := nil;
  FonServerConnected := nil;
  FonServerDisconnected := nil;
  FonDecodeReceivedMessage := nil;
  FonEncodeMessageToSend := nil;
  FonClientConnected := nil;
  FonClientDisconnected := nil;
  FonClientLostConnection := nil;
  FonDecodeReceivedMessage := nil;
  FonEncodeMessageToSend := nil;
  FMessagesDict := TOlfSMMessagesDict.Create([doOwnsValues]);
  FSubscribers := TOlfSubscribers.Create([doOwnsValues]);
  FConnectedClients := TOlfSMSrvConnectedClientsList.Create;
end;

constructor TOlfSMServer.Create(AIP: string; APort: word);
begin
  Create;
  IP := AIP;
  Port := APort;
end;

destructor TOlfSMServer.Destroy;
var
  lst: TList<TOlfSMSrvConnectedClient>;
begin
  // On arrête la boucle d'attente de nouveau client
  TerminateAThreadAndWait(FThread);

  // On arrête les connections des clients connectés
  lst := FConnectedClients.LockList;
  try
    while lst.count > 0 do
      lst[0].free;
  finally
    FConnectedClients.UnlockList;
  end;

  FConnectedClients.free;
  FMessagesDict.free;
  FSubscribers.free;
  inherited;
end;

procedure TOlfSMServer.DoClientConnected(const AClient
  : TOlfSMSrvConnectedClient);
begin
  if assigned(onClientConnected) then
    onClientConnected(AClient);
end;

procedure TOlfSMServer.DoClientDisconnected(const AClient
  : TOlfSMSrvConnectedClient);
begin
  FConnectedClients.Remove(AClient);
  if assigned(onClientDisconnected) then
    onClientDisconnected(AClient);
end;

procedure TOlfSMServer.DoClientLostConnexion(const AClient
  : TOlfSMSrvConnectedClient);
begin
  FConnectedClients.Remove(AClient);
  if assigned(onClientLostConnection) then
    onClientLostConnection(AClient);

  AClient.free;
end;

procedure TOlfSMServer.ForEachConnectedClient(DoSomethingEvent
  : TOlfSMConnectedClientEvent; AllowParallelFor: boolean);
begin
  if not assigned(DoSomethingEvent) then
    exit;

  ForEachConnectedClient(
    procedure(Const AConnectedClient: TOlfSMSrvConnectedClient)
    begin
      DoSomethingEvent(AConnectedClient);
    end, AllowParallelFor);
end;

procedure TOlfSMServer.ForEachConnectedClient(DoSomethingProc
  : TOlfSMConnectedClientProc; AllowParallelFor: boolean);
var
  List: TList<TOlfSMSrvConnectedClient>;
  i: integer;
begin
  if not assigned(DoSomethingProc) then
    exit;

  if AllowParallelFor then
  begin
    List := FConnectedClients.LockList;
    try
      tparallel.For(0, List.count - 1,
        procedure(Index: integer)
        begin
          try
            if assigned(List[index]) then
              DoSomethingProc(List[index]);
          except

          end;
        end);
    finally
      FConnectedClients.UnlockList;
    end;
  end
  else
  begin
    List := FConnectedClients.LockList;
    try
      For i := 0 to List.count - 1 do
        try
          if assigned(List[i]) then
            DoSomethingProc(List[i]);
        except

        end;
    finally
      FConnectedClients.UnlockList;
    end;
  end;
end;

function TOlfSMServer.GetIP: string;
begin
  tmonitor.Enter(self);
  try
    Result := FIP;
  finally
    tmonitor.exit(self);
  end;
end;

function TOlfSMServer.GetPort: word;
begin
  tmonitor.Enter(self);
  try
    Result := FPort;
  finally
    tmonitor.exit(self);
  end;
end;

function TOlfSMServer.GetSocket: TSocket;
begin
  tmonitor.Enter(self);
  try
    Result := FSocket;
  finally
    tmonitor.exit(self);
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
    tmonitor.exit(self);
  end;
end;

function TOlfSMServer.isConnected: boolean;
begin
  Result := assigned(FSocket) and (TSocketState.Connected in FSocket.State);
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
  TerminateAThreadAndWait(FThread);

  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      ServerLoop;
    end);

{$IFDEF DEBUG}
  FThread.NameThreadForDebugging(ThreadNameForDebugging);
{$ENDIF}
  FThread.Start;

{$IFDEF CONSOLE}
  FThread.WaitFor;
{$ENDIF}
end;

procedure TOlfSMServer.SendMessageToAll(const AMessage: TOlfSMMessage;
Const ExceptToClient: TOlfSMSrvConnectedClient);
begin
  ForEachConnectedClient(
    procedure(Const Client: TOlfSMSrvConnectedClient)
    begin
      if not(assigned(ExceptToClient) and (ExceptToClient = Client)) then
        Client.SendMessage(AMessage);
    end);
end;

procedure TOlfSMServer.ServerLoop;
var
  NewClientSocket: TSocket;
  SrvClient: TOlfSMSrvConnectedClient;
begin
  Socket := TSocket.Create(tsockettype.tcp, tencoding.UTF8);
  try
    Socket.Listen(IP, '', Port);
    if (isConnected) then
    begin
      if (isListening) then
      begin
        if assigned(onServerConnected) then
          TThread.Synchronize(nil,
            procedure
            begin
              if assigned(onServerConnected) then
                onServerConnected(self);
            end);
        while not TThread.CheckTerminated do
        begin
          try
            NewClientSocket := Socket.accept(100); // wait 0.1 second max
            if assigned(NewClientSocket) then
            begin
              SrvClient := TOlfSMSrvConnectedClient.Create(self,
                NewClientSocket);
              FConnectedClients.Add(SrvClient);
              SrvClient.onConnected := DoClientConnected;
              SrvClient.onLostConnection := DoClientLostConnexion;
              SrvClient.onDisconnected := DoClientDisconnected;
              SrvClient.StartClientLoop;
            end
          except
            on e: exception do
              exception.RaiseOuterException
                (TOlfSMException.Create('Server except: ' + e.Message));
          end;
        end
      end
      else
        raise TOlfSMException.Create('Server not listening.');
      if assigned(onServerDisconnected) then
        TThread.Synchronize(nil,
          procedure
          begin
            if assigned(onServerDisconnected) then
              onServerDisconnected(self);
          end);
    end
    else
      raise TOlfSMException.Create('Server not connected.');
  finally
    CloseFreeAndNilASocket(FSocket);
  end;
end;

procedure TOlfSMServer.SetIP(const Value: string);
begin
  tmonitor.Enter(self);
  try
    FIP := Value;
  finally
    tmonitor.exit(self);
  end;
end;

procedure TOlfSMServer.SetonClientConnected(const Value
  : TOlfSMConnectedClientEvent);
begin
  FonClientConnected := Value;
end;

procedure TOlfSMServer.SetonClientDisconnected(const Value
  : TOlfSMConnectedClientEvent);
begin
  FonClientDisconnected := Value;
end;

procedure TOlfSMServer.SetonClientLostConnection(const Value
  : TOlfSMConnectedClientEvent);
begin
  FonClientLostConnection := Value;
end;

procedure TOlfSMServer.SetonDecodeReceivedMessage
  (const Value: TOlfSMEncodeDecodeMessageEvent);
begin
  FonDecodeReceivedMessage := Value;
end;

procedure TOlfSMServer.SetonEncodeMessageToSend(const Value
  : TOlfSMEncodeDecodeMessageEvent);
begin
  FonEncodeMessageToSend := Value;
end;

procedure TOlfSMServer.SetonServerConnected(const Value: TOlfSMServerEvent);
begin
  FonServerConnected := Value;
end;

procedure TOlfSMServer.SetonServerDisconnected(const Value: TOlfSMServerEvent);
begin
  FonServerDisconnected := Value;
end;

procedure TOlfSMServer.SetPort(const Value: word);
begin
  tmonitor.Enter(self);
  try
    FPort := Value;
  finally
    tmonitor.exit(self);
  end;
end;

procedure TOlfSMServer.SetSocket(const Value: TSocket);
begin
  tmonitor.Enter(self);
  try
    FSocket := Value;
  finally
    tmonitor.exit(self);
  end;
end;

procedure TOlfSMServer.SetThreadNameForDebugging(const Value: string);
begin
  tmonitor.Enter(self);
  try
    FThreadNameForDebugging := Value;
  finally
    tmonitor.exit(self);
  end;
end;

procedure TOlfSMServer.SubscribeToMessage(AMessageID: TOlfSMMessageID;
aReceivedMessageEvent: TOlfSMReceivedMessageEvent);
var
  sub: TOlfSubscribers;
  msgSub: TOlfMessageSubscribers;
  // found: boolean;
  // proc: TOlfSMReceivedMessageEvent;
begin
  if not assigned(aReceivedMessageEvent) then
    exit;

  sub := LockSubscribers;
  try
    if not sub.TryGetValue(AMessageID, msgSub) then
    begin
      msgSub := TOlfMessageSubscribers.Create;
      sub.Add(AMessageID, msgSub);
      msgSub.Add(aReceivedMessageEvent);
    end
    else if (msgSub.count < 1) then
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
  tmonitor.exit(FMessagesDict);
end;

procedure TOlfSMServer.UnlockSubscribers;
begin
  tmonitor.exit(FSubscribers);
end;

procedure TOlfSMServer.UnsubscribeToMessage(AMessageID: TOlfSMMessageID;
aReceivedMessageEvent: TOlfSMReceivedMessageEvent);
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
  msDecoded: TStream;
  MessageSize: TOlfSMMessageSize;
  MessageID: TOlfSMMessageID;
  ReceivedMessage: TOlfSMMessage;
begin
  if isConnected then
  begin
    if assigned(onConnected) then
      TThread.Synchronize(nil,
        procedure
        begin
          if assigned(onConnected) then
            onConnected(self);
        end);

    MessageSize := 0;
    ms := TMemoryStream.Create;
    try
      try
        while not TThread.CheckTerminated do
        begin
          if FErrorDuringSend then
            raise TOlfSocketMessagingException.Create
              ('Error detected during send.');
          tmonitor.Enter(FSocket);
          try
            RecCount := FSocket.Receive(Buffer);
          finally
            tmonitor.exit(FSocket);
          end;
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
                if assigned(FSocketServer) and
                  assigned(FSocketServer.onDecodeReceivedMessage) then
                  onDecodeReceivedMessage :=
                    FSocketServer.onDecodeReceivedMessage;
                if assigned(onDecodeReceivedMessage) then
                begin
                  msDecoded := onDecodeReceivedMessage(ms);
                  if not assigned(msDecoded) then
                    msDecoded := ms;
                end
                else
                  msDecoded := ms;
                msDecoded.Position := 0;
                msDecoded.Read(MessageID, sizeof(MessageID));
                ReceivedMessage := GetNewMessageInstance(MessageID);
                if assigned(ReceivedMessage) then
                  try
                    msDecoded.Position := 0;
                    ReceivedMessage.LoadFromStream(msDecoded);
                    DispatchReceivedMessage(ReceivedMessage);
                  finally
                    ReceivedMessage.free;
                  end
                else
                  raise TOlfSMException.Create('No message with ID ' +
                    MessageID.ToString);
                if (msDecoded <> ms) then
                  msDecoded.free;
                ms.Clear;
                MessageSize := 0;
              end;
            end
          else
            sleep(100);
        end;
      finally
        ms.free;
      end;
    except
      if assigned(onLostConnection) then
        TThread.Synchronize(nil,
          procedure
          begin
            if assigned(onLostConnection) then
              onLostConnection(self);
          end);
      raise;
    end;
    if assigned(onDisconnected) then
      TThread.Synchronize(nil,
        procedure
        begin
          if assigned(onDisconnected) then
            onDisconnected(self);
        end);
  end;
end;

procedure TOlfSMSrvConnectedClient.Connect;
begin
  // Do nothing here
end;

constructor TOlfSMSrvConnectedClient.Create;
begin
  inherited;
  FErrorDuringSend := false;
  FThread := nil;
  FSocket := nil;
  FSocketServer := nil;
  FonConnected := nil;
  FonDisconnected := nil;
  FonLostConnection := nil;
  FonDecodeReceivedMessage := nil;
  FonEncodeMessageToSend := nil;
end;

destructor TOlfSMSrvConnectedClient.Destroy;
begin
  if not FErrorDuringSend then
    TerminateAThreadAndWait(FThread);
  CloseFreeAndNilASocket(FSocket);
  inherited;
end;

procedure TOlfSMSrvConnectedClient.DispatchReceivedMessage
  (AMessage: TOlfSMMessage);
var
  Subscribers: TOlfSubscribers;
  MessageSubscribers: TOlfMessageSubscribers;
begin
  if not assigned(FSocketServer) then
    exit;

  Subscribers := FSocketServer.LockSubscribers;
  try
    if Subscribers.TryGetValue(AMessage.MessageID, MessageSubscribers) then
      tparallel.For(0, MessageSubscribers.count - 1,
        procedure(Index: integer)
        begin
          try
            MessageSubscribers[index](self, AMessage);
          except

          end;
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
    exit(nil);

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
    tmonitor.exit(self);
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
    tmonitor.exit(self);
  end;
end;

function TOlfSMSrvConnectedClient.isConnected: boolean;
begin
  Result := assigned(FSocket) and (TSocketState.Connected in FSocket.State);
end;

procedure TOlfSMSrvConnectedClient.SendMessage(Const AMessage: TOlfSMMessage);
var
  ms: TMemoryStream;
  msEncoded: TStream;
  MessageSize: TOlfSMMessageSize;
  ss: TSocketStream;
begin
  if not assigned(AMessage) then
    exit;

  if not assigned(FSocket) then
    exit;

  if not isConnected then
    exit;

  if FErrorDuringSend then
    exit;

  ms := TMemoryStream.Create;
  try
    AMessage.SaveToStream(ms);
    if assigned(FSocketServer) and assigned(FSocketServer.onEncodeMessageToSend)
    then
      onEncodeMessageToSend := FSocketServer.onEncodeMessageToSend;
    if assigned(onEncodeMessageToSend) then
    begin
      msEncoded := onEncodeMessageToSend(ms);
      if not assigned(msEncoded) then
        msEncoded := ms;
    end
    else
      msEncoded := ms;
    tmonitor.Enter(self);
    try
      ss := TSocketStream.Create(FSocket, false);
      try
        if (msEncoded.Size > high(TOlfSMMessageSize)) then
          raise exception.Create('Message too big (' + ms.Size.ToString + ').');
        MessageSize := msEncoded.Size;
        try
          tmonitor.Enter(FSocket);
          try
            FSocket.Send(MessageSize, sizeof(MessageSize));
          finally
            tmonitor.exit(FSocket);
          end;
        except
          FErrorDuringSend := true;
          raise;
        end;
        msEncoded.Position := 0;
        ss.CopyFrom(msEncoded);
      finally
        ss.free;
        if msEncoded <> ms then
          msEncoded.free;
      end;
    finally
      tmonitor.exit(self);
    end;
  finally
    ms.free;
  end;
end;

procedure TOlfSMSrvConnectedClient.SetonConnected
  (const Value: TOlfSMConnectedClientEvent);
begin
  FonConnected := Value;
end;

procedure TOlfSMSrvConnectedClient.SetonDecodeReceivedMessage
  (const Value: TOlfSMEncodeDecodeMessageEvent);
begin
  FonDecodeReceivedMessage := Value;
end;

procedure TOlfSMSrvConnectedClient.SetonDisconnected
  (const Value: TOlfSMConnectedClientEvent);
begin
  FonDisconnected := Value;
end;

procedure TOlfSMSrvConnectedClient.SetonEncodeMessageToSend
  (const Value: TOlfSMEncodeDecodeMessageEvent);
begin
  FonEncodeMessageToSend := Value;
end;

procedure TOlfSMSrvConnectedClient.SetonLostConnection
  (const Value: TOlfSMConnectedClientEvent);
begin
  FonLostConnection := Value;
end;

procedure TOlfSMSrvConnectedClient.SetSocket(const Value: TSocket);
begin
  tmonitor.Enter(self);
  try
    FSocket := Value;
  finally
    tmonitor.exit(self);
  end;
end;

procedure TOlfSMSrvConnectedClient.SetTag(const Value: nativeint);
begin
  FTag := Value;
end;

procedure TOlfSMSrvConnectedClient.SetTagBool(const Value: boolean);
begin
  FTagBool := Value;
end;

procedure TOlfSMSrvConnectedClient.SetTagFloat(const Value: single);
begin
  FTagFloat := Value;
end;

procedure TOlfSMSrvConnectedClient.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

procedure TOlfSMSrvConnectedClient.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

procedure TOlfSMSrvConnectedClient.SetThreadNameForDebugging
  (const Value: string);
begin
  tmonitor.Enter(self);
  try
    FThreadNameForDebugging := Value;
  finally
    tmonitor.exit(self);
  end;
end;

procedure TOlfSMSrvConnectedClient.StartClientLoop;
begin
  TerminateAThreadAndWait(FThread); // ne devrait pas exister, mais au cas où

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
    CloseFreeAndNilASocket(FSocket);

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
  FMessagesDict.free;
  FSubscribers.free;
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
      tparallel.For(0, MessageSubscribers.count - 1,
        procedure(Index: integer)
        begin
          try
            MessageSubscribers[index](self, AMessage);
          except

          end;
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
    tmonitor.exit(self);
  end;
end;

function TOlfSMClient.GeServerPort: word;
begin
  tmonitor.Enter(self);
  try
    Result := FServerPort;
  finally
    tmonitor.exit(self);
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
    tmonitor.exit(self);
  end;
end;

procedure TOlfSMClient.SetServerPort(const Value: word);
begin
  tmonitor.Enter(self);
  try
    FServerPort := Value;
  finally
    tmonitor.exit(self);
  end;
end;

procedure TOlfSMClient.SubscribeToMessage(AMessageID: TOlfSMMessageID;
aReceivedMessageEvent: TOlfSMReceivedMessageEvent);
var
  sub: TOlfSubscribers;
  msgSub: TOlfMessageSubscribers;
  // found: boolean;
  // proc: TOlfSMReceivedMessageEvent;
begin
  if not assigned(aReceivedMessageEvent) then
    exit;

  sub := LockSubscribers;
  try
    if not sub.TryGetValue(AMessageID, msgSub) then
    begin
      msgSub := TOlfMessageSubscribers.Create;
      sub.Add(AMessageID, msgSub);
      msgSub.Add(aReceivedMessageEvent);
    end
    else if (msgSub.count < 1) then
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
  tmonitor.exit(FMessagesDict);
end;

procedure TOlfSMClient.UnlockSubscribers;
begin
  tmonitor.exit(FSubscribers);
end;

procedure TOlfSMClient.UnsubscribeToMessage(AMessageID: TOlfSMMessageID;
aReceivedMessageEvent: TOlfSMReceivedMessageEvent);
begin
  // TODO : unsubscribe the listener
end;

{ TOlfSMMessage }

constructor TOlfSMMessage.Create;
begin
  inherited;
  FMessageID := 0;
end;

function TOlfSMMessage.GetNewInstance: TOlfSMMessage;
begin
  Result := TOlfSMMessage.Create;
end;

procedure TOlfSMMessage.LoadFromStream(Stream: TStream);
begin
  if not assigned(Stream) then
    exit;

  if (Stream.Read(FMessageID, sizeof(FMessageID)) <> sizeof(FMessageID)) then
    raise TOlfSMException.Create('');
end;

procedure TOlfSMMessage.SaveToStream(Stream: TStream);
begin
  if not assigned(Stream) then
    exit;

  Stream.Write(FMessageID, sizeof(FMessageID));
end;

procedure TOlfSMMessage.SetMessageID(const Value: TOlfSMMessageID);
begin
  FMessageID := Value;
end;

end.
