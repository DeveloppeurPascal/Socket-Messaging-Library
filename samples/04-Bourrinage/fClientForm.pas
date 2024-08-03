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
/// Signature : ac7ea7ca0f909a666407d1d2e76c577372767767
/// ***************************************************************************
/// </summary>

unit fClientForm;

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
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  Olf.Net.Socket.Messaging,
  UBourrinageMessages,
  System.Generics.Collections, FMX.StdCtrls, FMX.Layouts;

type
  TBourrinageClientsList = TObjectList<TBourrinageClient>;

  TfrmClients = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    GridPanelLayout1: TGridPanelLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSentMessages: integer;
    FCounter: integer;
    FReceivedMessages: integer;
    procedure SetCounter(const Value: integer);
    procedure SetReceivedMessages(const Value: integer);
    procedure SetSentMessages(const Value: integer);
  protected
    ClientsList: TBourrinageClientsList;
    property Counter: integer read FCounter write SetCounter;
    property SentMessages: integer read FSentMessages write SetSentMessages;
    property ReceivedMessages: integer read FReceivedMessages
      write SetReceivedMessages;
    procedure DoReceiveBourrinServerToClientMessage(Const ASender
      : TOlfSMSrvConnectedClient;
      Const AMessage: TBourrinServerToClientMessage);
  public
  end;

implementation

{$R *.fmx}

uses
  System.Threading;

{ TfrmClients }

procedure TfrmClients.DoReceiveBourrinServerToClientMessage
  (const ASender: TOlfSMSrvConnectedClient;
  const AMessage: TBourrinServerToClientMessage);
var
  msg: TBourrinClientToServerMessage;
begin
  tthread.Synchronize(nil,
    procedure
    begin
      ReceivedMessages := ReceivedMessages + 1;

      if Memo1.Lines.Count > 512 then
        Memo1.Lines.Clear;
      Memo1.Lines.Add('Received : ' + AMessage.RandomNumber.ToString);
      Memo1.GoToTextEnd;
    end);

  // msg := TBourrinClientToServerMessage.Create;
  // try
  // msg.RandomNumber := AMessage.RandomNumber;
  // ASender.SendMessage(msg);
  // finally
  // msg.Free;
  // end;
  // SentMessages := SentMessages + 1;
end;

procedure TfrmClients.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  application.Terminate;
end;

procedure TfrmClients.FormCreate(Sender: TObject);
begin
  Timer1.Interval := random(100) + 50;
  Timer1.Enabled := true;

  Counter := 0;
  SentMessages := 0;
  ReceivedMessages := 0;

  ClientsList := TBourrinageClientsList.Create;

  tthread.ForceQueue(nil,
    procedure
    var
      i, nb: integer;
    begin
      nb := random(50) + 2;
      for i := 1 to nb do
        tthread.CreateAnonymousThread(
          procedure
          var
            client: TBourrinageClient;
          begin
            sleep(random(1000));
            client := TBourrinageClient.Create;
            client.onReceiveBourrinServerToClientMessage :=
              DoReceiveBourrinServerToClientMessage;
            client.Connect('127.0.0.1', 8080);

            System.TMonitor.Enter(ClientsList);
            try
              ClientsList.Add(client);
            finally
              System.TMonitor.exit(ClientsList);
            end;

            Counter := Counter + 1;
          end).Start;
    end);
end;

procedure TfrmClients.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := false;
  ClientsList.Free;
end;

procedure TfrmClients.SetCounter(const Value: integer);
begin
  FCounter := Value;
  tthread.Synchronize(nil,
    procedure
    begin
      Label1.text := FCounter.ToString;
    end);
end;

procedure TfrmClients.SetReceivedMessages(const Value: integer);
begin
  FReceivedMessages := Value;
  tthread.Synchronize(nil,
    procedure
    begin
      Label3.text := FReceivedMessages.ToString;
    end);
end;

procedure TfrmClients.SetSentMessages(const Value: integer);
begin
  FSentMessages := Value;
  tthread.Synchronize(nil,
    procedure
    begin
      Label2.text := FSentMessages.ToString;
    end);
end;

procedure TfrmClients.Timer1Timer(Sender: TObject);
var
  msg: TBourrinClientToServerMessage;
begin
  if not Timer1.Enabled then
    exit;
  if not assigned(ClientsList) then
    exit;
  if (ClientsList.Count < 1) then
    exit;

  msg := TBourrinClientToServerMessage.Create;
  try
    msg.RandomNumber := random(high(integer));
    ClientsList[random(ClientsList.Count)].SendMessage(msg);
  finally
    msg.Free;
  end;
  SentMessages := SentMessages + 1;
end;

initialization

randomize;

end.
