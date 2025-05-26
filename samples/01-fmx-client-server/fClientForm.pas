(* C2PP
  ***************************************************************************

  Socket Messaging Library

  Copyright 2023-2025 Patrick Pr�martin under AGPL 3.0 license.

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
  File last update : 2025-02-09T11:04:06.327+01:00
  Signature : 73cdd6b4bfd518694cbfb8327964e60b1ab55ebc
  ***************************************************************************
*)

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
  Olf.Net.Socket.Messaging,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo;

type
  TfrmClient = class(TForm)
    pnlConnect: TPanel;
    edtIP: TEdit;
    edtPort: TEdit;
    btnConnectToTheServer: TButton;
    Edit1: TEdit;
    SendTextToServer: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectToTheServerClick(Sender: TObject);
    procedure SendTextToServerClick(Sender: TObject);
  private
    { D�clarations priv�es }
    ClientThread: TOlfSocketMessagingClient;
    procedure onReceivedMessage1(Const ASender
      : TOlfSocketMessagingServerConnectedClient;
      Const AMessage: TOlfSocketMessage);
    procedure onReceivedMessage2(Const ASender
      : TOlfSocketMessagingServerConnectedClient;
      Const AMessage: TOlfSocketMessage);
  public
    { D�clarations publiques }
  end;

var
  frmClient: TfrmClient;

implementation

{$R *.fmx}

uses
  uTestMessages;

procedure TfrmClient.btnConnectToTheServerClick(Sender: TObject);
begin
  ClientThread.Connect(edtIP.Text, edtPort.Text.ToInteger);
  pnlConnect.Enabled := false;
end;

procedure TfrmClient.FormCreate(Sender: TObject);
begin
  ClientThread := TOlfSocketMessagingClient.Create;
  RegisterMessages(ClientThread);
  ClientThread.SubscribeToMessage(1, onReceivedMessage1);
  ClientThread.SubscribeToMessage(2, onReceivedMessage2);
end;

procedure TfrmClient.FormDestroy(Sender: TObject);
begin
  ClientThread.Free;
end;

procedure TfrmClient.onReceivedMessage1(const ASender
  : TOlfSocketMessagingServerConnectedClient;
  const AMessage: TOlfSocketMessage);
begin
  if not(AMessage is TMessage1) then
    raise exception.Create('Wrong received message !');

  Memo1.Lines.Add((AMessage as TMessage1).Texte);
end;

procedure TfrmClient.onReceivedMessage2(const ASender
  : TOlfSocketMessagingServerConnectedClient;
  const AMessage: TOlfSocketMessage);
var
  msg: tmessage2;
begin
  if not(AMessage is tmessage2) then
    raise exception.Create('Wrong received message !');

  msg := AMessage as tmessage2;
  Memo1.Lines.Add(msg.x.tostring + ' / ' + msg.y.tostring + ' / ' +
    msg.z.tostring);

  ClientThread.SendMessage(msg);
end;

procedure TfrmClient.SendTextToServerClick(Sender: TObject);
var
  msg: TMessage1;
begin
  msg := TMessage1.Create;
  try
    msg.Texte := Edit1.Text;
    ClientThread.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

finalization

end.
