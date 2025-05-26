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
  Signature : d5f9449d360429b0019b19801c314ecb6ebdb3b9
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Layouts, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.Edit, Olf.Net.Socket.Messaging, UDialMessages;

type
  TForm1 = class(TForm)
    edtPseudo: TEdit;
    edtTexte: TEdit;
    Memo1: TMemo;
    btnSend: TButton;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    { Déclarations privées }
  protected
    Client: TDialClient;
    procedure DoReceiveBroadcastMessage(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TBroadcastMessage);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{ TForm1 }

procedure TForm1.btnSendClick(Sender: TObject);
var
  msg: TSendMessage;
begin
  if not assigned(Client) then
    exit;

  if not Client.isConnected then
    exit;

  msg := TSendMessage.Create;
  try
    msg.Pseudo := edtPseudo.Text;
    msg.Texte := edtTexte.Text;
    Client.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TForm1.DoReceiveBroadcastMessage(const ASender
  : TOlfSMSrvConnectedClient; const AMessage: TBroadcastMessage);
begin
  tthread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(datetimetostr(AMessage.DateTime) + slinebreak +
        AMessage.Pseudo + ' : ' + AMessage.Texte);
    end);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Client := TDialClient.Create('127.0.0.1', 8080);
  Client.onReceiveBroadcastMessage := DoReceiveBroadcastMessage;
  Client.Connect('127.0.0.1', 8080);
end;

end.
