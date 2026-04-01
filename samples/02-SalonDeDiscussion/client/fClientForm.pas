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
  File last update : 2025-05-26T15:45:34.793+02:00
  Signature : 3eb4bd70c589c21be63b179699305710c9f1d583
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
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Olf.Net.Socket.Messaging;

type
  TClient = class(tolfsmclient)
  private
  protected
    procedure onReceiveMessage(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);

    procedure AddLog(Const Txt: string);
  public
    constructor Create(AServerIP: string; AServerPort: Word); override;
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    btnPseudoChange: TButton;
    edtMessage: TEdit;
    btnSendInPublic: TButton;
    edtFromPseudo: TEdit;
    Memo1: TMemo;
    edtToPseudo: TEdit;
    btnSendInPrivate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnPseudoChangeClick(Sender: TObject);
    procedure btnSendInPublicClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSendInPrivateClick(Sender: TObject);
  private
    { Déclarations privées }
    FClient: TClient;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses SalonDeDiscussion;

procedure TForm1.btnPseudoChangeClick(Sender: TObject);
var
  msg: TIdentificationDUnUtilisateurMessage;
begin
  if not FClient.isConnected then
    FClient.Connect;

  msg := TIdentificationDUnUtilisateurMessage.Create;
  try
    msg.Pseudo := edtFromPseudo.Text;
    FClient.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TForm1.btnSendInPrivateClick(Sender: TObject);
var
  msg: TEnvoiDUnMessageAQuelquUnMessage;
begin
  if not FClient.isConnected then
    FClient.Connect;

  msg := TEnvoiDUnMessageAQuelquUnMessage.Create;
  try
    msg.Destinataire := edtToPseudo.Text;
    msg.Texte := edtMessage.Text;
    FClient.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TForm1.btnSendInPublicClick(Sender: TObject);
var
  msg: TEnvoiDUnMessageATousMessage;
begin
  if not FClient.isConnected then
    FClient.Connect;

  msg := TEnvoiDUnMessageATousMessage.Create;
  try
    msg.Emetteur := edtFromPseudo.Text;
    msg.Texte := edtMessage.Text;
    FClient.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FClient := TClient.Create('127.0.0.1', 8080);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FClient.Free;
end;

{ TClient }

procedure TClient.AddLog(const Txt: string);
begin
  tthread.Synchronize(nil,
    procedure
    begin
      Form1.Memo1.Lines.Insert(0, Txt)
    end);
end;

constructor TClient.Create(AServerIP: string; AServerPort: Word);
begin
  inherited;
  RegisterMessagesReceivedByTheClient(self);

  SubscribeToMessage(4, onReceiveMessage);
end;

destructor TClient.Destroy;
begin

  inherited;
end;

procedure TClient.onReceiveMessage(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TTransmissionDUnMessageRecuMessage;
begin
  if not(AMessage is TTransmissionDUnMessageRecuMessage) then
    exit;
  msg := AMessage as TTransmissionDUnMessageRecuMessage;

  AddLog('From : ' + msg.Emetteur + slinebreak + msg.Texte);
end;

end.
