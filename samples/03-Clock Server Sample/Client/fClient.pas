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
  File last update : 2025-05-26T15:45:34.819+02:00
  Signature : 2c7120ccdfc7a31ff99b3910f4d277738d73c3bd
  ***************************************************************************
*)

unit fClient;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  ClockSampleUnit,
  Olf.Net.Socket.Messaging, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    fcounter: integer;
    procedure DoReceiveCurrentDateAndTimeMessage(Const ASender
      : TOlfSMSrvConnectedClient; Const AMessage: TCurrentDateAndTimeMessage);
  public
    { Déclarations publiques }
    MyClient: TClockSampleClient;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.DoReceiveCurrentDateAndTimeMessage(const ASender
  : TOlfSMSrvConnectedClient; const AMessage: TCurrentDateAndTimeMessage);
var
  msg: TACKDateTimeMessage;
begin
  tthread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(AMessage.Date + ' ' + AMessage.Time);
    end);
  msg := TACKDateTimeMessage.Create;
  try
    inc(fcounter);
    msg.Counter := fcounter;
    ASender.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  fcounter := 0;
  MyClient := TClockSampleClient.Create;
  MyClient.onReceiveCurrentDateAndTimeMessage :=
    DoReceiveCurrentDateAndTimeMessage;
  try
    MyClient.Connect('127.0.0.1', 8080);
  except
    ShowMessage('Connexion avec le serveur imposible');
  end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  MyClient.Free;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
