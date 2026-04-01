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
  File last update : 2025-05-26T15:45:34.820+02:00
  Signature : 7078ea48c1138ad0cec35887a5f3d31afd2cbd26
  ***************************************************************************
*)

unit fServer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  ClockSampleUnit,
  Olf.Net.Socket.Messaging, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    FClientCount: integer;
    procedure DoReceiveACKDateTimeMessage(Const ASender
      : TOlfSMSrvConnectedClient; Const AMessage: TACKDateTimeMessage);
    procedure DoClientConnected(Const AConnectedClient
      : TOlfSMSrvConnectedClient);
  public
    { Déclarations publiques }
    MyClockServer: TClockSampleServer;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.DoClientConnected(const AConnectedClient
  : TOlfSMSrvConnectedClient);
begin
  tthread.Synchronize(nil,
    procedure
    begin
      inc(FClientCount);
      AConnectedClient.TagString := 'Client' + FClientCount.ToString;
      Memo1.Lines.Add('Hello ' + AConnectedClient.TagString);
    end);
end;

procedure TForm1.DoReceiveACKDateTimeMessage(const ASender
  : TOlfSMSrvConnectedClient; const AMessage: TACKDateTimeMessage);
begin
  tthread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(ASender.TagString + ' - ' + AMessage.Counter.ToString);
    end);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FClientCount := 0;
  MyClockServer := TClockSampleServer.Create;
  MyClockServer.onReceiveACKDateTimeMessage := DoReceiveACKDateTimeMessage;
  MyClockServer.onClientConnected := DoClientConnected;
  MyClockServer.Listen('0.0.0.0', 8080);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MyClockServer);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  msg: TCurrentDateAndTimeMessage;
  dt: TDateTime;
begin
  if assigned(MyClockServer) and MyClockServer.isListening then
  begin
    msg := TCurrentDateAndTimeMessage.Create;
    try
      dt := now;
      msg.Date := datetostr(dt);
      msg.Time := timetostr(dt);
      MyClockServer.SendMessageToAll(msg);
    finally
      msg.Free;
    end;
  end;
end;

initialization

ReportMemoryLeaksOnShutdown := true;

end.
