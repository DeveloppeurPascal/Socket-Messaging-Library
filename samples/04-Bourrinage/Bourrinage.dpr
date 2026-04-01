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
  File last update : 2025-05-26T15:45:36.000+02:00
  Signature : c763cb3be81b2ef1b7aec065ec87883794bf39de
  ***************************************************************************
*)

program Bourrinage;

uses
  System.StartUpCopy,
  FMX.Forms,
  fServerForm in 'fServerForm.pas' {frmServer},
  fClientForm in 'fClientForm.pas' {frmClients},
  UBourrinageMessages in 'UBourrinageMessages.pas',
  Olf.Net.Socket.Messaging in '..\..\src\Olf.Net.Socket.Messaging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmServer, frmServer);
  Application.Run;
end.
