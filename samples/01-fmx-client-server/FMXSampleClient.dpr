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
  Signature : 5ad1a0626e9c3e87e9645a3e741b18abb9f03491
  ***************************************************************************
*)

program FMXSampleClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  fClientForm in 'fClientForm.pas' {frmClient},
  uTestMessages in 'uTestMessages.pas',
  Olf.Net.Socket.Messaging in '..\..\src\Olf.Net.Socket.Messaging.pas',
  Olf.RTL.Maths.Conversions in '..\..\lib-externes\librairies\src\Olf.RTL.Maths.Conversions.pas',
  Olf.RTL.Streams in '..\..\lib-externes\librairies\src\Olf.RTL.Streams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClient, frmClient);
  Application.Run;
end.
