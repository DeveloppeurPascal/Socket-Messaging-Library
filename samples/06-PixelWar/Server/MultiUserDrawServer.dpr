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
/// Signature : 37247ace6e338d0d5cc36bde7cf8a31fa6e1596f
/// ***************************************************************************
/// </summary>

program MultiUserDrawServer;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF LINUX}
  Posix.Stdlib,
  Posix.SysStat,
  Posix.SysTypes,
  Posix.Unistd,
  Posix.Signal,
  Posix.Fcntl,
  {$ENDIF }
  System.SysUtils,
  UMultiUserDrawMessages in '..\UMultiUserDrawMessages.pas',
  Olf.Net.Socket.Messaging in '..\..\..\src\Olf.Net.Socket.Messaging.pas';

type
  TMyServer = class(TMultiUserDrawServer)
  public
    procedure doReceiveMUDAddAPixelMessage(Const ASender
      : TOlfSMSrvConnectedClient; Const AMessage: TMUDAddAPixelMessage);
  end;

  { TMyServer }

procedure TMyServer.doReceiveMUDAddAPixelMessage(const ASender
  : TOlfSMSrvConnectedClient; const AMessage: TMUDAddAPixelMessage);
var
  msg: TMUDChangePixelColorFromAnOtherUserMessage;
begin
  msg := TMUDChangePixelColorFromAnOtherUserMessage.Create;
  try
    msg.Color := AMessage.Color;
    msg.X := AMessage.X;
    msg.y := AMessage.y;
    try
      SendMessageToAll(msg, ASender);
    except
    end;
  finally
    msg.Free;
  end;
end;

procedure RunServer(port: integer);
var
  Server: TMyServer;
begin
  Writeln('Server started');
  Server := TMyServer.Create('0.0.0.0', port);
  try
    Server.onReceiveMUDAddAPixelMessage := Server.doReceiveMUDAddAPixelMessage;
    Server.Listen;
  finally
    Writeln('Server ended');
  end;
end;

{$IFDEF LINUX}

const
  // Missing from linux/StdlibTypes.inc !!! <stdlib.h>
  EXIT_FAILURE = 1;
  EXIT_SUCCESS = 0;

var
  pid: pid_t;
  fid: integer;
  idx: integer;
  running: Boolean;

procedure HandleSignals(SigNum: integer); cdecl;
begin
  case SigNum of
    SIGTERM:
      begin
        running := False;
      end;
    SIGHUP:
      begin
        // syslog(LOG_NOTICE, 'daemon: reloading config');
        // Reload configuration
      end;
  end;
end;
{$ENDIF}

var
  port: integer;
  paramvalue: string;

begin
  if findcmdlineswitch('h') then
  begin
    Writeln('MultiUserDrawServer');
    Writeln('(c) 2023 Patrick Prémartin');
    Writeln('');
    Writeln('See https://github.com/DeveloppeurPascal/DCB2023-Socket-Messaging-library-and-generator for informations about this program.');
    Writeln('');
    Writeln('-h => display this help');
    Writeln('-port number => port number to listen (8081 by default)');
{$IFDEF LINUX}
    Writeln('-daemon => start the server as Linux daemon');
{$ENDIF}
  end
  else
  begin
    if findcmdlineswitch('port', paramvalue, True, [clstValueNextParam]) then
      port := paramvalue.ToInteger
    else
      port := 8081;

{$IFDEF LINUX}
    // Want to understand how to create a Linux daemon ?
    // Look at Paolo Rossi blog :
    // https://blog.paolorossi.net/building-a-real-linux-daemon-with-delphi-part-1/
    // https://blog.paolorossi.net/building-a-real-linux-daemon-with-delphi-part-2/
    if findcmdlineswitch('daemon') then
    begin
      // openlog(nil, LOG_PID or LOG_NDELAY, LOG_DAEMON);
      try
        if getppid() > 1 then
        begin
          pid := fork();
          if pid < 0 then
            raise exception.Create('Error forking the process');

          if pid > 0 then
            Halt(EXIT_SUCCESS);

          if setsid() < 0 then
            raise exception.Create
              ('Impossible to create an independent session');

          Signal(SIGCHLD, TSignalHandler(SIG_IGN));
          Signal(SIGHUP, HandleSignals);
          Signal(SIGTERM, HandleSignals);

          pid := fork();
          if pid < 0 then
            raise exception.Create('Error forking the process');

          if pid > 0 then
            Halt(EXIT_SUCCESS);

          for idx := sysconf(_SC_OPEN_MAX) downto 0 do
            __close(idx);

          fid := __open('/dev/null', O_RDWR);
          dup(fid);
          dup(fid);

          umask(027);

          chdir('/');
        end;

        running := True;

        RunServer(port);

        ExitCode := EXIT_SUCCESS;
      except
        on E: exception do
        begin
          // syslog(LOG_ERR, 'Error: ' + E.Message);
          ExitCode := EXIT_FAILURE;
        end;
      end;

      // syslog(LOG_NOTICE, 'daemon stopped');
      // closelog();
    end
    else
{$ENDIF}
      try
        RunServer(port);
      except
        on E: exception do
          Writeln(E.ClassName, ': ', E.Message);
      end
  end;

end.
