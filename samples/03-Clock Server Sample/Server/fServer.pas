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
/// Signature : 04c103b039d922525169e254e936bfe61e2d0d55
/// ***************************************************************************
/// </summary>

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
