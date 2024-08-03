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
/// Signature : ee658abbe55edf1145824b7e540da70714dd269d
/// ***************************************************************************
/// </summary>

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
