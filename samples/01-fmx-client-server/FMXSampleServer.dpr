program FMXSampleServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  fServerForm in 'fServerForm.pas' {frmServer},
  uTestMessages in 'uTestMessages.pas',
  Olf.Net.Socket.Messaging in '..\..\src\Olf.Net.Socket.Messaging.pas',
  Olf.RTL.Streams in '..\..\lib-externes\librairies\Olf.RTL.Streams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmServer, frmServer);
  Application.Run;
end.
