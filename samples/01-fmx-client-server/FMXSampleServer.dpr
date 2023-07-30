program FMXSampleServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  fServerForm in 'fServerForm.pas' {frmServer},
  Olf.Net.Socket.Messaging in '..\..\src-library\Olf.Net.Socket.Messaging.pas',
  uTestMessages in 'uTestMessages.pas',
  Olf.RTL.Streams in '..\..\lib-externes\librairies\Olf.RTL.Streams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmServer, frmServer);
  Application.Run;
end.
