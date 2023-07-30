program FMXSampleClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  fClientForm in 'fClientForm.pas' {frmClient},
  uTestMessages in 'uTestMessages.pas',
  Olf.Net.Socket.Messaging in '..\..\src-library\Olf.Net.Socket.Messaging.pas',
  Olf.RTL.Streams in '..\..\lib-externes\librairies\Olf.RTL.Streams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClient, frmClient);
  Application.Run;
end.
