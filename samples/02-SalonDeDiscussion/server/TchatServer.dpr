program TchatServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  fServerForm in 'fServerForm.pas' {Form2},
  SalonDeDiscussion in '..\SalonDeDiscussion.pas',
  Olf.RTL.Streams in '..\..\..\lib-externes\librairies\Olf.RTL.Streams.pas',
  Olf.Net.Socket.Messaging in '..\..\..\src-library\Olf.Net.Socket.Messaging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
