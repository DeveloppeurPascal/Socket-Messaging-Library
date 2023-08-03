program TchatClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  fClientForm in 'fClientForm.pas' {Form1},
  SalonDeDiscussion in '..\SalonDeDiscussion.pas',
  Olf.RTL.Streams in '..\..\..\lib-externes\librairies\Olf.RTL.Streams.pas',
  Olf.Net.Socket.Messaging in '..\..\..\src-library\Olf.Net.Socket.Messaging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
