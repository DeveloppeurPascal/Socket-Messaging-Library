program TchatClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  fClientForm in 'fClientForm.pas' {Form1},
  SalonDeDiscussion in '..\SalonDeDiscussion.pas',
  Olf.Net.Socket.Messaging in '..\..\..\src\Olf.Net.Socket.Messaging.pas',
  Olf.RTL.Maths.Conversions in '..\..\..\lib-externes\librairies\src\Olf.RTL.Maths.Conversions.pas',
  Olf.RTL.Streams in '..\..\..\lib-externes\librairies\src\Olf.RTL.Streams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
