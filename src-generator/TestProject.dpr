program TestProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  fTestProject in 'fTestProject.pas' {Form2},
  uProject in 'uProject.pas',
  Olf.Net.Socket.Messaging in '..\src-library\Olf.Net.Socket.Messaging.pas',
  Olf.RTL.Streams in '..\lib-externes\librairies\Olf.RTL.Streams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
