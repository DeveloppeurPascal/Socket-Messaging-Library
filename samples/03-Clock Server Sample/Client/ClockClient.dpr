program ClockClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  fClient in 'fClient.pas' {Form2},
  ClockSampleUnit in '..\ClockSampleUnit.pas',
  Olf.Net.Socket.Messaging in '..\..\..\src\Olf.Net.Socket.Messaging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
