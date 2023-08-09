program ClockServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  fServer in 'fServer.pas' {Form1},
  ClockSampleUnit in '..\ClockSampleUnit.pas',
  Olf.Net.Socket.Messaging in '..\..\..\src\Olf.Net.Socket.Messaging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
