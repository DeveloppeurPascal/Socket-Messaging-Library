program MultiUserDraw;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  UMultiUserDrawMessages in '..\UMultiUserDrawMessages.pas',
  Olf.Net.Socket.Messaging in '..\..\..\src\Olf.Net.Socket.Messaging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
