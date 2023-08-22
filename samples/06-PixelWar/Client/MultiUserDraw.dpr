program MultiUserDraw;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  Olf.Net.Socket.Messaging in '..\Olf.Net.Socket.Messaging.pas',
  UMultiUserDrawMessages in '..\UMultiUserDrawMessages.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
