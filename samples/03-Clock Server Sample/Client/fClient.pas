unit fClient;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  ClockSampleUnit,
  Olf.Net.Socket.Messaging, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    fcounter: integer;
    procedure DoReceiveCurrentDateAndTimeMessage(Const ASender
      : TOlfSMSrvConnectedClient; Const AMessage: TCurrentDateAndTimeMessage);
  public
    { Déclarations publiques }
    MyClient: TClockSampleClient;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.DoReceiveCurrentDateAndTimeMessage(const ASender
  : TOlfSMSrvConnectedClient; const AMessage: TCurrentDateAndTimeMessage);
var
  msg: TACKDateTimeMessage;
begin
  tthread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(AMessage.Date + ' ' + AMessage.Time);
    end);
  msg := TACKDateTimeMessage.Create;
  try
    inc(fcounter);
    msg.Counter := fcounter;
    ASender.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  fcounter := 0;
  MyClient := TClockSampleClient.Create;
  MyClient.onReceiveCurrentDateAndTimeMessage :=
    DoReceiveCurrentDateAndTimeMessage;
  MyClient.Connect('127.0.0.1', 8080);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  MyClient.Free;
end;

end.
