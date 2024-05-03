unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  UMultiUserDrawMessages, Olf.Net.Socket.Messaging, FMX.Layouts, FMX.Objects,
  FMX.Colors;

Const
  CPixelSize = 20;

type
  TForm1 = class(TForm)
    ScaledLayout1: TScaledLayout;
    ComboColorBox1: TComboColorBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ComboColorBox1Change(Sender: TObject);
  private
    { Déclarations privées }
    procedure ClickOnCell(Sender: TObject);
    procedure FillDrawZone;
  public
    { Déclarations publiques }
    Client: TMultiUserDrawClient;
    CurrentColor: TAlphaColor;
    procedure DoReceivePixel(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TMUDChangePixelColorFromAnOtherUserMessage);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.ClickOnCell(Sender: TObject);
var
  r: trectangle;
  c: TAlphaColor;
begin
  if not(Sender is trectangle) then
    exit;

  r := (Sender as trectangle);

  r.Fill.Color := CurrentColor;
  c := CurrentColor;

  tthread.CreateAnonymousThread(
    procedure
    var
      msg: TMUDAddAPixelMessage;
    begin
      msg := TMUDAddAPixelMessage.Create;
      try
        msg.Color := c;
        msg.x := r.Tag;
        msg.Y := trunc(r.TagFloat);
        Client.SendMessage(msg);
      finally
        msg.Free;
      end;
    end).Start;
end;

procedure TForm1.ComboColorBox1Change(Sender: TObject);
begin
  CurrentColor := ComboColorBox1.Color;
end;

procedure TForm1.DoReceivePixel(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TMUDChangePixelColorFromAnOtherUserMessage);
var
  r: trectangle;
  i: integer;
begin
  for i := 0 to ScaledLayout1.ChildrenCount - 1 do
    if (ScaledLayout1.Children[i] is trectangle) then
    begin
      r := ScaledLayout1.Children[i] as trectangle;
      if (r.Tag = AMessage.x) and (r.TagFloat = AMessage.Y) then
        r.Fill.Color := AMessage.Color;
    end;
end;

procedure TForm1.FillDrawZone;
var
  x, Y: integer;
  r: trectangle;
begin
  for x := 0 to (trunc(ScaledLayout1.OriginalWidth) div CPixelSize) - 1 do
    for Y := 0 to (trunc(ScaledLayout1.Originalheight) div CPixelSize) - 1 do
    begin
      r := trectangle.Create(self);
      r.Parent := ScaledLayout1;
      r.Width := CPixelSize;
      r.height := CPixelSize;
      r.Position.x := x * CPixelSize;
      r.Position.Y := Y * CPixelSize;
      r.Tag := x;
      r.TagFloat := Y;
      r.Stroke.Kind := TBrushKind.None;
      r.OnClick := ClickOnCell;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF RELEASE}
  Client := TMultiUserDrawClient.Create('92.222.216.2', 8081);
  {$ELSE}
  Client := TMultiUserDrawClient.Create('127.0.0.1', 8081);
  {$ENDIF}
  Client.onReceiveMUDChangePixelColorFromAnOtherUserMessage := DoReceivePixel;
  Client.Connect;

  FillDrawZone;

  CurrentColor := TAlphaColors.Red;
  ComboColorBox1.Color := CurrentColor;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Client.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if ClientWidth > Clientheight then
  begin
    ScaledLayout1.Width := Clientheight - 20;
    ScaledLayout1.height := Clientheight - 20;
  end
  else
  begin
    ScaledLayout1.Width := ClientWidth - 20;
    ScaledLayout1.height := ClientWidth - 20;
  end;
end;

end.
