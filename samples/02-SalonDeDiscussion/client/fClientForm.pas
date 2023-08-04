unit fClientForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Olf.Net.Socket.Messaging;

type
  TClient = class(tolfsmclient)
  private
  protected
    procedure onReceiveMessage(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);

    procedure AddLog(Const Txt: string);
  public
    constructor Create(AServerIP: string; AServerPort: Word); override;
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    btnPseudoChange: TButton;
    edtMessage: TEdit;
    btnSendInPublic: TButton;
    edtFromPseudo: TEdit;
    Memo1: TMemo;
    edtToPseudo: TEdit;
    btnSendInPrivate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnPseudoChangeClick(Sender: TObject);
    procedure btnSendInPublicClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSendInPrivateClick(Sender: TObject);
  private
    { Déclarations privées }
    FClient: TClient;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses SalonDeDiscussion;

procedure TForm1.btnPseudoChangeClick(Sender: TObject);
var
  msg: TIdentificationDUnUtilisateurMessage;
begin
  if not FClient.isConnected then
    FClient.Connect;

  msg := TIdentificationDUnUtilisateurMessage.Create;
  try
    msg.Pseudo := edtFromPseudo.Text;
    FClient.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TForm1.btnSendInPrivateClick(Sender: TObject);
var
  msg: TEnvoiDUnMessageAQuelquUnMessage;
begin
  if not FClient.isConnected then
    FClient.Connect;

  msg := TEnvoiDUnMessageAQuelquUnMessage.Create;
  try
    msg.Destinataire := edtToPseudo.Text;
    msg.Texte := edtMessage.Text;
    FClient.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TForm1.btnSendInPublicClick(Sender: TObject);
var
  msg: TEnvoiDUnMessageATousMessage;
begin
  if not FClient.isConnected then
    FClient.Connect;

  msg := TEnvoiDUnMessageATousMessage.Create;
  try
    msg.Emetteur := edtFromPseudo.Text;
    msg.Texte := edtMessage.Text;
    FClient.SendMessage(msg);
  finally
    msg.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FClient := TClient.Create('127.0.0.1', 8080);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FClient.Free;
end;

{ TClient }

procedure TClient.AddLog(const Txt: string);
begin
  tthread.Synchronize(nil,
    procedure
    begin
      Form1.Memo1.Lines.Insert(0, Txt)
    end);
end;

constructor TClient.Create(AServerIP: string; AServerPort: Word);
begin
  inherited;
  RegisterMessagesReceivedByTheClient(self);

  SubscribeToMessage(4, onReceiveMessage);
end;

destructor TClient.Destroy;
begin

  inherited;
end;

procedure TClient.onReceiveMessage(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TTransmissionDUnMessageRecuMessage;
begin
  if not(AMessage is TTransmissionDUnMessageRecuMessage) then
    exit;
  msg := AMessage as TTransmissionDUnMessageRecuMessage;

  AddLog('From : ' + msg.Emetteur + slinebreak + msg.Texte);
end;

end.
