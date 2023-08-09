unit fServerForm;

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
  Olf.Net.Socket.Messaging,
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo, SalonDeDiscussion;

type
  TServer = class(tolfsmserver)
  private
  protected
    procedure onClientIdentifie(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
    procedure onPublicMessage(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);
    procedure onPrivateMessage(Const ASender: TOlfSMSrvConnectedClient;
      Const AMessage: TOlfSMMessage);

    procedure ClientConnected(Const AClient: TOlfSMSrvConnectedClient);
    procedure ClientLostConnexion(Const AClient: TOlfSMSrvConnectedClient);
    procedure ClientDisconnected(Const AClient: TOlfSMSrvConnectedClient);

    procedure AddLog(Const Txt: string);
  public
    onMessage1
      : TOlfSMReceivedMessageEvent<TIdentificationDUnUtilisateurMessage>;
    constructor Create(AIP: string; APort: Word); override;
    destructor Destroy; override;
  end;

  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
begin
  TServer.Create('0.0.0.0', 8080).Listen;
end;

{ TServer }

procedure TServer.AddLog(const Txt: string);
begin
  tthread.Synchronize(nil,
    procedure
    begin
      Form2.Memo1.Lines.Insert(0, Txt)
    end);
end;

procedure TServer.ClientConnected(const AClient: TOlfSMSrvConnectedClient);
begin
  AddLog('New user');
end;

procedure TServer.ClientDisconnected(const AClient: TOlfSMSrvConnectedClient);
begin
  AddLog('Bye ' + AClient.TagString);
end;

procedure TServer.ClientLostConnexion(const AClient: TOlfSMSrvConnectedClient);
begin
  AddLog('Lost ' + AClient.TagString);
end;

constructor TServer.Create(AIP: string; APort: Word);
begin
  inherited;
  RegisterMessagesReceivedByTheServer(self);

  SubscribeToMessage(1, onClientIdentifie);
  SubscribeToMessage(2, onPublicMessage);
  SubscribeToMessage(3, onPrivateMessage);

  onClientConnected := ClientConnected;
  onClientLostConnection := ClientLostConnexion;
  onClientDisconnected := ClientDisconnected;
end;

destructor TServer.Destroy;
begin

  inherited;
end;

procedure TServer.onClientIdentifie(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TIdentificationDUnUtilisateurMessage;
begin
  if not(AMessage is TIdentificationDUnUtilisateurMessage) then
    exit;
  msg := AMessage as TIdentificationDUnUtilisateurMessage;

  AddLog(msg.Pseudo + ' connected');
  ASender.TagString := msg.Pseudo;
  if not assigned(onMessage1) then
    exit;
  onMessage1(ASender, AMessage as TIdentificationDUnUtilisateurMessage);
end;

procedure TServer.onPrivateMessage(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TEnvoiDUnMessageAQuelquUnMessage;
  msg2: TTransmissionDUnMessageRecuMessage;
  cli: TOlfSMSrvConnectedClient;
begin
  if not(AMessage is TEnvoiDUnMessageAQuelquUnMessage) then
    exit;
  msg := AMessage as TEnvoiDUnMessageAQuelquUnMessage;

  AddLog('From : ' + ASender.TagString + slinebreak + 'To: ' + msg.Destinataire
    + slinebreak + msg.Texte);

  msg2 := TTransmissionDUnMessageRecuMessage.Create;
  try
    msg2.Emetteur := ASender.TagString;
    msg2.Texte := msg.Texte;
    ForEachConnectedClient(
      procedure(Const Client: TOlfSMSrvConnectedClient)
      begin
        if Client.TagString = msg.Destinataire then
          Client.SendMessage(msg2);
      end);
  finally
    msg2.Free;
  end;
end;

procedure TServer.onPublicMessage(const ASender: TOlfSMSrvConnectedClient;
const AMessage: TOlfSMMessage);
var
  msg: TEnvoiDUnMessageATousMessage;
  msg2: TTransmissionDUnMessageRecuMessage;
begin
  if not(AMessage is TEnvoiDUnMessageATousMessage) then
    exit;
  msg := AMessage as TEnvoiDUnMessageATousMessage;

  AddLog('From : ' + ASender.TagString + slinebreak + msg.Texte);

  msg2 := TTransmissionDUnMessageRecuMessage.Create;
  try
    msg2.Emetteur := msg.Emetteur;
    msg2.Texte := msg.Texte;
    // SendMessageToAll(msg2, ASender);
    SendMessageToAll(msg2);
  finally
    msg2.Free;
  end;
end;

end.
