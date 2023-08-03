unit SalonDeDiscussion;

// ****************************************
// * Salon de discussion
// ****************************************
// 
// ****************************************
// File generator : Socket Message Generator (v1.0)
// Website : https://socketmessaging.developpeur-pascal.fr/ 
// Generation date : 03/08/2023 21:46:53
// 
// Don't do any change on this file. They will be erased by next generation !
// ****************************************

// To compile this unit you need Olf.Net.Socket.Messaging.pas from
// https://github.com/DeveloppeurPascal/Socket-Messaging-Library
//
// Direct link to the file :
// https://raw.githubusercontent.com/DeveloppeurPascal/Socket-Messaging-Library/main/src-library/Olf.Net.Socket.Messaging.pas

// To compile this unit you need Olf.RTL.Streams.pas from
// https://github.com/DeveloppeurPascal/librairies
//
// Direct link to the file :
// https://raw.githubusercontent.com/DeveloppeurPascal/librairies/master/Olf.RTL.Streams.pas

interface

uses
  System.Classes,
  Olf.Net.Socket.Messaging;

type
  /// <summary>
  /// Envoi d'un message à quelqu'un
  /// </summary>
  TEnvoiDUnMessageAQuelquUnMessage = class(TOlfSMMessage)
  private
    FDestinataire: string;
    FTexte: string;
    procedure SetDestinataire(const Value: string);
    procedure SetTexte(const Value: string);
  public
    /// <summary>
    /// Destinataire
    /// </summary>
    property Destinataire: string read FDestinataire write SetDestinataire;
    /// <summary>
    /// Texte
    /// </summary>
    property Texte: string read FTexte write SetTexte;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  /// <summary>
  /// Envoi d'un message à tous
  /// </summary>
  TEnvoiDUnMessageATousMessage = class(TOlfSMMessage)
  private
    FEmetteur: string;
    FTexte: string;
    procedure SetEmetteur(const Value: string);
    procedure SetTexte(const Value: string);
  public
    /// <summary>
    /// Emetteur
    /// </summary>
    property Emetteur: string read FEmetteur write SetEmetteur;
    /// <summary>
    /// Texte
    /// </summary>
    property Texte: string read FTexte write SetTexte;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  /// <summary>
  /// Identification d'un utilisateur
  /// </summary>
  TIdentificationDUnUtilisateurMessage = class(TOlfSMMessage)
  private
    FPseudo: string;
    procedure SetPseudo(const Value: string);
  public
    /// <summary>
    /// Pseudo de l'utilisateur
    /// </summary>
    property Pseudo: string read FPseudo write SetPseudo;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

  /// <summary>
  /// Transmission d'un message reçu
  /// </summary>
  TTransmissionDUnMessageRecuMessage = class(TOlfSMMessage)
  private
    FEmetteur: string;
    FTexte: string;
    procedure SetEmetteur(const Value: string);
    procedure SetTexte(const Value: string);
  public
    /// <summary>
    /// Emetteur
    /// </summary>
    property Emetteur: string read FEmetteur write SetEmetteur;
    /// <summary>
    /// Texte
    /// </summary>
    property Texte: string read FTexte write SetTexte;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSMMessage; override;
  end;

procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);
procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);

implementation

uses
  Olf.RTL.Streams,
  System.SysUtils;

procedure RegisterMessagesReceivedByTheServer(Const Server: TOlfSMServer);
begin
  Server.RegisterMessageToReceive(TEnvoiDUnMessageAQuelquUnMessage.Create);
  Server.RegisterMessageToReceive(TEnvoiDUnMessageATousMessage.Create);
  Server.RegisterMessageToReceive(TIdentificationDUnUtilisateurMessage.Create);
end;

procedure RegisterMessagesReceivedByTheClient(Const Client: TOlfSMClient);
begin
  Client.RegisterMessageToReceive(TTransmissionDUnMessageRecuMessage.Create);
end;

{ TEnvoiDUnMessageAQuelquUnMessage }

constructor TEnvoiDUnMessageAQuelquUnMessage.Create;
begin
  inherited;
  MessageID := 3;
end;

function TEnvoiDUnMessageAQuelquUnMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TEnvoiDUnMessageAQuelquUnMessage.Create;
end;

procedure TEnvoiDUnMessageAQuelquUnMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FDestinataire := LoadStringFromStream(Stream);
  FTexte := LoadStringFromStream(Stream);
end;

procedure TEnvoiDUnMessageAQuelquUnMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FDestinataire, Stream);
  SaveStringToStream(FTexte, Stream);
end;

procedure TEnvoiDUnMessageAQuelquUnMessage.SetDestinataire(const Value: string);
begin
  FDestinataire := Value;
end;

procedure TEnvoiDUnMessageAQuelquUnMessage.SetTexte(const Value: string);
begin
  FTexte := Value;
end;

{ TEnvoiDUnMessageATousMessage }

constructor TEnvoiDUnMessageATousMessage.Create;
begin
  inherited;
  MessageID := 2;
end;

function TEnvoiDUnMessageATousMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TEnvoiDUnMessageATousMessage.Create;
end;

procedure TEnvoiDUnMessageATousMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FEmetteur := LoadStringFromStream(Stream);
  FTexte := LoadStringFromStream(Stream);
end;

procedure TEnvoiDUnMessageATousMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FEmetteur, Stream);
  SaveStringToStream(FTexte, Stream);
end;

procedure TEnvoiDUnMessageATousMessage.SetEmetteur(const Value: string);
begin
  FEmetteur := Value;
end;

procedure TEnvoiDUnMessageATousMessage.SetTexte(const Value: string);
begin
  FTexte := Value;
end;

{ TIdentificationDUnUtilisateurMessage }

constructor TIdentificationDUnUtilisateurMessage.Create;
begin
  inherited;
  MessageID := 1;
end;

function TIdentificationDUnUtilisateurMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TIdentificationDUnUtilisateurMessage.Create;
end;

procedure TIdentificationDUnUtilisateurMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FPseudo := LoadStringFromStream(Stream);
end;

procedure TIdentificationDUnUtilisateurMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FPseudo, Stream);
end;

procedure TIdentificationDUnUtilisateurMessage.SetPseudo(const Value: string);
begin
  FPseudo := Value;
end;

{ TTransmissionDUnMessageRecuMessage }

constructor TTransmissionDUnMessageRecuMessage.Create;
begin
  inherited;
  MessageID := 4;
end;

function TTransmissionDUnMessageRecuMessage.GetNewInstance: TOlfSMMessage;
begin
  result := TTransmissionDUnMessageRecuMessage.Create;
end;

procedure TTransmissionDUnMessageRecuMessage.LoadFromStream(Stream: TStream);
begin
  inherited;
  FEmetteur := LoadStringFromStream(Stream);
  FTexte := LoadStringFromStream(Stream);
end;

procedure TTransmissionDUnMessageRecuMessage.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FEmetteur, Stream);
  SaveStringToStream(FTexte, Stream);
end;

procedure TTransmissionDUnMessageRecuMessage.SetEmetteur(const Value: string);
begin
  FEmetteur := Value;
end;

procedure TTransmissionDUnMessageRecuMessage.SetTexte(const Value: string);
begin
  FTexte := Value;
end;

end.
