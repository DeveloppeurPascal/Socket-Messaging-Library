/// <summary>
/// ***************************************************************************
///
/// Socket Messaging Library
///
/// Copyright 2023-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Socket Messaging is a Delphi Library for managing a messaging system over
/// TCP/IP sockets between clients and servers or peer to peer programs.
///
/// The library can be used in any Delphi project on recent versions of the
/// environment. It uses the TSocket class from System.Net.Socket, threads and
/// generic collections.
///
/// To use this library more simply, and obtain the source code you need to
/// use a client and server in your project, with your own interface, without
/// coding, use this code generator : https://smcodegenerator.olfsoftware.fr/
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://socketmessaging.developpeur-pascal.fr/
///
/// Project site :
///      https://github.com/DeveloppeurPascal/Socket-Messaging-Library
///
/// ***************************************************************************
/// File last update : 28/05/2024 12:19:19
/// Signature : 9a7ef0bcf23947d4c09e42993080c95470845ed9
/// ***************************************************************************
/// </summary>

unit uTestMessages;

interface

uses
  System.Classes,
  Olf.Net.Socket.Messaging;

type
  TMessage1 = class(TOlfSocketMessage)
  private
    FTexte: string;
    procedure SetTexte(const Value: string);
  protected
  public
    property Texte: string read FTexte write SetTexte;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSocketMessage; override;
  end;

  TMessage2 = class(TOlfSocketMessage)
  private
    FZ: integer;
    FX: integer;
    FY: integer;
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
    procedure SetZ(const Value: integer);
  protected
  public
    property X: integer read FX write SetX;
    property Y: integer read FY write SetY;
    property Z: integer read FZ write SetZ;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetNewInstance: TOlfSocketMessage; override;
  end;

procedure RegisterMessages(Const ClientOrServer: IOlfSocketMessagesRegister);

implementation

uses
  System.SysUtils,
  Olf.RTL.Streams;

procedure RegisterMessages(Const ClientOrServer: IOlfSocketMessagesRegister);
begin
  ClientOrServer.RegisterMessageToReceive(TMessage1.Create);
  ClientOrServer.RegisterMessageToReceive(TMessage2.Create);
end;

{ TMessage1 }

constructor TMessage1.Create;
begin
  inherited;
  messageid := 1;
  FTexte := '';
end;

function TMessage1.GetNewInstance: TOlfSocketMessage;
begin
  result := TMessage1.Create;
end;

procedure TMessage1.LoadFromStream(Stream: TStream);
begin
  inherited;
  FTexte := LoadStringFromStream(Stream);
end;

procedure TMessage1.SaveToStream(Stream: TStream);
begin
  inherited;
  SaveStringToStream(FTexte, Stream);
end;

procedure TMessage1.SetTexte(const Value: string);
begin
  FTexte := Value;
end;

{ TMessage2 }

constructor TMessage2.Create;
begin
  inherited;
  messageid := 2;
  FX := 0;
  FY := 0;
  FZ := 0;
end;

function TMessage2.GetNewInstance: TOlfSocketMessage;
begin
  result := TMessage2.Create;
end;

procedure TMessage2.LoadFromStream(Stream: TStream);
begin
  inherited;
  if (Stream.read(FX, sizeof(FX)) <> sizeof(FX)) then
    raise exception.Create('Can''t load X value.');
  if (Stream.read(FY, sizeof(FY)) <> sizeof(FY)) then
    raise exception.Create('Can''t load Y value.');
  if (Stream.read(FZ, sizeof(FZ)) <> sizeof(FZ)) then
    raise exception.Create('Can''t load Z value.');
end;

procedure TMessage2.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FX, sizeof(FX));
  Stream.Write(FY, sizeof(FY));
  Stream.Write(FZ, sizeof(FZ));
end;

procedure TMessage2.SetX(const Value: integer);
begin
  FX := Value;
end;

procedure TMessage2.SetY(const Value: integer);
begin
  FY := Value;
end;

procedure TMessage2.SetZ(const Value: integer);
begin
  FZ := Value;
end;

end.
