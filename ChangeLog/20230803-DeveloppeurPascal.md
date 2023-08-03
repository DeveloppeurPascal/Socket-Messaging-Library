# 20230802 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## librairie

* réduction des noms des types et utilisation de la racine TOlfSM (TOlfSocketMessaging) commune à tous :
  TOlfMessageID = TOlfSMMessageID;
  TOlfMessageSize = TOlfSMMessageSize;
  TOlfSocketMessagingServerConnectedClient = TOlfSMSrvConnectedClient;
  TOlfSocketMessagingException = TOlfSMException;
  TOlfSocketMessagingServer = TOlfSMServer;
  TOlfSocketMessagingClient = TOlfSMClient;
  TOlfSocketMessage = TOlfSMMessage;
  TOlfSocketMessagesDict = TOlfSMMessagesDict;
  IOlfSocketMessagesRegister = IOlfSMMessagesRegister;
  
* renommer DispatchMessage() en DispatchReceivedMessage()

## modifications sur la classe du serveur

* ajout de isConnected()
* ajout de isListening()
* ajout de l'événement onServerConnected
* ajout de l'événement onServerDisconnected

* ajout de l'événement onEncodeMessageToSend
* ajout de l'événement onDecodeReceivedMessage

* ajout d'une liste des clients connectés et prise en charge de leur déconnexion
* ajout de SendMessageToAll() pour un envoi de message à tous les clients connectés

## modifications sur la classe du client connecté au serveur

* ajout de isConnected()
* interdiction de l'envoi de messages plus grands que Max(TOlfSMMessageSize) pour éviter les débordements lors de la réception
* ajout de l'événement onConnected
* ajout de l'événement onLostConnection
* ajout de l'événement onDisconnected

## modifications sur la classe du client

* correction de l'initialisation de l'IP et du port dans le constructeur du client

## générateur de code Delphi
