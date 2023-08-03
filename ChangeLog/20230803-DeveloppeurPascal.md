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
* ajout de propriétés de stockage de données (Tag, TagBool, TagString, TagFloat, TagObject)

## modifications sur la classe du client

* correction de l'initialisation de l'IP et du port dans le constructeur du client

## générateur de code Delphi

* utilisation des nouveaux noms courts pour les classes de la librairie dans le code exporté
* ajout du lien vers l'unité de la librairie sur GitHub dans l'unité générée (au cas où le générateur se promène tout seul)
* ajout du lien "raw" de GitHub vers les autres dépendances
* autorise les "." dans le nom des unités générées
* suppression de l'initialisation des champs n'ayant pas de valeur par défaut (dans le cas contraire elle est reprise telle quelle en Create())
* changement des textes associés à la valeur par défaut sur l'édition des champs
* remplacement de "register on the server/client" par "received by the serverclient" pour simplifier la compréhension de ces cases et leur impact en génération de code
* changement de texte pour l'option d'export de l'unité Delphi
* ajout de boutons sur la fiche du projet pour le sauvegarder, le fermet et l'exporter
* ajout de "Message" en fin de nom des classes des messages s'il n'y est pas déja (dans le nommage par défaut)
* ajout d'un "T" devant les identifiants de types (dans les champs de saisie et dans la base)
* ajout du "F" des champs de classes à l'écran pour indiquer qu'il n'est pas utile dans l'identifiant saisi ou autogénéré

* dans la génération des identifiants Delphi :
- on ignore maintenant les chiffres en début de texte
- le premier caractère de l'identifiant est en majuscule
- le premier caractère le '_' est ignoré mais le caractère suivant est mis en majuscule

## exemples

* ajout d'un exemple de salon de discussion simple
