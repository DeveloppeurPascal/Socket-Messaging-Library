# 20230804 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## librairie

* renommage du type TOlfReceivedMessageEvent en TOlfSMReceivedMessageEvent
* ajout du type TOlfSMReceivedMessageEvent<T: TOlfSMMessage> utilisé comme événement et méthode d'interception des messgaes reçus sur le serveur et le client générés par le programme

## modifications sur la classe du serveur

* ajout de ForEachConnectedClient() pour exécuter du code sur chaque client connecté
* modification de SendMessageToAll() pour utiliser ForEachConnectedClient() (et ses futures évolutions)
* ajout d'un filtre sur SendMessageToAll() pour ne pas envoyer à un client en particulier (en général celui qui est à l'origine de l'envoi)
* ajout des événements onClientConnected, onClientDisconnected et onClientLostConnection pour informer l'utilisateur du serveur d'activités sur les clients connectés

## modifications sur la classe du client connecté au serveur

## modifications sur la classe du client

## générateur de code Delphi

* ajout du MessageID dans le <SUMMARY> généré pour chaque classe de message
* implémentation de la supression des champs et des messages 
* mise à jour du status du projet lorsqu'on ajoute/retire un élément d'une liste (champs ou messages)
* correction du titre de la fenêtre (retrait de "*") lorsqu'on enregistre un projet déjà nommé
* correction du titre de la fenêtre (ajout de "*") lorsqu'on manipule l'arborescence du projet (ajout ou suppression de champs ou messages)
* ajout d'un numéro de version dans le fichier du projet afin d'éviter de l'ouvrir avec un programme trop ancien (et perdre des données qui n'étaient pas encore prises en charge)
* ajout dans la structure du projet et à l'écran des noms de classe à utiliser (et par défaut) pour le serveur et le client générés lors de l'export de l'unité Delphi
* ajout d'un champ de stockage d'une liste d'unités nécessaires en interface de l'unité Delphi générée (si on utilise par exemple des types personnalisés dans les champs des messages)

* refonte de prérenseigneemnt des noms de classes pour les types et changement au niveau de la génération afin d'ajouter le 'T' en dur sur les types
* ajout d'un préfix et d'un suffix au niveau du projet ajoutés aux noms de classes des messages (par défaut ou saisis)

* ajout du type de chargement/enregistrement par rapport à un stream sur les champs (TODO, SizeOF, String, Class.LoadFromStream/Class.SaveToStream)

* génère désormais directement les procédures LoadStringFromStream() et SaveStringToStream() provenant de l'unité Olf.RTL.Streams.pas du dépôt [DeveloppeurDelphi/Librairies](https://github.com/DeveloppeurPascal/librairies) quand un champ de message est déclaré comme chaîne de caractère 

* génère la classe serveur "clé en main" sur laquelle juste remplir les événéments qui nous intéressent

* génère aussi la classe client "clé en main" sur laquelle juste remplir les événéments qui nous intéressent

## exemples

* l'échange de messages privés a été ajouté à l'exemple de salon de discussion
* ajout de tests sur les nouvelles fonctionnalités au niveau de la librairie dans l'exemple de salon de discussion

* un nouvel exemple de serveur d'horloge a été codé avec le moisn de codage possible...
