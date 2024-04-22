# 20240422 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* mise à jour des sous-modules et dépendances
* updated FR/EN docs
* tests de client/serveur avec modifications de Johan sur l'exemple 3 "clock server sample"
* tests avec la version d'origine
* regénération de l'unité des messages de cet exemple
* initialisation des événements à nil dans le serveur et les clients connectés (au cas où)

* correction de bug sur événements liés aux déconnexions et pertes de connections clients : les événements côté serveur étaient inversés
* correction : non prise en charge des déconnexions sur les envois sans plantage en lecture du socket
* correction : libération de la mémoire liée aux clients encore connectés lors de la fermeture du serveur
* fiabilisation de la libération des threads et sous-threads liés au serveur et aux clients (connectés au serveur ou à distance)
* fiabilisation de la libération des sockets (prise en charge du statut du socket et interception d'erreurs bloquantes)
