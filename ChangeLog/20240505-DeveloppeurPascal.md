# 20240505 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

Durant le développement de la fonctionnalité de signature de code à distance dans [Exe Bulk Signing](https://github.com/DeveloppeurPascal/ExeBulkSigning) plusieurs anomalies non repérées jusque là sont apparues dans les échanges de messages via la librairie Socket Messaging. Les correctifs correspondants ont été appliqués ce jour.

* corrigé : verrou "self" au lieu de "fsocket" entrainant des blocages
* corrigé : intercepter les exceptions dans les TParallel.For()
