# Filtre LZWDecode

Traduit de Portable Document Format Reference Manual Version 1.2 / Adobe Systems Incorporated / 27 novembre 1996

Ce filtre décode les données encodées à l'aide de la méthode de compression de données LZW, qui est une méthode de compression adaptative à longueur variable. Le codage LZW compresse les données binaires et les données textuelles ASCII, mais produit toujours des données binaires, même si les données d'origine étaient du texte ASCII. Si nécessaire, ces données binaires peuvent être converties en données de 7 bits à l'aide des codages ASCII hexadécimal ou ASCII base-85 décrits dans les sections précédentes.

La compression LZW peut découvrir et exploiter de nombreux modèles dans les données d'entrée, qu'il s'agisse de texte ou d'images. La compression obtenue à l'aide de la méthode LZW varie d'un fichier à l'autre ; le meilleur cas (un fichier composé de tous les zéros) fournit une compression approchant 1365:1 pour les longs fichiers, tandis que le pire cas (un fichier dans lequel aucune paire de caractères adjacents n'apparaît deux fois) peut produire une expansion d'environ 50 %.

Les données codées à l'aide de LZW consistent en une séquence de codes d'une longueur de 9 à 12 bits. Chaque code représente un seul caractère des données d'entrée (0-255), un marqueur de table vide (256), un marqueur EOD (257) ou une entrée de table représentant une séquence de plusieurs caractères qui a été rencontrée précédemment dans l'entrée (258 et plus).

Au départ, la longueur du code est de 9 bits et la table ne contient que des entrées pour les codes fixes 258. Au fur et à mesure du codage, des entrées sont ajoutées à la table, associant de nouveaux codes à des séquences de caractères d'entrée de plus en plus longues. Les filtres de codage et de décodage conservent des copies identiques de cette table.

Chaque fois que le codeur et le décodeur réalisent indépendamment (mais de façon synchrone) que la longueur du code actuel n'est plus suffisante pour représenter le nombre d'entrées dans la table, ils augmentent le nombre de bits par code d'une unité. Le premier code de sortie d'une longueur de 10 bits est celui qui suit la création de l'entrée 511 de la table, et de même pour 11 (1023) et 12 (2047) bits. Les codes ne sont jamais plus longs que 12 bits, c'est pourquoi l'entrée 4095 est la dernière entrée de la table LZW.

Le codeur exécute la séquence d'étapes suivante pour générer chaque code de sortie :

1. Accumuler une séquence d'un ou plusieurs caractères d'entrée correspondant à une séquence déjà présente dans la table. Pour une compression maximale, le codeur recherche la séquence la plus longue.
2. Produis le code correspondant à cette séquence.
3. Crée une nouvelle entrée de table pour le premier code inutilisé. Sa valeur est la séquence trouvée à l'étape 1, suivie du caractère d'entrée suivant.

Pour s'adapter aux séquences d'entrée changeantes, le codeur peut à tout moment émettre un code d'effacement de table, ce qui entraîne le redémarrage du codeur et du décodeur avec des tables initiales et un code de 9 bits. Par convention, le codeur commence par émettre un code d'effacement de table. Il doit émettre un code d'effacement de table lorsque la table est pleine ; il peut le faire plus tôt.

Le filtre LZW peut être utilisé pour compresser du texte ou des images. Lors de la compression d'images, plusieurs techniques permettent de réduire la taille des données compressées. Par exemple, les données d'une image changent souvent très peu d'un échantillon à l'autre. En soustrayant les valeurs des échantillons adjacents (un processus appelé différentiation) et en codant par LZW les différences plutôt que les valeurs brutes des échantillons, la taille des données de sortie peut être réduite. En outre, lorsque les données d'image contiennent plusieurs composantes de couleur (rouge-vert-bleu ou cyan-magenta-jaune-noir) par échantillon, le fait de prendre la différence entre les valeurs de composantes similaires dans des échantillons adjacents, plutôt qu'entre différentes composantes de couleur dans le même échantillon, permet souvent de réduire la taille des données de sortie. Afin de contrôler ces options et d'autres, le filtre LZW accepte plusieurs paramètres optionnels, présentés dans le tableau 4.4. Toutes les valeurs fournies au filtre de décodage par les paramètres optionnels doivent correspondre à celles utilisées lors du codage des données.

La méthode de compression LZW fait l'objet du brevet américain numéro 4,558,302 détenu par Unisys Corporation. Adobe Systems a accordé une licence pour l'utilisation de ce brevet dans ses produits, y compris les produits Acrobat. Toutefois, les fournisseurs de logiciels indépendants peuvent être tenus d'obtenir une licence de ce brevet directement auprès d'Unisys pour développer des logiciels utilisant LZW. De plus amples informations peuvent être obtenues auprès du Welch Licensing Department, Law Department, M/S C2SW1, Unisys Corporation, Blue Bell, Pennsylvania, 19424.