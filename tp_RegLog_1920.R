# ----- Chargement des fonctions utiles
source("./fonctions_tp_RegLog.R")
source("./nnets.R")
library(e1071)


################### Exercice 1 : ####################
# Régression logistique sur un jeu de données jouet (2 descripteurs, donc visualisable, 
# variable cible binaire)

# Objectifs de cet exercice :
# A la fin de cet exercice, vous devez être capable de : 
#  - Charger un jeu de données (sous forme de fichier txt) dans R
#  - Analyser (succinctement) le jeu de données, et visualiser les données
#  - Faire une séparation aléatoire du jeu de données en A/V/T
#  - Construire un modèle de régression logistique
#  - L'utiliser pour faire des prédictions
#  - Visualiser la frontière de décision
#  - Complexifier le modèle en ajoutant des puissances dans les données
#  - Sélectionner la complexité idoine pour ce jeu de données
#  - Estimer l'erreur de généralisation de ce modèle


# ----- Chargement des données (prenez le fichier qui vous est associé)
ex1 = read.table("./donnees_exo1.txt", header = T)


# Affichage du début des données
head(ex1) # Cette commande affiche les 6 premières lignes du tableau ex1

# Question
# Combien de variables décrivent les individus de ce jeu de données ? Comment s'appellent t'elles?
dim(ex1)
# deux variables X et Y
# Rappels: 
# - on peut accéder à la i-ème ligne d'un tableau T par la commande T[i,]
# - on peut accéder à la i-ème colonne d'un tableau T par la commande T[,i]
# - on peut aussi accéder à une colonne par l'intermédiaire de son nom, avec le $ : T$X renvoie
# la colonne qui s'appelle X du tableau T (si elle existe biensur)

# ----- Statistiques sur les données

# Questions
# Pour les questions suivantes, les commandes dim(), summary(), table() vous seront utiles

# Combien y a t'il d'individus dans ce jeu de données ? 
dim(ex1)
#118 lignes et 3 colonnes
# Quelles sont les valeurs des variables descriptives de l'individu numéro 12 ?
ex1[12,]
#12 x=-0.11578 y=-0.39693     classe= 1
# De quel type est la variable de la colonne Classe ?
ex1$Classe=as.factor(ex1$Classe)
ex1$Classe
class(ex1$Classe)
# appartient a la classe factor
# Combien la variable Classe peut-elle prendre de valeurs différentes?
#2 valeurs 1 ou 0
# Quelle est la répartition des valeurs de Classe dans ce jeu de données ?
length(which(ex1$Classe==1))
#58
length(which(ex1$Classe==0))
#60
# Que vaut la moyenne de la variable X?
summary(ex1$X)
mean(ex1$X)
#Mean=0.05779
# Que vaut la moyenne de la variable Y?
mean(ex1$Y)
#Mean=0.1831016
# Que vaut la moyenne de la variable X pour les individus de la classe 0 ?
mean(ex1[which(ex1$Classe==0),1])
#0.09081584

# ----- Affichage du jeu de données 
plot(ex1[which(ex1$Classe==0), 1:2], col = "blue")
points(ex1[which(ex1$Classe==1), 1:2], col = "red")

# Question:
# Ce jeu de données est-il linéairement séparable ?
#reponse Non car mal distribu�


# ----- Separation Apprentissage/Validation/Test 
# On va mettre 70% des individus dans un ensemble d'apprentissage, 15% dans un ensemble de validation et 15% dans un ensemble de test
# Le mieux est de le faire de façon aléatoire (et non pas garder l'ordre du jeu de données initial)
# Pour cela, vous pouvez utiliser la commande sample(118) qui génère une permutation aléatoire 
# de l'ensemble [1, ..., 118] (118 ici car on a 118 individus)
idx_random = sample(118)
idx_random

# Puis, on va permuter les lignes de ex1 grâce à cette permutation aléatoire :
ex1 = ex1[idx_random,]
ex1
# On a gardé toutes les lignes de ex1 mais on a changé l'ordre.

# Question : 
# Créer un tableau ex1_app (ensemble d'apprentissage) qui contient 70 % des individus de ex1 (ensemble d'apprentissage)
# Aide : 70% de 118, fait environ 82. Donc vous devez prendre les 82 premières lignes de ex1 (qui a été 
# permuté aléatoirement)
ex1_app=(ex1[1:82,])
ex1_app

# Puis créer ex1_val (ensemble de validation) en mettant les 18 individus (15%) suivants de ex1
ex1_val=ex1[83:100,]
ex1_val
dim(ex1_val)
# Et enfin, ex1_test (ensemble de test) en mettant les 18 derniers individus (15%) de ex1
ex1_test=ex1[101:118,]
ex1_test
dim(ex1_test)

# ----- Affichage du jeu de données (en différenciant app/val)

#Affichage des données d apprentissage
# D'abord ceux qui ont échoués (pour lesquels ex1$Reussite = 0) en rouge
plot(ex1_app[which(ex1_app$Classe==0),1:2], col = "red", xlim = c(-1,1.2), ylim = c(-0.85,1.2))
# puis ceux qui ont réussis, en bleu
points(ex1_app[which(ex1_app$Classe==1),1:2], col = "blue")

# Affichage des données de validation (on les affiche avec un triangle , pch = 2)
points(ex1_val[which(ex1_val$Classe==1),1:2], col = "blue", pch = 2)
points(ex1_val[which(ex1_val$Classe==0),1:2], col = "red", pch = 2)


# ----- Estimation du modele de regression logistique sur les données d apprentissage
# On utilise la fonction glm. On doit lui indiquer le nom de la colonne dans laquelle est stockée la variable
# cible, ici Classe. On utilise le . pour dire qu'on prends toutes les autres colonnes comme descripteurs des individus pour construire le classifieur.
# Le terme "family = binomial(link = 'logit')" sert à préciser que l'on fait de la régression logistique
reg_ex1 = glm(Classe~., data = ex1_app, family = binomial(link = 'logit'))
# Question : 
# Sans regarder le modèle (pour l'instant), combien de paramètres ont été estimés dans ce modèle ?
# 3 parametre: une constante + les 2 variable explicative X et Y
# Affichage du modèle
reg_ex1
# Questions : 
# Quels sont les paramètres (coefficients) estimés ? Quelle est l'équation du modèle ?
# X, Y et une constance
#equation = (1/(1+expo-(-0.1561-0.5429X+0.2786Y))

# Calculez à la main (enfin en utilisant la console R quand même) la sortie du modèle associée au premier 
# individu de votre ensemble d'apprentissage. Aide : il suffit d'appliquer l'équation du modèle avec les données de cet individu.
var=(1/(1+exp(-(-0.1561-0.5429*(-0.0466590)+0.2786*(-0.579680)))))
var
# Cette valeur correspond à la probabilité que l'individu 1 soit de la classe 1 (Classe=1)
#Non car proba inferieur a 0.5
# Quelle décision allez-vous prendre pour cet individu d'après ce modèle ?
#de le mettre dans la classe 0
# Est-ce une bonne décision ?
#oui
ex1_app
# Vous pouvez calculer directement la sortie du modèle avec la commande predict:
idx = 1 # pour dire qu'on regarde le 1er individu pour l'instant
ex1_app[idx,]
p = predict(reg_ex1, ex1_app[idx,], type = 'response')
p
# Vérifier que le résultat est bien le même que celui que vous avez obtenu juste avant.
# verification OK
# Ecrivez une commande qui permet de déterminer à partir de la probabilité obtenue ci-dessus, la classe (prédite par la modèle) de l'individu 
# Aide : commande ifelse(condition, reponse si oui, reponse si non)
# A REMPLIR
ifelse(var>=0.5,1,0)
# ----- Calcul des trois types d'erreurs (app/val/test) de ce modele

# Estimez la classe de chacun des individus du jeu d'apprentissage. 
# Aide : appliquer la commande predict à tout le jeu d'apprentissage, puis ifelse de la même façon
# A REMPLIR
predict_app=predict(reg_ex1, ex1_app, type = 'response')
predict_app
p_app=ifelse(predict_app>=0.5,1,0)
p_app
# Combien d'erreurs de prediction sont faites par ce modele sur le jeu d'apprentissage ?
# Les commandes suivantes vous seront surement utiles : 
# - la commande != permet de comparer les éléments deux à deux de deux vecteurs et dire (via TRUE or FALSE) s'ils sont différents
# - sum() appliquée à un vecteur booléen permet de compter le nombre d'éléments égaux à TRUE dans ce vecteur
# Ici vous devez comparer le vecteur contenant vos prédictions avec la vérité (colonne Classe de ex1_app) et compter combien de prédictions sont fausses

# A REMPLIR
p_app!=ex1_app$Classe
Nbre_err=sum((p_app!=ex1_app$Classe))
Nbre_err

# Identifiez un individu pour lequel le modèle se trompe
p_app
ex1_app$Classe
# A REMPLIR
#l'individu 55
# Donner la matrice de confusion sur le jeu d'apprentissage. Aide : commande table avec 2 paramètres : les prédictions et la vérité. En ligne, les classes prédites et en colonnes les vraies classes

# A REMPLIR
ex1_app$Classe
p_app
Mat=table(p_app,ex1_app$Classe)
Mat
# Combien d'individus (du jeu d'apprentissage) ont été classés 1 alors qu'ils étaient 0 en réalité ?
# 16 individus
# A REMPLIR

# Estimez la classe de chacun des individus du jeu de validation et calculez le nombre d'erreurs 
# faites par ce modele sur le jeu de validation. Aide : idem 

# A REMPLIR
ex1_val
predict_valid=predict(reg_ex1, ex1_val, type = 'response')
predict_valid
p_valid=ifelse(predict_valid>=0.5,1,0)
p_valid
#Nombre d'erreur

#la prediction
p_valid
#la classe validation
ex1_val$Classe
#diff en la prediction et la classe de validation
p_valid!=ex1_val$Classe
#comptage du nombre d'erreur
p_valid!=ex1_val$Classe
Nbre_err_val=sum((p_valid!=ex1_val$Classe))
Nbre_err_val
#Matrice de confusion
Mat_val=table(p_valid,ex1_val$Classe)
Mat_val
# Idem pour l'ensemble de test
ex1_test
predict_test=predict(reg_ex1, ex1_test, type = 'response')
predict_test
p_test=ifelse(predict_test>=0.5,1,0)
p_test
#Nombre d'erreur

#la prediction
p_test
#la classe validation
ex1_test$Classe
#diff en la prediction et la classe de validation
p_test!=ex1_test$Classe
#comptage du nombre d'erreur
Nbre_err_tes=sum((p_test!=ex1_test$Classe))
Nbre_err_tes
#Matrice de confusion
Mat_val=table(p_test,ex1_test$Classe)
#tous les individus de la classe ont �t� predit a 0
#la matrice de confusion indique que 7 indivus ont �t� predit classe0 alors qu'il etait de la clase 1
Mat_val

# Tracé de la frontière de décision
# La fonction remplissage (donnée dans le fichier fonctions_tp_RegLog, allez voir les explications) vous permet de visualiser la frontière de décision
# Le principe : prédire tous les points du plan et les afficher avec la couleur associée à la prédiction.
# Vous remarquerez que les données d'apprentissage et de validation sont également affichées
# Ici le paramètre puissance est mis à 1, les couleurs sont rouge et bleus (vous pouvez changer)
remplissage(ex1_app, ex1_val, reg_ex1,1,"red","blue",-1,1.2,-0.85,1.2)

# Questions:
# De quel type est la frontière associée à la régression logistique ?
 #Lineaire
# Repérez (visuellement) les individus de l'ensemble d'apprentissage qui sont mal classés par le modèle
# Même question avec les individus de l'ensemble de validation

# Remplir la fonction calcul_erreur du fichier fonctions_tp_RegLog (allez voir sa description) et vérifier que les résultats sont corrects (identiques à ceux obtenus avant)
source("./fonctions_tp_RegLog.R")
AVT1=calcul_erreur(ex1_app, ex1_val, ex1_test, reg_ex1, 3)
# Pour créer un modèle de regression logistique qui permette une frontière non-linéaire, il faut ajouter des nouvelles 
# variables à nos données, en mettant les variables initiales à des puissances p. 
# C'est ce que vous allez faire maintenant, et vous pourrez observer l'évolution de la frontière de décision eon fonction de la valeur 
# des puissances choisies.
    
# ----- Création de nouvelles variables (élévation à la puissance p des variables initiales)

# ----- p = 2 

# Au départ, nous avons X et Y.  On va créer 3 nouvelles variables : X^2, Y^2 et X*Y
# Ceci est fait par la fonction puissance (donnée dans fonctions_tp_RegLog.R), qui prend un jeu de données en 
# paramètres et une valeur p et éleve les variables de départ à la puissance p

#Création des nouveaux jeux de données (apprentissage, validation et test)
head(ex1_app)
pow = 2
pow
ex1_d2_app = puissance(ex1_app,pow)
head(ex1_d2_app)
ex1_d2_val = puissance(ex1_val,pow)
ex1_d2_test = puissance(ex1_test,pow)

# Affichage du début de ex1_d2_app
head(ex1_d2_app)
dim(ex1_d2_app)
#Question : 
# Comparer (manuellement) la première ligne de ex1_app (individu numéro 1 du jeu de données initial) et la première ligne de ex1_d2_app (individu numéro 1 avec les nouvelles variables). Dans quelles colonnes sont mises les nouvelles variables ? Vous remarquerez que la variable cible (Classe) est toujours en dernière colonne. Repérez l'indice de cette dernière colonne
# X3 et X4
# Indice col classe = 5
ncol(ex1_d2_app)
# Calcul du modèle de regression logistique associé aux données d apprentissage
reg_ex1_d2 = glm(Classe~., data = ex1_d2_app, family = binomial(link = 'logit'))
reg_ex1_d2
# Tracé de la frontière de décision (on indique  le nouveau nom du modèle et que les données sont à la puissance 2)
remplissage(ex1_app, ex1_val, reg_ex1_d2,2,"red","blue",-1,1.2,-0.85,1.2)

# Calculer le nb d erreur que fait ce modèle sur les donnes d apprentissage, validation et test (en utilisant la fonction calcul_erreur écrite précédemment. Attention à donner le bon idx_classe en paramètre). Est-il meilleur que le modèle initial ?

# A FAIRE
source("./fonctions_tp_RegLog.R")
AVT2=calcul_erreur(ex1_d2_app, ex1_d2_val, ex1_d2_test, reg_ex1_d2, ncol(ex1_d2_app))
AVT2
# OUI le nombre d'erreur a baiss�
# ----- Refaire les mêmes opérations en élevant aux puissances 2 et 3 (en mettant c(2,3) en paramètre de puissance)
# puis 2, 3 et 4
# puis 2,3,4,6
# et essayer d'aller jusque 10 ou 12
#
pow = 3
pow
ex1_d2_app = puissance(ex1_app,pow)
head(ex1_d2_app)
ex1_d2_val = puissance(ex1_val,pow)
ex1_d2_test = puissance(ex1_test,pow)
reg_ex1_d2 = glm(Classe~., data = ex1_d2_app, family = binomial(link = 'logit'))
remplissage(ex1_app, ex1_val, reg_ex1_d2,pow,"red","blue",-1,1.2,-0.85,1.2)
AVTpow=calcul_erreur(ex1_d2_app, ex1_d2_val, ex1_d2_test, reg_ex1_d2, ncol(ex1_d2_app))
AVTpow

pow = c(2:3)
pow
ex1_d2_app = puissance(ex1_app,pow)
head(ex1_d2_app)
ex1_d2_val = puissance(ex1_val,pow)
ex1_d2_test = puissance(ex1_test,pow)
reg_ex1_d2 = glm(Classe~., data = ex1_d2_app, family = binomial(link = 'logit'))
remplissage(ex1_app, ex1_val, reg_ex1_d2,pow,"red","blue",-1,1.2,-0.85,1.2)
AVTpow=calcul_erreur(ex1_d2_app, ex1_d2_val, ex1_d2_test, reg_ex1_d2, ncol(ex1_d2_app))
AVTpow

pow = c(2:6)
pow
ex1_d2_app = puissance(ex1_app,pow)
head(ex1_d2_app)
ex1_d2_val = puissance(ex1_val,pow)
ex1_d2_test = puissance(ex1_test,pow)
reg_ex1_d2 = glm(Classe~., data = ex1_d2_app, family = binomial(link = 'logit'))
remplissage(ex1_app, ex1_val, reg_ex1_d2,pow,"red","blue",-1,1.2,-0.85,1.2)
AVTpow=calcul_erreur(ex1_d2_app, ex1_d2_val, ex1_d2_test, reg_ex1_d2, ncol(ex1_d2_app))
AVTpow

pow = c(2:7)
pow
ex1_d2_app = puissance(ex1_app,pow)
head(ex1_d2_app)
ex1_d2_val = puissance(ex1_val,pow)
ex1_d2_test = puissance(ex1_test,pow)
reg_ex1_d2 = glm(Classe~., data = ex1_d2_app, family = binomial(link = 'logit'))
remplissage(ex1_app, ex1_val, reg_ex1_d2,pow,"red","blue",-1,1.2,-0.85,1.2)
AVTpow=calcul_erreur(ex1_d2_app, ex1_d2_val, ex1_d2_test, reg_ex1_d2, ncol(ex1_d2_app))
AVTpow

pow = c(2:8)
pow
ex1_d2_app = puissance(ex1_app,pow)
head(ex1_d2_app)
ex1_d2_val = puissance(ex1_val,pow)
ex1_d2_test = puissance(ex1_test,pow)
reg_ex1_d2 = glm(Classe~., data = ex1_d2_app, family = binomial(link = 'logit'))
remplissage(ex1_app, ex1_val, reg_ex1_d2,pow,"red","blue",-1,1.2,-0.85,1.2)
AVTpow=calcul_erreur(ex1_d2_app, ex1_d2_val, ex1_d2_test, reg_ex1_d2, ncol(ex1_d2_app))
AVTpow

pow = c(2:10)
pow
ex1_d2_app = puissance(ex1_app,pow)
head(ex1_d2_app)
ex1_d2_val = puissance(ex1_val,pow)
ex1_d2_test = puissance(ex1_test,pow)
reg_ex1_d2 = glm(Classe~., data = ex1_d2_app, family = binomial(link = 'logit'))
remplissage(ex1_app, ex1_val, reg_ex1_d2,pow,"red","blue",-1,1.2,-0.85,1.2)
AVTpow=calcul_erreur(ex1_d2_app, ex1_d2_val, ex1_d2_test, reg_ex1_d2, ncol(ex1_d2_app))
AVTpow

pow = c(2:100)
pow
ex1_d2_app = puissance(ex1_app,pow)
head(ex1_d2_app)
ex1_d2_val = puissance(ex1_val,pow)
ex1_d2_test = puissance(ex1_test,pow)
reg_ex1_d2 = glm(Classe~., data = ex1_d2_app, family = binomial(link = 'logit'))
remplissage(ex1_app, ex1_val, reg_ex1_d2,pow,"red","blue",-1,1.2,-0.85,1.2)
AVTpow=calcul_erreur(ex1_d2_app, ex1_d2_val, ex1_d2_test, reg_ex1_d2, ncol(ex1_d2_app))
AVTpow
#


# Quel est le meilleur modèle de régression logistique que vous choisissez pour ces données ?
#c(2,3)
# Quel estimation de l'erreur de généalisation de ce modèle pouvez-vous faire ?
#6 

###################### Exercice 2 #########################
# Utilisation d'un jeu de données réel : images en noir et blancs de chiffres manuscrits
# Régression multi-classes (et non plus binaire)
# Individus décrits par beaucoup de descripteurs
# 2 parties dans cet exercice :
#  1) vous allez utiliser les images telles quelles (une image sera représentée par un vecteur contenant la valeur de ses pixels (0 ou 1))
#  2) ensuite on transformera chaque image en une représentation vectorielle plus adaptée au problème (cf cours de Thomas Corpetti)
#   Vous verrez que les performances de reconnaissance de chiffres sont meilleures en utilisant une telle représentation.
# Vous pourrez aussi créer vos propres images de chiffres manuscrits et tester si le modèle marche sur vos fichiers.

##### Partie 1 


# Chargement des données (on a déjà fait pour vous 3 ensembles A/V/T)
cp_app= read.table("./chiffres_app.txt", header=T)
cp_val= read.table("./chiffres_val.txt", header=T)
cp_test= read.table("./chiffres_test.txt", header=T)

# ----- Les donnees

# Regardez la dimension de l'ensemble d'apprentissage
dim(cp_app)
#ligne : 150 colonne : 785

# Il comporte 150 lignes (qui correspondent à 150 images de chiffres). Ces chiffres sont représentés par
# 784 pixels (image de taille 28*28). Les colonnes pixel0, pixel1, etc... représentent les valeurs de pixels pour chaque image (0 ou 1, noir ou blanc). La colonne label représente le chiffre qui est inscrit sur l'image (de 1 à 10, sachant que la classe 10 représente les zéros).
head(cp_app)


# Question : Quelle est la valeur du 7ème pixel de l'image 4 de l'ensemble d'apprentissage ? Aide : il faut aller chercher l'élément à la ligne 4 et colonne 8 (car la premiere colonne est le label) de cp_app
cp_app[4,8]

# Quel est le chiffre représenté sur l'image 4 de l'ensemble d'apprentissage ? Ligne 4 colonne 1 ...
cp_app[4,1]
# Quelles sont les dimensions des ensembles de validation et de test ?
dim(cp_test)
dim(cp_val)
# ----- Visualisation des images 

# La fonction display du fichier tp1_RegLog.R permet de visualiser les images que l'on a à notre disposition
idx = 8# pour afficher la 4ème image (vous pourrez changer)

display(matrix(as.numeric(cp_app[idx,2:785]), byrow = T,ncol = 28)) 



# ----- Apprentissage du modèle

# On utilise maintenant la fonction learn_nn (de nnets.R) car glm ne fonctionne que pour de la classification binaire (deux classes)
# Cette fonction apprend un modèle de régression logistique multi-classes (donc plusieurs modèles de régression logistique) à partir d'un ensemble d'apprentissage
# Les paramètres de cette fonction sont: 
# 1) une matrice contenant les données d'apprentissage (sans les classes). Individus en lignes 
# 2) un vecteur contenant les classes des individus de l'ensemble d'apprentissage (dans l'ordre de la matrice du premier paramètre)
# 3) lambda, pour l'instant ce sera toujours 0 ( on verra si on a le temps d'en parler)
# 4) nombre d'itérations pour trouver le meilleur modèle. Ici j'ai mis 100 car les données sont volumineuses donc c'est long. On pourra mettre une valeur plus grande dans d'autres circonstances.
# 5) un vecteur avec 2 valeurs : nombre de variables d'entrée (ici 784 pixels) et nombre de classes distinctes (ici 10 car dix chiffres)
# Lancez cette commande et soyez un peu patients (cela prend 1 minute environ). C'est moi qui l'ai implementée et je n'ai pas du tout fait au plus efficace! 
RL_chiffres = learn_nn(cp_app[,2:785], cp_app$label, 0,100, c(784, 10))
# Le modèle appris est stocké dans RL_chiffres[[1]]. C'est une matrice qui comporte les coefficients du modèle appris.

# Question : Combien de coefficients comportent ce modèle ? Sans regarder RL_chiffres[[1]] ...
#785
# Vérifiez en affichant la dimension de cette matrice de coefficient :
dim(RL_chiffres[[1]]) 
RL_chiffres



# ----- Performance du modèle sur le jeu d'apprentissage

# La fonction calcul_sorties permet de calculer les 10 sorties du modèle (qui correspondent aux 10 chiffres possibles)
# pour une image (représentée par son vecteur de 784 pixels)
# Paramètres : 
# 1) le modèle appris par learn_nn
# 2) la ou les images que l'on souhaite classifier (matrice avec les individus en ligne)

# Question : Calculer les sorties du modèle lorsque l'on tente de prédire la classe de l'image numéro 2 du jeu d'apprentissage
# Quel chiffre allez-vous prédire pour cette image ?
calcul_sorties(RL_chiffres,cp_app[2,2:785])
#Dans la prediction 10=0
# 0 car il a le score le plus �l�v�
display(matrix(as.numeric(cp_app[2,2:785]), byrow = T,ncol = 28)) 
# Ecrivez une commande qui permet de calculer automatiquement une décision à partir du vecteur contenant les sorties du modèle.
# Aide = which.max(...)

which.max(calcul_sorties(RL_chiffres,cp_app[2,2:785]))

# Question : prédire les classes de toutes les images du jeu d'apprentissage

calcul_sorties(RL_chiffres,cp_app[,2:785])

# Question :
# Quelle est la vraie classe de la 3 ème image ? (utilisez display ou bien regarder le label de la 3 ème ligne de cp_app)
display(matrix(as.numeric(cp_app[3,2:785]), byrow = T,ncol = 28)) 
#4
# Quelle est la prédiction faite par le modèle pour cette même image ?
which.max(calcul_sorties(RL_chiffres,cp_app[3,2:785]))
#4
# Calculer le nombre de bonnes prédictions que fait ce modèle sur le jeu d'apprentissage
zz=(calcul_sorties(RL_chiffres,cp_app[,2:785]))
# A FAIRE
predict_cp_app=apply(zz,2,which.max)
# Donner la matrice de confusion associée au jeu d'apprentissage
# A FAIRE
Mat_val=table(predict_cp_app,cp_app$label)
Mat_val
# ----- Performance du modèle sur le jeu de validation

# Calculer le nombre (et le proucentage) de bonnes prédictions sur l'ensemble de validation
# A FAIRE
zp=(calcul_sorties(RL_chiffres,cp_val[,2:785]))
predict_cp_val=apply(zp,2,which.max)
Mat_v=table(predict_cp_val,cp_val$label)
Mat_v
predict_cp_val
cp_val[,1]
Nbre_ok_val=sum( predict_cp_val==cp_val[,1])
Nbre_ok_val
# Donner la matrice de confusion associée au jeu de validation
# A FAIRE
Mat_val2=table(predict_cp_val,cp_val[,1])
Mat_val2
# Quels sont les chiffres qui sont le plus souvent confondus par ce modèle ?
# 7 et 9
# Affichez des instances de ces chiffres correctement classées. De même avec des instances incorrectement classées. (avec la commande display)
display(matrix(as.numeric(cp_app[4,2:785]), byrow = T,ncol = 28)) 

# ----- Performance du modèle sur le jeu de test
# Idem pour l'ensemble de test (calculer le nb d'erreur)

## Pour améliorer ce modèle, on pourrait tenter d'élever les données à des puissances mais ici les descripteurs sont binaires (0 ou 1), donc
# les puissances ne feront rien.
# Pour améliorer les performances de reconnaissance de chiffres, on va plutôt utiliser une représentation des images plus adaptée.`

# ----- Création de nouveaux chiffres et prédiction

# Vous pouvez tester votre modèle sur des chiffres que vous allez créer vous-mêmes.
# Ouvrez un logiciel de dessin (Paint ou autre)
# Créez un nouveau document de taille 28*28
# Dessinez un chiffre en essayant de le centrer dans un carré de dimension 20*20 
# Sauvegardez ce dessin au format PNG (test1.png par exemple) et mettez le dans votre répertoire de travail.


# Lecture du fichier PNG dans R
monimage = readPNG("test1.png")[,,1] # si probleme essayez monimage = readPNG("test1.png")
monimage = readPNG("test1.png")
monimage
dim(monimage)
#Que represente le 3 ?
# Verifiez que la dimension est bien 28*28
# On met l'image sous forme d'un vecteur
monimage = matrix(t(monimage), nrow = 1)
monimage
# Et on la classifie en utilisant le modele 
source("./fonctions_tp_RegLog.R")
calcul_sorties(RL_chiffres,cp_app[2,2:785])
dim(cp_app)
dim(monimage)
calcul_sorties(RL_chiffres, monimage)
#ici c'est un quatre car il a la plus grande proba
# etc


### Partie 2 : utilisation de la transformation HOG (histogram of oriented gradients, cf Wiki pour infos)

# Il faut d'abord installer le package OpenImageR
install.packages("OpenImageR")
library(OpenImageR)

# HOG transforme une image en 1 vecteur qui contient des informations sur la direction des contours présents dans l'image (sous forme d'histogramme)
# 2 paramètres :
# - cells le nombre de cases que l'on considère dans l'image (l'image est séparée en n*n carrés et un histogramme est calculé par carrés)
# - orientations le nombre d'orientations possibles (plus il y en a plus la transformation est informative mais plus le vecteur résultant est grand)

# On va essayer de transformer la première image de notre jeu d'apprentissage en demandant 3 cells et 8 orientations
display(matrix(as.numeric(cp_app[1,2:785]), byrow = T,ncol = 28)) 
#l'image est un 3'
cp_app[1,2:785]
h = HOG(matrix(as.numeric(cp_app[1,2:785]), nrow = 28, byrow = T), cells = 3, orientations = 8)
h
# matrix(as.numeric(cp_app[1,2:785]), nrow = 28, byrow = T) correspond à la remise en forme de l'image 1 sous forme de matrice 28*28 pixels

# Question : quelle est la taille de la transformation de l'image ? Est ce normal ?
dim(h)
h
# On va maintenant transormer toutes les images de nos ensembles pour créer 3 nouveaux ensembles app/val/test
cells = 3
orientations = 8 
# Vous pourrez changer plus tard 
# je pense que c'est mieux les iphone prends du temps pour se rallumer ok 
#ok c'est bon bonne nuit on bosse demain a la meme heure moi je vais continuer un peu'
hog_app = matrix(0,nrow(cp_app),1+cells*cells*orientations)
for(i in 1:nrow(cp_app)){hog_app[i,] = c(cp_app$label[i],HOG(matrix(as.numeric(cp_app[i,2:785]), nrow = 28, byrow = T), cells = cells, orientations = orientations))}

hog_val = matrix(0,nrow(cp_val),1+cells*cells*orientations)


for(i in 1:nrow(cp_val)){hog_val[i,] = c(cp_val$label[i],HOG(matrix(as.numeric(cp_val[i,2:785]), nrow = 28, byrow = T), cells = cells, orientations = orientations))}

hog_test = matrix(0,nrow(cp_test),1+cells*cells*orientations)
for(i in 1:nrow(cp_test)){hog_test[i,] = c(cp_test$label[i],HOG(matrix(as.numeric(cp_test[i,2:785]), nrow = 28, byrow = T), cells = cells, orientations = orientations))}

# Nos ensembles sont maintenant hog_app, hog_val, hog_test
hog_app
# Créer un modèle de regression logistique en utilisant l'ensemble d'apprentissage.
ex1_app
reg_hog = glm(Classe~., data = hog_app, family = binomial(link = 'logit'))
reg_hog
# Calculer les performances de ce modèle (nombre d'erreurs) sur l'ensemble d'apprentissage et de validation.
# Le modèle est-il meilleur que celui qui utilise les images brutes?


# Vous pourrez essayer de prédire vos images (surtout celles qui ne fonctionnait pas bien avec le modèle d'avant).
# Ca marchera parfois un peu mieux.


# Vous pourrez aussi ensuite changer les valeurs de cells et de orientations pour tenter de trouver un meilleur modèle.
# Puis mettre des puissances eventuellement.

