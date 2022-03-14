#Exercice 2
#1- Création de la fonction qui permet de calculer la moyenne

moyenne <- function(vecteur) {
  x = 0
  for (i in vecteur) x = x+i    #Je somme les éléments du vecteur
  n = length(vecteur)
  return(x/n)   #je retourne la somme divisé par le nbr d'éléments
}
#Vérification
moyenne(c(1, 3, 5))


#Création de la variance
variance <- function(vecteur) {
  vect = vecteur - moyenne(vecteur)   #je crée uun nouveau vecteur constitué des modalités moins la moyenne
  x = 0
  for (i in vect) x = x + i^2   #Je fais la somme des elements du nouveau vecteur
  n = length(vecteur)     #Je fais la somme totale divisée par le nbr d'éléments du vecteur moins 1
  return(x/n)
}
#Vérification
variance(c(2, 4, 4, 4, 5, 5, 7, 9))
var(c(2, 4, 4, 4, 5, 5, 7, 9))

#Calcul de l'écart type
ecart_type <- function(vecteur) {
  return(sqrt(variance(vecteur)))   #Il suffit de considérer la racine carrée de la variance calculée précédemment
}
#Vérification
ecart_type(c(2, 4, 4, 4, 5, 5, 7, 9))
sd(c(2, 4, 4, 4, 5, 5, 7, 9))

#La médiane
mediane <- function(vecteur) {
  sort(vecteur, decreasing = FALSE)
  if (length(vecteur) %% 2 == 0) {   #V�rifie si le nombre de modalit�s est pair
    i = length(vecteur) / 2
    return(vecteur[i])
  }else {
    i = (length(vecteur)+1)/ 2    #V�rifie si le nombnre de modalit�s est impair
    return(vecteur[i])
  }
}
mediane(c(2, 4, 4, 4, 5, 5, 7, 9))

#la covariance
covariance <- function(vecteur1, vecteur2){
  vect1 = vecteur1 - mean(vecteur1)
  vect2 = vecteur2 - mean(vecteur2)
  x = 0
  for (i in 1:length(vect1)){
    x = x + vect1[i]*vect2[i]
  }
  return(x/length(vect2)-2)
}

covariance(c(1, 5, 7, 8), c(6, 9, 0, 5))
cov(c(1, 5, 7, 8), c(6, 9, 0, 5))

#2- R�solution d'�quation du second d�gr�
equation <- function(a, b, c){
  x = 0
  y = 0
  discr = b^2 - 4*a*c
  if (discr == 0) {
    x = -b/(2*a)
    print(paste("L'unique solution est ", x))
  } else if (discr<0) {
    x = complex(real = -b/(2*a), imaginary = sqrt(-discr)/(2*a))
    y = complex(real = -b/(2*a), imaginary = -sqrt(-discr)/(2*a))
    print(paste(...="Les solutions de l'�quation sont ", x, "et ", y))
    } else {
    x = (-b + sqrt(discr))/(2*a)
    y = (-b - sqrt(discr))/(2*a)
    print(paste(...="Les solutions de l'�quation sont ", x, "et ", y))
      }
}

#3- Calcul du factoriel d'un nombre
factoriel <- function(n) {
  x = 1
  for (i in 1:n) x = x*i
  return(x)
}
factoriel(4)
