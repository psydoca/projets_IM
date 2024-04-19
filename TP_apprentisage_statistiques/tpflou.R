# createUcorr = function(data){
#   Nbcol = ncol(data)
#   Nbdata= nrow(data)
#   data=data[order(data[,Nbcol]),]
#   label = data[,Nbcol]
#   uniquelabel = unique(label)
#   nblabel = length(uniquelabel)
#   U = matrix(0,ncol=Nbdata,nrow=nblabel)
#   for (i in 1:nblabel){
#     U[i,]= as.numeric(label == uniquelabel[i])
#   }
# }
# fuzziknn = function(x,Data,k,lbd,U=0){
#   Nbcol = ncol(data)
#   Nbdata= nrow(data)
#   data=data[order(data[,Nbcol]),]
#   donnee = Data[,-Nbcol]
#   label = Data[,nbcol]
#   uniquelabel = unique(label)
#   nblabel = length(uniquelabel)
#   if (U==0){U=createUcorr(Data)}
#   D = apply(donnee,matrix(x,Nbdata),ncol=Nbcol-1,byrow = T)
#   ODk = order(D)[1:k]
#   app = rep(0,nblabel)
#   for (i in 1:nblabel){
#     app[i ] =
#   }
# }
source("C:/Users/jorda/OneDrive/Bureau/Fac/M1_IM/S2/APPRENTISSAGE STATS/Generation donnees.R")


X<-matrix(c(rnorm(50,0,1),rnorm(50,2,1),rnorm(50,4,1),rnorm(50,6,1)),ncol=2,byrow=TRUE)
Y<-c(rep(0,25),rep(4,25),rep(6,25),rep(3,25))
Data<-cbind(X,Y)

NormEucl=function(x){sqrt(sum(x^2))}

create_U = function (Data){
  #browser()
  label_obs = unique(Data[,ncol(Data)])
  label_obs = sort(label_obs)
  U = matrix(0,nrow= length(label_obs),ncol = nrow(Data))
  for (j in 1: nrow(Data)){
    for (i in label_obs){
      if (i == Data[j,ncol(Data)]) {
        U[which(label_obs==i),j] = 1
      }
    }
  }
  return ( U)
}
create_U(Data)

create_Upred = function (Data,x,U,k,lbd){
   #browser()
  Upred = matrix(0, nrow= nrow(U),ncol = 1)
  X = Data[,-ncol(Data)]
  c=rep(x,nrow(X))
  d=c-X
  D=apply(d,1,NormEucl)
  voisin = order(D)[1:k]
  norm_voisin = sort(D)[1:k]
  for (i in 1:nrow(Upred)){
    a=0
    b=0
      for (j in 1:length(voisin)){
        if (norm_voisin[j]==0){
          a=a + U[i,voisin[j]] * (1/(1e-10)**(2/(lbd-1)))
          b= b + (1/(1e-10)**(2/(lbd-1)))
        }
        else{
          a=a + U[i,voisin[j]] * (1/(norm_voisin[j])**(2/(lbd-1)))
          b= b + (1/(norm_voisin[j])**(2/(lbd-1)))
            }
        }
    Upred[i,1]= a/b
  }
  return(Upred)
}
x =rnorm(2,4,1)
A=create_Upred(Data,x,create_U(Data),3,2)

Xbis<-matrix(c(rnorm(50,0,1),rnorm(50,2,3),rnorm(50,4,1),rnorm(50,6,2)),ncol=2,byrow=TRUE)

create_upred2 = function(Data, X, k, lbd){
  #browser()
  U = create_U(Data)
  Upred = apply(X,1,function(x)create_Upred(Data,x,U,k,lbd))
  row.names(Upred)= paste('Label',sort(unique(Data[,ncol(Data)])) )
  return(Upred)
}

A=create_upred2(Data,Xbis,3,2)

# if(kmax==0){kmax=floor(sqrt(proportion*nrow(Data)))}
# k_vect = 1:kmax
# lbd_vect = 2:kmax

library(stringr)
holdout_fuzzy = function (Data,k,lbd,proportion = 0.8){
  browser()
  v=sample(1:nrow(Data),floor(nrow(Data)*proportion))
  Data_app = Data[v,]
  X_test = Data[-v,-ncol(Data)]
  Y_test = Data[-v,ncol(Data)]
  holdout_prediction = create_upred2(Data_app, X_test, k, lbd)
  Ypred = c()
  erreur=c()
  for (j in 1:ncol(holdout_prediction)){
    Ypred_label = names(which.max(holdout_prediction[,j]))
    Ypred = c(Ypred,as.integer(str_extract_all(Ypred_label, "\\d+")[[1]]) )}
  erreur =c(erreur,mean(Ypred!=Y_test))
  
  return (erreur)
}


B = holdout_fuzzy(data,2,3)

vfold_fuzzy <- function(Data, k, lbd, folds = 5) {
  #browser()
  # Calculer le nombre d'observations par pli
  fold_size <- as.integer(nrow(Data)/ folds) 
  indice <- sample(1:nrow(Data))
  predictions <- list()
  erreur=c()
  for (i in 1:folds) {
    indice_folds = indice[(((i - 1) * fold_size) + 1): (i * fold_size)]
    if (i ==folds){
      indice_folds = indice[(((i - 1) * fold_size) + 1):nrow(Data)]}
    test_indice = indice[indice_folds]
    train_indice = indice[-indice_folds]
    train_data = Data[train_indice, ]
    test_data = Data[test_indice, ]
    Y_test = Data[test_indice,ncol(Data)]
    fold_predictions <- create_upred2(train_data, test_data[, -ncol(test_data)], k, lbd)
    Ypred = c()
    for (j in 1:ncol(fold_predictions)){
      Ypred_label = names(which.max(fold_predictions[,j]))
      Ypred = c(Ypred,as.integer(substr(Ypred_label,nchar(Ypred_label),nchar(Ypred_label)) ) )}
    erreur =c(erreur,mean(Ypred!=Y_test))
    predictions[[i]] <- fold_predictions
  }
  return(mean(erreur))
  #return(erreur)
}
A = vfold_fuzzy(Data,3,2,3)


vfold_fuzzy_meilleur= function(Data,kmax=0,lbdmax=10,folds = 10){
  #browser()
  if(kmax==0){kmax=floor(sqrt((1/folds)*nrow(Data)))}
  vect_fold=2:folds
  vect_k = 1:kmax
  vect_lbd = 2:lbdmax
  Matrice_erreur = matrix(0,nrow = length(vect_lbd) ,ncol = length(vect_k))
  liste_mat = list()
  for (a in 1:length(vect_fold)){
    for (j in 1:length(vect_k)){
      for (i in 1:length(vect_lbd)){
        Matrice_erreur[i,j] = vfold_fuzzy(Data,vect_k[j],vect_lbd[i],vect_fold[a])
      }
    }
    row.names(Matrice_erreur)=vect_lbd
    colnames(Matrice_erreur)=paste('k:',vect_k)
    liste_mat[[a]] = Matrice_erreur
  }
  return(liste_mat)
}


A=vfold_fuzzy_meilleur(Data,folds =4)

par(mfrow=c(2,2))
for (i in 1:length(A)){
  boxplot(A[[i]],main = paste('Avec ',i+1,' folds'))
}
for (i in 1:length(A)){
  boxplot(t(A[[i]]),main = paste('Avec ',i+1,' folds'),xlab='Lambda')
}


