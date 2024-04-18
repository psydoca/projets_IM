#creation matrice

X<-matrix(c(rnorm(50,0,1),rnorm(50,2,1),rnorm(50,4,1),rnorm(50,6,1)),ncol=2,byrow=TRUE)
Y<-c(rep(0,25),rep(1,25),rep(2,25),rep(3,25))
Data<-cbind(X,Y)

#création de la fonction des k plus proches voisins(TP1)
Kppv = function(Data,k,x){
  X=Data[,-ncol(Data)]
  Y=Data[,ncol(Data)]
  c=x
  for (i in 2:length(X[,1])) {
    c=rbind(c,x)
  }
  d=c-X
  norm=apply(d,1,function(x) sqrt(sum(x**2)))
  voisinproche=order(c(norm))
  voisinproche=c(voisinproche[1:k])
  kpp=c()
  for(i in voisinproche){
    kpp=c(kpp,Y[i])
  }
  freq = table(kpp)
  max_occurrence<-as.integer((names(freq)[which.max(freq)]))
  return((max_occurrence))
}

#formule du TP2

ErreurEmpirique<-function(Data,k,p){
  v=sample(1:nrow(Data),round(nrow(Data)*p))
  Data_EA=Data[v,]
  Data_ET=Data[-v,]
  X_ET=Data_ET[,-ncol(Data_ET)]
  Y_ET=Data_ET[,ncol(Data_ET)]
  Ynew=apply(X_ET,1,Kppv,Data=Data_EA,k=k)
  MC = mean(Ynew!=Y_ET)
  return(MC)
}
#calcul k optimal et son erreur
k_optimal<-function(Data,p){
  kmax=floor(2*sqrt(p*nrow(Data)))
  min_err=1
  Data_mélangé<-Data[sample(nrow(Data)),]
  for(i in 1:kmax){
    if( ErreurEmpirique(Data_mélangé,i,p)<min_err){
      min_err = ErreurEmpirique(Data_mélangé,i,p)
      k=i
    }
  } 
  list(k,min_err)
}
#méthode de monte Carlo pour approximer k et l'erreur pour un holdout
MC_kop<-function(Data,p){
  mkopt=c()
  merr=c()
  for (i in 1:50){
    mkopt=c(mkopt,k_optimal(Data,p)[[1]])
    merr=c(merr,k_optimal(Data,p)[[2]])
  }
  return(list(mkopt,merr,mean(mkopt),mean(merr)))
}
#fonction du TP3
#adaptation de l'erreur empirique pour les vfold
ErreurEmpirique_v<-function(Data,k,v){
  Data<-Data[sample(nrow(Data)),]
  longueur=round(nrow(Data)*(1/v))
  if(longueur*v== nrow(Data)){   # cas ou toutes les séquences sont de meme longueurs
    cpt=0
    for (i in 1:v){
      Data_ET=Data[seq(longueur*(i-1),longueur*i),]
      Data_EA=Data[-seq(longueur*(i-1),longueur*i),]
      X_ET=Data_ET[,-ncol(Data_ET)]
      Y_ET=Data_ET[,ncol(Data_ET)]
      Ynew=apply(X_ET,1,Kppv,Data=Data_EA,k=k)
      cpt = cpt+ mean(Ynew!=Y_ET)
    }
  }
  else{   # cas ou la derniere sequence n'est pas de même taille
    cpt=0
    Data_ET=Data[seq(longueur*(v-1),nrow(Data)),]
    Data_EA=Data[-seq(longueur*(v-1),nrow(Data)),]
    X_ET=Data_ET[,-ncol(Data_ET)]
    Y_ET=Data_ET[,ncol(Data_ET)]
    Ynew=apply(X_ET,1,Kppv,Data=Data_EA,k=k)
    cpt = cpt+ mean(Ynew!=Y_ET)
    for(i in 1:(v-1)){
      Data_ET=Data[seq(longueur*(i-1),longueur*i),]
      Data_EA=Data[-seq(longueur*(i),longueur*i),]
      X_ET=Data_ET[,-ncol(Data_ET)]
      Y_ET=Data_ET[,ncol(Data_ET)]
      Ynew=apply(X_ET,1,Kppv,Data=Data_EA,k=k)
      cpt = cpt+ mean(Ynew!=Y_ET)
    }
  }
  return(cpt/v)
}
# renvoie le k ayant la plus petite erreur 
k_optimal_v<-function(Data,v){
  kmax=floor(2*sqrt((1/v)*nrow(Data)))
  min_err=1
  for(i in 1:kmax){
    if( ErreurEmpirique_v(Data,i,v)<min_err){
      min_err = ErreurEmpirique_v(Data,i,v)
      k=i
    }
  } 
  list(k,min_err)
}
#méthode de MonteCarlo des kop
MC_kop_v<-function(Data,v){
  mkopt=c()
  merr=c()
  for (i in 1:50){   #on fait une boucle sur 25 pour alléger le temps de calcul
    mkopt=c(mkopt,k_optimal_v(Data,v)[[1]])
    merr=c(merr,k_optimal_v(Data,v)[[2]])
  }
  return(list(mkopt,merr,mean(mkopt),mean(merr)))
}



#par des soucis de calculs trop long j'ai preferé  me suis concentrer sur deux boites à moustaches
boxplot_croise<-function(Data,v,d){
  L1<-MC_kop_v(Data,v)
  L2<-MC_kop(Data,1-(1/v))# p=1-(1/v) pour avoir un hold out avec la meme taille d'apprentissage que le vfold
  temp_execution_L1<-system.time(MC_kop_v(Data,v))
  temp_execution_L2<-system.time(MC_kop(Data,1-(1/v)))
  if(d==0){ # d == 0 pour avoir les boites a moustaches selon k 
  boxplot(L1[[1]],L2[[1]],names=c("v_fold","Holdout"),main='koptimal')
  }
  else{
  boxplot(L1[[2]],L2[[2]],names=c("v_fold","Holdout"),main='erreur')
    
  }
  
  
  return(list(temp_execution_L1,temp_execution_L2))
  }

  