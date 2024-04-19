#commencer par charger les fonctions précédentes
# ATTENTION : J'AI MODIFIE LES FICHIERS PRECEDENTS SUR MOODL, PENSEZ A LES RE TELECHARGER!!
# PROGRAMMES A TROU A COMPLETER

rm(list=ls())
source("TP_Fuzzy.R")
source("TP_Vfold.R")

#### V-FOLD ####
#Premiere fonction : calcul de l'erreur

CalculErreurFuzzyVFold=function(Data,k,gamma=2,v=5){
	...		 			#Nombre de donnees
	...		 			#Nombre de colonnes
	...		 			#nombre de donnees dans chaque sous echantillon (sauf le dernier)
	...		 			#mélange de l'échantillon
	ERROR=rep(0,v) 				#pour rentrer les erreurs sous echantillon
	for(i in (1:(v-1))) 			#Pour les v-1 premiers morceaux
		{
		indices=...								#les indices des donnees a selectionner 
		DataAppr=...								#echantillon de validation = ieme paquet
	 	DataTest=...								#echantillon d'apprentissage = tout sauf ieme paquet
 		DataTest=DataTest[order(DataTest[,ncol(DataTest)]),]			#Ordonner data test selon les valeurs croissantes des label
		donnees=DataTest[,-NbCol]						#On reprend le calcul d'erreur comme dans le hold out
		UVrai=...								#Creation de la vraie matrice U
  		fuzzypred=apply(donnees,1,Fuzzyknn,Data=DataAppr,k=k,gamma=gamma,U=0)	#Creation de la prediction floue
		Upred=...								#Creation de la prediction "defloutee"
		ERROR[i]=sum(Upred!=Unew)/(...)						#Calcul de l'erreur
		}
	indices=((v-1)*NbSubData+1):NbData 	#Pour le dernier paquet (peut etre plus grand que les autres)	
	...					#on refait pareil que dans la boucle
	...
	...
	...	
	...
  	...
  	...
	ERROR[v]=...
	return(mean(ERROR))
}

#Deuxieme fonction : selection du meilleur K
MeilleurKFuzzyVFold=function(Data,v=5,vectk=2:floor(nrow(Data)/v),gamma=2){
	...
}

##### Evaluation de l'erreur ####
#Premiere fonction pour simplifier l'evaluation de l'erreur finale
CalculErreurFinalFuzzy=function(DataAppr,DataTest,k,gamma=2){
	DataAppr=... 		#reordonner selon les valeurs croissante des label
	DataTest=... 		#reordonner selon les valeurs croissante des label
	donnees=...
	Unew=...
	fuzzypred=...
  	...			#Calcul final de l'erreur
}


#On va comparer les erreurs de Hold out de de differents vfold
#Parametres de la fonction :
#	Nbdata le nombre de donnees
#	NbIter le nombre de répétitions
#	ProportionAppr les differentes proportions de donnees gardees pour l'echantillon d'apprentissage (on en deduira le v)
#	gamma

EvaluateFuzzy=function(NbData=500,NbDataFinal=500,NbIter=50,proportionAppr=c(0.5,0.66,0.8),gamma=2){
#on commence par calculer les v correspondant au proportions du Hold out
	v=sort(round(1/(1-proportionAppr)))
#on va ranger les resultats dans des matrices 
	SelectedK=matrix(...)
	Error=SelectedK
#Simulation du jeu de donnees servant à l'evaluation
	FinalData=simulnormb(NbDataFinal)
	FinalData=... 						#reordonner selon les valeurs croissante des label
#Boucle sur les iterations (celle qui sera paralellisable)
	for(i in 1:NbIter)
		{
#on simule un jeu de donnees
		Data=simulnormb(NbData)
		Data=... 					#reordonner selon les valeurs croissante des label
#on selectionne les meilleurs k
		SelectedVfold=... 				#meilleur k pour chaque valeur de v
		SelectedK[i,]=SelectedVfold
#on calculr les erreurs finales associees au differents k choisis
		Error[i,]=...					#on applique la fonction de calcul d'erreur definie au dessus pour les k choisis
		print(paste(100*i/NbIter,"Percent Done"))
		}
#on renomme les colonnes des matrices
	colnames(SelectedK)=paste("k pour ",v,"fold")
	colnames(Error)=paste("E pour ",v,"fold")
	return(list(SelectedK=SelectedK,Error=Error))
	
}

resfuzzy=EvaluateFuzzy(120,100,30,0.5)
res=Evaluate(120,100,30,0.5)
boxplot(cbind(res$Error,resfuzzy$Error))

