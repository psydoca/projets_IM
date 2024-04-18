Data<- read.csv("analyse_de_données/données/Data_Projet_1.csv",sep=",",dec=".",header=TRUE, stringsAsFactors = T)
Data<-subset(Data,select= -claim_id)
Data<-subset(Data,select= -customer_id)


library(ROSE)
set.seed(10)
Data<-ovun.sample(fraudulent~.,data=Data,method="over",N=1600)$data#equilibre les données pour qu'il y ait environ 50/50 fraudulent ou non

fraud_EA<-Data[1:1200,]
fraud_ET<-Data[1201:1600,]
View(Data)
pie(table(Data$fraudulent),main = "Répartition des classes")


#histogramme des fraudes selon les types de données
qplot(age,data= Data,bins=12,fill=fraudulent)
#entre 18 et 20 ans bcp de fraudes
qplot(gender,data= Data,fill=fraudulent)
#n
qplot(incident_cause,data= Data,fill=fraudulent)
#n
qplot(claim_area,data= Data,fill=fraudulent)
#n
qplot(days_to_incident,data=Data,fill=fraudulent)
#2/3 des incident traites le premier jour sont des fraudes
qplot(police_report,data= Data,fill=fraudulent)
#si oui, moins de fraudes et plus si non
qplot(claim_type,data= Data,fill=fraudulent)
#injury only moins de fraudes
qplot(claim_amount,data= Data,fill=fraudulent)
#>45k que des fraudes et en dessous de 20k presque aucune fraude
qplot(total_policy_claims,data= Data,fill=fraudulent)
# a partir de 4 + de fraudes

#nuage de points en fonctions de l'age

qplot(age,gender,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(age,incident_cause,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(age,claim_area,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(age,days_to_incident,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(age,police_report,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(age,claim_type,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
# 20aine plus de fraude pour material and injury
qplot(age,claim_amount,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(age,total_policy_claims,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#40ans plus d'une decla d'accident souvent fraudes

#Pas grand chose a tirer de l'age si ce nest qu'entre 18 et 20 ans les fraudes sont plus fréquentes et qu'aux alentours de 40 ans les fraudes le sont plus aussi a partir de deux déclas

#nuage de points en fonctions de le gender

qplot(gender,age,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(gender,incident_cause,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(gender,claim_area,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.250)
#n
qplot(gender,days_to_incident,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(gender,police_report,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(gender,claim_type,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(gender,claim_amount,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(gender,total_policy_claims,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n

# la variable gender n'a aucun poids, elle peut etre enlever

#nuage de points en fonctions de le incident_cause

qplot(incident_cause,age,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(gender,incident_cause,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(incident_cause,claim_area,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(incident_cause,days_to_incident,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(incident_cause,police_report,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#police report yes /driver error bcp de non fraudulent,pr yes/Crime fraudulent ++
qplot(incident_cause,claim_type,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#crime/MandI fraud++,Crime/injury only nfraud++
qplot(incident_cause,claim_amount,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(incident_cause,total_policy_claims,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n

# incident cause utile a garder

#nuage de points en fonctions de le claim_area

qplot(claim_area,age,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(gender,claim_area,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(incident_cause,claim_area,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_area,days_to_incident,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_area,police_report,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#home/yes moins de fraude
qplot(claim_area,claim_type,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_area,claim_amount,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#entre 30 et 40k persque pas fraude pr Home
qplot(claim_area,total_policy_claims,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n

#nuage de points en fonctions de le days_to_incident

qplot(days_to_incident,age,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(days_to_incident,gender,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(days_to_incident,claim_area,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(days_to_incident,incident_cause,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(days_to_incident,police_report,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(days_to_incident,claim_type,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(days_to_incident,claim_amount,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(days_to_incident,total_policy_claims,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
#aucun lien avec les autres mais utiles car 2/3 des decla traité 1er nour sont des fraudes

#nuage de points en fonctions de le police_report

qplot(police_report,age,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(police_report,gender,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(police_report,claim_area,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#home/yes moins de fraude
qplot(police_report,incident_cause,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#police report yes /driver error bcp de non fraudulent,pr yes/Crime fraudulent ++
qplot(police_report,days_to_incident,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(police_report,claim_type,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#injuryon/yes pas bcp de fraude
qplot(police_report,claim_amount,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(police_report,total_policy_claims,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n

#utile aussi

#nuage de points en fonctions de le claim_type

qplot(claim_type,age,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#20aine plus de fraude pour material and injury
qplot(claim_type,gender,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_type,claim_area,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_type,incident_cause,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#crime/MandI fraud++,Crime/injury only nfraud++
qplot(claim_type,days_to_incident,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_type,police_report,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#injuryon/yes pas bcp de fraude
qplot(claim_type,claim_amount,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_type,total_policy_claims,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n

#nuage de points en fonctions de le claim_amount

qplot(claim_amount,age,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_amount,gender,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_amount,claim_area,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#entre 30 et 40k persque pas fraude pr Homme
qplot(claim_amount,incident_cause,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_amount,days_to_incident,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_amount,police_report,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_amount,claim_type,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(claim_amount,total_policy_claims,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n

#nuage de points en fonctions de le total_policy_claims

qplot(total_policy_claims,age,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#40ans plus d'une decla d'accident souvent fraudes
qplot(total_policy_claims,gender,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(total_policy_claims,claim_area,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(total_policy_claims,incident_cause,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(total_policy_claims,days_to_incident,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(total_policy_claims,police_report,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(total_policy_claims,claim_type,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n
qplot(total_policy_claims,claim_amount,data= Data,col=fraudulent)+geom_jitter(height=0.25,width=0.25)
#n

#boxplot
boxplot(Data$age~Data$fraudulent,data= Data)
boxplot(Data$days_to_incident~Data$fraudulent,data= Data)
boxplot(Data$claim_amount~Data$fraudulent,data= Data)


#test
chisq.test(Data$claim_type, Data$fraudulent)#corréle
fisher.test(Data$claim_type, Data$fraudulent)

chisq.test(Data$gender, Data$fraudulent)#pas corelle
fisher.test(Data$gender, Data$fraudulent)

chisq.test(Data$age, Data$fraudulent)#correle
fisher.test(Data$age, Data$fraudulent)

chisq.test(Data$days_to_incident, Data$fraudulent)#correle
fisher.test(Data$days_to_incident, Data$fraudulent)#correle

chisq.test(Data$total_policy_claims, Data$fraudulent)#correle

chisq.test(Data$gender, Data$fraudulent)#correle

chisq.test(Data$incident_cause, Data$fraudulent)#pas correle
fisher.test(Data$incident_cause, Data$fraudulent)

chisq.test(Data$police_report, Data$fraudulent)#correle
fisher.test(Data$police_report, Data$fraudulent)

chisq.test(Data$claim_area, Data$fraudulent)# pas correlé
fisher.test(Data$claim_area, Data$fraudulent)


#table de contingence


prop.table(table(Data$age,Data$fraudulent))
prop.table(table(Data$claim_type,Data$fraudulent))

#cluster

DataC<-Data
#création d'une nuvelle table

DataC$incident_cause<-as.ordered(DataC$incident_cause)
DataC$claim_area<-as.ordered(Data$claim_area)
DataC$police_report<-as.ordered(DataC$police_report)
DataC$claim_type<-as.ordered(DataC$claim_type)
DataC$total_policy_claims<-as.ordered(DataC$total_policy_claims)
Data$fraudulent<-as.ordered(DataC$fraudulent)

# changement des variables en ordinal pour calculer dmatrix


dmatrix<-daisy(DataC)
#création de la matrice de distance

summary(dmatrix)
km4<-kmeans(dmatrix,5) 
table(km4$cluster,Data$fraudulent)
DataC$cluster1 <-km4$cluster 
DataC$cluster2 <-km4$cluster 
DataC$cluster3 <-km4$cluster 
View(DataC)
qplot(km4$cluster, data=Data, fill=fraudulent)
# 5 permet d'avoir ( groupe distinct avec que oui ou que non) mais methode un peu aléatoire

agn<-agnes(dmatrix)
agn4<-cutree(agn,k=9)
rect.hclust(agn, k=9, border="red")
table(agn4,DataC$fraudulent)
# méthode pas assez precise groupe 1 500 oui/500 non et ca peu importe le nb de cluster

dia<-diana(dmatrix)
plot(dia)
dia4<-cutree(dia,k=8) 
table(dia4,DataC$fraudulent)
#meme pb qu'avec agn

dbs <- dbscan(dmatrix, eps=0.05, minPts = 3)
table(dbs$cluster,DataC$fraudulent)
qplot(as.factor(dbs$cluster), data=DataC, fill=fraudulent)
#toujours meme pb

tsne_out <- tsne(dmatrix, k=2)
tsne_out <- data.frame(tsne_out)

qplot(tsne_out[,1], tsne_out[,2], col=as.factor(dbs$cluster))

Data2<-Data
Data2<-subset(Data2,select= -gender)
Data2<-subset(Data2,select= -claim_area)
Data2<-subset(Data2,select= -incident_cause)




fraud_EA<-Data2[1:1200,]
fraud_ET<-Data2[1201:1600,]


tree1 <- tree(fraudulent ~ ., fraud_EA)
tree2<- rpart(fraudulent ~ ., fraud_EA)
tree3<- C5.0(fraudulent ~ ., fraud_EA)
test_tree1 <- predict(tree1, fraud_ET, type="class")
test_tree2 <- predict(tree2, fraud_ET, type="class")
test_tree3 <- predict(tree3, fraud_ET, type="class")
print(test_tree1)
fraud_ET$Tree1 <- test_tree1 
fraud_ET$Tree2 <- test_tree2 
fraud_ET$Tree3 <- test_tree3 


taux_succes1 <-length(fraud_ET[fraud_ET$fraudulent==fraud_ET$Tree1,"fraudulent"])/400
taux_succes2 <-length(fraud_ET[fraud_ET$fraudulent==fraud_ET$Tree2,"fraudulent"])/400
taux_succes3 <-length(fraud_ET[fraud_ET$fraudulent==fraud_ET$Tree3,"fraudulent"])/400


##prédictions finale
Data_a_predire<- read.csv("analyse_de_données/données/Data_Projet_1_New.csv",sep=",",dec=".",header=TRUE, stringsAsFactors = T)

prediction <- predict(tree3, Data_a_predire, type="class")
Data_a_predire$prediction_fraud<-prediction

write.csv(Data_a_predire, file = "analyse_de_données/données/classifieur_final.csv", row.names = FALSE)