#chargement des packages 

library('rpart')
library('arules')
library('DMwR2')
library('e1071')
library('caTools')
library('class')
library('neuralnet') 
library('arules')
library(nnet)
#library('arulesViz')
#library('RColorBrewer')



suppression_na<-function(df){
  i=1
  n=ncol(df)
  
  while (i<= n ) {
    if(is.numeric(df[[i]])){
      df[[i]][is.na(df[[i]])]<-mean(df[[i]],na.rm=TRUE)
    }
    
    else{
      df[[i]][is.na(df[[i]])]<- getmode(df[[i]])
    }
    i=i+1
  }
  return(df)
}

getmode<-function(v){
  temp=na.omit(v)
  uniq<-unique(temp)
  uniq[which.max(tabulate(match(temp,uniq)))]
  
}



#Donnees d'apprentissage pretraitement




df = read.table("anneal.data",header = F,sep=',',na.strings = c('?'),dec = '.')# Les donnees
colnames(df) <-c("family","product-type","steel","carbon","hardness","temper_rolling","condition","formability","strength","non-ageing","surface-finish","surface-quality","enamelability","bc","bf","bt","bw/me","bl","m","chrom","phos","cbond","marvi","exptl","ferro","corr","blue/bright/varn/clean","lustre","jurofm","s","p","shape","thick","width","len", "oil","bore","packing","classe");


#colnames(df) = paste("A",1:ncol(df))


df<-subset(df, select = -c( 19,21,23,24,26,29,30,31))  #Suppression des colonnes n'ayant que de valeurs manquantes

df=suppression_na(df)


#Donnees de test pretraitement

test = read.table("anneal.test",header = F,sep=',',na.strings = c('?'),dec = '.') # les donnees de test
colnames(test) <-c("family","product-type","steel","carbon","hardness","temper_rolling","condition","formability","strength","non-ageing","surface-finish","surface-quality","enamelability","bc","bf","bt","bw/me","bl","m","chrom","phos","cbond","marvi","exptl","ferro","corr","blue/bright/varn/clean","lustre","jurofm","s","p","shape","thick","width","len", "oil","bore","packing","classe");

test=suppression_na(test)
test[[14]][is.na(test[[14]])]<-"Y"
test[[21]][is.na(test[[21]])]<-"Y"
test[[24]][is.na(test[[24]])]<-"B"
test[[38]][is.na(test[[38]])]<-2.888889
test<-subset(test, select = -c(19,21,23,24,26,29,30,31))# Suppression des colonnes n'ayant que de valeurs manquantes

# fin du pretraitement



# Arbre de decision

Arbre<- rpart(classe ~., data = df,method = 'class')

prediction = predict(Arbre,test[,-31],type = c("class"))
table_mat <- table(test$classe, prediction)


accuracy<- sum(diag(table_mat)) / sum(table_mat)
accuracy

#les plus proche voisin


#train
new_df= df<-subset(df, select = c(4,5,8,9,12,13,17,25,26,27,29,30,31))
new_df= df<-subset(df, select = -c(5,7))


#test
new_test  = test<-subset(test, select = c(4,5,8,9,12,13,17,25,26,27,29,30,31))
new_test = test<-subset(test, select = -c(5,7))


# Les plus proches voisins
knn <- kNN(classe ~ ., new_df, new_test,k=15)

table_knn<-table(new_test[, 11],knn)


accuracy2 <- sum(diag(table_knn)) / sum(table_knn)


#SVM (support vector machine)

d_svm = new_df
d_svm$classe[d_svm$classe=="U"]<-"2"
d_svm$classe = as.numeric(d_svm$classe)

d_test=new_test
d_test$classe[d_test$classe=="U"]<-"2"
d_test$classe = as.numeric(d_test$classe)

modelsvm <-svm(d_svm$classe ~.,data=d_svm, kernel="linear", cost=10, scale = FALSE)

#plot(modelsvm)
s<-summary(modelsvm)
s

prediction_svm=predict(modelsvm,subset(d_test,select = -c(11)))

table_svm = table(d_test[, 11],prediction_svm)
table_svm

predict(modelsvm, subset(new_test,select = -c(11)))

accuracy_svm <- sum(diag(table_svm)) / sum(table_svm)



#reseau de neuronne

nn=neuralnet(new_df$classe~.,data=new_df, hidden=3,act.fct = "logistic",linear.output = FALSE)


prediction_nn = predict=compute(nn,new_test)

probabilite=prediction_nn$net.result

#convertir les  probabilities en  1 et 0

pre <- ifelse(probabilite>0.5, 1, 0)
	



# Regles d'association












