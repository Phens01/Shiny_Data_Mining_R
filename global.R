library(factoextra)
library(class)
library(caTools)
library(e1071)
library(rpart)
library(nnet)
library(neuralnet)

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


# Prétraitement
donnees <- read.table("anneal.data.txt",na.strings = "?",dec = ".", sep = ",", header = T)
#View(donnees)
#colnames(donnees)=paste("A",1:length(colnames(donnees)), sep = "")
colnames(donnees) <- c("Family","Product-type","Steel","Carbon","Hardness",
                       "Temper_rolling","Condition","Formability","Strength","Non-ageing",
                       "Surface-finish","Surface-quality","Enamelability","Bc","Bf",
                       "Bt","Bw/me","Bl","M","Chrom","Phos","Cbond","Marvi","Exptl",
                       "Ferro","Corr","Blue/bright/varn/clean","Lustre","Jurofm","S","P",
                       "Shape","Thick","Width","Len","Oil","Bore","Packing","Classes")
vars <- setdiff(c("Carbon","Hardness","Strength","Thick","Width"),"Classes")
#dat <- read.table("Dm/anneal.data.txt", sep = ",",na.strings = c("?"),dec = ".", header = T)
#colnames(dat)=paste("A",1:length(colnames(dat)), sep = "")
#View(colSums(is.na(dat))==nrow(dat))
#nrow(dat)


donnees=suppression_na(donnees)
donnees[[14]][is.na(donnees[[14]])]<-"Y"
donnees[[21]][is.na(donnees[[21]])]<-"Y"
donnees[[24]][is.na(donnees[[24]])]<-"R"
donnees[[38]][is.na(donnees[[38]])]<-2.888889
donnees[is.na(donnees$Carbon),"Carbon"] <- mean(donnees$Carbon)
donnees[is.na(donnees$Hardness),"Hardness"] <- mean(!is.na(donnees$Hardness), na.rm = T)
donnees[is.na(donnees$Strength),"Strength"] <- mean(!is.na(donnees$Strength), na.rm = T)
donnees[is.na(donnees$Thick),"Thick"] <- mean(!is.na(donnees$Thick), na.rm = T)
donnees[is.na(donnees$Width),"width"] <- mean(!is.na(donnees$Width), na.rm = T)
donnees<-subset(donnees, select = -c(19,21,23,24,26,29,30,31))
donnees$Classes[donnees$Classes=="U"] <- "6"
donnees$Classes <- as.numeric(donnees$Classes)
donnees
donnee <- which(colSums(is.na(donnees))==nrow(donnees))


#Donées de test pretraitement
test = read.table("anneal.test",header = F,sep=',',na.strings = c('?'),dec = '.') # les donnees de test
colnames(test) <-c("Family","Product-type","Steel","Carbon","Hardness",
                   "Temper_rolling","Condition","Formability","Strength","Non-ageing",
                   "Surface-finish","Surface-quality","Enamelability","Bc","Bf","Bt",
                   "Bw/me","Bl","M","Chrom","Phos","Cbond","Marvi","Exptl","Ferro",
                   "Corr","Blue/bright/varn/clean","Lustre","Jurofm","S","P","Shape",
                   "Thick","Width","Len", "Oil","Bore","Packing","Classes");

test=suppression_na(test)
test[[14]][is.na(test[[14]])]<-"Y"
test[[21]][is.na(test[[21]])]<-"Y"
test[[24]][is.na(test[[24]])]<-"B"
test[[38]][is.na(test[[38]])]<-2.888889
test$Classes[test$Classes=="U"] <- "6"
test<-subset(test, select = -c(19,21,23,24,26,29,30,31))# Suppression des colonnes n'ayant que de valeurs manquantes

#train
new_df= donnees<-subset(donnees, select = c(4,5,8,9,12,13,17,25,26,27,29,30,31))
new_df= donnees<-subset(donnees, select = -c(5,7))


#test
new_test  = test<-subset(test, select = c(4,5,8,9,12,13,17,25,26,27,29,30,31))
new_test = test<-subset(test, select = -c(5,7))

Data = data.frame(donnees$Carbon,donnees$Hardness,donnees$Strength,donnees$Thick,donnees$Width,donnees$Classes)
colnames(Data) <- c("Carbon","Hardness","Strength","Thick","Width","Classes")

dataset = Data

## Réseau de neurones
#nn = neuralnet(1+2+3+4+5+U)


## Arbre de décision

arbre <- rpart(Classes ~., method = "class", data = donnees)




## Plus proches voisins
# Enconding the label
#dataset$Classes = factor(dataset$Classes, level = c(1, 2, 3, 4, 5, "U"))




##SVM
#d_svm = donnees
#d_svm$Classes[d_svm$Classes=="U"]<-"6"
#d_svm$Classes = as.numeric(d_svm$Classes)
#modelsvm <- svm(d_svm$Classes~.,d_svm)
#s <- Summary(modelsvm)




## Classification Hierarchique
Data.labels <- Data$Classses
table(Data.labels)

# Data
F_data = Data[1:5]

# Scale
F_data_std = scale(F_data)

# Distance
Data.dist = dist(F_data_std)

# Hierarchical clustering algorithm
hc.out_Data <- hclust(Data.dist, method = "complete")
hc.out_Data

# Dendogram
#plot(hc.out_Data)
#rect.hclust(hc.out_Data, k=3, border = 2:5)


