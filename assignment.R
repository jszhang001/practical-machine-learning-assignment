setwd("C:/Users/jiasheng/Desktop/machine learning assignment")
library(caret)
library(tree)

training<-read.csv("pml-training.csv", na.strings = c("NA",""),header =  T)
testing<-read.csv("pml-testing.csv",na.strings=c("NA",""),header = T)
dim(training);dim(testing)

#remove varaibles that have too many NA values
trainingData<-training[,colSums(is.na(training))==0]
#remove unrelevant variables
remove=c('X','user_name','raw_timestamp_part_1','raw_timestamp_part_2','cvtd_timestamp','new_window','num_window')
trainingset<-trainingData[,-which(names(training)%in% remove)]
dim(trainingset)
#check whether the variable has low variance
zeroVar<-nearZeroVar(trainingset[sapply(trainingset,is.numeric)],saveMetrics = TRUE)
training.nonzerovar=trainingset[,zeroVar[,'nzv']==0]
dim(training.nonzerovar)
#remove high correlated variables 
corrMatrix<-cor(na.omit(training.nonzerovar[sapply(training.nonzerovar,is.numeric)]))
dim(corrMatrix)
remove_cor<-findCorrelation(corrMatrix,cutoff = .90,verbose = TRUE)

training_finaldata<-training.nonzerovar[,-remove_cor]
dim(training_finaldata)
#we have 19622 samples and 46 variables

##Data Partitioning
inTrain<-createDataPartition(y=training_finaldata$classe,p=0.7,list = FALSE)
trainset<-training_finaldata[inTrain,];testset<-training_finaldata[-inTrain,]
dim(trainset);dim(testset)

##Regression Tree
tree.training<-tree(classe~. ,data=trainset)
summary(tree.training)
plot(tree.training)
text(tree.training,pretty = 0,cex=0.8)

##cross validation
tree.pred=predict(tree.training,testset,type="class")
predMatrix=with(testset,table(tree.pred,classe))
sum(diag(predMatrix))/sum(as.vector(predMatrix))

##Random Forests
require(randomForest)
rf.trainset=randomForest(classe~.,data=trainset,ntree=100,importance=TRUE)
varImpPlot(rf.trainset)


##Out-of Sample Accuracy
tree.pred=predict(rf.trainset,testset,type="class")
predMatrix = with(testset,table(tree.pred,classe))
sum(diag(predMatrix))/sum(as.vector(predMatrix))

#conclusion
pred_final<-predict(rf.trainset,testing)
pred_final

