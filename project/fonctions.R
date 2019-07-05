init_performance <- function(var, env = globalenv()){
  assign(eval(substitute(var)), data.frame(method=numeric(0),TPrate=numeric(0),FPrate=numeric(0),precision=numeric(0),recall=numeric(0),accuracy=numeric(0),F1score=numeric(0),AUC=numeric(0)), envir = env)
}

add_Performance <- function(method,predicted_class, true_class, Performance = Performances, AUC=NA){
  idx = nrow(Performance)+1
  cat("La matrice de confusion pour",method,"\n")
  print(table(predicted_class,true_class))
  CM = table(predicted_class,true_class)
  #TPA:true positif rate; FPR:false positif rate
  TPR = CM[2,2]/(CM[1,2]+CM[2,2])
  FPR = CM[2,1]/(CM[1,1]+CM[2,1])
  precision = CM[2,2]/(CM[2,1]+CM[2,2])
  recall = TPR
  accuracy = sum(diag(CM))/length(predicted_class)
  F1score = 2*precision*recall/(precision+recall)
  Performance[idx,'method'] = method
  Performance[idx,'TPrate'] = TPR
  Performance[idx,'FPrate'] = FPR
  Performance[idx,'precision'] = precision
  Performance[idx,'recall'] = recall
  Performance[idx,'accuracy'] = accuracy
  Performance[idx,'F1score'] = F1score
  Performance[idx,'AUC'] = AUC
  return(Performance)
}

KNN = function(train_under_x,train_under_y,test_under_x,test_under_y,train_over_x,train_over_y,test_over_x,test_over_y){
  library(pROC)
  library(FNN)
  library(ROCit)

  knn.under.pred <<- knn(train_under_x,test_under_x,as.factor(train_under_y),k=60)[1:length(test_under_y)]
  ROCit_obj <- rocit(score=as.numeric(as.character(knn.under.pred)),class=test_under_y)
  plot(ROCit_obj)
  
  knn.under.roc <<- roc(test_under_y, predictor = as.numeric(as.character(knn.under.pred)))
  Performances <<- add_Performance("KNN_under",knn.under.pred,test_under_y,AUC=knn.under.roc$auc)
  
  knn.over.pred <<- knn(train_over_x,test_over_x,as.factor(train_over_y),k=60)[1:length(test_over_y)]
  knn.over.roc <<- roc(response =  test_over_y, predictor= as.numeric(as.character(knn.over.pred)))
  Performances <<- add_Performance("KNN_over",knn.over.pred,test_over_y,AUC=knn.over.roc$auc)
  
  plot.roc(knn.under.roc,main="ROC pour KNN",col = 'blue')
  plot.roc(knn.over.roc,add = TRUE ,col = 'red')
  legend("bottomright",inset=.05,c("KNN_under","KNN_over"),lty=c(1,1),col=c("blue","red")) 
}


LDA = function(train_data,test_data,train_under_data,test_under_data,train_over_data,test_over_data,formu="Class~."){
  library(MASS)
  library(pROC)
  formu <- as.formula(formu)
  lda.model <<- lda(formu,data = train_data)
  lda.pred <<- predict(lda.model, newdata = test_data)
  lda.roc <<- roc(test_data$Class, as.numeric(lda.pred$class))
  Performances <<- add_Performance("LDA",lda.pred$class,test_data$Class,AUC=lda.roc$auc)
  
  lda.under.model <<- lda(formu,data = train_under_data)
  lda.under.pred <<- predict(lda.under.model, newdata = test_under_data)
  lda.under.roc <<- roc(test_under_data$Class, as.numeric(lda.under.pred$class))
  Performances <<- add_Performance("LDA-under",lda.under.pred$class,test_under_data$Class,AUC=lda.under.roc$auc)
  
  lda.over.model <<- lda(formu,data = train_over_data)
  lda.over.pred <<- predict(lda.over.model, newdata = test_over_data)
  lda.over.roc <<- roc(test_over_data$Class, as.numeric(lda.over.pred$class))
  Performances <<- add_Performance("LDA-over",lda.over.pred$class,test_over_data$Class,AUC=lda.over.roc$auc)
  
  plot(lda.roc,main="ROC pour LDA")
  plot.roc(lda.under.roc,add = TRUE ,col = 'blue')
  plot.roc(lda.over.roc,add = TRUE ,col = 'red')
  legend("bottomright",inset=.05,c("LDA","LDA_under","LDA_over"),lty=c(1,1,1),col=c("black","blue","red"))
  #return(Performances)
}


QDA = function(train_data,test_data,train_under_data,test_under_data,train_over_data,test_over_data,formu="Class~."){
  library(MASS)
  library(pROC)
  formu <- as.formula(formu)
  qda.model <<- qda(formu,data = train_data)
  qda.pred <<- predict(qda.model, newdata = test_data)
  qda.roc <<- roc(test_data$Class, as.numeric(qda.pred$class))
  Performances <<- add_Performance("QDA",qda.pred$class,test_data$Class,AUC=qda.roc$auc)
  
  qda.under.model <<- qda(formu,data = train_under_data)
  qda.under.pred <<- predict(qda.under.model, newdata = test_under_data)
  qda.under.roc <<- roc(test_under_data$Class, as.numeric(qda.under.pred$class))
  Performances <<- add_Performance("QDA-under",qda.under.pred$class,test_under_data$Class,AUC=qda.under.roc$auc)
  
  qda.over.model <<- qda(formu,data = train_over_data)
  qda.over.pred <<- predict(qda.over.model, newdata = test_over_data)
  qda.over.roc <<- roc(test_over_data$Class, as.numeric(qda.over.pred$class))
  Performances <<- add_Performance("QDA-over",qda.over.pred$class,test_over_data$Class,AUC=qda.over.roc$auc)
  
  plot(qda.roc,main="ROC pour QDA")
  plot.roc(qda.under.roc,add = TRUE ,col = 'blue')
  plot.roc(qda.over.roc,add = TRUE ,col = 'red')
  legend("bottomright",inset=.05,c("QDA","QDA_under","QDA_over"),lty=c(1,1,1),col=c("black","blue","red")) 
}

NB = function(train_data,test_data,train_under_data,test_under_data,train_over_data,test_over_data,formu="as.factor(Class)~."){
  library(pROC)
  library(e1071)
  #library(klaR)
  formu <- as.formula(formu)
  nb.model <<- naiveBayes(formu,data = train_data)
  nb.pred <<- predict(nb.model, newdata = test_data)
  nb.roc <<- roc(test_data$Class, as.numeric(nb.pred))
  Performances <<- add_Performance("NB",nb.pred,test_data$Class,AUC=nb.roc$auc)
  
  nb.under.model <<- naiveBayes(formu,data = train_under_data)
  nb.under.pred <<- predict(nb.under.model, newdata = test_under_data)
  nb.under.roc <<- roc(test_under_data$Class, as.numeric(nb.under.pred))
  Performances <<- add_Performance("NB-under",nb.under.pred,test_under_data$Class,AUC=nb.under.roc$auc)
  
  nb.over.model <<- naiveBayes(formu,data = train_over_data)
  nb.over.pred <<- predict(nb.over.model, newdata = test_over_data)
  nb.over.roc <<- roc(test_over_data$Class, as.numeric(nb.over.pred))
  Performances <<- add_Performance("NB-over",nb.over.pred,test_over_data$Class,AUC=nb.over.roc$auc)
  
  plot(nb.roc,main="ROC pour naive bayes")
  plot.roc(nb.under.roc,add = TRUE ,col = 'blue')
  plot.roc(nb.over.roc,add = TRUE ,col = 'red')
  legend("bottomright",inset=.05,c("NB","NB_under","NB_over"),lty=c(1,1,1),col=c("black","blue","red"))
}


LogReg = function(train_data,test_data,train_under_data,test_under_data,train_over_data,test_over_data,formu="Class~."){
  library(pROC)
  formu <- as.formula(formu)
  logreg.model <<- glm(formu, data = train_data, family = binomial(link='logit'))
  ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
  logreg.pred <<- predict(logreg.model, newdata = test_data, type = "response")
  logreg.pred[logreg.pred>0.5] = 1
  logreg.pred[logreg.pred<=0.5] = 0
  #reglog.roc <<- roc(test_data$Class, as.vector(predict(logreg.model, newdata = test_data)))
  reglog.roc <<- roc(test_data$Class, as.numeric(logreg.pred))
  Performances <<- add_Performance("Logistic regression",logreg.pred,test_data$Class,AUC=reglog.roc$auc)
  
  logreg.under.model <<- glm(formu, data = train_under_data, family = binomial(link='logit'))
  logreg.under.pred <<- predict(logreg.under.model, newdata = test_under_data, type = "response")
  logreg.under.pred[logreg.under.pred>0.5] = 1
  logreg.under.pred[logreg.under.pred<=0.5] = 0
  reglog.under.roc <<- roc(test_under_data$Class, as.numeric(logreg.under.pred))
  Performances <<- add_Performance("Logistic regression under",logreg.under.pred,test_under_data$Class,AUC=reglog.under.roc$auc)
  
  logreg.over.model <<- glm(formu, data = train_over_data, family = binomial(link='logit'))
  logreg.over.pred <<- predict(logreg.over.model, newdata = test_over_data, type = "response")
  logreg.over.pred[logreg.over.pred>0.5] = 1
  logreg.over.pred[logreg.over.pred<=0.5] = 0
  reglog.over.roc <<- roc(test_over_data$Class, as.numeric(logreg.over.pred))
  Performances <<- add_Performance("Logistic regression over",logreg.over.pred,test_over_data$Class, AUC=reglog.over.roc$auc)
  
  plot(reglog.roc,main="ROC pour regression logistique")
  plot.roc(reglog.under.roc,add = TRUE ,col = 'blue')
  plot.roc(reglog.over.roc,add = TRUE ,col = 'red')
  legend("bottomright",inset=.05,c("reglog","reglog_under","reglog_over"),lty=c(1,1,1),col=c("black","blue","red"))
  
}


SVM = function(train_under_x,train_under_y,test_under_x,test_under_y,train_over_x,train_over_y,test_over_x,test_over_y){
  library(pROC)
  library(e1071)
  #svm.under.model <<- svm(train_under_x,as.factor(train_under_y),scale = TRUE)
  #svm.under.pred <<- predict(svm.under.model,newdata = test_under_x)
  #svm.under.roc <<- roc(test_under_y, as.numeric(svm.under.pred))
  #Performances <<- add_Performance("SVM-under",svm.under.pred,test_under_y,AUC=svm.under.roc$auc)
  
  svm.under.model <<- svm(train_under_x,as.factor(train_under_y),scale = TRUE)
  svm.under.pred <<- predict(svm.under.model,newdata = test_under_x)
  svm.under.roc <<- roc(test_under_y, as.numeric(svm.under.pred))
  Performances <<- add_Performance("SVM-under",svm.under.pred,test_under_y,AUC=svm.under.roc$auc)
  
  svm.over.model <<- svm(train_over_x,as.factor(train_over_y),scale = TRUE)
  svm.over.pred <<- predict(svm.over.model,newdata = test_over_x)
  svm.over.roc <<- roc(test_over_y, as.numeric(svm.over.pred))
  Performances <<- add_Performance("SVM-over",svm.over.pred,test_over_y,AUC=svm.over.roc$auc)
  
  plot.roc(svm.under.roc,col = 'blue',main="ROC pour SVM")
  plot.roc(svm.over.roc,add = TRUE ,col = 'red')
  legend("bottomright",inset=.05,c("svm_under","svm_over"),lty=c(1,1),col=c("blue","red"))
}
