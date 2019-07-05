library(tree)
tree = tree(as.factor(Class)~.,train_data)
tree.size <- cv.tree(tree)$size
DEV <- rep(0,length(tree.size))
for(i in 1:10){
  tree.cv <- cv.tree(tree)
  DEV <- DEV+tree.cv$dev
}
DEV <- DEV/10
#plot(tree.size, DEV, type = 'b')
#best on prend 5
tree.model <- prune.tree(tree, best=5)
plot(tree.model)
text(tree.model,pretty = 0)
tree.pred = predict(tree.model,newdata = test_x,type = "class")
tree.roc <<- roc(test_y, as.numeric(tree.pred))
Performances <<- add_Performance("Decision-tree",tree.pred,test_y,AUC=tree.roc$auc)


tree.under = tree(as.factor(Class)~.,train_under_data)
tree.size <- cv.tree(tree.under)$size
DEV <- rep(0,length(tree.size))
for(i in 1:10){
  tree.under.cv <- cv.tree(tree.under)
  DEV <- DEV+tree.under.cv$dev
}
DEV <- DEV/10
#plot(tree.size, DEV, type = 'b')
tree.under.model <- prune.tree(tree.under, best=tree.size[which.min(DEV)])
plot(tree.under.model)
text(tree.under.model,pretty = 0)
tree.under.pred = predict(tree.under.model,newdata = test_under_x,type = "class")
tree.under.roc <<- roc(test_under_y, as.numeric(tree.under.pred))
Performances <<- add_Performance("Decision-tree-under",tree.under.pred,test_under_y,AUC=tree.under.roc$auc)

tree.over = tree(as.factor(Class)~.,train_over_data)
tree.size <- cv.tree(tree.over)$size
DEV <- rep(0,length(tree.size))
for(i in 1:10){
  tree.over.cv <- cv.tree(tree.over)
  DEV <- DEV+tree.over.cv$dev
}
DEV <- DEV/10
#plot(tree.size, DEV, type = 'b')
tree.over.model <- prune.tree(tree.over, best=tree.size[which.min(DEV)])
plot(tree.over.model)
text(tree.over.model,pretty = 0)
tree.over.pred = predict(tree.over.model,newdata = test_over_x,type = "class")
tree.over.roc <- roc(test_over_y, as.numeric(tree.over.pred))
Performances <- add_Performance("Decision-tree-over",tree.over.pred,test_over_y,AUC=tree.over.roc$auc)

plot(tree.roc,main="ROC pour arbre decision")
plot.roc(tree.under.roc,add = TRUE ,col = 'blue')
plot.roc(tree.over.roc,add = TRUE ,col = 'red')
legend("bottomright",inset=.05,c("arbre decision","arbre decision under","arbre decision over"),lty=c(1,1,1),col=c("black","blue","red"))


######################################################################################################
######################################################################################################
library(ipred)
#il marche pas sur grandes donnees
bagging_train_sub = sample(nrow(data),floor(0.3*nrow(data)),replace = FALSE)
bagging_train_data = data[bagging_train_sub,]
bagging_train_x = subset(bagging_train_data,select = -c(Class))
bagging_train_y = bagging_train_data$Class

bagging.model <- bagging(Class~.,data = bagging_train_data)
bagging.pred <- predict(bagging.model, newdata=test_x)
bagging.roc <- roc(test_y, as.numeric(bagging.pred))
Performances <- add_Performance("Bagging",bagging.pred,test_y,AUC=bagging.roc$auc)

bagging.under.model <- bagging(as.factor(Class)~.,data = train_under_data) 
bagging.under.pred <- predict(bagging.under.model, newdata=test_under_x)
bagging.under.roc <- roc(test_under_y, as.numeric(bagging.under.pred))
Performances <- add_Performance("Bagging-under",bagging.under.pred,test_under_y,AUC=bagging.under.roc$auc)

bagging.over.model <- bagging(as.factor(Class)~.,data = train_over_data) 
bagging.over.pred <- predict(bagging.over.model, newdata=test_over_x)
bagging.over.roc <- roc(test_over_y, as.numeric(bagging.over.pred))
Performances <- add_Performance("Bagging-over",bagging.over.pred,test_over_y,AUC=bagging.over.roc$auc)

plot(bagging.roc,main="ROC pour bagging")
plot(bagging.under.roc,add = TRUE ,col = 'blue')
plot.roc(bagging.over.roc,add = TRUE ,col = 'red')
legend("bottomright",inset=.05,c("bagging","bagging under","bagging over"),lty=c(1,1,1),col=c("black","blue","red"))

######################################################################################################
######################################################################################################
library(randomForest)
RF_train_sub = sample(nrow(data),floor(0.1*nrow(data)),replace = FALSE)
RF_train_data = data[RF_train_sub,]
RF_train_x = subset(RF_train_data,select = -c(Class))
RF_train_y = RF_train_data$Class

RF.model <- randomForest(RF_train_x,y=RF_train_y,mtry = 6)
RF.pred <- predict(RF.model, newdata = test_x)
RF.roc <- roc(test_y, as.numeric(RF.pred))
Performances <- add_Performance("RF",RF.pred,test_y,AUC=RF.roc$auc)

RF.under.model <- randomForest(train_under_x,y=train_under_y,mtry = 6)
RF.under.pred <- predict(RF.under.model, newdata = test_under_x)
#table(RF.under.pred, test_under_y)
RF.under.roc <- roc(test_under_y, as.numeric(RF.under.pred))
Performances <- add_Performance("RF-under",RF.under.pred,test_under_y,AUC=RF.under.roc$auc)

RF.over.model <- randomForest(train_over_x,y=train_over_y,mtry = 6)
RF.over.pred <- predict(RF.over.model, newdata = test_over_x)
#table(RF.over.pred, test_over_y)
RF.over.roc <- roc(test_over_y, as.numeric(RF.over.pred))
Performances <- add_Performance("RF-over",RF.over.pred,test_over_y,AUC=RF.over.roc$auc)

plot(RF.roc,main="ROC pour forêt aléatoire")
plot.roc(RF.under.roc,add = TRUE ,col = 'blue')
plot.roc(RF.over.roc,add = TRUE ,col = 'red')
legend("bottomright",inset=.05,c("RF","RF under","RF over"),lty=c(1,1,1),col=c("black","blue","red"))

#####################################################################################################
#####################################################################################################