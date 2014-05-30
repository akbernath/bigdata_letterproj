##  ST599 - Big Data Analysis
##  Project 3 - Letter recognition
##  Bagging code

setwd("D:/School/Spring 14/ST599/bigdata_letterproj")

library(dplyr)
library(ggplot2)
library(rpart)
library(sampling)
library(rpart.plot)
library(adabag)

#   Read in data, keeping letter as a factor, else integers
let.learn <- read.csv("data/learn_set.csv", stringsAsFactors=F)
let.learn$letter <- as.factor(let.learn$letter)

let.xvalid <- read.csv("data/xvalid_set.csv", stringsAsFactors=F)
let.xvalid$letter <- as.factor(let.xvalid$letter)

head(let.learn)
head(let.xvalid)
let.learn <- let.learn[,-1]
let.xvalid <- let.xvalid[,-1]

#   Build full tree with learning set
#   Use default Gini splits and proportional priors
mod.init <- rpart(letter ~ .,
                  data=let.learn,
                  method="class",
                  control=rpart.control(xval=10, cp=0))

##  Plot full tree
prp(mod.init, main="Full Classification Tree")
##  So that's a mess... let's see cp parameters to get a sense of 
##  how much to prune

plotcp(mod.init)
CP.tab.0 <- printcp(mod.init)
min(CP.tab.0[,4])
one.minus.SE <- CP.tab.0[,4]-CP.tab.0[,5]
one.minus.SE
new.CP <- CP.tab.0[82,1]
#   By 1-SE Rule we want to choose 0.0002345399
#   corresponds to tree with 336 ... yikes!


##  Pruned tree
mod.pruned <- prune(mod.init, cp=new.CP)

##  Plot full tree
prp(mod.pruned, main="Pruned Classification Tree")
plotcp(mod.pruned)
#   This is not very helpful and will take a long time 
#   to minimize


##  Let's try the automatic process
mod.1 <- rpart(letter ~ .,
                  data=let.learn,
                  method="class",
                  control=rpart.control(xval=10))

##  Plot the new model
prp(mod.1, main="Automatic Classification Tree")

##  Check model errors
plotcp(mod.init)
printcp(mod.init)

mod1.pred <- predict(mod.1, newdata=let.xvalid, type="class")
CT.confusion <- table(mod1.pred, let.xvalid$letter)

##  Output confusion matrix to csv
write.csv(CT.confusion, "data/CT_confusion.csv", na="NA")
##  Missing E, F, K, O, R, S, Y
##  W appears at 2 terminal nodes

#   Misclassification error
mc.CT <- 1-(sum(diag(CT.confusion))/sum(CT.confusion))



####  So the full tree overfits, the pruned tree , and 
####  the automatically generated tre fails to classify several
####  letters, namely, E, F, K, O, R, S, Y
####  Switch from CART to bagging and see if we can improve ourr estimates
bag.init <- bagging(letter~.,
                 data=let.learn,
                 mfinal=50,
                 control=rpart.control(xval=10,cp=0.005))
names(bag.init)

##  Variable importance
bag.init$importance

##  Check predictions
bag.0.pred <- predict(bag.init, newdata=let.xvalid,
                      newmfinal=length(bag.init$trees))

#   Find bagging confusion estimates
bag.confusion <- bag.0.pred$confusion

#  Output confusion matrix to csv
write.csv(bag.confusion, "data/bag_confusion.csv", na="NA")
##  Classes F, H, O, and S have considerable confusion



#   Misclassification error for bagging
mc.error <- 1-(sum(diag(bag.confusion))/sum(bag.confusion))


##  Error evolution versus number of iterations
evol.learn <- errorevol(bag.init,let.learn)
evol.xvalid <- errorevol(bag.init,let.xvalid)

#   Plot learning error and prediction error
plot(evol.xvalid$error, type="l", ylim=c(0.35,0.5),
     main="Bagging Error versus Iterations",  
     xlab="Iterations", ylab="Error", col = "red")
lines(evol.learn$error, cex = .5 ,col="blue", lty=2)
legend("topright", c("learn","xvalid"), col = c("red", "blue"), lty=1:2)
##  Stabilizes by 40 iterations


