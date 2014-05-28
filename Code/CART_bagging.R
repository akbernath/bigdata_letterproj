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
                  control=rpart.control(cp=0))
prp(mod.init)
##  So that's a mess... let's see cp parameters
plotcp(mod.init)
##  Looks like a good relative error is at ~0.00025
##  but that corresponds with over 300 terminal nodes
##  Let's shoot for around 30 terminal nodes to try 
##  and capture all letters.  Set cp = 0.005

summary(mod.init)
mod.init
##  Missing E, F, K, O, R, S, Y
##  W appears at 2 terminal nodes


mod.2 <- rpart(letter ~ .,
               data=let.learn,
               method="class",
               control=rpart.control(cp=0.005))
prp(mod.1)
##  Missing "O."
plotcp(mod.1)
##  Several attempts with various cp parameters
##  down to 0.003 still do not capture class "O".



####  Switch from CART to bagging
bag.0 <- bagging(letter~.,
                 data=let.learn,
                 mfinal=50,
                 control=rpart.control(cp=0.005))
names(bag.0)
bag.0$importance

##  Check predictions
bag.0.pred <- predict(bag.0, newdata=let.xvalid,
                      newmfinal=length(bag.0$trees))
names(bag.0.pred)
bag.0.pred$confusion
##  Classes F, H, O, and S have considerable confusion
bag.0.pred$error

##  Error evolution versus number of iterations
evol.learn <- errorevol(bag.0,let.learn)
evol.xvalid <- errorevol(bag.0,let.xvalid)

#   Plot learning error and prediction error
plot(evol.xvalid$error, type="l", ylim=c(0.35,0.5),
     main="Bagging Error versus Iterations",  
     xlab="Iterations", ylab="Error", col = "red")
lines(evol.learn$error, cex = .5 ,col="blue", lty=2)
legend("topright", c("learn","xvalid"), col = c("red", "blue"), lty=1:2)
##  Stabilizes to ~0.40 around 28 iterations