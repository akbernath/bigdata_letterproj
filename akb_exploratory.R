##  Andrew Bernath
##  ST599 - Big Data Analysis
##  Project 3 - Letter classify
##  You brought her, YOU let her classify!!!

##  Sorry, couldn't resist.

setwd("D:/School/Spring 14/ST599/bigdata_letterproj")

library(dplyr)
library(ggplot2)
library(rpart)
library(sampling)


#   Read in data, keeping letter as a factor, else integers
letters <- read.csv("data/letter_data.csv", stringsAsFactors=F)
letters$letter <- as.factor(letters$letter)


#   SRS with replacement sample for learning set
learn_samp <- srswr(round(2*nrow(letters)/3), nrow(letters))


#   Create data frame and for loop to build learning set from SRS
learn_set <- data.frame()
for(i in 1:length(learn_samp)){     # index to iterate through each selected row
  if(learn_samp[i] > 0){            # logical test for row inclusion
    n <- learn_samp[i]
    for(j in 1:n){                  # appends replicates chosen rows to data frame
      learn_set <- rbind(learn_set,letters[i,])
    }
  }
}
rownames(learn_set) <- NULL         # reset row names to generic numbering  
dim(learn_set)                      # verify correct dimensions


#   Build tree with learning set
mod.init <- rpart(letter ~ .,
                  data=learn_set,
                  method="class")
summary(mod.init)
plot(mod.init,)
text(mod.init)
printcp(mod.init)
??rpart

#   Playing with random forests... not really sure how they work
library(randomForest)
forest.mod <- randomForest(letter~.,
             data=learn_set,
             importance=TRUE,
             replace=TRUE,
             sampsize=round(2*nrow(letters)/3))
summary(forest.mod)
importance(forest.mod)
plot(forest.mod)
