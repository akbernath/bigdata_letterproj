##  Andrew Bernath
##  ST599 - Big Data Analysis
##  Project 3 - Code to split data

##  Set directory and load libraries
setwd("D:/School/Spring 14/ST599/bigdata_letterproj")
library(dplyr)
library(Hmisc)
library(sampling)


##  Read in data, keeping letter as a factor, else integers
letters <- read.csv("data/letter_data.csv", stringsAsFactors=F)
letters$letter <- as.factor(letters$letter)
letters <- letters[order(letters[,1]),]


##  Sample data by letter class
learn_sizes <- as.numeric(round(2*table(letters$letter)/3))    # Define sample sizes
learn_samp <- strata(letters,
                     stratanames="letter",
                     size=learn_sizes,
                     method="srswor")
dim(learn_samp)    # Verify that this generates 13333 obs


##  Create learning set
learn_set <- getdata(letters, learn_samp)
table(learn_set[,"letter"])  # Verify counts
learn_sizes


##  Create cross validation set from rows not selected for learning
learn_rows <- as.numeric(rownames(learn_samp))
xvalid_set <- subset(letters, rownames(letters) %nin% learn_rows)
dim(xvalid_set)    # Verify that this generates 6667 obs


##  Clean up data frames and reset row names to default
names(learn_set)
learn_set <- learn_set[,-c(18:20)]
rownames(learn_set) <- NULL
rownames(xvalid_set) <- NULL


##  Output sets to csv
write.csv(learn_set, "data/learn_set.csv", na="NA")
write.csv(xvalid_set, "data/xvalid_set.csv", na="NA")

