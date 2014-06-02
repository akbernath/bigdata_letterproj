install.packages("gplots")
library(gplots)
testdf<-data.frame(Before=c(10,7,5,9),During=c(8,6,2,5),After=c(5,3,4,3))
rownames(testdf)<-c("Red","Green","Blue","Lightblue")
mm <- as.matrix(testdf, ncol = 3)
heatmap.2(x = mm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = mm, notecol = "black", notecex = 2,
          trace = "none", key = FALSE, margins = c(7, 11))
?heatmap.2
mm
logs<-log(mm)
install.packages("plotrix")
library(plotrix)
mm <- as.matrix(testdf, ncol = 3)
cols <- color.scale(logs, extremes = c("yellow", "lightblue"))

par(mar = c(0.5, 1, 2, 0.5))
# create empty plot
plot(1:10, axes = FALSE, xlab = "", ylab = "", type = "n")

# add table
addtable2plot(x = 1, y = 1, table = testdf,
              bty = "o", display.rownames = TRUE,
              hlines = TRUE, vlines = TRUE,
              bg = cols, cex = 3)

#####NOW MY TURN

logbst<-read.csv("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/hkLogBSTPerc.csv",header=TRUE)
logbst.trim<-logbst[,-1]
row.names(logbst.trim)<-logbst[,1]
logs<-matrix(0,nrow=26,ncol=26)
for(i in 1:26){
  for(j in 1:26){
    if(logbst.trim[i,j]>0){
      logs[i,j]=log(logbst.trim[i,j])
    }
    if(logbst.trim[i,j]==0){
      logs[i,j]=NA
    }
  }
}
head(logs)
log.col<-cols <- color.scale(logs, extremes = c("lightyellow", "dodgerblue"))

par(mar = c(0.5, 1, 2, 0.5))
# create empty plot
plot(1:12, axes = FALSE, xlab = "True Letter", ylab = "Chosen Letter",main="Logistic Regression Binary Search Tree Confusion Matrix", type = "n")

# add table
addtable2plot(x = 1, y = 1, table = logbst.trim,
              bty = "o", display.rownames = TRUE,
              hlines = TRUE, vlines = TRUE,
              bg = log.col,cex=.65)

###CART
cart.dat<-read.csv("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/percentCART.csv",header=TRUE)
cart.trim<-cart.dat[,-1]
rownames(cart.trim)<-cart.dat[,1]

logs<-matrix(0,nrow=26,ncol=26)
for(i in 1:26){
  for(j in 1:26){
    if(cart.trim[i,j]>0){
      logs[i,j]=log(cart.trim[i,j])
    }
    if(cart.trim[i,j]==0){
      logs[i,j]=NA
    }
  }
}
head(logs)
log.col<- color.scale(logs, extremes = c("lightyellow", "dodgerblue"))

par(mar = c(0.5, 1, 2, 0.5))
# create empty plot
plot(1:12, axes = FALSE, xlab = "True Letter", ylab = "Chosen Letter",main="CART Confusion Matrix", type = "n")

# add table
addtable2plot(x = 1, y = 1, table = cart.trim,
              bty = "o", display.rownames = TRUE,
              hlines = TRUE, vlines = TRUE,
              bg = log.col,cex=.65)

install.packages("ggplot2")
library(ggplot2)

cart.prob<-as.numeric(diag(as.matrix(cart.trim)))
letter=LETTERS[1:26]
cart.sum<-cbind(letter,cart.prob)
cart.sum<-as.data.frame(cart.sum)
ggplot(cart.sum, aes(x =as.factor(letter), y = cart.prob,fill=as.numeric(cart.prob))) + geom_bar(stat = "identity")+ylab("Sample Probability of Correct Specification")+xlab("Letter")+scale_fill_continuous(guide = guide_legend(title = ""))+ggtitle("Classification Performance of CART Method by Letter") 

cart.overall<-read.csv("/Users/heatherhisako1/Downloads/CT_confusion.csv",header=TRUE)
cart.o<-cart.overall[,-1]
rownames(cart.o)<-cart.overall[,1]
correct<-sum(as.numeric(diag(as.matrix(cart.o))))

total<-0
for(i in 1:26){
total=total+sum(cart.o[,1])
}

correct/total

####Bag Method
bag.dat<-read.csv("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/percentBAG.csv",header=TRUE)
bag.trim<-bag.dat[,-1]
rownames(bag.trim)<-bag.dat[,1]

logs<-matrix(0,nrow=26,ncol=26)
for(i in 1:26){
  for(j in 1:26){
    if(bag.trim[i,j]>0){
      logs[i,j]=log(bag.trim[i,j])
    }
    if(bag.trim[i,j]==0){
      logs[i,j]=NA
    }
  }
}
head(logs)
log.col<- color.scale(logs, extremes = c("lightyellow", "dodgerblue"))

par(mar = c(0.5, 1, 2, 0.5))
# create empty plot
plot(1:12, axes = FALSE, xlab = "True Letter", ylab = "Chosen Letter",main="BAG Confusion Matrix", type = "n")

# add table
addtable2plot(x = 1, y = 1, table = bag.trim,
              bty = "o", display.rownames = TRUE,
              hlines = TRUE, vlines = TRUE,
              bg = log.col,cex=.65)

install.packages("ggplot2")
library(ggplot2)

bag.prob<-as.numeric(diag(as.matrix(bag.trim)))
letter=LETTERS[1:26]
bag.sum<-cbind(letter,bag.prob)
bag.sum<-as.data.frame(bag.sum)
ggplot(bag.sum, aes(x =as.factor(letter), y = bag.prob,fill=as.numeric(bag.prob))) + geom_bar(stat = "identity")+ylab("Sample Probability of Correct Specification")+xlab("Letter")+scale_fill_continuous(guide = guide_legend(title = ""))+ggtitle("Classification Performance of BAG Method by Letter") 


bag.overall<-read.csv("/Users/heatherhisako1/Downloads/bag_confusion.csv",header=TRUE)
bag.o<-bag.overall[,-1]
rownames(bag.o)<-bag.overall[,1]
correct<-sum(as.numeric(diag(as.matrix(bag.o))))

total<-0
for(i in 1:26){
  total=total+sum(bag.o[,1])
}

correct/total
