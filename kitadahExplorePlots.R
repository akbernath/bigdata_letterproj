#Heather Kitada

let<-read.csv("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/letter-recognition.csv",head=FALSE)
head(let)
colnames(let)<-c("letter","xboxHor","yboxVert","boxWidth","boxHeight","onPixTot","meanXOnPix","meanYOnPix","meanXVar","meanYVar","meanXYCor","meanXXY","meanXYY","meanEdgeCountLR","corrXege","meanEdgeCountBT","corrYege")
attach(let)
allLetters<-LETTERS[1:26]

plot(xboxHor,yboxVert,type="n")
for(i in 1:4000){
  if(letter[i]=="I"){
    j=which(allLetters==paste(letter[i]))
    points(xboxHor[i],yboxVert[i],pch=paste(letter[i]),col=j)
  }
}
?plot
pairs(let)

###what about dendragrams 
#example from:
#http://gastonsanchez.com/blog/how-to/2012/10/03/Dendrograms.html
mtcars
dist(mtcars)
hc = hclust(dist(mtcars))
plot(hc)

#now with letters 
library(dplyr)
library(plyr)
lett.sum<-ddply(let, .(letter), summarise,mean_xboxHor=mean(xboxHor),mean_yboxVert=mean(yboxVert),
                mean_boxWidth=mean(boxWidth),mean_boxHeight=mean(boxHeight),mean_onPixTot=mean(onPixTot),
                mean_meanXOnPix=mean(meanXOnPix),mean_meanYOnPix=mean(meanYOnPix),mean_meanXVar=mean(meanXVar),
                mean_meanYVar=mean(meanYVar),mean_meanXYCor=mean(meanXYCor),mean_meanXXY=mean(meanXXY),
                mean_meanXYY=mean(meanXYY), mean_meanEdgeCountLR=mean(meanEdgeCountLR),mean_corrXege=mean(corrXege),
                mean_meanEdgeCountBT=mean(meanEdgeCountBT),mean_corrYege=mean(corrYege))
dim(lett.sum)

let.means<-lett.sum[,-1]
rownames(let.means)<-lett.sum[,1]
lc=hclust(dist(let.means))
#all labels at same height 
plot(lc,hang=-1)
#test

###Look at learning set
learn<-read.csv("/Users/heatherhisako1/Documents/bigdata_letterproj/Data/learn_set.csv",header=TRUE)
head(learn)
dim(learn)
library(dplyr)
library(plyr)
#learn.sum.n<-ddply(learn, .(letter), summarise,mean_xboxHor=mean(x_box),mean_yboxVert=mean(y_box),
               # mean_boxWidth=mean(width),mean_boxHeight=mean(high),mean_onPixTot=mean(onpix),
              #  mean_meanXOnPix=mean(x_bar),mean_meanYOnPix=mean(y_bar),mean_meanXVar=mean(x2_bar),
               # mean_meanYVar=mean(y2_bar),mean_meanXYCor=mean(xy_bar),mean_meanXXY=mean(x2y_bar),
              # mean_meanXYY=mean(xy2_bar), mean_meanEdgeCountLR=mean(x_ege),mean_corrXege=mean(x_egvy),
              #  mean_meanEdgeCountBT=mean(y_ege),mean_corrYege=mean(y_egvx),n=sum(x_box>0))
learn.sum<-ddply(learn, .(letter), summarise,mean_xboxHor=mean(x_box),mean_yboxVert=mean(y_box),
                 mean_boxWidth=mean(width),mean_boxHeight=mean(high),mean_onPixTot=mean(onpix),
                 mean_meanXOnPix=mean(x_bar),mean_meanYOnPix=mean(y_bar),mean_meanXVar=mean(x2_bar),
                 mean_meanYVar=mean(y2_bar),mean_meanXYCor=mean(xy_bar),mean_meanXXY=mean(x2y_bar),
                 mean_meanXYY=mean(xy2_bar), mean_meanEdgeCountLR=mean(x_ege),mean_corrXege=mean(x_egvy),
                 mean_meanEdgeCountBT=mean(y_ege),mean_corrYege=mean(y_egvx))
dim(learn.sum)
head(learn.sum)
sum(learn.sum$n)

learn.means<-learn.sum[,-1]
rownames(learn.means)<-learn.sum[,1]
lc=hclust(dist(learn.means),method="complete")
plot(lc)
names(lc)
lc$height
lc$order

###I want to make a matrix to create a binary search tree
##THIS CODE IS INCOMPLETE! 
##I was trying to automate the bst 

learn.bst<-matrix(c(rep(-1,650)),nrow=26,ncol=25)
rownames(learn.bst)<-LETTERS[1:26]
for(i in 1:25){#if both are negative this is the first merge
  if(lc$merge[i,1]<0 & lc$merge[i,2]<0){
    learn.bst[abs(lc$merge[i,1]),i:25]<-rep(0,length(i:25))
    learn.bst[abs(lc$merge[i,2]),i:25]<-rep(0,length(i:25))
    learn.bst[abs(lc$merge[i,1]),i]="L"
    learn.bst[abs(lc$merge[i,2]),i]="R"
}
  if(lc$merge[i,1]<0 & lc$merge[i,2]>0){
    learn.bst[abs(lc$merge[i,1]),i:25]<-rep(0,length(i:25))
    learn.bst[abs(lc$merge[i,1]),i]="L"
    tree<-lc$merge[i,2]
    while(tree>1){
    for(i in 1:i){
     ###INCOMPLETE! 
    }
    }
    learn.bst[abs(lc$merge[i,2]),i]="R"
  }
}

###BST 
learn.bst<-read.csv("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/learnBST.csv",header=FALSE)
learn.mat<-learn.bst[,-1]
rownames(learn.mat)<-learn.bst[,1]
depth<-matrix(0,nrow=26,ncol=1)
for(i in 1:26){
  depth[i]<-sum(learn.mat[i,]>=0)
}
learn.mat<-cbind(depth,learn.mat)


#test case
new.data<-learn[5000,2:17]
true<-learn[5000,18]
true

##RUN CODE
path<-c()
end<-1

while(end >=0){
  left<-c()
  right<-c()
for(i in 1:26){
  
##splits data into subsets for left and right 
  count.left<-0
  path.left<-c(path,0)
  for(a in 1:length(path.left)){
    b=a+1
    if(learn.mat[i,b]==path.left[a]){
      count.left=count.left+1
    }
  }
  if(count.left==length(path.left)){
    left<-c(left,rownames(learn.mat)[i])
  }
  count.right<-0
  path.right<-c(path,1)
  for(a in 1:length(path.right)){
    b=a+1
    if(learn.mat[i,b]==path.right[a]){
      count.right=count.right+1
    }
  }
  if(count.right==length(path.right)){
    right<-c(right,rownames(learn.mat)[i])
  }
}
print(right)
print(left)
##build a logistic regression model 
resp<-c(rep(NA,13333))
for(i in 1:13333){
  if(learn$letter[i] %in% right){
    resp[i]<-1
  }
  if(learn$letter[i] %in% left){
    resp[i]<-0
  }
  
}
mod<-glm(resp~learn$x_box+learn$y_box+learn$width+learn$high+learn$onpix+learn$x_bar+learn$y_bar+learn$x2_bar+learn$y2_bar+learn$xy_bar+learn$x2y_bar+learn$xy2_bar+learn$x_ege+learn$x_egvy+learn$y_ege+learn$y_egvx,family="binomial")
##this returns the sample probability from the new data 
this.logit<-as.numeric(mod$coef[1])
for(i in 2:17){
  j=i-1
  this.logit=this.logit+as.numeric(mod$coef[i])*as.numeric(new.data[j])
}
pi.hat=exp(this.logit)/(1+exp(this.logit))
##decide where to move (right=1, left=0)
if(pi.hat>=0.5){
  path<-c(path,1)
}
if(pi.hat<0.5){
  path<-c(path,0)
}

print(path)

if(length(left)==0 | length(right)==0){
  end=-1
}


}




  