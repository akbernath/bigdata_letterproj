#Heather Kitada
#BST Code 

##Import learn data
learn<-read.csv("/Users/heatherhisako1/Documents/bigdata_letterproj/Data/learn_set.csv",header=TRUE)
head(learn)
dim(learn)

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
new.data<-learn[10000,2:17]
true<-learn[10000,18]
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
  print(round(mod$coef,2))
  this.logit<-as.numeric(mod$coef[1])
  for(i in 2:17){
    j=i-1
    this.logit=this.logit+as.numeric(mod$coef[i])*as.numeric(new.data[j])
  }
  pi.hat=exp(this.logit)/(1+exp(this.logit))
  print(pi.hat)
  ##decide where to move (right=1, left=0)
  if(pi.hat>=0.5){
    move=1
  }
  if(pi.hat<0.5){
    move=0
  }
  path<-c(path,move)
  
  print(path)
  if(move==1 & length(right)==1) {
    choice<-right
  }
  if(move==0 & length(left)==1) {
    choice<-left
  }
  
  if((move==1 & length(right)==1) | (move==0 & length(left)==1)){
    end=-1
  }
}

choice==true

####Generalize code to run multiple tests 
valid<-read.csv("/Users/heatherhisako1/Documents/bigdata_letterproj/Data/xvalid_set.csv",head=TRUE)
head(valid)
dim(valid)
### Andrew did not code the validation set the same as the training set 
##labels are out of order 
specification<-c()
store.choice<-c()
store.true<-c()
for(h in 3001: 6667){
  new.data<-valid[h,3:18]
  true<-valid[h,2]
  
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
      move=1
    }
    if(pi.hat<0.5){
      move=0
    }
    path<-c(path,move)
    if(move==1 & length(right)==1) {
      choice<-right
    }
    if(move==0 & length(left)==1) {
      choice<-left
    }
    
    if((move==1 & length(right)==1) | (move==0 & length(left)==1)){
      end=-1
    }
  }
  
  specification<-c(specification,choice==true)
  store.choice<-c(store.choice,choice)
  store.true<-c(store.true,true)
  print(h)
}
mean(specification)
##test run on 5/26/14 --> 0.004499775
#this run doesn't count.. there was an error in the code 

##test run on 5/27/2014 --> 0.7483126
valid.letters<-as.character(valid[,2])
bst.out<-cbind(valid.letters, as.numeric(specification))
#write.csv(bst.out, file = "/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/bst_out.csv")
library(dplyr)
library(plyr)
bst_csv<-read.csv("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/bst_out.csv",header=TRUE)
bst.sum<-ddply(bst_csv, .(letter), summarise,prob=mean(valid))

#create plots to see how specification is distributed 
barplot( bst.sum$prob,names.arg=bst.sum$letter)

library(ggplot2)
ggplot(bst.sum, aes(x =as.factor(letter), y = prob,fill=as.numeric(prob))) + geom_bar(stat = "identity")+ylab("Sample Probability of Correct Specification")+xlab("Letter")+scale_fill_continuous(guide = guide_legend(title = ""))+ggtitle("Classification Performance of Logistic Regression BST by Letter") 
                                                                                                                                                                                             
##summarise validation set 
valid.sum<-ddply(valid, .(letter), summarise,n=sum(x_box>0))
#H is 1783 to 2050

ones<-rep(1,6667)
new<-cbind(letter=LETTERS[store.true],store.choice,specification,ones)
head(new)

write.csv(new, file = "/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/bst_all.csv")
all_csv<-read.csv("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/bst_all.csv",header=TRUE)


all.sum<-ddply(all_csv, .(letter,store.choice), summarise,prob=sum(ones))
head(all.sum)

spec.mat<-matrix(0, nrow=26, ncol=26)
rownames(spec.mat)<-LETTERS[1:26]
colnames(spec.mat)<-LETTERS[1:26]
for(i in 1:26){
  for(j in 1:26){
    for(k in 1:346){
      if(all.sum[k,1]==colnames(spec.mat)[i] & all.sum[k,2]==rownames(spec.mat)[j]){
        spec.mat[j,i]=all.sum[k,3]
      }
    }
  }
}

write.csv(spec.mat, file = "/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/spec_mat.csv")

