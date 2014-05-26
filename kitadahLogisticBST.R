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
new.data<-learn[531,2:17]
true<-learn[531,18]
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