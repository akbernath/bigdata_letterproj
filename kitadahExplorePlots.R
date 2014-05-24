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
