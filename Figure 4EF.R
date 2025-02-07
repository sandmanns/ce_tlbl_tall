library(colorspace)

dir<-"C:/Manuscript/Figures/"

analyse<-"2"
analyse3<-"3"

namen1<-c("E","F")
namen2<-c("T-ALL pediatric (n=31)","T-LBL pediatric (n=48)")

varianten_all<-c()
for(n in 1:length(namen1)){
  input<-read.table(paste0(dir,"Figure4",namen1[n],"_",analyse,".txt"),header=T,sep="\t")
  input<-input[input$Rel>=0.1,]
  input$prop[input$prop<0]<-0
  input<-input[order(input$prop,input$Rel,input$From,input$To),]
  input<-input[order(input$Rel,decreasing = T),]
  
  input3<-read.table(paste0(dir,"Figure4",namen1[n],"_",analyse3,".txt"),header=T,sep="\t")
  input3<-input3[order(input3$prop,input3$Rel,input3$From,input3$To,input3$Further),]
  input3<-input3[order(input3$Rel,decreasing = T),]
  input3<-input3[input3$Rel>=0.05,]
  input3$prop[input3$prop<0]<-0
  
  varianten<-unique(c(input$From,input$To,input3$From,input3$To,input3$Further))
  
  varianten_all<-unique(c(varianten_all,varianten))
}



farben1<-c("#6b7f8b","#bfc8cd","#a2c4c9","#a2c9ae","#939ecd","#a5e6d7",
           "#2c495a","#a5e6e5",
           "#3d85c6","#4b838f","#2c305a","#275e6a","#62bed2","#62d2cf","#24855c","#049a47","#2d1895","#062a8d")
names(farben1)<-varianten_all[grep("_",varianten_all,fixed = T)]

farben2<-c("#c57800","#f44336","#f09999","#f9cb9c","#ffe599","#f1c232","#f8ca78",
           "#c2c27b","#ff731f","#c05555","#ff7777",
           "#eb7d09","#f8e078","#f8a878","#fff2cc","#f4cccc","#cbb39a","#c2a755","#660000","#b45f06","#cc0000")
names(farben2)<-varianten_all[grep("_",varianten_all,fixed = T,invert = T)]

farben<-c(farben1,farben2)
farben<-farben[!is.na(names(farben))]



n<-1
input<-read.table(paste0(dir,"Figure4",namen1[n],"_",analyse,".txt"),header=T,sep="\t")
input<-input[input$Rel>=0.1,]
input$prop[input$prop<0]<-0
input<-input[order(input$prop,input$Rel,input$From,input$To),]
input<-input[order(input$prop,decreasing = F),]

input3<-read.table(paste0(dir,"Figure4",namen1[n],"_",analyse3,".txt"),header=T,sep="\t")
input3<-input3[order(input3$prop,input3$Rel,input3$From,input3$To,input3$Further),]
input3<-input3[order(input3$prop,decreasing = F),]
input3<-input3[input3$Rel>=0.05,]
input3$prop[input3$prop<0]<-0

varianten<-unique(c(input$From,input$To,input3$From,input3$To,input3$Further))

png(paste0(dir,"Figure 4",namen1[n],".png"),width=1600,height=1200)
par(mar=c(3,0.5,3,0.5))
plot(NULL,xlim=c(-0.3,4.1),ylim=c(-1*(length(input[,1])+length(input3[,1])),2),xlab="",ylab="",xaxt="n",xaxs="i",
     main=paste0(namen2[n]),yaxt="n",yaxs="i",bty="n",cex.main=5)

if(length(input[,1])>0){
  for(i in 1:length(input[,1])){
    rect(xleft=-0.3,ytop=1.3-i,ybottom=0.7-i,xright=0.3,col = farben[names(farben)==input$From[i]])
    rect(xleft=0.7,ytop=1.3-i,ybottom=0.7-i,xright=1.3,col = farben[names(farben)==input$To[i]])
    points(x=c(0.3,0.7),y=c(1-i,1-i),type="l",lwd=3)
    points(x=c(0.6,0.7),y=c(1.2-i,1-i),type="l",lwd=3)
    points(x=c(0.6,0.7),y=c(0.8-i,1-i),type="l",lwd=3)
    text(0,1-i,input$From[i],col=ifelse(as(hex2RGB(farben[names(farben)==input$From[i]]), "polarLUV")@coords[1]<55,"white","black"),font=2,cex=3)
    text(1,1-i,input$To[i],col=ifelse(as(hex2RGB(farben[names(farben)==input$To[i]]), "polarLUV")@coords[1]<55,"white","black"),font=2,cex=3)
  }
}
if(length(input3[,1])>0){
  points(x=c(-0.4,4.4),y=c(0.5-length(input[,1]),0.5-length(input[,1])),type="l",col="grey")
  for(i in 1:length(input3[,1])){
    rect(xleft=-0.3,ytop=1.3-i-length(input[,1]),ybottom=0.7-i-length(input[,1]),xright=0.3,col = farben[names(farben)==input3$From[i]])
    rect(xleft=0.7,ytop=1.3-i-length(input[,1]),ybottom=0.7-i-length(input[,1]),xright=1.3,col = farben[names(farben)==input3$To[i]])
    points(x=c(0.3,0.7),y=c(1-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    points(x=c(0.6,0.7),y=c(1.2-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    points(x=c(0.6,0.7),y=c(0.8-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    rect(xleft=1.7,ytop=1.3-i-length(input[,1]),ybottom=0.7-i-length(input[,1]),xright=2.3,col = farben[names(farben)==input3$Further[i]])
    points(x=c(1.3,1.7),y=c(1-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    points(x=c(1.6,1.7),y=c(1.2-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    points(x=c(1.6,1.7),y=c(0.8-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    text(0,1-i-length(input[,1]),input3$From[i],
         col=ifelse(as(hex2RGB(farben[names(farben)==input3$From[i]]), "polarLUV")@coords[1]<55,"white","black"),font=2,cex=3)
    text(1,1-i-length(input[,1]),input3$To[i],
         col=ifelse(as(hex2RGB(farben[names(farben)==input3$To[i]]), "polarLUV")@coords[1]<55,"white","black"),font=2,cex=3)
    text(2,1-i-length(input[,1]),input3$Further[i],
         col=ifelse(as(hex2RGB(farben[names(farben)==input3$Further[i]]), "polarLUV")@coords[1]<55,"white","black"),font=2,cex=3)
  } 
}
bar_helper<-c(input$Rel)*2
barplot(bar_helper,horiz = T,add = T,offset = 3,col="black",width = c(-0.6),space = c(-0.2,rep(0.6,length(bar_helper)-1)),axes = F)

bar_helper<-c(input3$Rel)*2
barplot(bar_helper,horiz = T,add = T,offset = 3,col="black",width = c(-0.6),space = c(11.3,rep(0.7,length(bar_helper)-1)),axes = F)

axis(1,at=c(3,3.5,4),labels = c("0%","25%","50%"),cex.axis=2)
text(x=3.75,y=0,"*",cex=3,font=2)
text(x=3.41,y=-7,"**",cex=3,font=2)
dev.off()




n<-2
input<-read.table(paste0(dir,"Figure4",namen1[n],"_",analyse,".txt"),header=T,sep="\t")
input<-input[input$Rel>=0.1,]
input$prop[input$prop<0]<-0
input<-input[order(input$prop,input$Rel,input$From,input$To),]
input<-input[order(input$prop,decreasing = F),]

input3<-read.table(paste0(dir,"Figure4",namen1[n],"_",analyse3,".txt"),header=T,sep="\t")
input3<-input3[order(input3$prop,input3$Rel,input3$From,input3$To,input3$Further),]
input3<-input3[order(input3$prop,decreasing = F),]
input3<-input3[input3$Rel>=0.05,]
input3$prop[input3$prop<0]<-0

varianten<-unique(c(input$From,input$To,input3$From,input3$To,input3$Further))

png(paste0(dir,"Figure 4",namen1[n],".png"),width=1600,height=800)
par(mar=c(3,0.5,3,0.5))
plot(NULL,xlim=c(-0.3,4.1),ylim=c(-1*(length(input[,1])+length(input3[,1])),2),xlab="",ylab="",xaxt="n",xaxs="i",
     main=paste0(namen2[n]),yaxt="n",yaxs="i",bty="n",cex.main=5)

if(length(input[,1])>0){
  for(i in 1:length(input[,1])){
    rect(xleft=-0.3,ytop=1.3-i,ybottom=0.7-i,xright=0.3,col = farben[names(farben)==input$From[i]])
    rect(xleft=0.7,ytop=1.3-i,ybottom=0.7-i,xright=1.3,col = farben[names(farben)==input$To[i]])
    points(x=c(0.3,0.7),y=c(1-i,1-i),type="l",lwd=3)
    points(x=c(0.6,0.7),y=c(1.2-i,1-i),type="l",lwd=3)
    points(x=c(0.6,0.7),y=c(0.8-i,1-i),type="l",lwd=3)
    text(0,1-i,input$From[i],col=ifelse(as(hex2RGB(farben[names(farben)==input$From[i]]), "polarLUV")@coords[1]<55,"white","black"),font=2,cex=3)
    text(1,1-i,input$To[i],col=ifelse(as(hex2RGB(farben[names(farben)==input$To[i]]), "polarLUV")@coords[1]<55,"white","black"),font=2,cex=3)
  }
}
if(length(input3[,1])>0){
  points(x=c(-0.4,4.4),y=c(0.5-length(input[,1]),0.5-length(input[,1])),type="l",col="grey")
  for(i in 1:length(input3[,1])){
    rect(xleft=-0.3,ytop=1.3-i-length(input[,1]),ybottom=0.7-i-length(input[,1]),xright=0.3,col = farben[names(farben)==input3$From[i]])
    rect(xleft=0.7,ytop=1.3-i-length(input[,1]),ybottom=0.7-i-length(input[,1]),xright=1.3,col = farben[names(farben)==input3$To[i]])
    points(x=c(0.3,0.7),y=c(1-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    points(x=c(0.6,0.7),y=c(1.2-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    points(x=c(0.6,0.7),y=c(0.8-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    rect(xleft=1.7,ytop=1.3-i-length(input[,1]),ybottom=0.7-i-length(input[,1]),xright=2.3,col = farben[names(farben)==input3$Further[i]])
    points(x=c(1.3,1.7),y=c(1-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    points(x=c(1.6,1.7),y=c(1.2-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    points(x=c(1.6,1.7),y=c(0.8-i-length(input[,1]),1-i-length(input[,1])),type="l",lwd=3)
    text(0,1-i-length(input[,1]),input3$From[i],
         col=ifelse(as(hex2RGB(farben[names(farben)==input3$From[i]]), "polarLUV")@coords[1]<55,"white","black"),font=2,cex=3)
    text(1,1-i-length(input[,1]),input3$To[i],
         col=ifelse(as(hex2RGB(farben[names(farben)==input3$To[i]]), "polarLUV")@coords[1]<55,"white","black"),font=2,cex=3)
    text(2,1-i-length(input[,1]),input3$Further[i],
         col=ifelse(as(hex2RGB(farben[names(farben)==input3$Further[i]]), "polarLUV")@coords[1]<55,"white","black"),font=2,cex=3)
  } 
}
bar_helper<-c(input$Rel)*2
barplot(bar_helper,horiz = T,add = T,offset = 3,col="black",width = c(-0.6),space = c(-0.2,rep(0.6,length(bar_helper)-1)),axes = F)

bar_helper<-c(input3$Rel)*2
barplot(bar_helper,horiz = T,add = T,offset = 3,col="black",width = c(-0.6),space = c(12.7,rep(0.7,length(bar_helper)-1)),axes = F)

axis(1,at=c(3,3.5,4),labels = c("0%","25%","50%"),cex.axis=2)
text(x=3.55,y=0,"**",cex=3,font=2)

text(x=3.3,y=-8,"***",cex=3,font=2)
text(x=3.3,y=-9,"***",cex=3,font=2)
dev.off()




