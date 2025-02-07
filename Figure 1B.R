library(openxlsx)
library(stringr)
library(png)

dir<-"C:/Manuscript/Figures/"
threshold<-0.1

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 1,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"adult"
temp$Relapse<-FALSE
temp$Relapse[grep("_",temp$ID,fixed=T)]<-TRUE
temp$NewID<-paste0("A_B_A_a_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-temp

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 2,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"adult"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("A_B_B_",helper,"_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 3,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"pediatric"
temp$Relapse<-FALSE
temp$NewID<-paste0("A_A_A_a_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 4,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"pediatric"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("A_A_B_",helper,"_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)


temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 5,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"adult"
temp$Relapse<-FALSE
temp$Relapse[grep("_",temp$ID,fixed=T)]<-TRUE
temp$NewID<-paste0("B_B_A_a_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 6,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"adult"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("B_B_B_",helper,"_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 7,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"pediatric"
temp$Relapse<-FALSE
temp$NewID<-paste0("B_A_A_a_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 8,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"pediatric"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("B_A_B_",helper,"_",temp$ID)
temp$IMI.ID<-str_split_fixed(temp$ID,"_",n=Inf)[,1]
input<-rbind(input,temp)

input<-input[grep("_r",input$ID,invert = T),]
input[!is.na(input[,13]>=threshold)&(input[,13]<threshold),c(2:17)]<-NA
input<-input[is.na(input[,13])|(input[,13]>=threshold),]


overview<-read.xlsx(paste0(dir,"/Supplemental Data 0.xlsx"),sheet = 1,startRow = 2)
input$Age<-NA

for(i in 1:length(input[,1])){
  input$Age[i]<-overview$Age[overview$ID==input$IMI.ID[i]]
}
input<-input[,c("Age","Chr","Gene","NewID","Disease","Category","Relapse")]
input<-unique(input)
input_all<-input
input<-input[!is.na(input$Chr),]


farben<-colorRampPalette(c(rgb(69/255,39/255,245/255,0.53),
                           rgb(39/255,80/255,245/255,0.53),
                           rgb(39/255,208/255,245/255,0.53),
                           rgb(39/255,245/255,187/255,0.53),
                           rgb(245/255,178/255,39/255,0.53),
                           rgb(245/255,234/255,39/255,0.53)))(1001)
farben2<-c()
for(i in 1:length(farben)){
  helper<-col2rgb(farben[i])
  farben2<-c(farben2,rgb(helper[1,1]/255,helper[2,1]/255,helper[3,1]/255,alpha = 0.53))
}







disease<-"T-ALL"
input2<-input[input$Disease==disease,]
genes_a<-sort(table(input2$Gene),decreasing = T)
genes_a2<-names(genes_a)[genes_a>=4]

disease<-"T-LBL"
input2<-input[input$Disease==disease,]
genes_b<-sort(table(input2$Gene),decreasing = T)
genes_b2<-names(genes_b)[genes_b>=7]

genes2<-unique(c(genes_a2,genes_b2))
alter<-c()
for(i in 1:length(genes2)){
  temp<-input$Age[input$Gene==genes2[i]]
  alter<-c(alter,mean(temp))
  names(alter)[i]<-genes2[i]
}
genes2<-genes2[order(alter,decreasing = T)]
alter<-alter[order(alter,decreasing = T)]


disease<-"T-ALL"
input2<-input[input$Disease==disease,]
png(paste0(dir,"Figure1B_",disease,".png"),width=1250,height=900)
par(mgp=c(3,1,0),mar=c(5,6.5,5.5,8))
plot(NULL,xlim=c(4,18*2+(max(input2$Age)-18)*0.5+0.5),
     ylim=c(-1*length(genes2),3),xlab="age [years]",ylab="",bty="n",
     cex.lab = 2,xaxs="i",yaxs="i",xaxt="n",yaxt="n",main="T-ALL (n=77)",cex.main=3)

temp12<-unique(input_all[input_all$Disease==disease,c("NewID","Age","Relapse")])
temp12<-temp12[order(temp12$Age),]
dens_all<-density(temp12$Age,n = 500,from = 2,to = max(input2$Age),"SJ")

for(i in 1:length(genes2)){
  points(x=c(4,18*2+(max(input2$Age)-18)*0.5+0.5),y=c(-1*i,-1*i),col="grey90",type="l")
  temp<-input2$Age[input2$Gene==genes2[i]]
  if(length(temp)>1){
    dens<-density(temp[!is.na(temp)],n = 500,from = 2,to = max(input2$Age),"SJ")
    densy<-round(1000*(dens$y/dens_all$y)/max(dens$y/dens_all$y),0)
    for(j in 1:499){
      if(dens$x[j]<18){
        rect(xleft=dens$x[j]*2,xright=dens$x[j+1]*2,ybottom = -1*(i-1),ytop = -1*i,border = NA,col = farben2[densy[j]+1])      
      }else{
        rect(xleft=(dens$x[j]-18)*0.5+18*2,xright=(dens$x[j+1]-18)*0.5+18*2,ybottom = -1*(i-1),ytop = -1*i,border = NA,col = farben2[densy[j]+1]) 
      }
    }
    j<-500
    rect(xleft=(dens$x[j]-18)*0.5+18*2,xright=(dens$x[j]-18)*0.5+18*2+0.5,ybottom = -1*(i-1),ytop = -1*i,border = NA,col = farben2[densy[j]+1])
    
    if(length(temp)>=4){
      temp1<-unique(input2[input2$Gene==genes2[i]&input2$Age<18,c("NewID","Age")])
      temp1b<-unique(input_all[input_all$Age<18&input_all$Disease==disease,c("NewID","Age")])
      temp2<-unique(input2[input2$Gene==genes2[i]&input2$Age>=18,c("NewID","Age")])
      temp2b<-unique(input_all[input_all$Age>=18&input_all$Disease==disease,c("NewID","Age")])
      
      temp12<-unique(input_all[input_all$Disease==disease,c("NewID","Age","Relapse")])
      temp12<-temp12[order(temp12$Age),]
      temp12$Mutated<-FALSE
      temp12$Mutated[temp12$NewID%in%temp1$NewID|temp12$NewID%in%temp2$NewID]<-TRUE
      
      p_raw<-15*wilcox.test(temp12$Age[temp12$Mutated==F],temp12$Age[temp12$Mutated==T])$p.value
      p_raw<-min(p_raw,1)
      
      p<-p_raw 
      p_angabe<-as.character(round(p,4))
      if(p_angabe=="1"){
        p_angabe<-"1         "
      }
      axis(2,at=(-1*(i-1)-0.5),label=paste0("p=",p_angabe),las=2,tick = F,cex.axis=1.5)
    }
  }else{
    rect(xleft=4,xright=18*2+(max(input2$Age)-18)*0.5+0.5,ybottom = -1*(i-1),ytop = -1*i,border = NA,col = farben2[1]) 
  }  
  
  temp1<-temp[temp<18]
  if(length(temp1)>0)
    text(x=temp1*2,y=rep(-1*(i-1)-0.5,length(temp1)),labels="x",cex=2,font=1)
  temp2<-temp[temp>=18]
  if(length(temp2)>0)
    text(x=(temp2-18)*0.5+18*2,y=rep(-1*(i-1)-0.5,length(temp2)),labels = "x",cex = 2,font = 1)
}
dens<-density(temp12$Age,n = 500,from = 2,to = max(input2$Age),"SJ")
densy<-3*dens$y/max(dens$y)
for(j in 1:499){
  if(dens$x[j]<18){
    rect(xleft=dens$x[j]*2,xright=dens$x[j+1]*2,ybottom = 0,ytop = densy[j],border = NA,col = "grey80")      
  }else{
    rect(xleft=(dens$x[j]-18)*0.5+18*2,xright=(dens$x[j+1]-18)*0.5+18*2,ybottom = 0,ytop = densy[j],border = NA,col = "grey80") 
  }
}
dens2<-density(temp12$Age[temp12$Relapse==T],n = 500,from = 0,to = max(input$Age),"SJ")
densy2<-3*dens2$y/max(dens2$y)
skalierung<-seq(sum(temp12$Relapse==T&temp12$Age<18)/sum(temp12$Age<18),
                sum(temp12$Relapse==T&temp12$Age>=18)/sum(temp12$Age>=18),length.out=7)

for(j in 1:499){
  if(dens2$x[j]<18){
    rect(xleft=dens2$x[j]*2,xright=dens2$x[j+1]*2,ybottom = 0,
         ytop = densy2[j]*skalierung[1],border = NA,col = "grey60")      
  }else{
    if(dens2$x[j]<19){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[2],border = NA,col = "grey60")  
    }
    if(dens2$x[j]<20){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[3],border = NA,col = "grey60")  
    }
    if(dens2$x[j]<21){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[4],border = NA,col = "grey60")  
    }
    if(dens2$x[j]<22){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[5],border = NA,col = "grey60")  
    }
    if(dens2$x[j]<23){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[6],border = NA,col = "grey60")  
    }
    if(dens2$x[j]<24){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[7],border = NA,col = "grey60")  
    }
  }
}
j<-500
rect(xleft=(dens$x[j]-18)*0.5+18*2,xright=(dens$x[j]-18)*0.5+18*2+0.5,ybottom = 0,
     ytop = densy[j],border = NA,col = "grey80")
rect(xleft=4,xright=18*2+(max(input2$Age)-18)*0.5+0.5,ybottom=-1*length(genes2),ytop=0)

genes2b<-c("NOTCH3","  JAK1"," SUZ12","  PHF6"," RUNX1","   MYB","  JAK3","  NRAS","STAT5B",
           "FBXW7","  DNM2","KMT2D","SETD1B","PIK3CD","  PTEN","  USP7","NOTCH1","  EZH2","BCL11B","PIK3R1","CCND3","PIK3CA")

axis(4,at=seq(-0.5,-1*length(genes2),-1),labels=genes2b,las=2,tick=F,cex.axis=2)
axis(1,at=seq(0,18*2,6),labels = seq(0,18,3),cex.axis=1.8)
axis(1,at=seq(18*2+6,18*2+(max(input2$Age)-18)*0.5,5),labels=seq(30,max(input2$Age),10),cex.axis=1.8)
abline(v=18*2)
dev.off()


png(paste0(dir,"Figure1B_Legend.png"),width=100,height=900)
par(mgp=c(3,1,0),mar=c(5,0,5.5,5))
plot(NULL,xlim=c(0,1),bty="n",
     ylim=c(0,length(genes2)+3),xlab="",ylab="",
     xaxs="i",yaxs="i",xaxt="n",yaxt="n")
for(i in 1:length(farben2)){
  rect(xleft=0.1,xright=1,ybottom = (length(genes2))*(i-1)/1001,ytop = (length(genes2))*i/1001,border = NA,col=farben2[i])
}
axis(4,at=seq(0,length(genes2),length.out=6),labels = seq(0,1,0.2),las=2,cex.axis=1)
rect(xleft=0.1,xright=1,ybottom=0,ytop=length(genes2))

text(x=3,y=length(genes2)/2,"normalized mutation density",xpd=T,srt=270,cex=2)
dev.off()









disease<-"T-LBL"
input2<-input[input$Disease==disease,]
maxage<-64
png(paste0(dir,"Figure1B_",disease,".png"),width=1150,height=900)
par(mgp=c(3,1,0),mar=c(5,0.3,5.5,7))
plot(NULL,xlim=c(0,18*2+(max(input2$Age)-18)*0.5+0.5),
     ylim=c(-1*length(genes2),3),xlab="age [years]",ylab="",bty="n",
     cex.lab = 2,xaxs="i",yaxs="i",xaxt="n",yaxt="n",main="T-LBL (n=134)",cex.main=3)

temp12<-unique(input_all[input_all$Disease==disease,c("NewID","Age","Relapse")])
temp12<-temp12[order(temp12$Age),]
dens_all<-density(temp12$Age,n = 500,from = 0,to = maxage,"SJ")

for(i in 1:length(genes2)){
  points(x=c(0,18*2+(max(input2$Age)-18)*0.5+0.5),y=c(-1*i,-1*i),col="grey90",type="l")
  temp<-input2$Age[input2$Gene==genes2[i]]
  if(length(temp)>1){
    dens<-density(temp[!is.na(temp)],n = 500,from = 0,to = maxage,"SJ")
    densy<-round(1000*(dens$y/dens_all$y)/max(dens$y/dens_all$y),0)
    for(j in 1:499){
      if(dens$x[j]<18){
        rect(xleft=dens$x[j]*2,xright=dens$x[j+1]*2,ybottom = -1*(i-1),ytop = -1*i,border = NA,col = farben2[densy[j]+1])      
      }else{
        rect(xleft=(dens$x[j]-18)*0.5+18*2,xright=(dens$x[j+1]-18)*0.5+18*2,ybottom = -1*(i-1),ytop = -1*i,border = NA,col = farben2[densy[j]+1]) 
      }
    }
    j<-500
    rect(xleft=(dens$x[j]-18)*0.5+18*2,xright=(dens$x[j]-18)*0.5+18*2+0.5,ybottom = -1*(i-1),ytop = -1*i,border = NA,col = farben2[densy[j]+1])
    
    if(length(temp)>=7){
      temp1<-unique(input2[input2$Gene==genes2[i]&input2$Age<18,c("NewID","Age","Relapse")])
      temp1b<-unique(input_all[input_all$Age<18&input_all$Disease==disease,c("NewID","Age","Relapse")])
      temp2<-unique(input2[input2$Gene==genes2[i]&input2$Age>=18,c("NewID","Age","Relapse")])
      temp2b<-unique(input_all[input_all$Age>=18&input_all$Disease==disease,c("NewID","Age","Relapse")])
      
      temp12<-unique(input_all[input_all$Disease==disease,c("NewID","Age","Relapse")])
      temp12<-temp12[order(temp12$Age),]
      temp12$Mutated<-FALSE
      temp12$Mutated[temp12$NewID%in%temp1$NewID|temp12$NewID%in%temp2$NewID]<-TRUE
      
      helper<-glm(Mutated ~ Age, family = binomial, data = temp12)
      
      p_raw<-17*wilcox.test(temp12$Age[temp12$Mutated==F],temp12$Age[temp12$Mutated==T])$p.value
      
      p_raw<-min(1,p_raw)
      p<-p_raw
      p_angabe<-as.character(round(p,4))
      if(p_angabe=="0.041")
        p_angabe<-"0.0410"
      if(p_angabe=="0.559")
        p_angabe<-"0.5590"
      
      axis(4,at=(-1*(i-1)-0.5),label=paste0("p=",p_angabe),las=2,tick = F,cex.axis=1.5)  
    }
  }else{
    rect(xleft=0,xright=18*2+(max(input2$Age)-18)*0.5+0.5,ybottom = -1*(i-1),ytop = -1*i,border = NA,col = farben2[1]) 
  }  
  
  temp1<-temp[temp<18]
  if(length(temp1)>0)
    text(x=temp1*2,y=rep(-1*(i-1)-0.5,length(temp1)),labels="x",cex=2,font=1)
  temp2<-temp[temp>=18]
  if(length(temp2)>0)
    text(x=(temp2-18)*0.5+18*2,y=rep(-1*(i-1)-0.5,length(temp2)),labels = "x",cex = 2,font = 1)
  
}
dens<-density(temp12$Age,n = 500,from = 0,to = max(input2$Age),"SJ")
densy<-3*dens$y/max(dens$y)
for(j in 1:499){
  if(dens$x[j]<18){
    rect(xleft=dens$x[j]*2,xright=dens$x[j+1]*2,ybottom = 0,ytop = densy[j],border = NA,col = "grey80")      
  }else{
    rect(xleft=(dens$x[j]-18)*0.5+18*2,xright=(dens$x[j+1]-18)*0.5+18*2,ybottom = 0,ytop = densy[j],border = NA,col = "grey80") 
  }
}
dens2<-density(temp12$Age[temp12$Relapse==T],n = 500,from = 0,to = max(input$Age),"SJ")
densy2<-3*dens2$y/max(dens2$y)
skalierung<-seq(sum(temp12$Relapse==T&temp12$Age<18)/sum(temp12$Age<18),
                sum(temp12$Relapse==T&temp12$Age>=18)/sum(temp12$Age>=18),length.out=7)
for(j in 1:499){
  if(dens2$x[j]<18){
    rect(xleft=dens2$x[j]*2,xright=dens2$x[j+1]*2,ybottom = 0,
         ytop = densy2[j]*skalierung[1],border = NA,col = "grey60")      
  }else{
    if(dens2$x[j]<19){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[2],border = NA,col = "grey60")  
    }
    if(dens2$x[j]<20){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[3],border = NA,col = "grey60")  
    }
    if(dens2$x[j]<21){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[4],border = NA,col = "grey60")  
    }
    if(dens2$x[j]<22){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[5],border = NA,col = "grey60")  
    }
    if(dens2$x[j]<23){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[6],border = NA,col = "grey60")  
    }
    if(dens2$x[j]<24){
      rect(xleft=(dens2$x[j]-18)*0.5+18*2,xright=(dens2$x[j+1]-18)*0.5+18*2,ybottom = 0,
           ytop = densy2[j]*skalierung[7],border = NA,col = "grey60")  
    }
  }
}
j<-500
rect(xleft=(dens$x[j]-18)*0.5+18*2,xright=(dens$x[j]-18)*0.5+18*2+0.5,ybottom = 0,ytop = densy[j],border = NA,col = "grey80")
rect(xleft=0,xright=18*2+(max(input2$Age)-18)*0.5+0.5,ybottom=-1*length(genes2),ytop=0)

axis(1,at=seq(0,18*2,6),labels = seq(0,18,3),cex.axis=1.8)
axis(1,at=seq(18*2+6,18*2+(max(input2$Age)-18)*0.5,5),labels=seq(30,max(input2$Age),10),cex.axis=1.8)
abline(v=18*2)
dev.off()




png(paste0(dir,"Figure 1B.png"),width=2400,height=900)
plot1<-readPNG(paste0(dir,"Figure1B_T-ALL.png"))
plot2<-readPNG(paste0(dir,"Figure1B_T-LBL.png"))
legend<-readPNG(paste0(dir,"Figure1B_Legend.png"))
layout(matrix(data=c(rep(1,23),rep(2,23),3,3),ncol=48))
par(mar=c(0,0,0,0))
plot(NULL,xlim=c(0,1),bty="n",ylim=c(0,1),xlab="",ylab="",xaxs="i",yaxs="i",xaxt="n",yaxt="n")
rasterImage(plot1,0,0,1,1)
plot(NULL,xlim=c(0,1),bty="n",ylim=c(0,1),xlab="",ylab="",xaxs="i",yaxs="i",xaxt="n",yaxt="n")
rasterImage(plot2,0,0,1,1)
plot(NULL,xlim=c(0,1),bty="n",ylim=c(0,1),xlab="",ylab="",xaxs="i",yaxs="i",xaxt="n",yaxt="n")
rasterImage(legend,0,0,1,1)
dev.off()









