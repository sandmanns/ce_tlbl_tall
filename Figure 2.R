library(stringr)
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





namen1<-c("A","B","C","D")
namen2<-c("T-ALL pediatric (n=","T-ALL adult (n=",
          "T-LBL pediatric (n=","T-LBL adult (n=")

subgroups<-list()
subgroups[[1]]<-input[input$Disease=="T-ALL"&input$Category=="pediatric",]
subgroups[[2]]<-input[input$Disease=="T-ALL"&input$Category=="adult",]
subgroups[[3]]<-input[input$Disease=="T-LBL"&input$Category=="pediatric",]
subgroups[[4]]<-input[input$Disease=="T-LBL"&input$Category=="adult",]



temp<-input
temp<-unique(temp[,c("ID","Gene","Disease","Category","Relapse","NewID","IMI.ID")])
temp2<-temp[!is.na(temp$Gene),]

temp2$GenePathway<-NA
pathways<-read.table(paste0(dir,"Pathways.txt"),header=T,sep="\t")
for(i in 1:length(pathways[,1])){
  temp2$GenePathway[which(!is.na(temp2$Gene)&temp2$Gene==pathways$Gene[i])]<-pathways$Pathway[i]
}

gene<-unique(temp2[,c("Gene","GenePathway")])
gene<-gene[order(gene$GenePathway,gene$Gene),]
counts<-as.data.frame.matrix(table(temp2$Gene,temp2$Disease))
gene<-cbind(gene,counts[match(gene$Gene,rownames(counts)),])

count_all<-table(unique(temp[,c("NewID","Disease")])[,2])
keep<-c()
for(i in 1:nrow(gene)){
  if(gene[i,3]>=round(0.05*count_all[1],0)||
     gene[i,4]>=round(0.05*count_all[2],0)){
    keep<-c(keep,T)
  }else{
    keep<-c(keep,F)
  }
}

gene<-gene[keep,]




temp<-input
temp<-unique(temp[,c("ID","Gene","Disease","Category","Relapse","NewID","IMI.ID")])
temp$Kombi<-paste0(temp$Disease,"_",temp$Category)
temp2<-temp[!is.na(temp$Gene),]

temp2$GenePathway<-NA
pathways<-read.table(paste0(dir,"Pathways.txt"),header=T,sep="\t")
for(i in 1:length(pathways[,1])){
  temp2$GenePathway[which(!is.na(temp2$Gene)&temp2$Gene==pathways$Gene[i])]<-pathways$Pathway[i]
}
temp2$Kombi<-paste0(temp2$Disease,"_",temp2$Category)

counts<-as.data.frame.matrix(table(temp2$Gene,temp2$Kombi))
gene<-cbind(gene[,c(1:2)],counts[match(gene$Gene,rownames(counts)),])




for(n in 1:4){
  message(n)
  temp<-subgroups[[n]]
  
  temp<-unique(temp[,c("ID","Gene","Disease","Category","Relapse","NewID","IMI.ID")])
  temp2<-temp[!is.na(temp$Gene),]
  
  temp2$GenePathway<-NA
  pathways<-read.table(paste0(dir,"Pathways.txt"),header=T,sep="\t")
  for(i in 1:length(pathways[,1])){
    temp2$GenePathway[which(!is.na(temp2$Gene)&temp2$Gene==pathways$Gene[i])]<-pathways$Pathway[i]
  }
  
  samples<-unique(temp$NewID)
  temp2<-temp2[temp2$Gene%in%gene$Gene,]
  
  
  output_freq<-matrix(data=rep(NA,length(gene[,1])*length(gene[,1])),nrow=length(gene[,1]))
  rownames(output_freq)<-colnames(output_freq)<-gene$Gene
  output_freq_p<-output_freq
  
  
  all<-as.data.frame.matrix(table(temp2$NewID,temp2$Gene))
  for(i in 1:(length(all[1,])-1)){
    for(j in (i+1):length(all[1,])){
      output_freq[colnames(all)[i]==rownames(output_freq),colnames(all)[j]==colnames(output_freq)]<-fisher.test(x=all[,i],y=all[,j])$estimate
      output_freq[colnames(all)[j]==colnames(output_freq),colnames(all)[i]==rownames(output_freq)]<-fisher.test(x=all[,i],y=all[,j])$estimate
      
      if(sum(all[,i])>=round(0.05*length(samples),0)&&sum(all[,j])>=round(0.05*length(samples),0)){
        output_freq_p[colnames(all)[i]==rownames(output_freq_p),colnames(all)[j]==colnames(output_freq_p)]<-fisher.test(x=all[,i],y=all[,j])$p.value
        output_freq_p[colnames(all)[j]==colnames(output_freq_p),colnames(all)[i]==rownames(output_freq_p)]<-fisher.test(x=all[,i],y=all[,j])$p.value
      }
    }
  }
  
  farben<-c(colorRampPalette(c("steelblue4","steelblue1","lightblue2"))(100),
            "grey90",
            colorRampPalette(c("pink","brown1","brown4"))(100))
  
  output_freq[output_freq>2]<-2

  png(paste0(dir,"Cooc_middle.png"),width = 1500,height=1500)
  par(mar=c(24,24,0.5,0.5),mgp=c(3,1,0))
  plot(NULL,xlim=c(0.5,length(output_freq[,1])+0.5),ylim=c(0.5,length(output_freq[,1])+0.5),
       xlab="",ylab="",xaxt="n",yaxt="n",xaxs="i",yaxs="i",
       main="",cex.main=2)
  axis(1,at=c(1:length(output_freq[,1])),labels = gene$Gene,las=2,tick=F,cex.axis=2)
  axis(2,at=c(1:length(output_freq[,1])),labels = gene$Gene,las=2,tick=F,cex.axis=2)
  for(i in 1:length(output_freq[,1])){
    for(j in 1:length(output_freq[,1])){
      if(!is.na(output_freq[i,j])){
        points(x=i,y=j,cex=ifelse(!is.na(output_freq_p[i,j]),4.3,1.5),
               col=farben[round(100*output_freq[i,j],0)+1],pch=16)  
        if(!is.na(output_freq_p[i,j])&&output_freq_p[i,j]<0.05){
          text(x=i,y=j,labels = "*",col="white",cex=4)
        }
      }
      
      if(i==j){
        points(x=i,y=j,col="black",pch=21,lwd=2,cex=0)
      }
    }
  }
  for(i in 1:length(output_freq[,1])){
    points(x=c(0.5,length(output_freq[,1])+0.5),y=c(i-0.5,i-0.5),type="l",col="grey85")
    points(y=c(0.5,length(output_freq[,1])+0.5),x=c(i-0.5,i-0.5),type="l",col="grey85")
  }
  count_p<-table(gene$GenePathway)
  names(count_p)[names(count_p)=="Zother"]<-"other"
  namen_path<-names(count_p)
  for(i in 1:length(namen_path)){
    if(nchar(namen_path[i])>14)
      namen_path[i]<-gsub(" ","\n",namen_path[i])
  }

  pos<-1
  par(mgp=c(3,9,0))
  for(i in cumsum(count_p)){
    points(x=c(0.5,length(output_freq[,1])+0.5),y=c(i+0.5,i+0.5),type="l",col="black")
    points(y=c(0.5,length(output_freq[,1])+0.5),x=c(i+0.5,i+0.5),type="l",col="black")
    axis(1,at=(0.5+i-count_p[pos]/2),namen_path[pos],tick=F,cex.axis=2.7,las=2)
    axis(2,at=(0.5+i-count_p[pos]/2),namen_path[pos],tick = F,cex.axis=2.7,las=1)
    pos<-pos+1
  }
  points(x=c(0.5,length(output_freq[,1])+0.5),y=c(0.5,0.5),type="l",col="black")
  points(y=c(0.5,length(output_freq[,1])+0.5),x=c(0.5,0.5),type="l",col="black")
  
  
  points(x=c(0.5,length(output_freq[,1])+0.5),y=c(0.5,length(output_freq[,1])+0.5),type="l",col="black",lwd=2)
  dev.off()
  
  
  
  
  png(paste0(dir,"Cooc_right.png"),width = 250,height=1500)
  par(mar=c(24,0.5,0.5,0.8),mgp=c(3,1,0))
  barplot(gene[,n+2],horiz=T,col="black",
          yaxt="n",yaxs="i",cex.axis=2)
  dev.off()
  
  png(paste0(dir,"Cooc_top.png"),width =1500,height=300)
  par(mar=c(0.5,24,6,0.5),mgp=c(3,1,0))
  barplot(gene[,n+2],horiz=F,col="black",
          xaxt="n",las=1,xaxs="i",main=paste0(namen2[n],length(samples),")"),cex.main=4,cex.axis=2)
  dev.off()
  
  png(paste0(dir,"Cooc_legend.png"),width =250,height=300)
  par(mar=c(0,0,0,0))
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  legend("center",legend = c(">2",1.5,1,0.5,0),pch=16,col=farben[c(201,151,101,51,1)],
         cex=2.5,pt.cex = 3.5,title = "Odds Ratio")
  dev.off()
  
  
  plot1<-readPNG(paste0(dir,"Cooc_middle.png"))
  plot2<-readPNG(paste0(dir,"Cooc_right.png"))
  plot3<-readPNG(paste0(dir,"Cooc_top.png"))
  plot4<-readPNG(paste0(dir,"Cooc_legend.png"))
  
  png(paste0(dir,"Figure 2",namen1[n],".png"),width = 1750,height=1800)
  layout(matrix(data=c(rep(3,30),4,4,4,4,4,
                       rep(3,30),4,4,4,4,4,
                       rep(3,30),4,4,4,4,4,
                       rep(c(rep(1,30),2,2,2,2,2),15)),nrow=18,byrow = T))
  par(mar=c(0,0,0,0))
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot1,0,0,1,1)
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot2,0,0,1,1)
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot3,0,0,1,1)
  plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xaxs="i",yaxs="i")
  rasterImage(plot4,0,0,1,1)
  dev.off()
  
}



