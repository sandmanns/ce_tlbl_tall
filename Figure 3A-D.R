library(openxlsx)
library(stringr)
library(GenomicRanges)
library(GenVisR)

dir<-"C:/Manuscript/Figures/"

##CNVs
temp<-read.xlsx(paste0(dir,"Supplemental Data 2.xlsx"),sheet = 1,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"adult"
temp$Relapse<-FALSE
temp$NewID<-paste0("A_B_A_a_",temp$ID)
input<-temp

temp<-read.xlsx(paste0(dir,"Supplemental Data 2.xlsx"),sheet = 3,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"pediatric"
temp$Relapse<-FALSE
temp$NewID<-paste0("A_A_A_a_",temp$ID)
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 2.xlsx"),sheet = 4,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"pediatric"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("A_A_A_",helper,"_",temp$ID)
temp<-temp[grep("A_A_A_p",temp$NewID),]
input<-rbind(input,temp)


temp<-read.xlsx(paste0(dir,"Supplemental Data 2.xlsx"),sheet = 5,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"adult"
temp$Relapse<-FALSE
temp$NewID<-paste0("B_B_A_a_",temp$ID)
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 2.xlsx"),sheet = 7,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"pediatric"
temp$Relapse<-FALSE
temp$NewID<-paste0("B_A_A_a_",temp$ID)
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 2.xlsx"),sheet = 8,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"pediatric"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("B_A_A_",helper,"_",temp$ID)
temp<-temp[grep("B_A_A_p",temp$NewID),]
input<-rbind(input,temp)




inputCNV<-input
categories<-c("A_A_A","A_B_A",
              "B_A_A","B_B_A")
names_categories<-c("A","B","C","D")
titles<-c("T-ALL pediatric",
          "T-ALL adult",
          "T-LBL pediatric",
          "T-LBL adult")

chr<-c(1:22,"X","Y")
genome<-c(0,
          249250621,
          243199373,
          198022430,
          191154276,
          180915260,
          171115067,
          159138663,
          146364022,
          141213431,
          135534747,
          135006516,
          133851895,
          115169878,
          107349540,
          102531392,
          90354753,
          81195210,
          78077248,
          59128983,
          63025520,
          48129895,
          51304566,
          155270560,
          59373566)
genome_sum<-c()
for(j in 1:length(genome)){
  genome_sum<-c(genome_sum,sum(genome[c(1:j)],na.rm=T))
}




for(i in 1:length(categories)){
  message(i)
  temp<-inputCNV[grep(categories[i],inputCNV$NewID),]
  samples_in_cat<-sort(unique(temp$ID))
  
  temp2<-temp[,c(1,2,3,4,5)]
  names(temp2)<-c("sample","chromosome","start","end","segmean")
  temp2<-temp2[!is.na(temp2$start),]
  
  temp2_del<-temp2[temp2$segmean==1,]
  out_del<-cnFreq(temp2_del,CN_low_cutoff = 0,CN_high_cutoff = 0.1,genome = "hg19",y_title_size = 0,out = "data")$data
  output<-cbind(out_del[,c(1,2,3)],count=out_del[,4],Type=1)
  
  temp2_loh<-temp2[temp2$segmean==2,]
  if(nrow(temp2_loh)>0){
    out_loh<-cnFreq(temp2_loh,CN_low_cutoff = 0,CN_high_cutoff = 0.1,genome = "hg19",y_title_size = 0,out = "data")$data
    output<-rbind(output,cbind(out_loh[,c(1,2,3)],count=out_loh[,4],Type=2))
  }
  
  temp2_dup<-temp2[temp2$segmean==3,]
  if(nrow(temp2_dup)>0){
    out_dup<-cnFreq(temp2_dup,CN_low_cutoff = 0,CN_high_cutoff = 0.1,genome = "hg19",y_title_size = 0,out = "data")$data
    output<-rbind(output,cbind(out_dup[,c(1,2,3)],count=out_dup[,4],Type=3))
  }
  
  temp2_del0<-temp2[temp2$segmean==0,]
  if(nrow(temp2_del0)>0){
    out_del0<-cnFreq(temp2_del0,CN_low_cutoff = 0,CN_high_cutoff = 0.1,genome = "hg19",y_title_size = 0,out = "data")$data
    output<-rbind(output,cbind(out_del0[,c(1,2,3)],count=out_del0[,4],Type=0))
  }
  temp2_dup4<-temp2[temp2$segmean==4,]
  if(nrow(temp2_dup4)>0){
    out_dup4<-cnFreq(temp2_dup4,CN_low_cutoff = 0,CN_high_cutoff = 0.1,genome = "hg19",y_title_size = 0,out = "data")$data
    output<-rbind(output,cbind(out_dup4[,c(1,2,3)],count=out_dup4[,4],Type=4))
  }
  
  output$chromosome<-as.character(output$chromosome)
  output$chromosome<-substring(output$chromosome,4,nchar(output$chromosome))
  output$chromosome<-gsub("X",23,output$chromosome)
  output$chromosome<-as.numeric(output$chromosome)
  
  output$rel<-2*output$count/length(samples_in_cat)
  
  png(paste0(dir,"Figure 3",names_categories[i],".png"),height=600,width=1500)
  par(mar=c(3,7,5,0.5),mgp=c(3,1,0))
  plot(NULL,xlim=c(0,sum(genome)),ylim=c(0,5),xlab="",ylab="",xaxt="n",yaxt="n",cex.lab=2,cex.axis=1.5,xaxs="i",yaxs="i",
       main=paste0(titles[i]," (n=",length(samples_in_cat),")"),cex.main=3)
  for(j in 1:23){
    if(j%%2==1)
      rect(xleft = genome_sum[j],xright = genome_sum[j+1],ybottom = 0,ytop = 5,col="grey90",border = NA)
  }
  
  for(j in 1:length(output[,1])){
    if(!is.na(output$chromosome[j])){
      if(output$Type[j]==1|output$Type[j]==0){
        rect(xleft=(sum(genome[1:output$chromosome[j]])+output$start[j]), xright=(sum(genome[1:output$chromosome[j]])+output$end[j]),
             ybottom=output$Type[j],ytop=output$Type[j]+output$rel[j],border = rgb(139/255,58/255,58/255),col = rgb(139/255,58/255,58/255),lwd=5)
      }
      if(output$Type[j]==2){
        rect(xleft=(sum(genome[1:output$chromosome[j]])+output$start[j]), xright=(sum(genome[1:output$chromosome[j]])+output$end[j]),
             ybottom=output$Type[j],ytop=output$Type[j]+output$rel[j],border = rgb(205/255,155/255,29/255),col = rgb(205/255,155/255,29/255),lwd=5)
      }
      if(output$Type[j]==3|output$Type[j]==4){
        rect(xleft=(sum(genome[1:output$chromosome[j]])+output$start[j]), xright=(sum(genome[1:output$chromosome[j]])+output$end[j]),
             ybottom=output$Type[j],ytop=output$Type[j]+output$rel[j],border =  rgb(82/255,139/255,139/255),col = rgb(82/255,139/255,139/255),lwd=5)
      }
    }
  }
  
  axis(1,at=genome_sum,labels=NA,lwd=3)
  axis(1,at=(genome_sum[2:16]+genome_sum[1:15])/2,tick = F,labels=c(1:15),cex.axis=2,font=2)
  axis(1,at=(genome_sum[24:25]+genome_sum[23:24])/2,tick = F,labels=c("X","Y"),cex.axis=2,font=2)
  
  axis(1,at=(genome_sum[17]+genome_sum[16])/2,tick = F,labels=c(16),cex.axis=1.8,font=2)
  axis(1,at=(genome_sum[18]+genome_sum[17])/2,tick = F,labels=c(17),cex.axis=1.7,font=2)
  axis(1,at=(genome_sum[19]+genome_sum[18])/2,tick = F,labels=c(18),cex.axis=1.7,font=2)
  axis(1,at=(genome_sum[20]+genome_sum[19])/2,tick = F,labels=c(19),cex.axis=1.6,font=2)
  axis(1,at=(genome_sum[21]+genome_sum[20])/2,tick = F,labels=c(20),cex.axis=1.5,font=2)
  axis(1,at=(genome_sum[22]+genome_sum[21])/2,tick = F,labels=c(21),cex.axis=1.5,font=2)
  axis(1,at=(genome_sum[23]+genome_sum[22])/2,tick = F,labels=c(22),cex.axis=1.4,font=2)

  points(x=c(genome_sum[1],genome_sum[25]),y=c(1,1),type="l",col="black",lwd=2)
  points(x=c(genome_sum[1],genome_sum[25]),y=c(2,2),type="l",col="black",lwd=2)
  points(x=c(genome_sum[1],genome_sum[25]),y=c(3,3),type="l",col="black",lwd=2)
  points(x=c(genome_sum[1],genome_sum[25]),y=c(4,4),type="l",col="black",lwd=2)
  
  axis(2,at=c(0,1,2,3,4,5),labels=NA)
  axis(2,at=c(2,3,5),labels=NA,tck = -0.06)
  points(x=c(0,sum(genome)),y=c(5,5),type="l")
  
  par(mgp=c(3,4.2,0))
  axis(2,at=c(1,2.5,4),labels=c("del","LOH","amp"),tick = F,cex.axis=3)
  
  par(mgp=c(3,0.5,0))
  axis(2,at=seq(0,1,0.25),labels=paste0(seq(0,50,12.5),"%"),las=2,cex.axis=1.5)
  
  dev.off()
}



