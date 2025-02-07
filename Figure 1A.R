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
input<-temp

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 2,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"adult"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("A_B_B_",helper,"_",temp$ID)
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 3,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"pediatric"
temp$Relapse<-FALSE
temp$NewID<-paste0("A_A_A_a_",temp$ID)
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 4,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-ALL"
temp$Category<-"pediatric"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("A_A_B_",helper,"_",temp$ID)
input<-rbind(input,temp)


temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 5,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"adult"
temp$Relapse<-FALSE
temp$NewID<-paste0("B_B_A_a_",temp$ID)
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 6,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"adult"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("B_B_B_",helper,"_",temp$ID)
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 7,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"pediatric"
temp$Relapse<-FALSE
temp$NewID<-paste0("B_A_A_a_",temp$ID)
input<-rbind(input,temp)

temp<-read.xlsx(paste0(dir,"Supplemental Data 1.xlsx"),sheet = 8,startRow = 2)
temp<-temp[temp$Chr!="no data",]
temp$Chr[temp$Chr=="no Variants"]<-NA
temp$Disease<-"T-LBL"
temp$Category<-"pediatric"
temp$Relapse<-TRUE
helper<-str_split_fixed(temp$ID,"_",n=Inf)[,2]
temp$NewID<-paste0("B_A_B_",helper,"_",temp$ID)
input<-rbind(input,temp)


input[!is.na(input[,13])&input[,13]<threshold,c(2:17)]<-NA
input<-unique(input)

input$Type2<-NA
for(i in 1:length(input[,1])){
  if(!is.na(input$Type[i])){
    if(input$Type[i]=="5_prime_UTR_premature_start_codon_gain_variant"){
      input$Type2[i]<-"Start_Gain"
    }
    if(input$Type[i]=="disruptive_inframe_deletion"||input$Type[i]=="inframe_deletion"
       ||input$Type[i]=="missense_variant+disruptive_inframe_deletion"
       ||input$Type[i]=="missense_variant+inframe_deletion"
       ||input$Type[i]=="stop_gained+disruptive_inframe_deletion"
       ||input$Type[i]=="inframe_deletion+synonymous_variant"
    ){
      input$Type2[i]<-"In_Frame_Del"
    }
    if(input$Type[i]=="disruptive_inframe_insertion"||input$Type[i]=="inframe_insertion"
       ||input$Type[i]=="inframe_insertion+synonymous_variant"
       ||input$Type[i]=="missense_variant+disruptive_inframe_insertion"
       ||input$Type[i]=="stop_gained+disruptive_inframe_insertion"
       ||input$Type[i]=="stop_gained+inframe_insertion"
       ||input$Type[i]=="stop_gained+missense_variant+inframe_insertion" 
       ||input$Type[i]=="missense_variant+inframe_insertion"){
      input$Type2[i]<-"In_Frame_Ins"
    }
    if(input$Type[i]=="frameshift_variant+splice_region_variant"
       ||input$Type[i]=="frameshift_variant+splice_region_variant+synonymous_variant"
       ||input$Type[i]=="missense_variant+inframe_deletion+splice_region_variant"
       ||input$Type[i]=="missense_variant+splice_region_variant"
       ||input$Type[i]=="splice_acceptor_variant+splice_region_variant+intron_variant"
       ||input$Type[i]=="splice_region_variant+synonymous_variant"
       ||input$Type[i]=="stop_gained+splice_acceptor_variant+missense_variant+splice_region_variant+intron_variant"
       ||input$Type[i]=="stop_gained+splice_region_variant"
       ||input$Type[i]=="splice_donor_variant+disruptive_inframe_deletion+splice_region_variant+intron_variant"
       ||input$Type[i]=="splice_donor_variant+intron_variant"
       ||input$Type[i]=="stop_gained;splice_region_variant"
       ||input$Type[i]=="splice_acceptor_variant+intron_variant"
       ||input$Type[i]=="splice_acceptor_variant+disruptive_inframe_deletion+splice_region_variant+synonymous_variant+intron_variant"
       ||input$Type[i]=="frameshift_variant+splice_donor_variant+splice_region_variant+intron_variant"
       ||input$Type[i]=="stop_gained+splice_acceptor_variant+missense_variant+inframe_deletion+splice_region_variant+intron_variant"
       ||input$Type[i]=="frameshift_variant+splice_donor_variant+missense_variant+splice_region_variant+intron_variant"
       ||input$Type[i]=="missense_variant+disruptive_inframe_deletion+splice_region_variant"
    ){
      input$Type2[i]<-"Splice_Site"
    }
    if(input$Type[i]=="synonymous_variant"){
      input$Type2[i]<-"Silent"
    }
    if(input$Type[i]=="missense_variant"
       ||input$Type[i]=="start_lost;missense_variant"){
      input$Type2[i]<-"Missense_Mutation"
    }
    if(input$Type[i]=="stop_gained"){
      input$Type2[i]<-"Nonsense_Mutation"
    }
    if(input$Type[i]=="start_lost"
       ||input$Type[i]=="initiator_codon_variant"){
      input$Type2[i]<-"Start_Lost"
    }
    if(input$Type[i]=="stop_lost"){
      input$Type2[i]<-"Stop_Lost"
    }
    if(input$Type[i]=="frameshift_variant"
       ||input$Type[i]=="frameshift_variant+missense_variant"
       ||input$Type[i]=="frameshift_variant+stop_gained"
       ||input$Type[i]=="frameshift_variant+stop_gained+missense_variant"
       ||input$Type[i]=="frameshift_variant+stop_lost"
       ||input$Type[i]=="frameshift_variant+synonymous_variant"
       ||input$Type[i]=="frameshift_variant+missense_variant"){
      if(nchar(input$Ref[i])==1&&nchar(input$Alt[i])>1){
        input$Type2[i]<-"Frame_Shift_Ins"
      }
      if(nchar(input$Alt[i])==1&&nchar(input$Ref[i])>1){
        input$Type2[i]<-"Frame_Shift_Del"
      }
      if(nchar(input$Ref[i])>1&&nchar(input$Alt[i])>1){
        input$Type2[i]<-"Frame_Shift_InDel"
      }
    }
  }
}

input$CombinedCategory<-substr(input$NewID,1,7)
input$GeneHitlist<-8

gene1<-input$NewID[!is.na(input$Gene)&input$Gene=="NOTCH1"]
gene2<-input$NewID[!is.na(input$Gene)&input$Gene=="PHF6"]
gene3<-input$NewID[!is.na(input$Gene)&input$Gene=="FBXW7"]

input$GeneHitlist[input$NewID%in%gene3]<-7
input$GeneHitlist[input$NewID%in%gene2]<-6
input$GeneHitlist[input$NewID%in%intersect(gene2,gene3)]<-5
input$GeneHitlist[input$NewID%in%gene1]<-4
input$GeneHitlist[input$NewID%in%intersect(gene1,gene3)]<-3
input$GeneHitlist[input$NewID%in%intersect(gene1,gene2)]<-2
input$GeneHitlist[input$NewID%in%intersect(gene1,intersect(gene2,gene3))]<-1


input$GenePathway<-input$Gene
pathways<-read.table(paste0(dir,"Pathways.txt"),header=T,sep="\t")
for(i in 1:length(pathways[,1])){
  input$GenePathway[which(!is.na(input$Gene)&input$Gene==pathways$Gene[i])]<-paste0(pathways$Pathway[i],"_",pathways$Gene[i])
}


input<-input[order(input$CombinedCategory,input$GeneHitlist),]

helpID<-input$NewID
helpID2<-c()
for(i in 1:length(helpID)){
  if(sum(helpID[i]==helpID2)>0){
    helpID[i]<-NA
  }else{
    helpID2<-c(helpID2,helpID[i])
  }
}

temp<-as.matrix(table(input$NewID,input$GenePathway))

temp<-temp[match(helpID2,rownames(temp)),]

temp2<-temp
temp2<-temp2!=0
temp3<-colSums(temp2)
temp3<-temp3[order(temp3,decreasing = F)]
temp3<-temp3[order(str_split_fixed(names(temp3),"_",Inf)[,1],decreasing=T)]

temp<-temp[,match(names(temp3),colnames(temp))]
colnames_alt<-str_split_fixed(colnames(temp),"_",Inf)[,1]
colnames(temp)<-str_split_fixed(colnames(temp),"_",Inf)[,2]



metadata<-read.xlsx(paste0(dir,"Supplemental Data 0.xlsx"),sheet = 1,startRow = 2)
farben_followup<-colorRampPalette(colors=c("lightblue1","darkblue"))(max(metadata[,20],na.rm=T))
farben_relapse<-colorRampPalette(colors=c("darkred","lightpink1"))(max(metadata[,c(13,16,19)],na.rm=T))

metadata_new<-data.frame(ID=row.names(temp),Time=NA)
for(i in 1:length(metadata_new[,1])){
  if(length(grep("_a_",metadata_new$ID[i]))>0&&substr(metadata_new$ID[i],3,3)=="A"){
    metadata_new$Time[i]<-metadata[metadata$ID==substr(metadata_new$ID,start = 9,stop=14)[i],20]
  }
  if(length(grep("_p_",metadata_new$ID[i]))>0&&substr(metadata_new$ID[i],3,3)=="A"){
    metadata_new$Time[i]<-metadata[metadata$ID==substr(metadata_new$ID,start = 9,stop=14)[i],13]
  }
  if(length(grep("_r_",metadata_new$ID[i]))>0&&substr(metadata_new$ID[i],3,3)=="A"){
    metadata_new$Time[i]<-metadata[metadata$ID==substr(metadata_new$ID,start = 9,stop=14)[i],13]
  }
  if(length(grep("_r1_",metadata_new$ID[i]))>0&&substr(metadata_new$ID[i],3,3)=="A"){
    metadata_new$Time[i]<-metadata[metadata$ID==substr(metadata_new$ID,start = 10,stop=15)[i],13]
  }
  if(length(grep("_r2_",metadata_new$ID[i]))>0&&substr(metadata_new$ID[i],3,3)=="A"){
    metadata_new$Time[i]<-metadata[metadata$ID==substr(metadata_new$ID,start = 10,stop=15)[i],16]
  }
  if(length(grep("_r3_",metadata_new$ID[i]))>0&&substr(metadata_new$ID[i],3,3)=="A"){
    metadata_new$Time[i]<-metadata[metadata$ID==substr(metadata_new$ID,start = 10,stop=15)[i],19]
  }
}
  



png(paste0(dir,"/Main.png"),width=1800,height=1000)
par(mar=c(0.5,6.2,5,4.2))
plot(NULL,xlim=c(0,length(temp[,1])),ylim=c(0,length(temp[1,])+5),xaxt="n",yaxt="n",xlab="",ylab="",xaxs="i",yaxs="i",bty="n")

for(i in 1:length(temp[,1])){
  points(x=c(i,i),y=c(0,length(temp[1,])),type="l",col="grey80")
}
for(i in 1:length(temp[1,])){
  points(x=c(0,length(temp[,1])),y=c(i,i),type="l",col="grey80")
}


for(i in 1:length(temp[,1])){
  for(j in 1:length(temp[1,])){
    if(temp[i,j]!=0){
      if(temp[i,j]>1){
        rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="black",border = NA)
      }
      else{
        sample<-row.names(temp)[i]
        gene<-colnames(temp)[j]
        typ<-input$Type2[input$NewID==sample&!is.na(input$Gene)&input$Gene==gene]
        if(typ=="Splice_Site"){
          rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="purple4",border = NA)
        }
        if(typ=="Missense_Mutation"){
          rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="firebrick1",border = NA)
        }
        if(typ=="Nonsense_Mutation"){
          rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="firebrick4",border = NA)
        }
        #if(typ=="Silent"){
        #  rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="lightpink",border = NA)
        #}
        if(typ=="Frame_Shift_Ins"){
          rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="lightblue1",border = NA)
        }
        if(typ=="Frame_Shift_Del"){
          rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="lightblue4",border = NA)
        }
        if(typ=="Frame_Shift_InDel"){
          rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="deepskyblue3",border = NA)
        }
        if(typ=="In_Frame_Ins"){
          rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="darkseagreen1",border = NA)
        }
        if(typ=="In_Frame_Del"){
          rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="darkseagreen3",border = NA)
        }
        #if(typ=="Start_Gain"){
        #  rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="goldenrod1",border = NA)
        #}
        if(typ=="Start_Lost"){
          rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="lightpink",border = NA)
        }
        #if(typ=="Stop_Lost"){
        #  rect(xleft = i-1,xright = i,ybottom = j-1,ytop = j,col="darkorange2",border = NA)
        #}
      }
    }
  }
}
for(i in 1:length(temp[,1])){
  if(length(grep("_a",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+3,ytop = length(temp[1,])+4,border = NA,col="darkslategray4")
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+2,ytop = length(temp[1,])+3,border = NA,col=farben_followup[metadata_new[i,2]])
  }
  if(length(grep("_p",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+3,ytop = length(temp[1,])+4,border = NA,col="darkslategray2")
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+1,ytop = length(temp[1,])+2,border = NA,col=farben_relapse[metadata_new[i,2]])
  }
  if(length(grep("_r",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+3,ytop = length(temp[1,])+4,border = NA,col="darkgoldenrod2")
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+1,ytop = length(temp[1,])+2,border = NA,col=farben_relapse[metadata_new[i,2]])
  }
  if(length(grep("A_B_A_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+3,ytop = length(temp[1,])+4,border = NA,col="darkslategray3")
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+2,ytop = length(temp[1,])+3,border = NA,col="white")
  }
  if(length(grep("B_B_A_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+3,ytop = length(temp[1,])+4,border = NA,col="darkslategray3")
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+2,ytop = length(temp[1,])+3,border = NA,col="white")
  }
  if(length(grep("^A_B_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+1,ytop = length(temp[1,])+2,border = NA,col="white")
  }
  if(length(grep("^B_B_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+1,ytop = length(temp[1,])+2,border = NA,col="white")
  }
  
  if(length(grep("A_A_A_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+4,ytop = length(temp[1,])+5,border = NA,col="brown1")
  }
  if(length(grep("A_A_B_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+4,ytop = length(temp[1,])+5,border = NA,col="brown1")
  }
  if(length(grep("A_B_A_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+4,ytop = length(temp[1,])+5,border = NA,col="brown4")
  }
  if(length(grep("A_B_B_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+4,ytop = length(temp[1,])+5,border = NA,col="brown4")
  }
  
  if(length(grep("B_A_A_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+4,ytop = length(temp[1,])+5,border = NA,col="lightblue1")
  }
  if(length(grep("B_A_B_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+4,ytop = length(temp[1,])+5,border = NA,col="lightblue1")
  }
  if(length(grep("B_B_A_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+4,ytop = length(temp[1,])+5,border = NA,col="deepskyblue")
  }
  if(length(grep("B_B_B_",row.names(temp)[i]))>0){
    rect(xleft = i-1,xright = i,ybottom = length(temp[1,])+4,ytop = length(temp[1,])+5,border = NA,col="deepskyblue")
  }
}
axis(2,at = seq(0.5,length(temp[1,]),1),labels = colnames(temp),las=2,tick=F,cex.axis=1.28)


temp2<-temp
temp2<-temp2!=0
axis(4,at= seq(0.5,length(temp[1,]),1),labels=format(paste0(100*round(colSums(temp2)/length(temp2[,1]),3),"%"),nsmall = 1),las=2,tick=F,cex.axis=1.25)

points(x=c(0,length(temp[,1])),y=c(0,0),type="l",lwd=3)
points(x=c(0,length(temp[,1])),y=c(length(temp[1,]),length(temp[1,])),type="l",lwd=3)
points(x=c(0,0),y=c(0,length(temp[1,])),type="l",lwd=3)
points(x=c(length(temp[,1]),length(temp[,1])),y=c(0,length(temp[1,])),type="l",lwd=3)

points(x=c(length(grep("^A_A_A_",row.names(temp))),
           length(grep("^A_A_A_",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=1,col="grey50")
points(x=c(length(grep("^A_A_A_",row.names(temp)))+length(grep("^A_A_B_p",row.names(temp))),
           length(grep("^A_A_A_",row.names(temp)))+length(grep("^A_A_B_p",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=1,col="grey50")

points(x=c(length(grep("^A_A_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=2)
points(x=c(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_A_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_A_",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=1,col="grey50")
points(x=c(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_A_",row.names(temp)))+length(grep("^A_B_B_p_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_A_",row.names(temp)))+length(grep("^A_B_B_p_",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=1,col="grey50")


points(x=c(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=3)


points(x=c(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_A_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_A_",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=1,col="grey50")

points(x=c(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_A_",row.names(temp)))+length(grep("^B_A_B_p_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_A_",row.names(temp)))+length(grep("^B_A_B_p_",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=1,col="grey50")
points(x=c(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=2)

points(x=c(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_",row.names(temp)))+length(grep("^B_B_A_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_",row.names(temp)))+length(grep("^B_B_A_",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=1,col="grey50")

points(x=c(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_",row.names(temp)))+length(grep("^B_B_A_",row.names(temp)))+length(grep("^B_B_B_p_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_",row.names(temp)))+length(grep("^B_B_A_",row.names(temp)))+length(grep("^B_B_B_p_",row.names(temp)))),
       y=c(0,length(temp[1,])),type="l",lwd=1,col="grey50")


points(x=c(0,length(temp[,1])),y=c(length(temp[1,])+1,length(temp[1,])+1),type="l",lwd=3)
points(x=c(0,length(temp[,1])),y=c(length(temp[1,])+3,length(temp[1,])+3),type="l",lwd=3)
points(x=c(0,length(temp[,1])),y=c(length(temp[1,])+5,length(temp[1,])+5),type="l",lwd=3)
points(x=c(0,0),y=c(length(temp[1,])+1,length(temp[1,])+5),type="l",lwd=3)
points(x=c(length(temp[,1]),length(temp[,1])),y=c(length(temp[1,])+1,length(temp[1,])+5),type="l",lwd=3)

points(x=c(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))),
       y=c(length(temp[1,])+1,length(temp[1,])+5),type="l",lwd=3)
points(x=c(length(grep("^A_A_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))),
       y=c(length(temp[1,])+1,length(temp[1,])+5),type="l",lwd=1)
points(x=c(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_",row.names(temp))),
           length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_",row.names(temp)))),
       y=c(length(temp[1,])+1,length(temp[1,])+5),type="l",lwd=1)



text(x=(length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp))))/2,
     y=length(temp[1,])+7.5,labels = "T-ALL",font=2,cex=3,xpd=T)
text(x=(length(grep("^A_A_",row.names(temp))))/2,
     y=length(temp[1,])+6,labels = "pediatric",font=2,cex=2,xpd=T)
text(x=length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))/2,
     y=length(temp[1,])+6,labels = "adult",font=2,cex=2,xpd=T)

text(x=length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+(length(grep("^B_A_",row.names(temp)))+length(grep("^B_B_",row.names(temp))))/2,
     y=length(temp[1,])+7.5,labels = "T-LBL",font=2,cex=3,xpd=T)
text(x=length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_",row.names(temp)))/2,
     y=length(temp[1,])+6,labels = "pediatric",font=2,cex=2,xpd=T)
text(x=length(grep("^A_A_",row.names(temp)))+length(grep("^A_B_",row.names(temp)))+length(grep("^B_A_",row.names(temp)))+length(grep("^B_B_",row.names(temp)))/2,
     y=length(temp[1,])+6,labels = "adult",font=2,cex=2,xpd=T)


counts_pathway<-rev(table(colnames_alt))
for(i in 1:length(counts_pathway)){
  points(x=c(0,length(temp[,1])),
         y=c(sum(counts_pathway[1:i]),sum(counts_pathway[1:i])),type="l",lwd=3)
}
dev.off()

names(counts_pathway)[names(counts_pathway)=="Zother"]<-"other"

png(paste0(dir,"Side_left.png"),width=200,height=1000)
par(mar=c(0.5,0,7.2,0))
plot(NULL,xlim=c(0,2),ylim=c(0,length(temp2[1,])+3),bty="n",xaxt="n",yaxt="n",xaxs="i",yaxs="i")
for(i in 1:length(counts_pathway)){
  if(nchar(names(counts_pathway)[i])>15){
    path_name<-gsub(" ","\n",names(counts_pathway[i]),fixed = T)
  }else{
    path_name<-names(counts_pathway)[i]
  }
  
  if(i==1){
    text(x=1,
         y=counts_pathway[i]/2,
         path_name,srt=0,cex=2)
  }else{
    text(x=1,
         y=sum(counts_pathway[1:(i-1)])+counts_pathway[i]/2,
         path_name,srt=0,cex=2)
  }
}
dev.off()



count_variants<-data.frame(Gene=colnames(temp),Multihit=0,Missense=0,Nonsense=0,
                           FrameshiftIns=0,FrameshiftDel=0,FrameshiftIndel=0,
                           InframeIns=0,InframeDel=0,StartLost=0,Splice=0)

for(i in 1:length(temp[,1])){
  for(j in 1:length(temp[1,])){
    if(temp[i,j]!=0){
      sample<-row.names(temp)[i]
      gene<-colnames(temp)[j]
      if(temp[i,j]>1){
        count_variants$Multihit[gene==count_variants$Gene]<-count_variants$Multihit[gene==count_variants$Gene]+1
      }
      else{
        typ<-input$Type2[input$NewID==sample&!is.na(input$Gene)&input$Gene==gene]
        if(typ=="Splice_Site"){
          count_variants$Splice[gene==count_variants$Gene]<-count_variants$Splice[gene==count_variants$Gene]+1
        }
        if(typ=="Missense_Mutation"){
          count_variants$Missense[gene==count_variants$Gene]<-count_variants$Missense[gene==count_variants$Gene]+1
        }
        if(typ=="Nonsense_Mutation"){
          count_variants$Nonsense[gene==count_variants$Gene]<-count_variants$Nonsense[gene==count_variants$Gene]+1
        }
        if(typ=="Silent"){
          count_variants$Silent[gene==count_variants$Gene]<-count_variants$Silent[gene==count_variants$Gene]+1
        }
        if(typ=="Frame_Shift_Ins"){
          count_variants$FrameshiftIns[gene==count_variants$Gene]<-count_variants$FrameshiftIns[gene==count_variants$Gene]+1
        }
        if(typ=="Frame_Shift_Del"){
          count_variants$FrameshiftDel[gene==count_variants$Gene]<-count_variants$FrameshiftDel[gene==count_variants$Gene]+1
        }
        if(typ=="Frame_Shift_InDel"){
          count_variants$FrameshiftIndel[gene==count_variants$Gene]<-count_variants$FrameshiftIndel[gene==count_variants$Gene]+1
        }
        if(typ=="In_Frame_Ins"){
          count_variants$InframeIns[gene==count_variants$Gene]<-count_variants$InframeIns[gene==count_variants$Gene]+1
        }
        if(typ=="In_Frame_Del"){
          count_variants$InframeDel[gene==count_variants$Gene]<-count_variants$InframeDel[gene==count_variants$Gene]+1
        }
        if(typ=="Start_Gain"){
          count_variants$StartGain[gene==count_variants$Gene]<-count_variants$StartGain[gene==count_variants$Gene]+1
        }
        if(typ=="Start_Lost"){
          count_variants$StartLost[gene==count_variants$Gene]<-count_variants$StartLost[gene==count_variants$Gene]+1
        }
        if(typ=="Stop_Lost"){
          count_variants$StopLost[gene==count_variants$Gene]<-count_variants$StopLost[gene==count_variants$Gene]+1
        }
      }
    }
  }
}

count_variants<-rbind(count_variants,c("",rep(0,10)),c("",rep(0,10)),c("",rep(0,10)))

png(paste0(dir,"Side_right.png"),width=400,height=1000)
par(mar=c(0.5,0.5,7.2,1))
barplot(height = t(as.matrix(count_variants[,c(2:length(count_variants[1,]))])),#names.arg = count_variants$Gene,
        beside = F,horiz = T,border = NA,axes = F,xlim=c(0,150),yaxs="i",xaxs="i",
        col = c("black","firebrick1","firebrick4","lightblue1","lightblue4","deepskyblue3",
                "darkseagreen1","darkseagreen3","lightpink","purple4"))
axis(3,at=c(0,50,100,150),cex.axis=1.3)

legend(x=55,y=61,
       legend = c("Alteration","","Multi-hit","Missense","Nonsense","Frameshift Insertion","Frameshift Deletion",
                  "Framshift Indel","In Frame Insertion","In Frame Deletion","Start Lost","Splice Site"),
       fill=c("white","white","black","firebrick1","firebrick4","lightblue1","lightblue4","deepskyblue3",
              "darkseagreen1","darkseagreen3","lightpink","purple4"),border = NA,bty="n",
       text.font = c(2,2,rep(1,10)),cex = c(2,1,rep(1.6,10)))

legend(x=55,y=30,
       legend=c("Condition","primary","primary, not relapsed","primary, relapsed","relapse"),
       fill=c("white","darkslategray3","darkslategray4","darkslategray2","darkgoldenrod2"),border = NA,bty="n",
       text.font = c(2,1,1,1,1),cex = c(2,1.6,1.6,1.6,1.6))

legend(x=55,y=16,
       legend=c("Follow-up [days]","5616","","0"),
       fill=c("white","white","white","white"),border = NA,bty="n",
       text.font = c(2,1,1,1),cex = c(1.5,1.5,1.3,1.5))
rect(xleft = 65,xright = 70,ybottom = 13,ytop = 12,col = farben_followup[5616],border = NA)
rect(xleft = 65,xright = 70,ybottom = 12,ytop = 11,col = farben_followup[3744],border = NA)
rect(xleft = 65,xright = 70,ybottom = 11,ytop = 10,col = farben_followup[1872],border = NA)
rect(xleft = 65,xright = 70,ybottom = 10,ytop = 9,col = farben_followup[1],border = NA)


legend(x=55,y=7,
       legend=c("Time to relapse [days]","1784","","0"),
       fill=c("white","white","white","white"),border = NA,bty="n",
       text.font = c(2,1,1,1),cex = c(1.5,1.5,1.3,1.5))
rect(xleft = 65,xright = 70,ybottom = 4,ytop = 3,col = farben_relapse[1784],border = NA)
rect(xleft = 65,xright = 70,ybottom = 3,ytop = 2,col = farben_relapse[1189],border = NA)
rect(xleft = 65,xright = 70,ybottom = 2,ytop = 1,col = farben_relapse[595],border = NA)
rect(xleft = 65,xright = 70,ybottom = 1,ytop = 0,col = farben_relapse[1],border = NA)
dev.off()


plot_main<-readPNG(paste0(dir,"Main.png"))
plot_side<-readPNG(paste0(dir,"Side_right.png"))
plot_side_left<-readPNG(paste0(dir,"Side_left.png"))

png(paste0(dir,"Figure 1A.png"),width=2400,height=1000)
layout(matrix(c(rep(3,2),rep(1,18),rep(2,4)),ncol = 24))
par(mar=c(0,0,0,0))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i",bty="n",xaxt="n",yaxt="n")
rasterImage(plot_main,0,0,1,1)

par(mar=c(0,0,0,0))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i",bty="n",xaxt="n",yaxt="n")
rasterImage(plot_side,0,0,1,1)

par(mar=c(0,0,0,0))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i",bty="n",xaxt="n",yaxt="n")
rasterImage(plot_side_left,0,0,1,1)
dev.off()




